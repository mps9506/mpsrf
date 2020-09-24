

#' Extract NLCD Areas
#'
#' Extracts and calculates the area of NLCD classes for a specified feature. Returns a long tibble with percent and area for each class and feature.
#'
#' @param feature feature to extract and calculate NLCD class areas. Can be class sp or sf with one or multiple features.
#' @param id_variable unquoted id variable in feature, must have unique value for each row.
#' @param area_units desired output units. probably one of \code{c("acres","mi^2")}. See \code{units::valid_udunits()} for more units.
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string
#' @param year An integer representing the year of desired NLCD product. Acceptable values are 2016 (default), 2011, 2008 (landcover only), 2006, 2004, and 2001.
#' @param dataset A character string representing type of the NLCD product. Acceptable values are 'Impervious', 'Land_Cover', 'Tree_Canopy' (2011 and 2016 only),
#'
#' @return something
#' @export
#' @importFrom FedData get_nlcd
#' @importFrom raster extract
#' @importFrom dplyr group_by summarize left_join select mutate n rowwise c_across
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom sf st_transform st_area
#' @importFrom units set_units
#' @importFrom tibble tibble as_tibble deframe
#' @importFrom rlang .data
extract_NLCD <- function(feature,
                         id_variable,
                         area_units = "acres",
                         crs,
                         year = 2016,
                         dataset = "Land_Cover") {

  ##check that feature is sp or sf

  ## download nlcd data using
  ## extent of the counties layer
  message("Downloading NLCD data, please wait...")
  NLCD <- FedData::get_nlcd(
    template = feature,
    year = year,
    dataset = dataset,
    label = "txnlcd",
    force.redo = TRUE
  )

  message("Extracting NLCD data...")
  ras_ext <- raster::extract(NLCD, feature,  df = TRUE, factors = TRUE)

  legend <- tibble::tibble(class = c("Open Water",
                                     "Perennial Ice/Snow",
                                     "Developed, Open Space",
                                     "Developed, Low Intensity",
                                     "Developed, Medium Intensity",
                                     "Developed, High Intensity",
                                     "Barren Land",
                                     "Deciduous Forest",
                                     "Evergreen Forest",
                                     "Mixed Forest",
                                     "Dwarf Scrub",
                                     "Shrub/Scrub",
                                     "Grassland/Herbaceous",
                                     "Sedge/Herbaceous",
                                     "Lichens",
                                     "Moss",
                                     "Pasture/Hay",
                                     "Cultivated Crops",
                                     "Woody Wetlands",
                                     "Emergent Herbaceous Wetlands"),
                           value = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95))

  ## make a vector of the unique feature ID
  feature_ID <- tibble::as_tibble(feature) %>%
    dplyr::select({{ id_variable}}) %>%
    tibble::deframe()

  message("Calculating...")
  ## calculates percent
  ras_ext %>%
    dplyr::group_by(.data$ID, .data$txnlcd_NLCD_2016_Land_Cover_L48_nlcd) %>%
    dplyr::summarize(land_cover = dplyr::n()) %>%
    dplyr::left_join(legend, by = c("txnlcd_NLCD_2016_Land_Cover_L48_nlcd" = "value")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$ID, .data$class, .data$land_cover) %>%
    tidyr::pivot_wider(names_from = .data$class,
                       values_from = .data$land_cover) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total = sum(dplyr::c_across(.data$`Open Water`:.data$`Emergent Herbaceous Wetlands`))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("{{id_variable}}" := feature_ID) %>%
    tidyr::pivot_longer(cols = .data$`Open Water`:.data$`Emergent Herbaceous Wetlands`,
                        names_to = "class",
                        values_to = "land_cover") %>%
    dplyr::mutate(percent = .data$land_cover/.data$Total * 100) -> nlcd_summary

  ## calculate acres in desired projection
  feature <- feature %>%
    sf::st_transform(crs) %>%
    dplyr::mutate(crs_area = sf::st_area(feature)) %>% ## in crs projection units
    dplyr::mutate(area = units::set_units(.data$crs_area, area_units, mode = "standard")) #%>% ## the units library can deal with some conversions automatically, these units are "sticky"
    #dplyr::mutate("{{ id_variable }}" := ID)
  message("y")
  print(tibble::as_tibble(feature))


  message("x")
  print(nlcd_summary)

  ##join area calculations to the nlcd summary tibble
  nlcd_summary %>%
    dplyr::left_join(tibble::as_tibble(feature) %>% ## as tibble so we remove the "geometry" which isn't needed here
                dplyr::select({{ id_variable }},  feature_area = .data$area)) %>%
    dplyr::mutate(land_use_area = .data$feature_area * (.data$percent/100)) -> nlcd_summary ## note that county area is put first so that units stick with the result

  ##create masked NLCD raster


  ##return summary and masked raster

  return(nlcd_summary)
}
