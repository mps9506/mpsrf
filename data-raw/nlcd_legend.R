## Code to generate nlcd_legend

nlcd_legend <- tibble(class = c("Open Water",
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

usethis::use_data(nlcd_legend)
