


# don't run 
# install packages
# install.packages(
# c("opendatatoronto", 
#  "sf"
#  )
# )



# download polygons for Toronto neighborhoods ----------------------------------

  # get files for Toronto open data
  files <- opendatatoronto::list_package_resources("https://open.toronto.ca/dataset/neighbourhoods/") # API URL
  
  #.shp shapefiles
  shapefiles <- files %>%  dplyr::filter(tolower(format) == "shp")
  
  # path to first file
  shapefiles_path <- shapefiles$id[1]
  
  # download the polygons
  to_boundary <- opendatatoronto::get_resource(shapefiles_path) %>% 
    # transform to WGS84 coordinates
    sf::st_transform(crs = 4326)



# spatial map of homcides in Toronto neighborhoods -----------------------------
  
  # aggregate data into cumulative total number of homicides and shootings for each neighbourhood
  joined_agg <- joined %>%
    dplyr::filter(year <= 2025) %>%
    dplyr::group_by(neighbourhood) %>%
    dplyr::summarise(
      shootings = sum(shootings, na.rm = TRUE),
      homicides = sum(homicides, na.rm = TRUE),
      .groups = "drop"
      )

  # join the aggregated data to the city boundary file
  map_data <- to_boundary %>%
    dplyr::left_join(joined_agg, by = c("AREA_NA7" = "neighbourhood")) %>% # column denotes Toronto
    dplyr::mutate(
      shootings = tidyr::replace_na(shootings, 0),
      homicides = tidyr::replace_na(homicides, 0)
      )

  # spatial coordinate data points for the point map layer
  geocoordinates <- joined %>%
    dplyr::filter(!is.na(lat) & !is.na(lon) & lat > 0 & lon < 0) %>%
    dplyr::group_by(neighbourhood, lon, lat) %>%
    dplyr::summarise(
      shootings = sum(shootings, na.rm = TRUE),
      homicides = sum(homicides, na.rm = TRUE), 
      .groups = "drop"
      ) %>%
    sf::st_as_sf(
      coords = c("lon", "lat"), 
      crs = 4326
      )

  
  
# saptial map
fig03 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = to_boundary, fill = "#ffffff", color = "#C0C0C0", linewidth = 0.3) + # white background canvas
  ggplot2::geom_sf( # plot shootings as bubbles
    data = geocoordinates,
    ggplot2::aes(size = shootings, color = shootings), # let size and color intensity vary
    alpha = 0.65 
    ) +
  # color and size scales for shootings
  ggplot2::scale_color_gradient(
    low = "#FFE5E5", 
    high = "#990000", 
    guide = "legend" # forces the guides to become matching types
    ) +
  ggplot2::scale_size_continuous(range = c(2, 10)) +
  ggplot2::geom_sf( # overlay homicides as 'X' markers
    data = dplyr::filter(geocoordinates, homicides > 0),
    shape = 4, # 4 is the universal symbol for an 'X' marker           
    color = "#111111",     
    size = 2, 
    ggplot2::aes(stroke = homicides * 0.5), # scale the 'X' marker thickness by cumulative number of homicides
    show.legend = FALSE    # Keeps the focus on your unified shooting scale
    ) +
  # scale the stroke/thickness without error
  ggplot2::scale_linewidth_continuous(range = c(1, 3.5), name = "Fatal Homicides") + 
  # labels
  ggplot2::labs(
    title = "Spatial distribution of shootings in Toronto neighbourhoods, 2004-25",
    x = "Longitude",
    y = "Latitude",
    color = "Total Shootings",  
    size = "Total Shootings",
    caption = "Note: Fatal shooting markers (x) scaled by the cumulative number of homicides, 2004-25"
    ) +
  # theme
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA), 
    plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
    panel.grid = ggplot2::element_blank(), 
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    axis.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(
      color = "#555555", 
      size = 10, 
      hjust = 0, # left alignment
      margin = ggplot2::margin(t = 15) # empty space buffer above the figure note
      )
    )
print(fig03)

# output figure
output(
  filename = "fig03.png", 
  figure = fig03, 
  path = "~/Desktop", 
  width = 20, 
  height = 10
  ) 





# end .R script
