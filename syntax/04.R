


# don't run
# install packages
# install.packages(
  # c("opendatatoronto",
    # "spatialreg",
    # "spdep",
    # "CARBayesST" 
    # )
  # )



# re-download the polygons for Toronto neighborhoods so that this script is self-contained

# download from API URL for Toronto neighbourhoods
files <- opendatatoronto::list_package_resources("https://open.toronto.ca/dataset/neighbourhoods/")

# .shp shapefiles
shapefiles <- files %>% dplyr::filter(tolower(format) == "shp")

# download the neighbourhood boundaries
to_boundary <- opendatatoronto::get_resource(shapefiles$id[1]) %>% 
  sf::st_transform(crs = 4326) # transform to matching WGS84 coordinates



# Moran's I for autocorrelation ------------------------------------------------

# fetch point coordinates from the shape object
xy <- sf::st_coordinates(geocoordinates)

# define spatial neighbors
knn <- spdep::knearneigh(xy, k = 6) # find the 6 nearest neighboring points for every neighborhood centroid
neighbors <- spdep::knn2nb(knn)

# transform the neighbor list into a standardized structural weights matrix (row-standardized)
weights_list <- spdep::nb2listw(neighbors, style = "W")

# calculate the spatial lag for shootings i.e., the average number of shootings in adjacent neighbourhoods
geocoordinates$shootings_lag <- spdep::lag.listw(weights_list, geocoordinates$shootings)

# standardize (Z-score scale) variables so the chart center rests perfectly at (0, 0)
geocoordinates <- geocoordinates %>%
  dplyr::mutate(
    shootings_scaled = as.vector(scale(shootings)),
    lag_scaled = as.vector(scale(shootings_lag))
    )

# run the Global Moran's I hypothesis test for shootings
moran_shootings <- spdep::moran.test(geocoordinates$shootings, weights_list)

  # print
  print(moran_shootings)

# don't run
# run the Global Moran's I hypothesis test for homicides
# moran_homicides <- spdep::moran.test(geocoordinates$homicides, weights_list)

  # print 
  # print(moran_homicides)

# Moran's I scatterplot
ggplot2::ggplot(geocoordinates, ggplot2::aes(x = shootings_scaled, y = lag_scaled)) +
 
  # quadrant divider lines that intersect the origin point
  ggplot2::geom_vline(xintercept = 0, color = "#A0A0A0", linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 0, color = "#A0A0A0", linetype = "dashed") +
  
  # plot the neighborhoods as data markers
  ggplot2::geom_point(color = "#990000", alpha = 0.6, size = 2.5) +
  
  # the slope of this line is mathematically identical to the Global Moran's I index
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#111111", se = TRUE, linewidth = 1) +

  # labels
  ggplot2::labs(
    title = paste0("Moran's I (Index: ",  moran_shootings$estimate[1], ")"),
    x = "Shootings (Standardized Deviation from the Mean)",
    y = "Spatial Lag (Neighbor Average Deviation)",
    caption = "Note: The spatial clustering of shootings is statistically different from random chance (p < 0.001)"
    ) +

  # theme
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    axis.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
    )



# don't run
# alternatively, construct the Queens Contiguity Spatial Weights Matrix, where cells = 1 indicate shared borders
# queens <- spdep::poly2nb(map_data, queen = TRUE)

# calculate the spatial weights 
# weights_list_queens <- spdep::nb2listw(queens, style = "W", zero.policy = TRUE)

# calculate global Moran's I for shootings
#moran_shootings_queens <- spdep::moran.test(map_data$shootings, weights_list_queens, zero.policy = TRUE); print(moran_shootings_queens)

# calculate local Moran's I (lisa) for shootings to identify neighborhood clusters
# local_shootings_queens <- spdep::localmoran(map_data$shootings, weights_list_queens, zero.policy = TRUE)
# print(local_shootings_queens)

# calculate Global Moran's I for homicides
# moran_homicides_queens <- spdep::moran.test(map_data$homicides, weights_list_queens, zero.policy = TRUE); print(moran_homicides_queens) # print

# calculate local Moran's I (lisa) for homicides to identify neighborhood clusters
# local_homicides_queens <- spdep::localmoran(map_data$homicides, weights_list_queens, zero.policy = TRUE)
# print(local_homicides_queens)



# fit spatio-temporal model ----------------------------------------------------

# first, calculate 1-year lag effect for homicides
panels <- joined %>%
  dplyr::arrange(neighbourhood, year) %>% # sort by neighborhood and year 
  dplyr::group_by(neighbourhood) %>% # group by neighborhood
  dplyr::mutate(
    homicides_lag1 = dplyr::lag(homicides, n = 1) 
    ) %>%
  dplyr::ungroup() %>%
  # drop the first year (2004) because it won't have an observation for the prior year
  dplyr::filter(!is.na(homicides_lag1))

# transform the panels into a spatial object
panels <- panels %>%
  dplyr::filter(!is.na(lat) & !is.na(lon) & lat > 0 & lon < 0) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# spatial weights matrix
knn <- spdep::knearneigh(sf::st_coordinates(panels), k = 6)
neighbors <- spdep::knn2nb(knn)
weights_list <- spdep::nb2listw(neighbors, style = "W")

# fit spatial model
st_model_linear <- spatialreg::lagsarlm(
  formula = homicides ~ homicides_lag1 + factor(year), 
  data = panels, 
  listw = weights_list,
  zero.policy = TRUE
  )
# print
summary(st_model_linear)



# fit Bayesian spatio-temporal model -------------------------------------------

# first, construct panel data
panels <- joined %>% # CARBayesST package requires perfect structural ordering such that neighbourhoods must be alphabetically listed within years
  dplyr::filter(year <= 2025) %>%
  dplyr::arrange(year, neighbourhood)

# calculate the panel dimensions for the MCMC engine
N_neighborhoods <- length(unique(panels$neighbourhood)) # N = 158
T_years         <- length(unique(panels$year))          # N = 22 (2004-2025)

# neighbour weights matrix from polygons

  # first, sort the downloaded city polygon layer alphabetically by neighborhood name (AREA_NA7)  to guarantee it matches the sorting of your panel data exactly.
  xy <- to_boundary %>% dplyr::arrange(AREA_NA7)

  # extract touching borders from the verified polygon geometries
  neighbors <- spdep::poly2nb(xy, queen = TRUE)
  W_matrix  <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)
  
  # don't run
  # fit the Bayesian linear model
  # st_model_bayesian <- CARBayesST::ST.CARlinear(
    # formula = homicides ~ shootings + factor(neighbourhood), 
    # data = as.data.frame(panels),  # transform spatial data frame to a regular data frame for CARBayesST processing                   
    # family = "poisson", # overdispersion and zero-clumping
    # W = W_matrix, # function wants a symmetric binary adjacency matrix
    # burnin = 10000, # 10,000 warm-up tuning iterations
    # n.sample = 50000, # 50,000 sampling iterations
    # thin = 10 # save every 10th sample
    # )

# print
# print(st_model_bayesian)



# fit the autoregressive model
st_model_bayesian <- CARBayesST::ST.CARar(
  formula = homicides ~ shootings, 
  data = as.data.frame(panels),  # transform spatial data frame to a regular data frame for CARBayesST processing                   
  family = "poisson", # overdispersion and zero-clumping
  W = W_matrix, # function wants a symmetric binary adjacency matrix
  AR = 1, # first-order autoregressive term
  # compute 20,000 posterior draws: 120,000 sampling iterations - 10,000 tuning iterations / take every 5th sample
  burnin = 10000, # 10,000 warm-up tuning iterations
  n.sample = 110000, # 120,000 sampling iterations
  thin = 5 # save every 5th sample
  )

# print
print(st_model_bayesian)

# calculate the variance explained for the autoregressive model

  # posterior predicted values
  fitted_values <- st_model_bayesian$fitted.values
  
  # actual homicide counts
  actual_values <- panels$homicides
  
  # squared correlation
  r2 <- cor(observed_values, fitted_values, method = "pearson")^2
  print(paste0("R-squared: ", round(r2, 2)))



# pull posterior distributions for each effect from the model object
mcmc_samples <- data.frame( # join columns together
  # "Shootings" = as.numeric(st_model_bayesian$samples$beta[, 2]),
  # "Spatial lag"      = as.numeric(st_model_bayesian$samples$rho[, 1]),
  "Temporal lag"     = as.numeric(st_model_bayesian$samples$rho[, 2]),
  check.names = FALSE
  )

# pivot into a long-format panel grid layout
mcmc_long <- mcmc_samples %>%
  tidyr::pivot_longer(
    cols = tidyr::everything(), 
    names_to = "parameter", 
    values_to = "value"
    )

# calculate the median values for the posterior distributions
mcmc_medians <- mcmc_long %>%
  dplyr::group_by(parameter) %>%
  dplyr::summarise(median_val = median(value), .groups = "drop")

# plot curves for the posterior distributions
fig04 <- ggplot2::ggplot(mcmc_long, ggplot2::aes(x = value)) +
  
  # draw smooth density curves
  ggplot2::geom_density(fill = "#990000", alpha = 0.5, color = "#990000", linewidth = 0.8) +
  
  # plant vertical lines at the median
  ggplot2::geom_vline(
    data = mcmc_medians, 
    ggplot2::aes(xintercept = median_val), 
    color = "#111111", 
    linetype = "dashed", 
    linewidth = 0.8
    ) +
  
  # text label for median
  ggplot2::geom_text(
    data = mcmc_medians,
    ggplot2::aes(x = median_val, y = 0, label = paste0("Median: ", round(median_val, 2))),
    angle = 90,
    vjust = -0.5,
    hjust = -0.2,
    fontface = "bold",
    size = 2.5,
    color = "#111111"
    ) +
  
  # don't run
  # facet
  # ggplot2::facet_wrap(~ Parameter, scales = "free", ncol = 3) +
  
  # set range for the x-axis
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  
  # set range for the y-axis
  ggplot2::scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
  
  # labels for text
  ggplot2::labs(
    title = "Year-over-year temporal stability in homicides in Toronto neighbourhoods, 2004-25, controlling for the number of shootings and the spatial concentration of homicides",
    x = "The effect of homicides in the prior year on homicides in the following year in Toronto neighbourhoods",
    y = "Percentage of sample for each effect size (%)",
    caption = "Note: The figure illustrates the posterior distribution for 20,000 posterior draws calculated from an autogressive Bayesian spatio-temporial model. The posteriors illustrate the uncertainty for the estimated lag effect."
    ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 8),
    axis.title = ggplot2::element_text(face = "bold", size = 8),
    axis.text = ggplot2::element_text(size = 8, color = "#000000"),
    # don't run
    # strip.text = ggplot2::element_text(face = "bold", size = 10, color = "#000000"), 
    # strip.background = ggplot2::element_rect(fill = "#ffffff", color = NA),          
    panel.spacing = ggplot2::unit(1.5, "lines"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = 0, size = 7.5),
    plot.caption.position = "plot"
    )
print(fig04)

# output figure
output(
  filename = "fig04.png",
  figure = fig04,
  path = path,
  width = 10,
  height = 5
  )





# end .R script
