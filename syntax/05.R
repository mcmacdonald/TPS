# This .R syntax aggregates point-level data (x,y) to tract-level of analysis

# don't run 
# install packages
# install.packages(
  # c("sf",
  # "ggplot2",
  # "scales"
  # )
  # )



# construct skeleton for tract-leve data
skeleton <- tidyr::crossing(GeoUID = tracts$GeoUID, year = as.character(2014:2025))

# aggregate shootings and fatal shootings
agg_shoot <- joined %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year >= 2014) %>%
  dplyr::filter(year <= 2025) %>%
  dplyr::group_by(GeoUID, year) %>%
  dplyr::summarise(
    shootings = sum(shootings, na.rm = TRUE),
    fatal = sum(homicides, na.rm = TRUE),
    .groups = "drop"
    )

# aggregate motor vehicle thefts
agg_auto <- autotheft %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year >= 2014) %>%
  dplyr::filter(year <= 2025) %>%
  dplyr::group_by(GeoUID, year) %>%
  dplyr::summarise( # plit into residential vs. non-residential premises, plus overall total
    mvt_total          = sum(mvt, na.rm = TRUE),
    mvt_residential    = sum(mvt[premises == "Residential"], na.rm = TRUE),
    mvt_nonresidential = sum(mvt[premises == "Non-residential"], na.rm = TRUE),
    .groups = "drop"
    )

# join together
crime <- skeleton %>%
  dplyr::left_join(agg_shoot, by = c("GeoUID", "year")) %>%
  dplyr::left_join(agg_auto,  by = c("GeoUID", "year")) %>%
  dplyr::mutate(
    dplyr::across(c(
      shootings, 
      fatal, 
      mvt_total, 
      mvt_residential, 
      mvt_nonresidential
      ), 
    ~ tidyr::replace_na(., 0)),
    non_fatal = shootings - fatal
    )



# join crime rates and neighbourhood effects together

# neighbourhood effects
neighbourhoodFx <- df_tracts %>%
  dplyr::select(
    GeoUID, 
    immigration, 
    housing_precarity, 
    housing_tenure, 
    residential_mobility, 
    pop,
    pop_15_34,
    Pd
    )

# join together
crime <- crime %>% dplyr::left_join(neighbourhoodFx, by = "GeoUID")



# calculate crime rates per 10,000 population
crime <- crime %>%
  dplyr::mutate(
    shootings = (shootings / pop) * 10000,
    fatal = (fatal / pop) * 10000,
    non_fatal = (non_fatal / pop) * 10000,
    mvt_total = (mvt_total / pop) * 10000,
    mvt_residential = (mvt_residential / pop) * 10000,
    mvt_nonresidential = (mvt_nonresidential / pop) * 10000
    ) %>%
  dplyr::mutate(
    dplyr::across(
      c(shootings, fatal, non_fatal, mvt_total, mvt_residential, mvt_nonresidential),
      ~ tidyr::replace_na(., 0)
      )
    )



# plot non-residiential motor vehicle thefts annually, 2014-25 

# attach shapefile to crime rates for plotting
mvt_annual <- tracts %>%
  dplyr::distinct(GeoUID, .keep_all = TRUE) %>%
  dplyr::select(GeoUID, geometry) %>%
  dplyr::left_join(
    crime %>% dplyr::select(GeoUID, year, mvt_nonresidential),
    by = "GeoUID"
    )

# print distribution to check for skewness
stats::quantile(crime$mvt_nonresidential, probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)

# plot motor vehcile thefts annually
fig03 <- ggplot2::ggplot(mvt_annual) +
  ggplot2::geom_sf(ggplot2::aes(fill = mvt_nonresidential), color = "grey40", linewidth = 0.1) +
  ggplot2::scale_fill_gradient(
    low = "white", 
    high = "#2c7bb9", 
    na.value = "grey90",
    limits = c(0, 200), 
    oob = scales::squish
    ) +
  ggplot2::facet_wrap(~ year) +
  ggplot2::labs(title = "MVTs (non-residential) per 10,000 population by census tract", fill = "MVTs") +
  ggplot2::theme_void() +
  ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
print(fig03)

# output figure
output(
  filename = "fig03.png", 
  figure = fig03, 
  path = "~/Desktop/TPS/", 
  width = 10, 
  height = 10
  )


# aggregate shootings and MVT to the census tract level
df_tracts <- tracts %>%
  dplyr::distinct(GeoUID, .keep_all = TRUE) %>%
  dplyr::select(GeoUID, geometry) %>%
  dplyr::left_join(
    crime %>% dplyr::select(GeoUID, year, mvt_nonresidential, shootings),
    by = "GeoUID"
    ) %>%
  # calculate the geometric average crime rates
  dplyr::mutate( # log
    mvt_nonresidential = log1p(mvt_nonresidential),
    shootings = log1p(shootings)
    ) %>%
  dplyr::group_by(GeoUID) %>%
  dplyr::summarise( # average
    mvt_nonresidential = mean(mvt_nonresidential, na.rm = TRUE),
    shootings = mean(shootings, na.rm = TRUE),
    geometry = dplyr::first(geometry),
    .groups = "drop"
    ) %>%
  dplyr::mutate( # exponentiate
    mvt_nonresidential = expm1(mvt_nonresidential),
    shootings = expm1(shootings)
    )


# clean geometry to drop problem tracts i.e., "zero neighbour island" tracts
df_tracts <- sf::st_make_valid(df_tracts)
cat("Invalid geometries:", sum(!sf::st_is_valid(df_tracts$geometry)), "\n")

# sanity check that each tract has neighbours
nb_check <- sfdep::st_contiguity(df_tracts$geometry)
cat("Tracts with zero neighbours:", sum(lengths(nb_check) == 0), "\n")



# wrapper function to manually compute bivariate Local Moran's I
moran <- function(data, x, y){
  
  # neighbour list
  nb <- spdep::poly2nb(data, queen = TRUE)
  
  # spatial weights 
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # standardized x
  x_std <- as.numeric(scale(x))
  # attach to data
  data$x_std <- x_std
  
  # standardized y
  y_std <- as.numeric(scale(y))
  # attach to data
  data$y_std <- y_std
  
  # spatial lag for y
  y_lag_std <- spdep::lag.listw(lw, y_std, zero.policy = TRUE)
  # attach to data
  data$y_lag_std <- y_lag_std
  
  # calculate local Moran's I
  data$local_moran_bv <- x_std * y_lag_std
  
  return(data)
}
df_tracts <- moran(
  data = df_tracts, 
  x = df_tracts$mvt_nonresidential, 
  y = df_tracts$shootings
  )

# don't run
# Moran's I at bivariate level of analysis 
# global_bv <- sum(x_std * y_lag_std, na.rm = TRUE) / sum(x_std^2, na.rm = TRUE)
# cat("Global bivariate Moran's I (approx):", global_bv, "\n")

# map variations in spatial autocorrelation
fig04 <- ggplot2::ggplot(data = df_tracts) +
  ggplot2::geom_sf(ggplot2::aes(fill = local_moran_bv), color = "black", linewidth = 0.1) +
  ggplot2::scale_fill_gradient2(
    low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0,
    na.value = "grey93",
    name = "Moran's I"
    ) +
  ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Spatial Correlation: Shooting Rates vs Auto Theft Rates",
    subtitle = "Local Moran's I, Toronto census tracts",
    x = "Longitude",
    y = "Latitude"
    )
print(fig04)

# output figure
output(
  filename = "fig04.png", 
  figure = fig04, 
  path = "~/Desktop/TPS/", 
  width = 10, 
  height = 10
  )
