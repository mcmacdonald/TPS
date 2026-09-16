# This .R syntax file joins homicides and shootings together

# don't run 
# install packages
# install.packages(
  # c("dplyr", 
  # "tidyr", 
  # "stringr", 
  # "magrittr",
  # "ggplot2",
  # "scales",
  # "forecast",
  # "purrr"
  # )
  # )

# call pipe to workspace
`%>%` <- magrittr::`%>%`


# don't run
# load homicides from url
# homicides <- readr::read_csv(url("https://stg-arcgisazurecdataprod.az.arcgis.com/exportfiles-6104-20706/Homicides_Open_Data_ASR_RC_TBL_002_8369086210015881422.csv?sv=2025-05-05&st=2026-09-14T16%3A02%3A38Z&se=2026-09-14T17%3A07%3A38Z&sr=b&sp=r&sig=m5d1umT0rUblkADkFL9L1%2FLqQMGxADuYwyvjsTxEJoo%3D"))

# type of homicide
table(homicides$HOMICIDE_TYPE, useNA = "always")

# retain shootings
homicides <- dplyr::filter(homicides, HOMICIDE_TYPE == "Shooting")

# drop column
homicides <- dplyr::select(homicides, -HOMICIDE_TYPE)


# don't run
# load shootings from url
# shootings <- readr::read_csv(url("https://stg-arcgisazurecdataprod.az.arcgis.com/exportfiles-6104-20707/Shooting_and_Firearm_Discharges_Open_Data_-3025367010736391071.csv?sv=2025-05-05&st=2026-09-14T16%3A03%3A40Z&se=2026-09-14T17%3A08%3A40Z&sr=b&sp=r&sig=20xSG6NTsxWAQvt4ACuh1o7Wa4cHGK6DGiaJjrjvm6s%3D"))

# type of firearm incident
table(shootings$EVENT_TYPE, useNA = "always")

# retain shootings, drop firearm discharges
shootings <- dplyr::filter(shootings, EVENT_TYPE == "Shooting")

# drop column
shootings <- dplyr::select(shootings, -EVENT_TYPE)



# join shootings and homicide data

# first, predefine a column to note that case is in fact a homicide
homicides$HOMICIDE <- 1

# drop column
homicides <- homicides %>% dplyr::select(-OBJECTID); shootings <- shootings %>% dplyr::select(-OBJECTID)

# join datasets
joined <- dplyr::full_join(
  homicides, shootings, 
  by = "EVENT_UNIQUE_ID", # joiner column
  suffix = c("_HOMICIDE", "_SHOOTING") # add to distinguish the source columns
  ) 

# get list of overlapping columns that need imputation
shared_cols <- dplyr::intersect(names(homicides), names(shootings)) %>%
  dplyr::setdiff("EVENT_UNIQUE_ID")

# coalesce shared columns 
for (col in shared_cols) {
  
  # append suffix to column names
  homicide_cols <- paste0(col, "_HOMICIDE"); shootings_cols <- paste0(col, "_SHOOTING")
  
  # prioritize homicide data over shootings data because it is likely to be more accurate
  # ... but also impute any missing data in the homicides database with data from the shootings database
  joined[[col]] <- dplyr::coalesce(joined[[homicide_cols]], joined[[shootings_cols]])
}

# drop columns that end with suffixs
joined <- joined %>% 
  dplyr::select(
    -dplyr::ends_with("_HOMICIDE"), 
    -dplyr::ends_with("_SHOOTING")
    )

# recode missings = 0
joined <- joined %>%
  dplyr::mutate(HOMICIDE = tidyr::replace_na(HOMICIDE, 0))

# join column that denotes shootings
joined$SHOOTINGS <- 1

# recode shootings that result in death
joined$DEATH[joined$DEATH > 1] <- 1 # values range 0, 1, 2, 3

# cross-tabulation of homicides and deaths recorded in the shootings data
table(joined$HOMICIDE, joined$DEATH) # good sign - strong agreement in the coding



# select columns
joined <- joined %>% dplyr::select(
  HOMICIDE, 
  SHOOTINGS, 
  OCC_YEAR, 
  OCC_MONTH, 
  OCC_DAY, 
  OCC_DOW, 
  OCC_DOY, 
  DIVISION, 
  HOOD_158, 
  NEIGHBOURHOOD_158, 
  LONG_WGS84, 
  LAT_WGS84
  )

# rename columns
colnames(joined) <- c("homicides",
                      "shootings",
                      "year",
                      "month",
                      "day",
                      "dow",
                      "doy",
                      "division",
                      "hood",
                      "neighbourhood",
                      # column names for geocoding
                      "longitude",
                      "latitude"
                      )

# strip numeric strings from neighborhoods
joined$neighbourhood <- stringr::str_replace(joined$neighbourhood, "\\s\\(\\d+\\)", "")



# calculate baseline probability of shooting resulting in a homicide, controlling for annual variation 
logit <- stats::glm(homicides ~ 1 + factor(year), family = "binomial", data = joined)

# inverse logit function to calculate probability
inv_logit <- function(x){ 1 / (1 + exp(-x)) }

# probability of homicide
p <- inv_logit(logit$coefficients[[1]])
message(round(p * 100, digits = 0),"% or roughly 1-in-", round(1/p, digits = 0), " shootings result in a homicide, controlling for annual variation in shootings.")





# map fatal and non-fatal shootings across Toronto's 158 neighborhoods

# aggregate incidents by (x,y) coordinates
agg_shots <- joined %>%
  dplyr::group_by(geometry) %>%
  dplyr::summarise(
    total_shootings = dplyr::n(),
    total_homicides = sum(homicides, na.rm = TRUE),
    .groups = "drop"
    )

# plot the spatial map
fig01 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = tracts, 
    fill = "#ffffff", 
    color = "#D3D3D3", 
    linewidth = 0.2
    ) + 
  ggplot2::geom_sf( 
    data = agg_shots,
    shape = 16, # circles
    ggplot2::aes(size = total_shootings, color = total_shootings), 
    alpha = 0.65
    ) +
  ggplot2::scale_color_gradient(
    low = "#FFE5E5", 
    high = "#990000",
    name = "Total Shootings",
    breaks = c(5, 10, 15, 20, 25),
    guide = ggplot2::guide_legend(
      order = 1,
      override.aes = list(
        shape = 16,
        color = grDevices::colorRampPalette(c("#FFE5E5", "#990000"))(5), 
        size = seq(3.5, 12.5, length.out = 5) 
        )
      )
    ) +
  ggplot2::scale_size_continuous(
    range = c(3, 14), 
    guide = "none" 
    ) +
  ggplot2::geom_sf( # overlay 'X' markers inside the shooting bubbles
    data = dplyr::filter(agg_shots, total_homicides > 0), 
    shape = 4,                       
    color = "#111111",     
    size = 2.0, 
    ggplot2::aes(stroke = total_homicides),
    show.legend = FALSE 
    ) +
  ggplot2::continuous_scale( # vary thickness scale for the 'X" markers
    aesthetics = "stroke", 
    scale_name = "stroke_scale",
    palette = scales::rescale_pal(c(0.8, 3.5)), 
    name = "Fatal Homicides",
    guide = "none"
    ) + 
  ggplot2::coord_sf( # Toronto city boundary
    xlim = c(-79.65, -79.15), 
    ylim = c(43.55, 43.85), 
    expand = FALSE
    ) +
  ggplot2::labs(
    title = "Spatial distribution of shootings in Toronto neighbourhoods, 2004-25",
    x = "Longitude",
    y = "Latitude",
    caption = "Note: Fatal shooting markers (x) scaled by the cumulative number of homicides, 2004-25"
    ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA), 
    plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
    panel.grid = ggplot2::element_blank(), 
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.box = "vertical",
    plot.caption = ggplot2::element_text(
      color = "#555555", 
      size = 10, 
      hjust = 0, 
      margin = ggplot2::margin(t = 15) 
      )
    )
print(fig01)

# output high resolution graphic
output(
  filename = "fig01.png", 
  figure = fig01, 
  path = "~/Desktop/TPS/", 
  width = 20, 
  height = 10
  )



# forecast model to predict homicides given shootings in Toronto neighborhoods

# first, calculate R-squared for the panels
arima_r2 <- joined %>%
  dplyr::filter(year <= 2025) %>%
  dplyr::arrange(neighbourhood, year) %>% 
  dplyr::group_by(neighbourhood) %>%
  tidyr::nest() %>% 
  dplyr::mutate(
    # fit the forecast model
    model = purrr::map(data, ~ tryCatch(
      forecast::auto.arima(.x$homicides, xreg = .x$shootings),
      error = function(e) NULL
      )
      ),
    # calculate R-squared
    r2 = purrr::map2_dbl(data, model, function(df, mod) {
      if (is.null(mod)) return(NA)
      actual_vals <- df$homicides
      fitted_vals <- as.numeric(mod$fitted)
      return(cor(actual_vals, fitted_vals, use = "complete.obs")^2)
      }
    )
  ) %>%
  dplyr::select(neighbourhood, r2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(r2_label = paste0("R² == ", round(r2, 3)
                                  )
                )

# calculate cumulative number of homicides across the time series
homicides_cumulative <- joined %>%
  dplyr::filter(year <= 2025) %>%
  dplyr::group_by(neighbourhood) %>%
  dplyr::summarise(homicides_cumulative = sum(homicides, na.rm = TRUE), .groups = "drop")

# rank order the variance explained for each neighbourhood
plot_data <- arima_r2 %>%
  # drop any models that failed
  dplyr::filter(!is.na(r2)) %>%
  # join cumulative homicides
  dplyr::left_join(homicides_cumulative, by = "neighbourhood") %>%
  # order by R-squared value
  dplyr::mutate(neighbourhood = forcats::fct_reorder(neighbourhood, r2))

# plot the ordered dot plot
fig02 <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = r2, y = neighbourhood)) +
  ggplot2::geom_segment( # add horizontal lines that connect the dot to the axis for easy reading
    ggplot2::aes(x = 0, xend = r2, y = neighbourhood, yend = neighbourhood),
    color = "grey85",
    linewidth = 0.5
     ) +
  ggplot2::geom_point(ggplot2::aes(color = homicides_cumulative), size = 2.5) + # add the high-contrast data points
  # set continuous color gradient scaling to identify neighborhoods by number of homicides
  ggplot2::scale_color_gradient(
    low = "#fee5d9",  
    high = "#a50f15", 
    name = "Homicides\n(2004-25)"
    ) +
  ggplot2::scale_x_continuous( # set the x-axis limits
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0.01, 0.01)
    ) +
  ggplot2::labs(
    title = "Total variance explained in homicides in Toronto neighbourhoods, 2004-25",
    x = "The percentage of variance explained in the cumulative number of homicides given the cumulative number of shootings, 2004-25",
    y = "Neighbourhood"
    ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    plot.subtitle = ggplot2::element_text(size = 9, color = "grey30"),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 5, color = "#111111"),
    panel.grid.major.y = ggplot2::element_blank(), # get rid of horizontal background grids
    panel.grid.minor = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(face = "bold", size = 9),
    legend.text = ggplot2::element_text(size = 8)
    )
print(fig02)

# output figure
output(
  filename = "fig02.png", 
  figure = fig02, 
  path = "~/Desktop/TPS/", 
  width = 10, 
  height = 10
  )





# end .R script


