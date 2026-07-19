


# don't run 
# install packages
# install.packages(
# c("ggrepel",
# "ggplot2",
# "forecast",
# "forcats"
# )
# )



# path to folder 
path <- "~/Desktop"

# function to output high resolution images
output <- function(filename, figure, path = path, width = 10, height = 5){
  ggplot2::ggsave(
    filename,
    figure,
    path = path, 
    width = width, 
    height = height, 
    device = 'png', 
    dpi = 250 # larger DPI increases the size of the plot aesthetics 
    )
  }



# compute annual trends in shootings and homicides
trends <- joined %>%
  dplyr::select(year, homicides, shootings) %>%
  dplyr::filter(year <= 2025) %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(
    homicides = sum(homicides, na.rm = TRUE), 
    shootings = sum(shootings, na.rm = TRUE),
    .groups = "drop"
    )

# scaling factor i.e., # calculate the geometric mean ratio
rescale <- exp(mean(log(trends$shootings / trends$homicides)))
rescale <- round(rescale, digits = 0) # round to nearest integer

# plot homicides against shootings
fig01 <- ggplot2::ggplot(trends, ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = homicides * rescale, color = "Homicide"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(y = shootings, color = "Shooting"), linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(y = homicides * rescale, color = "Homicide")) +
  ggplot2::geom_point(ggplot2::aes(y = shootings, color = "Shooting")) +
  ggplot2::scale_x_continuous(breaks = seq(2004, 2026, by = 2)) +
  ggplot2::scale_y_continuous(
    name = "Annual number of shootings",
    sec.axis = ggplot2::sec_axis(~ . / rescale, name = "Annual number of homicides") # secondary axis
    ) +
  ggplot2::labs(color = "Type of incident", x = "Year") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = c(0.9, 0.9),
    legend.justification = c("right", "top"),
    legend.background = ggplot2::element_rect(fill = "white", color = "white")
    )
print(fig01)

# annual population estimates for 15-64 year olds for Toronto CMA from Statistics Canada
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710014801&pickMembers%5B0%5D=2.1&pickMembers%5B1%5D=3.112&cubeTimeFrame.startYear=2004&cubeTimeFrame.endYear=2025&referencePeriods=20040101%2C20250101
pop <- c(3621891,
         3688813,	
         3760575,	
         3818765,	
         3878172,	
         3939707,	
         4009341,	
         4073175,	
         4121247,
         4160205,
         4193719,
         4215858,	
         4261619,	
         4310015,
         4385226,	
         4460647,	
         4500202,	
         4473738,
         4567319,
         4771827,
         4981352,	
         4938877
         )

# join population estimates
trends <- cbind(trends, pop)

# calculate crude rates to adjust for changes in population
trends <- trends %>% 
  dplyr::mutate(
    homicide_rate = (homicides/pop) * 100000, 
    shooting_rate = (shootings/pop) * 100000
    )

# plot homicide rates per 100,000 against shootings per capita
fig01 <- ggplot2::ggplot(trends, ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = homicide_rate * rescale, color = "Homicide"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(y = shooting_rate, color = "Shooting"), linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(y = homicide_rate * rescale, color = "Homicide")) +
  ggplot2::geom_point(ggplot2::aes(y = shooting_rate, color = "Shooting")) +
  ggplot2::scale_x_continuous(breaks = seq(2004, 2025, by = 2)) +
  ggplot2::scale_y_continuous(
    name = "Annual number of shootings per 100,000 population, ages 15-64",
    sec.axis = ggplot2::sec_axis(~ . / rescale, name = "Annual number of homicides per 100,000 population, ages 15-64") # secondary axis
    ) +
  ggplot2::labs(color = "Type of incident", x = "Year") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = c(0.9, 0.9),
    legend.justification = c("right", "top"),
    legend.background = ggplot2::element_rect(fill = "white", color = "white")
    )
print(fig01)

# output figure
output(
  filename = "fig01.png", 
  figure = fig01, 
  path = "~/Desktop", 
  width = 10, 
  height = 5
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
  path = "~/Desktop", 
  width = 10, 
  height = 10
  )



# end .R script
