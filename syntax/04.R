# This .R syntax file downloads geometry

# don't run 
# install packages
# install.packages(
  # c("cancensus", 
    # "sf",
    # "sfdep",
    # "ggplot2",
    # "scales"
    # )
    # )

# retrieve data from cancensus -------------------------------------------------

# don't run

# set API key [get free key at https://censusmapper.ca]
# options(cancensus.api_key = "YOUR_API_KEY_HERE")

# or, automatically install API key permanently across future console sessions
# cancensus::set_cancensus_api_key("YOUR_API_KEY_HERE", install = TRUE)

# The coordinate reference system (CRS) code for the city of Toronto
crs <- 4326

# download the complete Canadian Census variables directory
var_list <- cancensus::list_census_vectors("CA21")
View(var_list)

# pull 2021 census tracts for the Toronto CMA, including geometry and demographics
tracts <- cancensus::get_census(
  dataset = "CA21", 
  # regions = list(CMA = "35535"), # 35535 is the unique boundary code for Toronto CMA
  regions = list(CSD = "3520005"),
  vectors = c(
    "v_CA21_1", # Population, 2021
    "v_CA21_7", # Land area in square kilometres
    "v_CA21_69", # 15 to 64 years
    "v_CA21_72", # 15 to 19 years
    "v_CA21_90", # 20 to 24 years
    "v_CA21_108", # 25 to 29 years
    "v_CA21_126", # 30 to 34 years
    "v_CA21_548", # One-parent-family households
    "v_CA21_906", # Median total income of household in 2020 ($)
    "v_CA21_1020", # 18 to 64 year
    "v_CA21_1189", # Non-official languages
    "v_CA21_4274", # Major repairs needed
    "v_CA21_4311", # Median value of dwellings ($) (60)
    "v_CA21_4306", # % of owner households with a mortgage (58)
    "v_CA21_4314", # % of tenant households in subsidized housing (61)
    "v_CA21_4822", # First generation
    "v_CA21_4846", # Refugees
    "v_CA21_4855", # Asylum claim before admission
    "v_CA21_5746", # Total - Mobility status 1 year ago
    "v_CA21_5821", # No certificate, diploma or degree
    "v_CA21_6502"  # Unemployed
    ), 
  level = "CT",
  geo_format = "sf"
  ) %>% 
  sf::st_transform(crs = crs) # city boundary



# don't run
# wrapper for spatial data i.e., an sf object ----------------------------------
# st_data <- function(data, unit = tracts, crs = crs){
  
  # transform to an sf object
  # data <- sf::st_as_sf(
    # data,
    # coords = c("longitude", "latitude"),
    # crs = crs
    # )

  # match the coordinate reference systems
  # data <- sf::st_transform(data, crs = sf::st_crs(unit))
  
  # spatial join (point-in-polygon)
  # data <- sf::st_join(data, unit, join = sf::st_intersects)
  
  # return
  # return(data)
# }
# joined <- st_data(
  # data = joined, 
  # unit = tracts, 
  # crs = crs
  # )
# autotheft <- st_data(
  # data = autotheft, 
  # unit = tracts,
  # crs = crs
  # )




# principal components analysis of neighborhood effects

# caclulate tract-level neighbourhood effects
df_tracts <- tracts %>%
  dplyr::mutate(
    pop                  = `v_CA21_1: Population, 2021`,
    pop_15_34            = `v_CA21_72: 15 to 19 years` + `v_CA21_90: 20 to 24 years` + `v_CA21_108: 25 to 29 years` + `v_CA21_126: 30 to 34 years`,
    pct_pop_15_34        = pop_15_34 / `v_CA21_1: Population, 2021`,
    pct_one_parent       = `v_CA21_548: One-parent-family households` / Households,
    pct_non_official_lang = `v_CA21_1189: Non-official languages` / `v_CA21_1: Population, 2021`,
    pct_major_repairs    = `v_CA21_4274: Major repairs needed` / Dwellings,
    pct_first_gen        = `v_CA21_4822: First generation` / `v_CA21_1: Population, 2021`,
    pct_refugees         = `v_CA21_4846: Refugees` / `v_CA21_1: Population, 2021`,
    pct_asylum           = `v_CA21_4855: Asylum claim before admission` / `v_CA21_1: Population, 2021`,
    pct_mobility         = `v_CA21_5746: Total - Mobility status 1 year ago` / `v_CA21_1: Population, 2021`,
    pct_no_credential    = `v_CA21_5821: No certificate, diploma or degree` / `v_CA21_1020: 18 to 64 years`,
    pct_unemployed       = `v_CA21_6502: Unemployed` / `v_CA21_1020: 18 to 64 years`,
    median_income        = `v_CA21_906: Median total income of household in 2020 ($)`,
    median_dwelling_value= `v_CA21_4311: Median value of dwellings ($) (60)`,
    pct_mortgage         = `v_CA21_4306: % of owner households with a mortgage (58)`,
    pct_subsidized       = `v_CA21_4314: % of tenant households in subsidized housing (61)`,
    Pd                   = `v_CA21_1: Population, 2021` / `v_CA21_7: Land area in square kilometres`
    )

# column names
x_vars <- c(
  "pct_pop_15_34", 
  "pct_one_parent", 
  "pct_non_official_lang",
  "pct_major_repairs", 
  "pct_first_gen", 
  "pct_refugees",
  "pct_asylum",
  "pct_mobility", 
  "pct_no_credential",
  "pct_unemployed",
  "median_income", 
  "median_dwelling_value", 
  "pct_mortgage",
  "pct_subsidized"
  )

# set aside demographic measures
pop <- df_tracts[, c(
  "pop", # population, 2021 census
  "pop_15_34", # population 15-34 years of age, 2021 census
  "Pd" # population density 
   )
  ]

# tract-level data
df_tracts <- df_tracts %>% sf::st_drop_geometry() %>% dplyr::select(GeoUID, dplyr::all_of(x_vars))


# don't run
# install.packages("missForest")

# impute missing values
set.seed(123)
imputed <- missForest::missForest(
  df_tracts %>%
    sf::st_drop_geometry() %>%
    dplyr::select(GeoUID, dplyr::all_of(x_vars)) %>% 
    dplyr::select(-GeoUID)
    )

# fill missing values with imputed values
df_tracts <- df_tracts %>% dplyr::select(GeoUID) %>% dplyr::bind_cols(imputed$ximp)

# don't run
# gives out-of-bag imputation error
# imputed$OOBerror



# pull columns for principal components analysis
df_pca <- df_tracts %>%
  sf::st_drop_geometry() %>%
  dplyr::select(GeoUID, dplyr::all_of(x_vars)) %>%
  na.omit()

# principal components analysis
pca_fit <- stats::prcomp(df_pca %>% dplyr::select(-GeoUID), scale. = TRUE, center = TRUE)

# cumulative variance explained
summary(pca_fit)

# inspect loadings to interpret/name components
pca_fit$rotation[, 1:4]

# don't run
# install.packages("factoextra")

# scree plot of the number of principal components
factoextra::fviz_eig(pca_fit, addlabels = TRUE, ylim = c(0, 60))

# factor loadings plot that visualize what variables define each component
factoextra::fviz_pca_var(
  pca_fit, 
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
  )

# ranked contribution of the principal components
factoextra::fviz_contrib(pca_fit, choice = "var", axes = 1, top = 10)
factoextra::fviz_contrib(pca_fit, choice = "var", axes = 2, top = 10)
factoextra::fviz_contrib(pca_fit, choice = "var", axes = 3, top = 10)
factoextra::fviz_contrib(pca_fit, choice = "var", axes = 4, top = 10)

# pull factor scores
pca_scores <- df_pca %>%
  dplyr::select(GeoUID) %>%
  dplyr::bind_cols(as.data.frame(pca_fit$x[, 1:4]) %>%
                     setNames(c( # name the solutions
                       "immigration",
                       "housing_precarity",
                       "housing_tenure",
                       "residential_mobility"
                       )
                     )
                   )

# join factor scores to tract-level data
df_tracts <- df_tracts %>% dplyr::left_join(pca_scores, by = "GeoUID")

# join population measures to tract-level data
df_tracts <- cbind(df_tracts, pop); rm(pop)

# sanity checks
message("Test whether the number of rows match?"); nrow(df_pca) == nrow(pca_scores)
message("Test whether there is no missing data?"); sum(is.na(df_tracts$immigration)) 





# close .R script


