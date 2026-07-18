# This .R syntax file loads and joins homicides and shootings data 



# don't run 
# install packages
# install.packages(
  # c("dplyr", 
    # "tidyr", 
    # "stringr", 
    # "magrittr",
    # "readr",
    # "readxl",
    # )
  # )

# call pipe to workspace
`%>%` <- magrittr::`%>%`



# load open data from the Toronto Police Service
# https://www.tps.ca/data-maps/open-data/

# load homicides
homicides <- readr::read_csv(url("https://stg-arcgisazurecdataprod.az.arcgis.com/exportfiles-6104-20706/Homicides_Open_Data_ASR_RC_TBL_002_8369086210015881422.csv?sv=2025-05-05&st=2026-07-18T15%3A09%3A01Z&se=2026-07-18T16%3A14%3A01Z&sr=b&sp=r&sig=iV5jm0x2NNNwgEeH13Fvyf28lOLE3dpNkAHigYDolMM%3D"))

  # type of homicide
  table(homicides$HOMICIDE_TYPE, useNA = "always")

  # retain shootings
  homicides <- dplyr::filter(homicides, HOMICIDE_TYPE == "Shooting")
  
  # drop column
  homicides <- dplyr::select(homicides, -HOMICIDE_TYPE)
  


# load shootings from url
shootings <- readr::read_csv(url("https://stg-arcgisazurecdataprod.az.arcgis.com/exportfiles-6104-20707/Shooting_and_Firearm_Discharges_Open_Data_-3025367010736391071.csv?sv=2025-05-05&st=2026-07-18T15%3A03%3A53Z&se=2026-07-18T16%3A08%3A53Z&sr=b&sp=r&sig=elFasFGRlKZFvHLkI7Ywl%2Bxce7Fl%2FYX5X5AIapfcJeM%3D"))

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

# recode 
joined$DEATH[joined$DEATH > 1] <- 1 # values range 0, 1, 2, 3

# cross-tabulation of homicides and deaths recorded in the shootings data
table(joined$HOMICIDE, joined$DEATH) # good sign - strong agreement in the coding

# calculate baseline probability of shooting resulting in a homicide, controlling for annual variation 
logit <- stats::glm(HOMICIDE ~ 1 + factor(OCC_YEAR), family = "binomial", data = joined)

# inverse logit function to calculate probability
inv_logit <- function(x){ 1 / (1 + exp(-x)) }

# probability of homicide
p <- inv_logit(logit$coefficients[[1]])
message(round(p * 100, digits = 0),"% or roughly 1-in-", round(1/p, digits = 0), " shootings result in a homicide, controlling for annual variation in shootings.")



# aggregate data to the neighborhood level of analysis -----------------------------------------------

# join column that denotes shootings
joined$SHOOTINGS <- 1

# select columns
joined <- joined %>% dplyr::select(
  HOMICIDE, 
  SHOOTINGS, 
  OCC_YEAR, 
  # OCC_MONTH, 
  # OCC_DAY, 
  # OCC_DOW, 
  # OCC_DOY, 
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
                      # "month",
                      # "day",
                      # "dow",
                      # "doy",
                      "division",
                      "hood",
                      "neighbourhood",
                      "lon",
                      "lat"
                      )

# strip numeric strings from neighborhoods
joined$neighbourhood <- stringr::str_replace(joined$neighbourhood, "\\s\\(\\d+\\)", "")

# aggregate homicides and shootings by neighborhood
joined <- joined %>% dplyr::group_by(year, neighbourhood) %>%
  dplyr::summarise(
    homicides = sum(homicides, na.rm = TRUE),
    shootings = sum(shootings, na.rm = TRUE),
    # pull the first case in the group for the other columns
    dplyr::across(c(division, hood, lon, lat), ~ dplyr::first(.x)),
    # drop groups
    .groups = "drop"
    )



# get list of 158 census designated neighborhoods in Toronto
# hyperlink to data: https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/neighbourhood-profiles/about-toronto-neighbourhoods/

# 2. Create a temporary file path on your machine
tmp <- tempfile(fileext = ".xlsx")

readr::write_file(
  readr::read_file_raw(
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/6e19a90f-971c-46b3-852c-0c48c436d1fc/resource/19d4a806-7385-4889-acf2-256f1e079060/download/neighbourhood-profiles-2021-158-model.xlsx"),
  tmp
  )
neighbourhood_profiles <- readxl::read_excel(tmp)

# names of Toronto neighborhoods
neighbourhoods <- colnames(neighbourhood_profiles)
neighbourhoods <- neighbourhoods[-1] # drop first element

# as data frame
neighbourhoods <- as.data.frame(neighbourhoods); colnames(neighbourhoods) <- c("neighbourhood")

# complete grid skeleton for the 158 neighborhoods for all available years
grid_skeleton <- tidyr::crossing(
  neighbourhood = neighbourhoods$neighbourhood,
  year = 2004:2026
  )

# join aggregated data to this skeleton
joined <- grid_skeleton %>%
  dplyr::left_join(joined, by = c("neighbourhood", "year")) %>%
    dplyr::mutate( # 3. Clean up the missing data fields
    homicides = tidyr::replace_na(homicides, 0),
    shootings = tidyr::replace_na(shootings, 0)
    ) %>%
  # fill in any missing information about the neighborhoods
  dplyr::group_by(neighbourhood) %>% 
  tidyr::fill(division, hood, lon, lat, .direction = "downup") %>%
  dplyr::ungroup()



# end .r script
