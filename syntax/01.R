# This .R syntax file loads and joins homicides and shootings data 


# don't run 
# install packages
# install.packages(
  # c("dplyr", 
    # "tidyr", 
    # "stringr", 
    # "magrittr",
    # "ggplot2"
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





# end .r script
