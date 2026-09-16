# This .R syntax file cleans motor vehicle theft incident reporting

# call pipe to workspace
`%>%` <- magrittr::`%>%`

# don't run
# load motor vehicle thefts
# autos <- readr::read_csv(url("https://stg-arcgisazurecdataprod.az.arcgis.com/exportfiles-6104-20702/Auto_Theft_Open_Data_4481082360476864088.csv?sv=2025-05-05&st=2026-09-14T16%3A42%3A14Z&se=2026-09-14T17%3A47%3A14Z&sr=b&sp=r&sig=kl7VL%2Fqc35EseYCkIxaO96f%2BClSl6ESlRfE6IDfsprk%3D"))

# year of reporting
table(autotheft$OCC_YEAR, useNA = "always")

# drop the tiny fraction of cases prior to 2014
autotheft <- autotheft %>% dplyr::filter(OCC_YEAR >= 2014)

# drop current year of reporting
autotheft <- autotheft %>% dplyr::filter(OCC_YEAR <= 2025)


# location of theft
table(autotheft$PREMISES_TYPE, useNA = "always")

# drop any white spaces
autotheft <- dplyr::mutate(PREMISES_TYPE = trimws(PREMISES_TYPE)) %>%

# recode motor vehicle thefts at residential and non-residential places
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "Apartment"] <- "Residential"
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "House"] <- "Residential"
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "Commercial"] <- "Non-residential"
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "Educational"] <- "Non-residential"
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "Other"] <- "Non-residential"
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "Outside"] <- "Non-residential"
autotheft$PREMISES_TYPE[autotheft$PREMISES_TYPE == "Transit"] <- "Non-residential"



# clean up the data -----------------------------------------------

# join column that denotes motor vehicle theft
autotheft$MVT <- 1

# select columns
autotheft <- autotheft %>% dplyr::select(
  MVT,
  PREMISES_TYPE,
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
colnames(autotheft) <- c("mvt",
                         "premises",
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
autotheft$neighbourhood <- stringr::str_replace(autotheft$neighbourhood, "\\s\\(\\d+\\)", "")





# end .R script


