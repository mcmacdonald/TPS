# This .R syntax file loads homicides/shootings and also  motor vehicle thefts reported by the Toronto Police Service

# don't run 
# install.packages(
  # c("httr2", 
  # "jsonlite", 
  # "dplyr"
  # )
  # )

# load open data from the Toronto Police Service
# https://www.tps.ca/data-maps/open-data/

# boundary file for Toroto Police Service Divisions
# https://open.toronto.ca/dataset/police-boundaries/

# by-law enforcement by the Toronto Municipal Licensing and Standards Division (ML&S)
# https://open.toronto.ca/dataset/municipal-licensing-and-standards-investigation-activity/


# function to download TPS open data
fetch <- function(service, layer = 0, where = "1=1", page_size = 2000) {
  
  require(httr2); require(jsonlite); require(dplyr)
  
  base <- paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    service, 
    "/FeatureServer/", 
    layer, 
    "/query"
    )
  
  all_pages <- list()
  offset <- 0
  repeat {
    resp <- httr2::request(base) |>
      httr2::req_headers(
        "User-Agent" = "Mozilla/5.0",
        "Referer" = "https://hub.arcgis.com/"
      ) |>
      httr2::req_url_query(
        where = where,
        outFields = "*",
        f = "json",
        resultOffset = offset,
        resultRecordCount = page_size
      ) |>
      httr2::req_perform()
    
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
    page <- dplyr::as_tibble(parsed$features$attributes)
    
    if (nrow(page) == 0) break
    
    all_pages[[length(all_pages) + 1]] <- page
    offset <- offset + nrow(page)
    
    # stop if server returned fewer than requested (last page) 
    # and doesn't advertise more via exceededTransferLimit
    if (nrow(page) < page_size && !isTRUE(parsed$exceededTransferLimit)) break
  }
  
  dplyr::bind_rows(all_pages)
}
homicides <- fetch("Homicides_Open_Data_ASR_RC_TBL_002")
shootings <- fetch("Shooting_and_Firearm_Discharges_Open_Data")
autotheft <- fetch("Auto_Theft_Open_Data")



# path to folder 
path <- "~/Desktop/TPS/"

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





# end .R script 


