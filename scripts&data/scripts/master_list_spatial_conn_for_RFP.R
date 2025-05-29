#'---
#' title: "Connecting nonnative Phragmites master list to DNR spatial resources"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull in the master list of the phrag project, connect it to DNR spatial resources like 
#' the rust patched bumble bee habitat map and the public waters inventory. Then it will identify which sites 
#' require a backpack sprayer, which require an IAPM permit, and identify a "best known acreage."  From
#' there, the minimum data (only necessary columns) will be exported and the data will be summarized to
#' match previous RFP summaries (sites per region, acres per site, etc.)
#' 
#' 
#' 
# Outstanding Work:




#' # Document Preamble
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

strttime <- Sys.time()
getwd()


# load libraries ------------------------------------------------------------------

#' ## Libraries

library(data.table) 
# update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
library(sf)
library(gridExtra)
library(tidyr)
library(esri2sf)
library(utils)
library(dplyr)
library(stringr)
library(ggplot2)
library(tigris)
library(ggrepel)

# load in functions -------------------------------------------------------
#' ## Functions

# load in data -------------------------------------------------

#' ## Data

# Master list: 
  sites <- fread(file = "scripts&data/data/input/Phragmites Master Workbook.xlsx - Master.csv")
  
  # Convert to sf object using WGS84 (EPSG:4326)
  
  sites_sf <- st_as_sf(sites[!is.na(Longitude)], coords = c("Longitude", "Latitude"), crs = 4326, remove=F)
  
  
# 2025 Control Priority List
  
  priority25 <- fread(file = "scripts&data/data/input/Statewide_Contractor_Subset for DNR review.csv")

# RPBB habitat shapefile

  # FeatureServer layer URL (note the /0 at the end for the first layer)
  url <- "https://services.arcgis.com/QVENGdaPbd4LUkLV/arcgis/rest/services/RPBB_HPZModel_Update/FeatureServer/0"
  
  # Load the spatial data
  rustyhab <- esri2sf(url)
  rustyhab_fixed <- st_make_valid(rustyhab)
  
  #
  
  # Check it
  plot(sites_sf["geometry"])
  
#Public waters delins
  # Define the URL and destination file paths
  url <- "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_mn_public_waters/gpkg_water_mn_public_waters.zip"
  zip_file <- "gpkg_water_mn_public_waters.zip"
  dest_dir <- "scripts&data/data/input/mn_public_waters"
  
  # Download the zip file
  download.file(url, destfile = zip_file, mode = "wb")
  
  # Unzip the contents
  unzip(zip_file, exdir = dest_dir)
  
  # List the unzipped files
  list.files(dest_dir)
  
  # Find the .gpkg file in the directory
  gpkg_file <- list.files(dest_dir, pattern = "\\.gpkg$", full.names = TRUE)
  print(gpkg_file)
  
  # Read the GeoPackage
  public_waters <- st_read(gpkg_file)
  
  # View the first few rows
  head(public_waters)
  
  # List all layers in the GeoPackage
  st_layers(gpkg_file)

  # Replace 'layer_name' with the actual layer name
  lakes <- st_read(gpkg_file, layer = "public_waters_basin_delineations")
  rivers <- st_read(gpkg_file, layer = "public_waters_watercourses_delineations")



# joins across data layers ------------------------------------------------

  
  
#' Do a join to these layers 
#' 
# Check projections match
  st_crs(sites_sf)
  st_crs(rustyhab)
  st_crs(lakes)
  st_crs(rivers)
  
  lakes <- st_transform(lakes, crs = st_crs(sites_sf))
  rivers <- st_transform(rivers, crs = st_crs(sites_sf))
  
  
  
  sf::sf_use_s2(TRUE)  # Enable/disable s2 geometry engine as needed for fussy topology

  joined_data <- st_join(sites_sf, rustyhab_fixed, join = st_intersects, left = TRUE)
  
  joined_data_lk <- st_join(joined_data, lakes, join = st_intersects, left = TRUE)
  
  joined_data_lk_rv <- st_join(joined_data_lk, rivers, join = st_intersects, left = TRUE)
  

  # View results
  head(joined_data_lk_rv)
  
  #how many river sites?
  setDT(joined_data_lk_rv)
  
  joined_data_lk_rv[ ,.N , "KITTLE_NBR" ]
  joined_data_lk_rv[ ,.N , pw_basin_name ]
  
#' Fix RPBB zone  
  joined_data_lk_rv[ , .N , .(Zone, `RPBB Priority Zone`) ]
  joined_data_lk_rv[ , 'RPBB Priority Zone']

  ### ... not complete
  

# subset for 2025 priority ------------------------------------------------


#'
#' Join and subset for 2025 priority sites:
  
  joined_data_lk_rv[priority25, on = .(EDDMapS), RFP_PriorityInclude_2025 := !is.na(EDDMapS)]
  
  #how many sites?
  joined_data_lk_rv[ , .N , .(RFP_PriorityInclude_2025)]

  
  
#'  Assign priority values:
#'  1. S. to N. 
#'  
#'  2. Outstate to metro
#'  
#'  3. flooded sites pushed later in year
#'  
#'  
#'  
  joined_data_lk_rv[ , .N , .(RFP_PriorityInclude_2025,`DNR Region`)][order(RFP_PriorityInclude_2025)]
  
  region_priority = data.table('DNR Region' = c(1, 2 , 3, 4), PriorityRank2025 = c(2,2,3,1))
  joined_data_lk_rv[ region_priority, on = .(`DNR Region`) , PriorityRank2025 := PriorityRank2025]
  
  
  #boost SE mn counties up to #1:
  
  joined_data_lk_rv[County %in% c("Goodhue", "Wabasha", "Olmsted", "Winona", "Fillmore", "Houston"), .N , County]
  joined_data_lk_rv[County %in% c("Goodhue", "Wabasha", "Olmsted", "Winona", "Fillmore", "Houston"), PriorityRank2025 := 1]
  
  joined_data_lk_rv[ , .N , .(PriorityRank2025, County) ][order(PriorityRank2025)]
  
  #boost central MN counties to level 2 (from 3)
  joined_data_lk_rv[County %in% c("Stearns", "Morrison", "Todd", "Mille lacs", "Sherburne", "Isanti"), .N , .(County, PriorityRank2025)]
  joined_data_lk_rv[County %in% c("Stearns", "Morrison", "Todd", "Mille lacs", "Sherburne", "Isanti"), PriorityRank2025 := 2]
  
  names(joined_data_lk_rv)

#' Additional priority ranking work: 

  #committed/high need 2025:
  joined_data_lk_rv[County == "Chisago" & !is.na(pw_basin_name), CommittedWork2025 := T  , ]
  joined_data_lk_rv[str_detect(`Location Name`, "Swessinger") , CommittedWork2025 := T  ,]
  joined_data_lk_rv[str_detect(`Location Name`, "Bass Ponds")   , CommittedWork2025 := T  ,] 
  joined_data_lk_rv[EDDMapS == 10724895  ,  CommittedWork2025 := T  ,] 
  joined_data_lk_rv[EDDMapS == 8700861   ,  CommittedWork2025 := T  ,] 
  joined_data_lk_rv[str_detect(`Landowner Type`, "City Champlin")   , CommittedWork2025 := T  ,] 
  joined_data_lk_rv[str_detect(`Landowner1`, "James Barton")   , CommittedWork2025 := T  ,] 
  joined_data_lk_rv[str_detect(`Notes 2024`, "looded")   , CommittedWork2025 := T  ,]
  
  
  #export a draft list for UMN
  Draft_priority_list_29May2025 <- joined_data_lk_rv[ !is.na(RFP_PriorityInclude_2025) ,.SD , .SDcols = c("EDDMapS", "County", "Location Name", "Location Comments", "Twp", "Latitude", "Longitude", "Habitat", "Orig Area
  Sq Ft", "DNR Region", "PriorityRank2025", "CommittedWork2025") ]

    fwrite(Draft_priority_list_29May2025, file = "scripts&data/data/output/RFPList_PriorityandCommit_29May2025.csv")
    
  
    joined_data_lk_rv <- st_as_sf(joined_data_lk_rv)

# map it ------------------------------------------------------------------


    ggplot() +
      geom_sf(data = joined_data_lk_rv, aes(color = PriorityRank2025) , size = 2) +
      labs(title = "Point Shapefile", x = "Longitude", y = "Latitude")
    
    # Get MN counties shapefile
    mn_counties <- counties(state = "MN", cb = TRUE, class = "sf")
    
    # Create centroids for labeling
    mn_counties_centroids <- st_centroid(mn_counties)
    
    # Build your plot
    ggplot() +
      # Add MN counties
      geom_sf(data = mn_counties, fill = NA, color = "black", size = 0.5) +
      
      # Add county labels
      geom_text_repel(data = mn_counties_centroids,
                      aes(label = NAME, geometry = geometry),
                      stat = "sf_coordinates",
                      size = 2.5) +
      
      # Your layer of interest
      geom_sf(data = joined_data_lk_rv, aes(color = PriorityRank2025), size = 2) +
      
      # Labels
      labs(title = "Point Shapefile with MN Counties",
           x = "Longitude",
           y = "Latitude") +
      
      # Optional theme cleanup
      theme_minimal()
    
  
# MDA report export -------------------------------------------------------


#' Consolidated list for MDA Report
  keepcols = c("EDDMapS", "County" )
  MDA_list2025 <- joined_data_lk_rv[ !is.na(pw_basin_name) & !is.na(RFP_PriorityInclude_2025) ,.SD , .SDcols = c("EDDMapS", "County", "Location Name", "Location Comments", "Twp", "Latitude", "Longitude", "Habitat", "Orig Area
  Sq Ft", "DNR Region", "PriorityRank2025") ]


  MDA_list2025[County == "Chisago", PriorityRank2025 := 1  , ]
  
  # fwrite(MDA_list2025, file = "scripts&data/data/output/MDAList_PubWaters_2025.csv")
  
  joined_data_lk_rv[EDDMapS == 7801980]

  
















