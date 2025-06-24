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
  
  joined_data_lk_rv[ , `RPBB Priority Zone` := Zone , ]

  joined_data_lk_rv[, Zone := NULL]

  # final RPBB zone assignemnts
  joined_data_lk_rv[ , .N , .(`RPBB Priority Zone`) ]
  
  
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
  #fix to records with bad DNR region
  joined_data_lk_rv[EDDMapS == 12334450, `DNR Region` := 4]
  joined_data_lk_rv[EDDMapS == 12051822, `DNR Region` := 3]
  joined_data_lk_rv[is.na(`DNR Region`) & County == "Sherburne", `DNR Region` := 3]
  
  
  joined_data_lk_rv[ , .N , .(RFP_PriorityInclude_2025,`DNR Region`)][order(RFP_PriorityInclude_2025)]
  
  region_priority = data.table('DNR Region' = c(1, 2 , 3, 4), PriorityRank2025 = c(2,2,3,1))
  joined_data_lk_rv[ region_priority, on = .(`DNR Region`) , PriorityRank2025 := PriorityRank2025]
  
  
  #boost SE mn counties up to #1:
  
  joined_data_lk_rv[County %in% c("Goodhue", "Wabasha", "Olmsted", "Winona", "Fillmore", "Houston", "Dodge"), .N , County]
  joined_data_lk_rv[County %in% c("Goodhue", "Wabasha", "Olmsted", "Winona", "Fillmore", "Houston", "Dodge"), PriorityRank2025 := 1]
  
  joined_data_lk_rv[ , .N , .(PriorityRank2025, County) ][order(PriorityRank2025)]
  
  #boost central MN counties to level 2 (from 3)
  joined_data_lk_rv[County %in% c("Stearns", "Morrison", "Todd", "Mille lacs", "Sherburne", "Isanti"), .N , .(County, PriorityRank2025)]
  joined_data_lk_rv[County %in% c("Stearns", "Morrison", "Todd", "Mille lacs", "Sherburne", "Isanti"), PriorityRank2025 := 2]
  
  names(joined_data_lk_rv)


  
# commited work labeling -------------------------------------------------
#' Commited work
  #committed/high need 2025:
  joined_data_lk_rv[County == "Chisago" & !is.na(pw_basin_name), CommittedWork2025 := T  , ]
  joined_data_lk_rv[str_detect(`Location Name`, "Swessinger") , CommittedWork2025 := T  ,]
  joined_data_lk_rv[str_detect(`Location Name`, "Bass Ponds")   , CommittedWork2025 := T  ,] 
  joined_data_lk_rv[EDDMapS == 10724895  ,  CommittedWork2025 := T  ,] 
  joined_data_lk_rv[EDDMapS == 8700861   ,  CommittedWork2025 := T  ,] 
  joined_data_lk_rv[str_detect(`Landowner Type`, "City Champlin")   , CommittedWork2025 := T  ,] 
  joined_data_lk_rv[str_detect(`Landowner1`, "James Barton")   , CommittedWork2025 := T  ,] 
  joined_data_lk_rv[str_detect(`Notes 2024`, "looded")   , CommittedWork2025 := T  ,]
  joined_data_lk_rv[is.na(CommittedWork2025), CommittedWork2025 := F]
  joined_data_lk_rv[ , .N , CommittedWork2025]
  
  joined_data_lk_rv[ , .N , PriorityRank2025 ]
  joined_data_lk_rv[RFP_PriorityInclude_2025 == T, .N, PriorityRank2025]
  
  #export a draft list for UMN
  # Draft_priority_list_29May2025 <- joined_data_lk_rv[ !is.na(RFP_PriorityInclude_2025) ,.SD , .SDcols = c("EDDMapS", "County", "Location Name", "Location Comments", "Twp", "Latitude", "Longitude", "Habitat", "Orig Area
  # Sq Ft", "DNR Region", "PriorityRank2025", "CommittedWork2025") ]
  # 
  #   fwrite(Draft_priority_list_29May2025, file = "scripts&data/data/output/RFPList_PriorityandCommit_29May2025.csv")
  #   
  
    joined_data_lk_rvSF <- st_as_sf(joined_data_lk_rv)

# map it ------------------------------------------------------------------


    ggplot() +
      geom_sf(data = joined_data_lk_rvSF, aes(color = PriorityRank2025, shape = CommittedWork2025) , size = 2) +
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
      geom_sf(data = joined_data_lk_rvSF, aes(color = PriorityRank2025, shape = CommittedWork2025), size = 2) +
      
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

  # nix this
  # MDA_list2025[County == "Chisago", PriorityRank2025 := 1  , ]
  
  # fwrite(MDA_list2025, file = "scripts&data/data/output/MDAList_PubWaters_2025.csv")
  


# choose best current acreages -------------------------------------------------------

  names(joined_data_lk_rv)[22] <- "Orig Area Acres"
  names(joined_data_lk_rv)[21] <- "Orig Area Sq Ft"
  
  joined_data_lk_rv[ RFP_PriorityInclude_2025 == T, .N ,
                     .(is.na(`2024 Area (ac)`),
                       is.na(`Orig Area Acres`),
                       is.na(`Orig Area Sq Ft`)) ]
  
  #backfill Orig area acres where we have sqft
  joined_data_lk_rv[ is.na(`Orig Area Acres`), 
                           `Orig Area Acres` := `Orig Area Sq Ft`/ 43560 ]
  
  joined_data_lk_rv[  , RFP_acres := `2024 Area (ac)`  ]
  joined_data_lk_rv[ is.na(RFP_acres) , RFP_acres := `Orig Area Acres`  ]
  

# designate backpack ------------------------------------------------------

  joined_data_lk_rv[ , .N , `RPBB Priority Zone` ]
    joined_data_lk_rv[`RPBB Priority Zone` == "High Potential Zone", RFP_backpack_glyphosate := T]
  
  joined_data_lk_rv[ ,  unique(`Location Comments`), ]
    joined_data_lk_rv[str_detect(`Location Comments`, "GLYPHOSATE"), RFP_backpack_glyphosate := T]
  
  joined_data_lk_rv[ ,  unique(`**Access Notes**`), ]
    joined_data_lk_rv[str_detect(`**Access Notes**`, "GLYPHOSATE", ), RFP_backpack_glyphosate := T]
    joined_data_lk_rv[str_detect(`**Access Notes**`, "glyphosate", ), RFP_backpack_glyphosate := T]
    
    
  joined_data_lk_rv[ , .N , .(RFP_backpack_glyphosate, County) ][order(County)]  
  


# assign bidgroups --------------------------------------------------------

  #group by the following bid groups: for non-lake groups, exclude lakes. 
  # For each group, parse the count of backpack sites in each county
  # Use only 2025 Priority ("RFP_PriorityInclude_2025")
  names(joined_data_lk_rv)
  
  all_mn_counties <-  c(
    "Aitkin", "Anoka", "Becker", "Beltrami", "Benton", "Big Stone", "Blue Earth", "Brown",
    "Carlton", "Carver", "Cass", "Chippewa", "Chisago", "Clay", "Clearwater", "Cook",
    "Cottonwood", "Crow Wing", "Dakota", "Dodge", "Douglas", "Faribault", "Fillmore",
    "Freeborn", "Goodhue", "Grant", "Hennepin", "Houston", "Hubbard", "Isanti", "Itasca",
    "Jackson", "Kanabec", "Kandiyohi", "Kittson", "Koochiching", "Lac Qui Parle", "Lake",
    "Lake of the Woods", "Le Sueur", "Lincoln", "Lyon", "McLeod", "Mahnomen", "Marshall",
    "Martin", "Meeker", "Mille Lacs", "Morrison", "Mower", "Murray", "Nicollet", "Nobles",
    "Norman", "Olmsted", "Otter Tail", "Pennington", "Pine", "Pipestone", "Polk",
    "Pope", "Ramsey", "Red Lake", "Redwood", "Renville", "Rice", "Rock", "Roseau",
    "St. Louis", "Scott", "Sherburne", "Sibley", "Stearns", "Steele", "Stevens",
    "Swift", "Todd", "Traverse", "Wabasha", "Wadena", "Waseca", "Washington", "Watonwan",
    "Wilkin", "Winona", "Wright", "Yellow Medicine")
  
  
  #southwest: 
  southwest <- c("Cottonwood", "Jackson", "Kandiyohi", "Lac Qui Parle", "Lyon",
                 "McLeod", "Meeker", "Murray", "Nobles", "Redwood", "Stearns", "Martin", "Lincoln" )
  #southeast: 
  southeast <- c("Blue Earth", "Dodge", "Fillmore", "Freeborn", "Goodhue",
                 "Le Sueur", "Mower", "Nicollet", "Olmsted", "Rice",
                 "Steele", "Wabasha", "Waseca", "Winona", "Sibley")
  #MetroN
  metro_n <- c("Chisago", "Isanti", "Sherburne", "Wright", "Anoka")
  
  #MetroS
  metro_s <- c("Dakota", "Hennepin", "Ramsey", "Scott", "Washington", "Carver")
  
  #North
  north <- c("Aitkin", "Becker", "Carlton", "Douglas", "Grant",
             "Morrison", "Otter Tail", "Polk", "Todd", "Clay", "Pine",
             "Mahnomen", "St. Louis", "Cass", "Wilkin", "Norman", "Stevens", "LakeoftheWoods" )
  
  #which counties are in the data:
  dataco <- unique(joined_data_lk_rv$County)
  #are any infested counties missing from our lists of regions?
  any(!c(southwest, southeast, metro_s, metro_n, north) %in% dataco)
  
  rm(dataco, all_mn_counties)

  joined_data_lk_rv %>% 
    mutate(RFP_bidgroup = case_when( County %in% southeast & is.na(dowlknum) ~ "B: Southeast",
                                     County %in% southwest & is.na(dowlknum) ~ "A: Southwest",
                                     County %in% metro_n   & is.na(dowlknum) ~ "C: Metro North",
                                     County %in% metro_s   & is.na(dowlknum) ~ "D: Metro South",
                                     County %in% north     & is.na(dowlknum)~ "E: North",
                                     !is.na(dowlknum) ~ "F: Lakes")) %>% 
    {joined_data_lk_rv <<- .}
  
    joined_data_lk_rv[ RFP_PriorityInclude_2025 == T ,.N , RFP_bidgroup ]
    
    
# Site listing ------------------------------------------------------------

#' Consolidated list for RFP site list
names(joined_data_lk_rv)
keepcols = c("RFP_bidgroup","EDDMapS", "County", "Twp", "pw_basin_name", "dowlknum", "Latitude", "Longitude", "RFP_backpack_glyphosate", "RFP_acres", "PriorityRank2025" )
RFPsitelist2025 <- joined_data_lk_rv[ RFP_PriorityInclude_2025 == T ,.SD , .SDcols = keepcols ]

RFPsitelist2025[ , .N , EDDMapS ][N>1]

# 2025.all.sites.pdf


# fwrite(RFPsitelist2025, file = "scripts&data/data/output/2025.all.sites.csv")


# RFP groupings summary generation ----------------------------------------

# #here are the counties no listed in the 2024 call: 
#   missing_counties <- c(
#     "Anoka", "Beltrami", "Benton", "Big Stone", "Brown", "Carver", "Cass", "Chippewa",
#     "Clay", "Clearwater", "Cook", "Crow Wing", "Faribault", "Houston", "Hubbard", "Itasca",
#     "Kanabec", "Kittson", "Koochiching", "Lake", "Lake of the Woods", "Lincoln", "Mahnomen",
#     "Marshall", "Martin", "Mille Lacs", "Norman", "Pennington", "Pine", "Pipestone", "Pope",
#     "Red Lake", "Renville", "Rock", "Roseau", "Sibley", "St. Louis", "Stevens", "Swift",
#     "Traverse", "Wadena", "Watonwan", "Wilkin", "Yellow Medicine"
#   )
#   
#   joined_data_lk_rv[RFP_PriorityInclude_2025 == T &  County %in% missing_counties, .N , County]
#   
#   
  

#SW  
  #count of sites
  joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% southwest, #in group
                    .N
                      ]
  #acreage sums by county and RFP group
  sw <- joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% southwest, #in group
                    .("CountofEDDMapS" = .N, "TotalAcres" = sum(RFP_acres), "Bidgroup" = "A: Southwest"),
                    .(County,RFP_backpack_glyphosate)
  ][order(County)]
  
#SE  
  #count of sites
  joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% southeast, #in group
                    .N
  ]
  #acreage sums by county and RFP group
  se <- joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% southeast, #in group
                    .("CountofEDDMapS" = .N, "TotalAcres" = sum(RFP_acres), "Bidgroup" = "B: Southeast"),
                    .(County,RFP_backpack_glyphosate)
  ][order(County)]
  
#North
  #count of sites
  joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% north, #in group
                    .N
  ]
  #acreage sums by county and RFP group
  no <- joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% north, #in group
                    .("CountofEDDMapS" = .N, "TotalAcres" = sum(RFP_acres), "Bidgroup" = "E: North"),
                    .(County,RFP_backpack_glyphosate)
  ][order(County)]
  
#Metro North
  #count of sites
  joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% metro_n, #in group
                    .N
  ]
  #acreage sums by county and RFP group
  mn <- joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% metro_n, #in group
                    .("CountofEDDMapS" = .N, "TotalAcres" = sum(RFP_acres), "Bidgroup" = "C: Metro North"),
                    .(County,RFP_backpack_glyphosate)
  ][order(County)] 
  
#Metro South
  #count of sites
  joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% metro_s, #in group
                    .N
  ]
  #acreage sums by county and RFP group
  ms <- joined_data_lk_rv[RFP_PriorityInclude_2025 == T & #in 2025 Priority list
                      is.na(pw_basin_name) & #not a lake site
                      County %in% metro_s, #in group
                    .("CountofEDDMapS" = .N, "TotalAcres" = sum(RFP_acres), "Bidgroup" = "D: Metro South"),
                    .(County,RFP_backpack_glyphosate)
  ][order(County)]   
  
  #Lakes
  lk <- joined_data_lk_rv[RFP_PriorityInclude_2025 == T & 
                      !is.na(pw_basin_name) ,
                    .("CountofEDDMapS" = .N, "TotalAcres" = sum(RFP_acres), "Bidgroup" = "F: Lakes"), 
                    .(County, pw_basin_name, dowlknum, RFP_backpack_glyphosate)
  ][order(dowlknum)]
  


RFPlist <- rbindlist(list(sw, se, no, ms, mn, lk), use.names = T, fill = T) 
rm(sw, se, no, ms, mn, lk)
  
joined_data_lk_rv[RFPlist, on = .(County = County), RFP_bidgroup := Bidgroup ]


# fwrite(RFPlist, file = "scripts&data/data/output/RFP_SummaryList_2025_b.csv")









