
GetSourceFiles<-function(packred){
  
  #@@@@@ SOURCE FILES @@@@@#
  # Basic functions:
  source('RCode/Setup/Functions.R')
  
  if(!packred){
    # Hazard related:
    source('RCode/MainlyHazardData/GetGDACS.R')
    source('RCode/MainlyHazardData/GetUSGS.R')
    source('RCode/MainlyHazardData/GetDisaster.R')
    source('RCode/MainlyHazardData/GetGLIDEnumber.R')
    # Impact related:
    source('RCode/MainlyImpactData/GetDisplacements.R')
    # Admin boundaries & Infrastructure related:
    source('RCode/Spatio-Infra-Political/GetOSM.R')
    # Other:
    
  }
  
}

LoadLibraries<-function(packred){
  
  options(stringsAsFactors = FALSE)
  
  library(dplyr)
  library(magrittr)
  library(tidyverse)
  library(ggplot2)
  library(sp)
  library(sf)
  library(xml2)
  library(ggmap)
  library(geojsonR)
  library(countrycode)
  library(stringr)
  library(pracma)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(abind)
  library(gstat)
  library(raster)
  library(geosphere)
  library(terra)
  
  if(!packred) {
    library(codetools)
    library(osmdata)
    library(OpenStreetMap)
    library(osmdata)
  }
  
}

GetPackages<-function(packred){

  list.of.packages <- c("dplyr", "ggplot2","sf","tidyverse","openxlsx","pracma",
                        "geojsonR", "tiff", "gstat", "mvtnorm","xml2","rgdal",
                        "RColorBrewer", "geosphere","GGally", "wbstats",
                        "countrycode","rworldmap","rworldxtra","chron","ncdf4",
                        "GADMTools","akima","adehabitatMA","flexsurv", "ExtDist", 
                        'EnvStats', 'posterior', 'doParallel', 'VGAM', 'abind',
                        'Rmpi', 'openxlsx', 'ecochange','googlesheets4')
  
  if(!packred) list.of.packages<-c(list.of.packages,
                                   "codetools","latex2exp",
                                   "rJava","devtools","OpenStreetMap","osmdata",
                                   "tidyRSS","geojsonR", "tiff", "gstat",
                                   "FactoMineR","factoextra","xtable",
                                   "gsubfn","mapsapi","leaflet", "ssh","RPostgres",
                                   "GADMTools", "pscl","multiColl")
  
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) install.packages(new.packages, repos='http://cran.us.r-project.org')
  
  if(length(list.of.packages[!("ggmap" %in% installed.packages()[,"Package"])])){devtools::install_github("dkahle/ggmap")}
  # if(length(list.of.packages[!("countrycodes" %in% installed.packages()[,"Package"])])){devtools::install_github("vincentarelbundock/countrycode")}
  
  LoadLibraries(packred)
  GetSourceFiles(packred)
  
}

GetPackages(packred=T)

# Delimiter used for GCDB objects, all CSV files, etc
dely<-"!@!"

# Retrieve the most up-to-date version of the hazard & impact taxonomies


# # Check the structure of the repository
# filers<-c(paste0(dir,"Plots"))
# # Make sure these files exist
# tmp<-vapply(filers, function(fff) dir.create(fff, showWarnings = FALSE),numeric(1)) ; rm(tmp)

