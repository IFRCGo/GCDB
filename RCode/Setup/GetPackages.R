directory<-paste0(getwd(),"/")
# Prep work
dir.create("./RawData/tmp",showWarnings = F,recursive = T); dir.create("./CleanedData",showWarnings = F,recursive = T)

GetSourceFiles<-function(packred){
  
  #@@@@@ SOURCE FILES @@@@@#
  # Basic functions:
  source(paste0(directory,'RCode/Setup/Functions.R'))
  if(!file.exists(paste0(directory,'RCode/Setup/GetEnv.R'))) 
    file.copy(paste0(directory,'RCode/Setup/GetEnv_Example.R'),
              paste0(directory,'RCode/Setup/GetEnv.R'),overwrite = F)
  source(paste0(directory,'RCode/Setup/GetEnv.R'))
  # Objects
  source(paste0(directory,'RCode/GCDB_Objects/MontyMethods.R'))
  source(paste0(directory,'RCode/GCDB_Objects/CheckMonty.R'))
  # Impact related:
  source(paste0(directory,'RCode/MainlyImpactData/GetDesinventar.R'))
  source(paste0(directory,'RCode/MainlyImpactData/GetEMDAT.R'))
  source(paste0(directory,'RCode/MainlyImpactData/GetGIDD.R'))
  source(paste0(directory,'RCode/MainlyImpactData/GetReliefWeb.R'))
  source(paste0(directory,'RCode/MainlyImpactData/GetIFRCappeal.R'))
  # Hazard related:
  source(paste0(directory,'RCode/MainlyHazardData/GetGDACS.R'))
  source(paste0(directory,'RCode/MainlyHazardData/GetDFO.R'))
  source(paste0(directory,'RCode/MainlyHazardData/GetUSGS.R'))
  source(paste0(directory,'RCode/MainlyHazardData/GetGLIDEnumber.R'))
  source(paste0(directory,'RCode/MainlyHazardData/GetGFD.R'))
  # Admin boundaries & Infrastructure related:
  source(paste0(directory,'RCode/Spatio-Infra-Political/GetOSM.R'))
  source(paste0(directory,'RCode/Spatio-Infra-Political/GetAdminBoundaries.R'))
  # Other:
  source(paste0(directory,'RCode/Other/GetWorldBank.R'))
  # source(paste0(directory,'RCode/Other/GetGoogleEarthEngineData.R'))
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
  library(patchwork)
  library(uuid)
  # library(lwgeom)
  
  if(!packred) {
    library(codetools)
    library(osmdata)
    library(OpenStreetMap)
    library(osmdata)
  }
  
}

GetPackages<-function(packred){

  list.of.packages <- c("devtools","dplyr", "ggplot2","sf","tidyverse","openxlsx","pracma",
                        "geojsonR", "tiff", "gstat", "mvtnorm","xml2","rgdal",
                        "RColorBrewer", "geosphere","GGally", "wbstats","reshape2",
                        "rjson","geodist","lwgeom","ggthemes","patchwork","uuid",
                        "countrycode","rworldmap","rworldxtra","chron","ncdf4",
                        "GADMTools","akima","adehabitatMA","flexsurv", "ExtDist", 
                        'EnvStats', 'posterior', 'doParallel', 'VGAM', 'abind',
                        'Rmpi', 'openxlsx', 'ecochange','googlesheets4', 'bookdown',
                        "AzureStor", "plumber","ghql","wbstats","rmdformats","plotly",
                        "prettydoc")
  
  if(!packred) list.of.packages<-c(list.of.packages,
                                   "codetools","latex2exp","fixest",
                                   "OpenStreetMap","osmdata","rJava",
                                   "tidyRSS","geojsonR", "tiff", "gstat",
                                   "FactoMineR","factoextra","xtable",
                                   "gsubfn","mapsapi","leaflet", "ssh","RPostgres",
                                   "GADMTools", "pscl","multiColl")
  
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) install.packages(new.packages, repos='http://cran.us.r-project.org')
  
  if(length(list.of.packages[!("entity" %in% installed.packages()[,"Package"])])){devtools::install_github("trinker/entity")}
  if(length(list.of.packages[!("ggmap" %in% installed.packages()[,"Package"])])){devtools::install_github("dkahle/ggmap")}
  if(length(list.of.packages[!("GADMTools" %in% installed.packages()[,"Package"])])){devtools::install_github("cran/GADMTools")}
  if(length(list.of.packages[!("ggwordcloud" %in% installed.packages()[,"Package"])])){devtools::install_github("lepennec/ggwordcloud")}
  if(length(list.of.packages[!("deeplr" %in% installed.packages()[,"Package"])])){devtools::install_github("zumbov2/deeplr")}
  
  # if(length(list.of.packages[!("countrycodes" %in% installed.packages()[,"Package"])])){devtools::install_github("vincentarelbundock/countrycode")}
  
  LoadLibraries(packred)
  GetSourceFiles(packred)
  
}

GetPackages(packred=T)

# How does the Monty delimit some of the data variables, such as ISO3C codes?
delim<-dely<-"  :  "

# Retrieve the most up-to-date version of the hazard & impact taxonomies


# # Check the structure of the repository
# filers<-c(paste0(directory,"Plots"))
# # Make sure these files exist
# tmp<-vapply(filers, function(fff) dir.create(fff, showWarnings = FALSE),numeric(1)) ; rm(tmp)

