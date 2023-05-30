source("./RCode/Setup/GetPackages.R")

# Get the admin boundaries - GAUL - using Google Earth Engine
library(reticulate)
# use_python(Sys.which("python3"))
if (!("rgee" %in% installed.packages()[,1])) {
  print("Installing package `rgee`...")
  install.packages("rgee")
} else { print("Package `rgee` already installed") }
library(rgee) 
ee_install()
Sys.setenv("EARTHENGINE_GCLOUD" = "/home/hamishwp/google-cloud-sdk/bin/")
py_install( "earthengine-api==0.1.277", "rgee")
ee_check()
# rgee::ee_install_set_pyenv(py_path = "/usr/bin/python3", py_env="rgee")
# reticulate::import("sklearn")


ee_Authenticate()
# ee_Initialize()
ee_Initialize(user = 'hamish.patten', drive = TRUE)

ADM <- ee$FeatureCollection('FAO/GAUL/2015/level2')

GetGAUL_iso3<-function(iso3){
  
    task_vector <- ee_table_to_drive(folder = "./CleanedData/SocioPoliticalData/EMDAT/",
                                     collection = ADM$filter(ee$Filter$eq('ADM0_NAME', 'France')),
                                     fileFormat = "GEO_JSON",
                                     fileNamePrefix = paste0("geom_GAUL_",iso3))
    
    task_vector$start()
    tmp<-ee_monitoring(task_vector) # optional
    ee_drive_to_local(task = task_vector,
                      paste0("./CleanedData/SocioPoliticalData/EMDAT/geom_GAUL_",
                             iso3,".geojson"))
    
    tmp<-ee_as_sf(ADM$filter(ee$Filter$eq('ADM0_NAME', 'France')),via = "drive")
    types <- vapply(sf::st_geometry(tmp), function(x) class(x)[2], "")
    
    tmp$ADM2_NAME[!grepl("*POLYGON", types)]
    
    polys <- tmp[grepl("*POLYGON", types),]
    
    # oth <- tmp[!grepl("*POLYGON", types),]
    # geoms <- lapply( oth$geometry, `[` )
    # mp <- lapply( geoms, function(x) sf::st_multipolygon( x = x ) )
    
    spPolys <- as(polys, "Spatial")

}

tmp<-geojsonio::geojson_read(paste0("./CleanedData/SocioPoliticalData/EMDAT/geom_GAUL_",iso3,".geojson"), what = "sp")
tmp%<>%geojsonio::geojson_sp()
class(tmp)
# 


rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
ROI <- c(rlist$xmin, rlist$ymin,
         rlist$xmax, rlist$ymin,
         rlist$xmax, rlist$ymax,
         rlist$xmin, rlist$ymax,
         rlist$xmin, rlist$ymin)
ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
  list() %>%
  st_polygon() %>%
  st_sfc() %>%
  st_set_crs(4326) %>%
  sf_as_ee()

amk_fc <- ee$FeatureCollection(
  list(ee$Feature(ee_ROI, list(name = "Amarakaeri")))
)

task_vector <- ee_table_to_drive(
  collection = amk_fc,
  fileFormat = "GEO_JSON",
  fileNamePrefix = "geom_Amarakaeri"
)
task_vector$start()
ee_monitoring(task_vector) # optional
ee_drive_to_local(task = task_vector)






