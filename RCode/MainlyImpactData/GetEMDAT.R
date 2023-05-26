source("./RCode/Setup/GetPackages.R")

PrepareGEE<-function(){
  # Get the admin boundaries - GAUL - using Google Earth Engine
  library(reticulate)
  # use_python(Sys.which("python3"))
  if (!("rgee" %in% installed.packages()[,1])) {
    print("Installing package `rgee`...")
    install.packages("rgee")
    ee_install()
    py_install( "earthengine-api==0.1.277", "rgee")
    ee_check()
    # rgee::ee_install_set_pyenv(py_path = "/usr/bin/python3", py_env="rgee")
    # reticulate::import("sklearn")
    Sys.setenv("EARTHENGINE_GCLOUD" = "/home/hamishwp/google-cloud-sdk/bin/")
  } else { print("Package `rgee` already installed") }
  
  return(T)
}

library(rgee) 
# ee_Authenticate()
ee_Initialize(user = gee_user, drive = TRUE)

ADM <- ee$FeatureCollection('FAO/GAUL/2015/level2')

EMDAT<-openxlsx::read.xlsx("./RawData/MostlyImpactData/EMDAT/emdat_public_EQ_20230526.xlsx",startRow = 7); EMDAT[EMDAT=="Source:"]<-NA

GetGAULmeta<-function(){
  loccy<-"./RawData/SocioPoliticalData/GAUL/GAUL2015_AdditionalAttributes/G2015_InternationalCountryCodesAttributes.xls"
  if(file.exists(loccy)) return(readxl::read_xls(loccy))
  # Extract the GAUL metadata file (from https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/9c35ba10-5649-41c8-bdfc-eb78e9e65654)
  gaulurl<-"https://data.apps.fao.org/map/catalog/srv/api/records/9c35ba10-5649-41c8-bdfc-eb78e9e65654/attachments/GAUL2015_AdditionalAttributes.zip"
  temp<-"./RawData/tmp/tmp.zip"
  download.file(gaulurl,temp)
  # Check the end location exists
  outloc<-"./RawData/SocioPoliticalData/GAUL"
  dir.create(outloc,showWarnings = F)
  # Unpack the files in the zip document
  unzip(temp,exdir = outloc)
  
  return(readxl::read_xls(loccy))
}

GAULcod<-GetGAULmeta()%>%dplyr::select(ADM_NAME,ADM_CODE,ISO3)%>%na.omit()

GetGAUL_iso3<-function(iso3,GAULcod,ADM){
  # Make sure we're not unecessarily duplicating work
  if(file.exists(paste0("./CleanedData/SocioPoliticalData/EMDAT/",iso3,"/ADM_",iso3,".geojson"))) return(T)
  # Create a folder for the results
  dir.create(paste0("./RawData/SocioPoliticalData/GAUL/",iso3),showWarnings = F,recursive = T)
  # Convert iso3 code to the GAUL admin code
  nGAUL<-GAULcod$ADM_NAME[GAULcod$ISO3==iso3]
  # Extract country shapefile from Google Earth Engine
  gaul<-ee_as_sf(ADM$filter(ee$Filter$eq('ADM0_NAME', nGAUL)),via = "drive",quiet = T,overwrite = T)
  # Temporarily save it out
  tmp<-file.copy(from=attributes(gaul)$metadata$dsn,
            to=paste0("./RawData/SocioPoliticalData/GAUL/",iso3,"/tmp_",iso3,".geojson"))
  # Check the types to make sure everything is a polygon, not lines
  types <- vapply(sf::st_geometry(gaul), function(x) class(x)[2], "")
  # Extract all elements without issues
  polys <- gaul[grepl("*POLYGON", types),]
  # Highlight which elements have issues (are lines, etc)
  oth <- gaul[!grepl("*POLYGON", types),]
  # What to do if it didn0't work out as planned
  if(nrow(oth)!=0) {
    # Sort out the error in the geometries
    geoms <- lapply( oth$geometry, `[` )
    mp <- tryCatch(do.call(rbind,lapply( geoms, function(x) sf::st_multipolygon( x = x[2] ) )),
                   error=function(e) NA)
    if(all(is.na(mp)) | length(mp)!=nrow(oth)){
      print(paste0(nrow(oth)," non-polygon elements in shapefile, out of ",nrow(gaul)," = ",signif(100*nrow(oth)/nrow(gaul),2),"%"))
      # If everything failed
      if(nrow(oth)==nrow(gaul)) stop("NOPE: EMDAT didn't work!")
      # Only store what worked
      out<-as(polys,"Spatial")
    } else {
      # Patch it over
      for(i in 1:nrow(oth)) oth$geometry[i]<-mp[[i]]
      # Convert both into spatial objects and return as one
      out<-rbind(as(polys,"Spatial"),as(oth,"Spatial"))
    }
  } else out<-as(polys,"Spatial")
  # Create the cleaned folder
  dir.create(paste0("./CleanedData/SocioPoliticalData/EMDAT/",iso3),showWarnings = F,recursive = T); 
  # Save out to be read in later on
  rgdal::writeOGR(out,
                  dsn=paste0("./CleanedData/SocioPoliticalData/EMDAT/",iso3,"/ADM_",iso3,".geojson"),
                  layer = paste0("/ADM_",iso3),
                  driver = "GeoJSON",overwrite_layer = T)
  return(T)

}
# task_vector <- ee_table_to_drive(folder = "./CleanedData/SocioPoliticalData/EMDAT/",
#                                  collection = ADM$filter(ee$Filter$eq('ADM0_NAME', 'France')),
#                                  fileFormat = "GEO_JSON",
#                                  fileNamePrefix = paste0("geom_GAUL_",iso3))
# 
# task_vector$start()
# tmp<-ee_monitoring(task_vector) # optional
# ee_drive_to_local(task = task_vector,
#                   paste0("./CleanedData/SocioPoliticalData/EMDAT/geom_GAUL_",
#                          iso3,".geojson"))

if(sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Adm.Level)) &
   sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin1.Code)) & 
   sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin2.Code))) stop("EMDAT has irregular admin locations")
# Get all the iso codes that we need admin data for
isoGAUL<-unique(EMDAT$ISO[!is.na(EMDAT$Geo.Locations)])
# Load all required shapefiles
succy<-lapply(isoGAUL, function(is) {
  tryCatch(GetGAUL_iso3(is,GAULcod,ADM),error=function(e) F)
})
# Let's patch over the ones that didn't properly work
GaulInc<-isoGAUL[!isoGAUL%in%list.files("./CleanedData/SocioPoliticalData/EMDAT/")]
# Make sure to save which ones didn't fully work
outer<-data.frame(ISO3C=isoGAUL,Status="Complete")
# Set those that didn't work
outer$Status[outer$ISO3C%in%GaulInc]<-"Some Elements Missing"
# Write out
write_csv(outer,"./CleanedData/SocioPoliticalData/EMDAT/fully_complete.csv")
# Try again!
succy<-lapply(GaulInc, function(is) {
  tryCatch(GetGAUL_iso3(is,GAULcod,ADM),error=function(e) F)
})





