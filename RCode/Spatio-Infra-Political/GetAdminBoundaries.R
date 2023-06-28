#################################################################
#@@@@@@@@@@@@@@@@@ ADMIN BOUNDARY AGGREGATION @@@@@@@@@@@@@@@@@@#
#################################################################
GetUNMaps<-function(ISO){
  # Extract boundaries file (1st admin level)
  # ADM1<-as(sf::st_read("./CleanedData/SocioPoliticalData/UN_Clearmaps/BNDA_A1.shp"),"Spatial")
  # ADM1 <- ADM1[ADM1@data$ISO3CD ==ISO, ]
  ADM<-as(sf::st_read("./CleanedData/SocioPoliticalData/UN_Clearmaps/BNDA_A2.shp"),"Spatial")
  projection(ADM)<-"+proj=longlat +datum=WGS84 +no_defs"
  ADM <- ADM[ADM@data$ISO3CD ==ISO, ]
  ADM@data%<>%dplyr::select(ISO3CD,ADM1NM,ADM2NM,ADM1CD,ADM2CD)
  # Calculate the area (in kilometres squared) of each admin boundary region
  # ADM$AREA_km2<-as.numeric(st_area(st_as_sf(ADM))/1e6)
  # centroids<-rgeos::gCentroid(ADM,byid=TRUE)
  # ADM$LONGITUDE<-centroids@coords[,1]
  # ADM$LATITUDE<-centroids@coords[,2]
  # In case the admin level 2 boundaries do not exist, but level 1 do!
  if(all(is.na(ADM@data[,c("ADM2NM","ADM2CD")])) & !all(is.na(ADM@data[,c("ADM1NM","ADM1CD")]))) {
    ADM@data$ADM2NM<-ADM@data$ADM1NM
    ADM@data$ADM2CD<-ADM@data$ADM1CD
  }
  
  ADM<-ADM[!is.na(ADM$ADM1NM) & !is.na(ADM$ADM2NM) & 
           !is.na(ADM$ADM1CD) & !is.na(ADM$ADM2CD),]
  
  return(ADM)
}

GetExtent<-function(ADM,expander=NULL){
  bbox<-ADM@bbox
  # Expand the bounding box, useful for the interpolation
  if(!is.null(expander)) bbox%<>%expandBbox(1.1)
  ext<-as(extent(bbox[c(1,3,2,4)]), 'SpatialPolygons')
  crs(ext) <- "+proj=longlat +datum=WGS84 +no_defs"  
  return(ext)
}

CheckLandLock<-function(ISO){
  # From Wikipedia page on landlocked countries, but I also added Sudan
  landl<-xlsx::read.xlsx(paste0(dir,"/CleanedData/SocioPoliticalData/LandlockedCountries.xlsx"),
                  sheetName = "Sheet1",as.data.frame = T)%>%filter(ISO3C==ISO)
  return(nrow(landl)>0)
}

# From Sub-national HDI at Global Data Lab
GetSHDIadmin<-function(ISO){
  ADM<-as(sf::st_read("./CleanedData/SocioPoliticalData/GDL_Shapefiles_V6/shdi2022_World_large.shp"),"Spatial")
  ADM <- ADM[!is.na(ADM@data$iso_code) & ADM@data$iso_code ==ISO, ]
  
  return(ADM)
  
}

# Load the administrative boundaries at level 2 from GADM
GetGADM<-function(ISO,level=0){
  ADM<-GADMTools::gadm_sp_loadCountries(
    unique(ISO),
    level = level,
    basefile="./CleanedData/SocioPoliticalData/GADM/"
  )
  ADM<-ADM$spdf
  
  if(level==2) {
    ADM@data%<>%dplyr::select(GID_0,NAME_1,NAME_2,GID_1,GID_2)
    names(ADM)<-c("ISO3CD","ADM1NM","ADM2NM","ADM1CD","ADM2CD")
  } else if(level==1) {
    ADM@data%<>%dplyr::select(GID_0,NAME_1,GID_1)
    names(ADM)<-c("ISO3CD","ADM1NM","ADM1CD")
  } else {
    ADM@data%<>%dplyr::select(GID_0)
    names(ADM)<-c("ISO3CD")
  }
  
  # Calculate the area (in kilometres squared) of each admin boundary region
  ADM$AREA_km2<-as.numeric(st_area(st_as_sf(ADM))/1e6)
  centroids<-suppressWarnings(st_coordinates(st_centroid(st_as_sf(ADM))))
  ADM$LONGITUDE<-centroids[,1]
  ADM$LATITUDE<-centroids[,2]
  ADM@bbox[]<-c(min(ADM$LONGITUDE),
              min(ADM$LATITUDE),
              max(ADM$LONGITUDE),
              max(ADM$LATITUDE))
  return(ADM)
}

ADMexceptions<-function(ADM){
  
  if(ADM$ISO3CD[1]=="ETH"){
    ADM@polygons[[8]]@Polygons[[3]]<-NULL
    ADM@polygons[[8]]@Polygons[[2]]<-NULL
    ADM@polygons[[8]]@plotOrder<-c(1L)
  }
    
  return(ADM)
}

# Using the @polygon component of a SpatialPolygonsDataFrame, gives the bounding box
RecalcBBOX<-function(polygons){
  coords<-matrix(ncol = 2)
  for (i in 1:length(polygons)){
    for (j in 1:length(polygons[[i]]@Polygons)){
      coords%<>%rbind(polygons[[i]]@Polygons[[j]]@coords)
    }
  }
  return(c(apply(coords,2,min,na.rm=T),apply(coords,2,max,na.rm=T)))
}
# Filter the admin boundary shapes by country or admin level
filterADM<-function(ADM,iso=NULL,adlev=NULL){
  
  if(is.null(iso) & is.null(adlev)) return(ADM)
  
  inds<-rep(TRUE,nrow(ADM))
  if(!is.null(iso))  inds<-inds & ADM$ISO3CD%in%iso
  # if(!is.null(adlev)) inds<-inds & ADM$
  
  ADM@data<-ADM@data[inds,]
  ADM@polygons<-ADM@polygons[inds]
  ADM@plotOrder<-ADM@plotOrder[inds]
  ADM@bbox[1:4]<-RecalcBBOX(ADM@polygons)
  
  return(ADM)
  
}

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

GetGAUL<-function(ISO3C,lADM=2){
  # Setup Google Earth Engine library and objects
  SetupGEE()
  # Extract the GAUL admin boundary data extraction function from GEE
  ADM<-ee$FeatureCollection(paste0('FAO/GAUL/2015/level',lADM))
  # Extract the GAUL admin boundary metadata
  GAULcod<-GetGAULmeta()%>%dplyr::select(ADM_NAME,ADM_CODE,ISO3)%>%na.omit()
  # For each country, extract the GAUL data
  do.call(rbind,lapply(ISO3C,function(iso3) {
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
    
    return(data.frame(iso3=iso3,checker=T))
  }))
  
}

GetIFRCADM<-function(ISO,level=0){
  ADM<-as(sf::st_read(paste0("./CleanedData/SocioPoliticalData/IFRC/GO-admin1-shp/",ISO,"-admin1/",ISO,"-admin1.shp")),"Spatial")
  projection(ADM)<-"+proj=longlat +datum=WGS84 +no_defs"
  if(level==0){
    ADM%<>%transmute(ISO3CD=iso3)
    
    # MERGE ALL ADM POLYGONS!
    
  } else if(level==1){
    ADM%<>%transmute(ISO3CD=iso3,ADM1NM=name,ADM1CD=district_i)
  }
  ADM$AREA_km2<-as.numeric(st_area(st_as_sf(ADM))/1e6)
  centroids<-suppressWarnings(st_coordinates(st_centroid(st_as_sf(ADM))))
  ADM$LONGITUDE<-centroids[,1]
  ADM$LATITUDE<-centroids[,2]
  ADM@bbox[]<-c(min(ADM$LONGITUDE),
                min(ADM$LATITUDE),
                max(ADM$LONGITUDE),
                max(ADM$LATITUDE))
  return(ADM)
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


# if(ISO=="MDV"){
#   
#   ADM2$AltName<-"Total"
#   ADM2$AltName[ADM2$ADM1NM%in%c("Haa Alifu","Haa Dhaalu","Shaviyani")]<-"North"
#   ADM2$AltName[ADM2$ADM1NM%in%c("Noonu","Raa","Baa","Lhaviyani")]<-"North Central"
#   ADM2$AltName[ADM2$ADM1NM%in%c("Male'","Kaafu","Alifu Alifu","Alifu Dhaalu","Vaavu")]<-"Male"
#   ADM2$AltName[ADM2$ADM1NM%in%c("Faafu","Dhaalu","Meemu")]<-"Central"
#   ADM2$AltName[ADM2$ADM1NM%in%c("Thaa","Laamu","Gaafu Alifu","Gaafu Dhaalu")]<-"South Central"
#   ADM2$AltName[ADM2$ADM1NM%in%c("Gnaviyani","Seenu")]<-"South"
# 
#   ADM2%<>%merge(SHDI,by="AltName")
#   
# }

#--------data manipulation + analysis on ADM shapefile-----------
# library(sf)
# library(dplyr)
#  
# #shapefile
# adm<- adm %>%
#   st_drop_geometry() %>%
#   mutate_all(function(x) ifelse(is.nan(x), NA, x))
# 
# #get country stats, row statistics
# adm_group <- aggregate(x = adm[,which(sapply(adm, class) == "numeric")], by = list(adm$ISO3CD), FUN = mean, na.rm=TRUE) %>%
#   mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
#   rename_at(1,~"iso") 
# 
# #Rank based on the current number of countries in the shapefile.......
# 
# Adm_cRank <- function(data){
#   len <- nrow(data)
#   iso3 <- data$iso
#   data[data == 0]<-NA
#   ranks <- apply(data[,-1], 2, function(x) ntile(desc(x), len)) %>% #descending, so rank 1 is highest risk
#     data.frame(iso3, .)
#   rank_out_of <- apply(data[,-1], 2, function(x) sum(!is.na(x))) %>%
#     sapply(., function (x) rep(x,len)) %>%
#     data.frame(iso3, .)
#   rank_class <- apply(data[,-1], 2, function(x) as.integer(ntile(x, 5))) %>%
#     data.frame(iso3, .)
#   
#   list_all <-list(data, ranks, rank_out_of, rank_class)
#   names(list_all) <-c("Value","Rank", "Rank_out_of", "Rank_class")
#   
#   
#   return(list_all)
# }
# 
# adm_ranks<-Adm_cRank(adm_group)
