#################################################################
#@@@@@@@@@@@@@@@@@ ADMIN BOUNDARY AGGREGATION @@@@@@@@@@@@@@@@@@#
#################################################################
GetUNMaps<-function(ISO){
  # Extract boundaries file (1st admin level)
  # ADM1<-as(sf::st_read("./Data/AdminBoundaries/UNmap0_shp/BNDA_A1.shp"),"Spatial")
  # ADM1 <- ADM1[ADM1@data$ISO3CD ==ISO, ]
  ADM<-as(sf::st_read("./Data/AdminBoundaries/UNmap0_shp/BNDA_A2.shp"),"Spatial")
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
  landl<-xlsx::read.xlsx(paste0(dir,"/Data/AdminBoundaries/LandlockedCountries.xlsx"),
                  sheetName = "Sheet1",as.data.frame = T)%>%filter(ISO3C==ISO)
  return(nrow(landl)>0)
}

# From Sub-national HDI at Global Data Lab
GetSHDIadmin<-function(ISO){
  ADM<-as(sf::st_read("./Data/AdminBoundaries/GDL_Shapefiles_V6/shdi2022_World_large.shp"),"Spatial")
  ADM <- ADM[!is.na(ADM@data$iso_code) & ADM@data$iso_code ==ISO, ]
  
  return(ADM)
  
}

# Load the administrative boundaries at level 2 from GADM
GetGADM<-function(ISO){
  ADM<-gadm_sp_loadCountries(
    unique(ISO),
    level = 2,
    basefile="./Data"
  )
  ADM<-ADM$spdf
  ADM@data%<>%dplyr::select(GID_0,NAME_1,NAME_2,GID_1,GID_2)
  names(ADM)<-c("ISO3CD","ADM1NM","ADM2NM","ADM1CD","ADM2CD")
  # Calculate the area (in kilometres squared) of each admin boundary region
  ADM$AREA_km2<-as.numeric(st_area(st_as_sf(ADM))/1e6)
  centroids<-st_coordinates(st_centroid(st_as_sf(ADM)))
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
library(sf)
library(dplyr)
 
#shapefile
adm<- adm %>%
  st_drop_geometry() %>%
  mutate_all(function(x) ifelse(is.nan(x), NA, x))

#get country stats, row statistics
adm_group <- aggregate(x = adm[,which(sapply(adm, class) == "numeric")], by = list(adm$ISO3CD), FUN = mean, na.rm=TRUE) %>%
  mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
  rename_at(1,~"iso") 

#Rank based on the current number of countries in the shapefile.......

Adm_cRank <- function(data){
  len <- nrow(data)
  iso3 <- data$iso
  data[data == 0]<-NA
  ranks <- apply(data[,-1], 2, function(x) ntile(desc(x), len)) %>% #descending, so rank 1 is highest risk
    data.frame(iso3, .)
  rank_out_of <- apply(data[,-1], 2, function(x) sum(!is.na(x))) %>%
    sapply(., function (x) rep(x,len)) %>%
    data.frame(iso3, .)
  rank_class <- apply(data[,-1], 2, function(x) as.integer(ntile(x, 5))) %>%
    data.frame(iso3, .)
  
  list_all <-list(data, ranks, rank_out_of, rank_class)
  names(list_all) <-c("Value","Rank", "Rank_out_of", "Rank_class")
  
  
  return(list_all)
}

adm_ranks<-Adm_cRank(adm_group)
