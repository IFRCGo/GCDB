
GetOSMobj<-function(bbox=NULL,invertbbox=T){
  
  if(invertbbox) {
    bbox<-matrix(c(bbox),nrow=2,ncol=2,dimnames = list(c("x","y"),c("min","max")))
  }
  
  opq(bbox = bbox)
  
}

ExtractOSMbuild<-function(bbox,timeout=60){
  
  obj<-opq(bbox = bbox,timeout = timeout)%>%add_osm_feature("building") %>%
    opq_string()%>%osmdata_sf()
  obj<-obj$osm_polygons
  inds<-st_is_valid(obj$geometry); inds[is.na(inds)]<-FALSE
  obj<-obj[inds,]
  # obj%<>%st_as_sf()
  # st_crs(obj)<-st_crs("urn:ogc:def:crs:EPSG::4326")
  obj%<>%dplyr::select(geometry)
  #obj%<>%dplyr::select(building.levels,geometry,name)
  #obj$building.levels%<>%as.numeric()
  # sf::sf_use_s2(FALSE)
  obj$area<-as.double(st_area(st_as_sf(obj$geometry)))
  # obj$area<-vapply(1:nrow(obj),function(i) st_area(st_as_sf(obj$geometry[i])),FUN.VALUE = numeric(1))
  obj$geometry%<>%st_centroid()
  obj$Longitude<-st_coordinates(obj$geometry)[,1]
  obj$Latitude<-st_coordinates(obj$geometry)[,2]
  obj%<>%as.data.frame%>%dplyr::select(-geometry)
  
  return(obj)
  
}

GetOSMbuildings<-function(bbox, BD=NULL,minnum=50,plotty=F,timeout=60){ 
  
  #if(is.null(bbox)) bbox<-BD@bbox
  
  area<-(bbox[4]-bbox[2])*(bbox[3]-bbox[1])
  # If the bounding box size is too large, separate into pixels
  # I know that an area of 0.01 normally returns decent results
  if(area>0.01){
    buildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
    if(is.null(buildings)) {
      p<-ggplot(as.data.frame(BD),aes(Longitude,Latitude))+stat_bin_2d(drop = F,binwidth = 0.1) #LOOSEEND: will not work if BD has not been passed as an argument
      pg<-(ggplot_build(p))$data[[1]]; rm(p)
      buildings<-data.frame()
      for(i in 1:nrow(pg)){
        bbox<-as.double(pg[i,c("xmin","ymin","xmax","ymax")])
        tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
        if(is.null(tbuildings)) next
        buildings%<>%rbind(tbuildings)
      }
    }
  } else {
    # ensure bbox has an area of at least 0.01
    bbox<-expandBbox(bbox,0.01,scaling = F)
    buildings<-ExtractOSMbuild(bbox,timeout=timeout)
  }
  # i<-1
  # while((nrow(buildings)<minnum | sum(!is.na(buildings$building.levels))<minnum) & i<10){
  #   bbox%<>%expandBbox(1.1,scaling = T)
  #   buildings%<>%rbind(ExtractOSMbuild(bbox))
  #   buildings%<>%distinct()
  #   i<-i+1
  # }

  return(SpatialPointsDataFrame(coords = buildings[,c("Longitude","Latitude")],
                              data = buildings[,c("building.levels","area")],
                              proj4string = crs("+proj=longlat +datum=WGS84 +ellps=WGS84")))
    
  return(buildings)
  
}

getOSMBuildingCount <- function(ODDy){
  #ODDy <- readRDS('/home/manderso/Documents/GitHub/IIDIPUS_InputRealwithMort/ODDobjects/EQ20131015PHL_-13')
  bbox <- ODDy@bbox
  buildings<-GetOSMbuildingsODD(ODDy, timeout=60)
  rastered <- rasterize(cbind(buildings$Longitude, buildings$Latitude), raster(ODDy), fun='count')
  rastered_spdf <- as(rastered, "SpatialPixelsDataFrame")
  
  data <- ODDy@data
  data$Longitude <-  round(ODDy@coords[,1], 8)
  data$Latitude <- round(ODDy@coords[,2], 8)
  data$id <- 1:NROW(data)
  data <- merge(data, data.frame(Longitude=round(rastered_spdf@coords[,1], 8), Latitude=round(rastered_spdf@coords[,2], 8), Buildings_OSM = rastered_spdf@data$layer), 
                by=c('Latitude', 'Longitude'), all.x = TRUE)
  data <- data[order(data$id),]
  data$Buildings_OSM[which(is.na(ODDy@data$Buildings_OSM))] <- 0
  data$Buildings_OSM[which(is.na(data$ISO3C))] <- NA
  #ODDy@data <- dplyr::select(data, -c(Longitude, Latitude, id))
  
  return(data$Buildings_OSM)
}


GetOSMbuildingsODD<-function(ODD,bbox=NULL,minnum=50,plotty=F,timeout=60){
  
  if(is.null(bbox)) bbox<-ODD@bbox
  
  area<-(bbox[4]-bbox[2])*(bbox[3]-bbox[1])
  # If the bounding box size is too large, separate into pixels
  # I know that an area of 0.01 normally returns decent results
  if(area>0.01){
    tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
    if(is.null(tbuildings)) {
      p<-ggplot(as.data.frame(ODD),aes(Longitude,Latitude))+stat_bin_2d(drop = F,binwidth = 0.1)
      pg<-(ggplot_build(p))$data[[1]]; rm(p)
      buildings<-data.frame()
      for(i in 1:nrow(pg)){
        bbox<-as.double(pg[i,c("xmin","ymin","xmax","ymax")])
        tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
        if(is.null(tbuildings)) next
        buildings%<>%rbind(tbuildings)
      }
    }
  } else {
    # ensure bbox has an area of at least 0.01
    bbox<-expandBbox(bbox,0.01,scaling = F)
    buildings<-ExtractOSMbuild(bbox,timeout=timeout)
  }
  # i<-1
  # while((nrow(buildings)<minnum | sum(!is.na(buildings$building.levels))<minnum) & i<10){
  #   bbox%<>%expandBbox(1.1,scaling = T)
  #   buildings%<>%rbind(ExtractOSMbuild(bbox))
  #   buildings%<>%distinct()
  #   i<-i+1
  # }
  
  return(SpatialPointsDataFrame(coords = buildings[,c("Longitude","Latitude")],
                                data = buildings[,c("building.levels","area")],
                                proj4string = crs("+proj=longlat +datum=WGS84 +ellps=WGS84")))
  
  return(buildings)
  
}

GetOSMbuildingsBbox<-function(bbox,minnum=50,plotty=F,timeout=60){
  
  area<-(bbox[4]-bbox[2])*(bbox[3]-bbox[1])
  # If the bounding box size is too large, separate into pixels
  # I know that an area of 0.01 normally returns decent results
  if(area>0.01){
    tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
    if(is.null(tbuildings)) {
      stepsize <- 0.008333333333333333
      grid <- expand.grid(seq(bbox[2], bbox[4], stepsize), seq(bbox[1], bbox[3], stepsize))
      colnames(grid) <- c('Latitude', 'Longitude')
      p<-ggplot(grid,aes(Longitude,Latitude))+stat_bin_2d(drop = F,binwidth = 0.1)
      pg<-(ggplot_build(p))$data[[1]]; rm(p)
      buildings<-data.frame()
      for(i in 1:nrow(pg)){
        bbox<-as.double(pg[i,c("xmin","ymin","xmax","ymax")])
        tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout*2),error=function(e) NULL)
        if(is.null(tbuildings)) next
        buildings%<>%rbind(tbuildings)
      }
    }
  } else {
    # ensure bbox has an area of at least 0.01
    bbox<-expandBbox(bbox,0.01,scaling = F)
    buildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
    if(is.null(buildings)) {
      stepsize <- 0.008333333333333333
      grid <- expand.grid(seq(bbox[2], bbox[4], stepsize), seq(bbox[1], bbox[3], stepsize))
      colnames(grid) <- c('Latitude', 'Longitude')
      p<-ggplot(grid,aes(Longitude,Latitude))+stat_bin_2d(drop = F,binwidth = 0.1)
      pg<-(ggplot_build(p))$data[[1]]; rm(p)
      buildings<-data.frame()
      for(i in 1:nrow(pg)){
        bbox<-as.double(pg[i,c("xmin","ymin","xmax","ymax")])
        tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
        if(is.null(tbuildings)) next
        buildings%<>%rbind(tbuildings)
      }
    }
  }
  # i<-1
  # while((nrow(buildings)<minnum | sum(!is.na(buildings$building.levels))<minnum) & i<10){
  #   bbox%<>%expandBbox(1.1,scaling = T)
  #   buildings%<>%rbind(ExtractOSMbuild(bbox))
  #   buildings%<>%distinct()
  #   i<-i+1
  # }
  
  return(SpatialPointsDataFrame(coords = buildings[,c("Longitude","Latitude")],
                                data = buildings[,c("Longitude", "Latitude", "area")],
                                proj4string = crs("+proj=longlat +datum=WGS84 +ellps=WGS84")))
  
  return(buildings)
  
}


# Could even do like this:
# bbox<-getbb("East Gippsland") # square bounding box
# bbox<-getbb("East Gippsland, Australia",format_out="sf_polygon",limit=1) # polygon bounding box

# library(osrm)
# bbox<-as.numeric(getbb(paste0(place,", ",country)))
# locA<-convbbox(bbox)
# placeB<-"Geneva"
# countryB<-"Switzerland"
# bboxB<-as.numeric(getbb(paste0(placeB,", ",countryB)))
# locB<-convbbox(bboxB)
# # Answer given as duration[minutes] and distance[km]
# osrmRoute(src = locA, dst = locB,returnclass="sf", overview=FALSE) 
# router<- osrmRoute(src = locA, dst = locB,returnclass="sf")
# iso <- osrmIsochrone(loc = locA, breaks = seq(0,60,4),returnclass="sf")
# plot(st_geometry(iso), col = c('grey80','grey60','grey50','grey40','grey30','grey20'))
# library("cartography")
# breaks <- sort(c(unique(iso$min), max(iso$max)))
# cartography::choroLayer(x = iso,var = "center", breaks = breaks,col = rev(carto.pal("green.pal",6)),border = NA,legend.pos = "topleft",legend.frame = TRUE,legend.title.txt = "Isochrones\n(min)")
# mp_directions(locA, waypoints = NULL, destination=locB,mode = c("driving", "transit"),
#               arrival_time = NULL, departure_time = NULL, alternatives = FALSE,avoid = NULL, region = NULL,
#               key = NULL, quiet = FALSE)

AddFeat<-function(q,feature,values){
  # apply the function for as many values in list then return combined objects
  obj<-NULL
  for (strgy in values){
    tmp<-q%>%add_osm_feature(feature,strgy)
    obj<-c(obj,osmdata_sf(tmp))
  }
  return(obj)
}


GetOSMhomes<-function(bbox=NULL,place=NULL,country,plotty=FALSE, check=FALSE){
  tmp<-GetOSMobj(bbox,place,country,check)
  bbox<-tmp[[1]];  obj<-tmp[[2]]; rm(tmp)
  # bbox<-as.double(unlist(strsplit(obj$bbox,",")))
  
  ### ALL PEOPLE ###
  homes<-AddFeat(obj,"building",c("residential"))
  
  if(plotty){
    mad_map <- get_map(bbox,source = "stamen",maptype = "toner")
    p<-ggmap(mad_map)+
      geom_sf(data=homes$osm_points,
              inherit.aes =FALSE,
              colour="#238443",
              fill="#004529",
              alpha=.5,
              size=4,
              shape=21)+
      labs(x="",y="")
    print(p)
  }
  
  
  return(list(homes$osm_points,obj))
  
}

GetOSMtourist<-function(bbox=NULL,place=NULL,country,plotty=FALSE, check=FALSE){
  tmp<-GetOSMobj(bbox,place,country,check)   
  bbox<-tmp[[1]];  obj<-tmp[[2]]; rm(tmp)
  
  ### TOURISTS ###
  tourists<-AddFeat(obj,"tourism",c("alpine_hut","apartment","camp_pitch","camp_site","caravan_site","chalet","guest_house","hostel","hotel","motel"))
  
  if(plotty){
    mad_map <- get_map(bbox,source = "stamen",maptype = "toner")
    p<-ggmap(mad_map)+
      geom_sf(data=tourists$osm_points,
              inherit.aes =FALSE,
              colour="#238443",
              fill="#004529",
              alpha=.5,
              size=4,
              shape=21)+
      labs(x="",y="")
    print(p)
  }
  
  return(list(tourists,obj))
  
}

GetOSMpubtransp<-function(bbox=NULL,place=NULL,country,plotty=FALSE, check=FALSE){
  tmp<-GetOSMobj(bbox,place,country,check)
  bbox<-tmp[[1]];  obj<-tmp[[2]]; rm(tmp)
  
  pubtransp<-AddFeat(obj,"public_transport",c("station","stop_position"))  
  
  if(plotty){
    mad_map <- get_map(bbox,source = "stamen",maptype = "toner")
    p<-ggmap(mad_map)+
      geom_sf(data=pubtransp$osm_points,
              inherit.aes =FALSE,
              colour="#238443",
              fill="#004529",
              alpha=.5,
              size=4,
              shape=21)+
      labs(x="",y="")
    print(p)
  }
  
  return(list(pubtransp,obj))
  
}

GetOSMroads<-function(bbox=NULL,place=NULL,country,plotty=FALSE, check=FALSE){
  tmp<-GetOSMobj(bbox,place,country,check)
  bbox<-tmp[[1]];  obj<-tmp[[2]]; rm(tmp)
  
  listy<-c("motorway","primary","secondary","tertiary","road")
  
  rdPoints<-data.frame()
  rdLines<-data.frame()
  for (road in listy){
    thing<-AddFeat(obj,"highway",road)
    if(length(thing$osm_points$geometry)>0){
      tmp<-as.array(as(thing$osm_points$geometry, "Spatial")@coords)
      colnames(tmp)<-c("Longitude","Latitude")
      # lenny<-length(tmp[,1])
      # print(lenny)
      rdPoints<-rbind(rdPoints,data.frame(tmp,Road=rep(road,length(tmp[,1]))))
    }
  }
  
  if(plotty){
    mad_map <- get_map(bbox,source = "stamen",maptype = "toner")
    p<-ggmap(mad_map)+
      geom_point(data = rdPoints,mapping = aes(x=Longitude,y=Latitude,fill=Road,colour=Road)) +
      xlab("Longitude") + ylab("Latitude") + ggtitle(paste0(place,", ",country)) +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  
  return(list(rdPoints,obj))
  
}

GetOSMurban_rural<-function(bbox=NULL,place=NULL,country,plotty=FALSE, check=FALSE){
  tmp<-GetOSMobj(bbox,place,country,check)
  bbox<-tmp[[1]];  obj<-tmp[[2]]; rm(tmp)
  
  urban<-AddFeat(obj,"place","city")
  urban<-AddFeat(urban,"place","town")
  rural<-AddFeat(obj,"place","village")
  
  if(plotty){
    mad_map <- get_map(bbox,source = "stamen",maptype = "toner")
    p<-ggmap(mad_map)+
      geom_sf(data=urban$osm_lines,
              inherit.aes =FALSE,
              colour="red",
              fill="#004529",
              alpha=.5,
              size=4,
              shape=21)+
      labs(x="",y="") +
      geom_sf(data=rural$osm_lines,
              inherit.aes =FALSE,
              colour="red",
              fill="#004529",
              alpha=.5,
              size=4,
              shape=21)
    print(p)
    
  }
  
  return(list(urban,rural,obj))
  
}

#osmdata(bbox = NULL,overpass_call = NULL,meta = NULL,osm_points = NULL,
#        osm_lines = NULL,osm_polygons = NULL,osm_multilines = NULL,osm_multipolygons = NULL)

#p<-plot_sf(bbox)

# Also check out 'landuse':
# q<-obj%>%add_osm_feature("landuse",c("residential"))
# homes2<-osmdata_sf(q)
# q<-obj%>%add_osm_feature("landuse",c("commercial"))
# officejobs<-osmdata_sf(q)
# q<-obj%>%add_osm_feature("landuse",c("retail"))
# shopping<-osmdata_sf(q)
# q<-obj%>%add_osm_feature("landuse",c("construction","industrial))
# workingjobs<-osmdata_sf(q)

### TRAVEL - ROADS ###
# flights<-AddFeat(obj,"aeroway","aerodrome")
# 
# roads<-AddFeat(obj,"highway","motorway")
# # Integrate and multiply by speed factor
# roads<-AddFeat(obj,"highway","trunk")
# # Integrate and multiply by speed factor

### TRAVEL - PUBLIC TRANSPORT ###

#transport<-AddFeat(obj,"amenity",c("bus_station","ferry_terminal"))

### WATER RELATED ###
# water<-AddFeat(obj,"natural","water")

### PLACES ###
# country<-AddFeat(obj,"place","country")
# state<-AddFeat(obj,"place","state")

