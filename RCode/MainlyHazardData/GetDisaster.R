
ExtractParams<-function(haz="EQ"){
  if(haz=="EQ") return(list(I0=4.5,minmag=5))
  if(haz=="TC") return(list(I0=3))
  if(haz=="FL") return(list(I0=0))
  stop("Hazard type not recognised")
}

GetDisaster<-function(DispData, bbox=NULL, EQparams=NULL){
  if(all(DispData$hazard=="EQ")) {
    
    # Just in case multiple hazards are run simultaneously
    tDisp<-DispData%>%filter(hazard=="EQ")
    # Extract standard (or user modified) EQ parameters
    if(is.null(EQparams)) EQparams<-ExtractParams("EQ")
    # Extract bounding box of affected countries
    if(is.null(bbox)) bbox<-countriesbbox(unique(tDisp$iso3))
    # Extract all earthquakes from USGS:
    return(GetUSGS(bbox=bbox,sdate=min(tDisp$sdate),fdate = max(tDisp$fdate),I0=EQparams$I0,minmag=EQparams$minmag))
    
  } else if(all(DispData$hazard=="TC")) {
    
    # Just in case multiple hazards are run simultaneously
    tDisp<-DispData%>%filter(hazard=="TC")
    # Extract standard (or user modified) EQ parameters
    TCparams<-ExtractParams("TC")
    # Extract bounding box of affected countries
    bbox<-countriesbbox(unique(tDisp$iso3))
    # Extract all earthquakes from USGS:
    return(GetEOSDISwind(hazard="TC",bbox=bbox,sdate=min(tDisp$sdate),I0=TCparams$I0))
    
  } else if(all(DispData$hazard=="FL")) {
    
    stop("Not yet ready for floods")
    
  } else {
    
    if(length(unique(DispData$hazard))>1) stop("Inserted more than one hazard into GetDisaster")
    stop(paste0("Hazard type",unique(DispData$hazard) ,"not recognised"))
  }
}

# Auto extract specifically for earthquakes
GetEarthquake<-function(input){
  # Extract standard (or user modified) EQ parameters
  EQparams<-ExtractParams("EQ")
  
  if(!is.null(input$USGSid)) {
    out<-GetUSGS(USGSid=input$USGSid,
                 I0=EQparams$I0,minmag=EQparams$minmag)
  } else {
    # Extract bounding box of affected countries
    bbox<-countriesbbox(input$iso3)
    # Extract all earthquakes from USGS:
    out<-GetUSGS(bbox=bbox,sdate=input$sdate,fdate = input$fdate,
                 I0=EQparams$I0,minmag=EQparams$minmag)
  }
  if(is.null(out)) stop("Earthquake not found, try giving a broader/narrower range of dates")
  return(out)
}














# folder<-"/home/patten/Documents/Coding/Oxford/IIDIPUS/Disaster_Data/Australia_NASA/VIIRS_Aug-Feb/"
country<-"Australia"

KrigMeUp<-function(poly,values=NULL){
  
  # if(nrow(poly[poly$ncontour==0,])==0) stop("ERROR: Kriging requires EQ centroid")
  
  poly%<>%dplyr::select(Intensity,Longitude,Latitude)%>%distinct()
  
  # bbox<-c(min(poly$Longitude),min(poly$Latitude),max(poly$Longitude),max(poly$Latitude))
  
  # if(bound) {
  #   n<-30
  #   R<-max(c((bbox[3]-bbox[1]),(bbox[4]-bbox[2])))
  #   theta<-seq(0.1,2*pi,length.out = n)
  #   
  #   poly%<>%rbind(data.frame(Intensity=rep(0,n),
  #                            Longitude=(R*cos(theta) + mean(bbox[c(3,1)])),
  #                            Latitude=(R*sin(theta) + mean(bbox[c(4,2)]))))
  # }
  
  coordinates(poly) <- ~ Longitude + Latitude
  
  vario <- gstat::variogram(Intensity~1, poly)
  
  bbox<-array(bbox(poly),dim=4)
  dist<-0.5*sqrt((bbox[3]-bbox[1])*(bbox[4]-bbox[2]))
  
  fit <- gstat::fit.variogram(vario, model=gstat::vgm(1, "Sph", dist, 1))
  
  # plot(vario,fit)
  
  if (is.null(values)) return(fit)
  
  # if(gridded) gridded(values) <- ~ Longitude + Latitude
  
  # values <- expand.grid(Longitude = seq(from = min(tpoly$Longitude), max(tpoly$Longitude),length.out = 100),
  #                       Latitude = seq(from = min(tpoly$Latitude), max(tpoly$Latitude),length.out = 100))
  
  coordinates(values) <- ~ Longitude + Latitude # step 3 above
  return(gstat::krige(Intensity ~ 1, poly, values, model=fit))
  
  
  kvalues %>% as.data.frame %>%
    ggplot(aes(x=Longitude, y=Latitude)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
    scale_fill_gradient(low = "yellow", high="red") +
    theme_bw()
  
}

GetDisaster_old<-function(d_type,country,bbox,s_date,f_date,folder=NULL,plotty=FALSE){
  # d_type - disaster type, explained in https://eonet.sci.gsfc.nasa.gov/api/v3-beta/categories
  # bbox is bounding box in the form 'min lon, max lat, max lon, min lat'
  # s_date, f_date - start and end date for search of events in format YYYY-MM-DD
  # wordcloudy plots a word cloud of the disasters found in the search
  
  if(!is.null(folder)){
    
    filer<-paste0(folder,'/',list.files(path=folder,pattern='.*shp'))
    # event <- lapply(filer, st_read)
    event <- st_read(filer)
    
    mad_map <- get_stamenmap(bbox,source = "stamen",maptype = "toner-lite",zoom=6)
    p<-ggmap(mad_map)
    
    q<-p+stat_density2d(mapping = aes(x=LONGITUDE, y=LATITUDE,alpha = ..level..),
                        data=filter(event,FRP>1),fill="red", size=0.01, bins=20, geom="polygon",contour = T,na.rm = T,
                        show.legend = F) + xlab("Longitude") + ylab("Latitude")
    
    ggsave(paste0("Oz_VIIRS_aug-feb.png"), plot=q,path = paste0(folder,'/Plots/'),width = 6,height = 5.)
    
    bbox<-c(110.18,-44.93,115.01,-10.17)
    mad_map <- get_stamenmap(bbox,source = "stamen",maptype = "toner-lite",zoom=7)
    p<-ggmap(mad_map)
    
    p + geom_point(mapping = aes(x=LONGITUDE, y=LATITUDE,group=ACQ_DATE,colour=ACQ_DATE),alpha=0.05,data=filter(event,FRP>30)) 
    
    stop()
    # cnt<-maps::map('world',country)
    
    #     stat_density2d(aes(alpha=..level.., fill=..level.., weight=BRIGHTNESS), 
    #                size=2, bins=10, geom="polygon") + 
    # scale_fill_gradient(low = "yellow", high = "red") +
    # scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    # geom_density2d(colour="black", bins=10, aes(weight=BRIGHTNESS))
    
    if(is.null(bbox)){
      bbox<-unname(st_bbox(event[[2]]))
      disasters<-data.frame(LATITUDE=event[[2]]$LATITUDE,LONGITUDE=event[[2]]$LONGITUDE,BRIGHTNESS=event[[2]]$BRIGHTNESS,ACQ_DATE=event[[2]]$ACQ_DATE)
    } else {
      disasters<-data.frame(LATITUDE=event[[2]]$LATITUDE,LONGITUDE=event[[2]]$LONGITUDE,BRIGHTNESS=event[[2]]$BRIGHTNESS,ACQ_DATE=event[[2]]$ACQ_DATE) %>%
        filter(LATITUDE>bbox[2]&LATITUDE<bbox[4]&LONGITUDE>bbox[1]&LONGITUDE<bbox[3])
      breakz<-c(0,0.02,Inf)
      limitz<-c(0.02,0.05)
    }
    
    p<-ggplot(disasters, aes(x=LONGITUDE, y=LATITUDE,
                             weight=BRIGHTNESS,fill=..level..) ) +
      stat_density2d(size=2, bins=50, geom="polygon",alpha=0.1) + 
      scale_fill_gradient(low = "yellow", high = "red") +
      geom_density2d(colour="black", bins=10) + coord_fixed() +
      xlab("Longitude") + ylab("Latitude") + ggtitle(paste0(d_type," ",country)) + theme(plot.title = element_text(hjust = 0.5))
    #p<-p+geom_path(inherit.aes = FALSE,mapping=aes(x,y),data=data.frame(x=cnt$x,y=cnt$y))+xlim(bbox[1]-2,bbox[3]+2)+ylim(bbox[2]-2,bbox[4]+2)
    print(p)
    ggsave(paste0(d_type,"_",country,"_WeightedDisasterKernel.png"), plot=p,path = paste0(folder,'/'),width = 6,height = 5.)
    
    mad_map <- get_stamenmap(bbox,source = "stamen",maptype = "terrain")
    p<-ggmap(mad_map)
    
    # p<- p+ theme_bw() + stat_density2d(mapping = aes(x=LONGITUDE, y=LATITUDE,fill=..level.., alpha=cut(..level..,breaks=breakz)),
    p<- p+ theme_bw() + stat_density2d(mapping = aes(x=LONGITUDE, y=LATITUDE,fill=..level..),alpha=0.05, 
                                       data=disasters,size=1, bins=50, geom="polygon") + 
      #scale_alpha_manual(values=c(0,1),guide="none") +
      # scale_fill_gradient2(low = rgb(1,0,0,alpha = 0),mid="yellow", high = "red",limits=limitz, na.value = rgb(0,1,0,alpha = 0)) +
      scale_fill_gradient2(low = rgb(1,0,0,alpha = 0),mid="yellow", high = "red", na.value = rgb(0,1,0,alpha = 0)) +
      geom_density2d(colour="black", bins=3, mapping = aes(x=LONGITUDE, y=LATITUDE,
                                                           fill=..level..), 
                     data=disasters) + 
      xlab("Longitude") + ylab("Latitude") + ggtitle(paste0(d_type," ",country)) + theme(plot.title = element_text(hjust = 0.5))
    print(p)
    
    ggsave(paste0("East_Gippsland_",d_type,"_",country,"_DisasterKernel.png"), plot=p,path = paste0(folder,'/Plots/'),width = 6,height = 5.)
    
    dlist<-as.list(unique(disasters$ACQ_DATE))
    p<-ggmap(mad_map)
    for (day in dlist){
      q<-p+ stat_density2d(mapping = aes(x=LONGITUDE, y=LATITUDE,
                                         fill=..level.., alpha=cut(..level..,breaks=c(0,0.01,Inf))), 
                           data=filter(disasters,ACQ_DATE==day),size=2, bins=50, geom="polygon") + 
        scale_alpha_manual(values=c(0,1),guide="none") +
        scale_fill_gradient2(low = rgb(1,0,0,alpha = 0),mid="yellow", high = "red",limits=c(0.01,0.05), na.value = rgb(0,1,0,alpha = 0)) +
        #scale_alpha(range = c(0.00, 0.5), guide = FALSE) #+
        geom_density2d(colour="black", bins=3, mapping = aes(x=LONGITUDE, y=LATITUDE), 
                       filter(disasters,ACQ_DATE==day)) + 
        xlab("Longitude") + ylab("Latitude") + ggtitle(paste0(d_type," ",country,": ",day)) + theme(plot.title = element_text(hjust = 0.5))
      ggsave(paste0(d_type,"_",country,"_",day,"_DisasterKernel.png"), plot=q,path = paste0(folder,'/Plots/'),width = 6,height = 5.)
    }  
    nameGIF<-paste0(folder,"/",d_type,"_",country,"_DisasterKernel")
    system(command= paste0("convert ",folder,"/",d_type,"_",country,"_20* -delay 100 -loop 0 ",nameGIF,".gif"))
    
    return(event)
  }
  
  d_choice<-c("Drought"=6,"Dust and Haze"=7,"Wildfires"=8,"Floods"=9,"Severe Storms"=10,"Volcanoes"=12,"Landslides"=14,"Earthquakes"=16,"Snow"=17,"Temperature Extremes"=18,"Manmade"=19)
  if (is.na(d_choice[d_type])){stop("GetDisaster error: input disaster type (e.g. 'Severe Storms') does not exist")}
  
  if(d_type=="Earthquakes"){
    # https://earthquake.usgs.gov/fdsnws/event/1/
    # "https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2014-01-01&endtime=2014-01-02"
    
    return(event)
  }
  
  if(d_type=="Wildfires"){
    #https://firms.modaps.eosdis.nasa.gov/active_fire/#firms-shapefile
    
    return(event)
  }
  
  #loc<-paste0(as.character(unlist(bbox)),collapse=",")
  loc<-paste0(as.character(unlist(c(100.,5.,175.,70.))),collapse=",")
  library(geojsonR)
  events<-FROM_GeoJson(paste0("https://eonet.sci.gsfc.nasa.gov/api/v3-beta/events/geojson?start=",s_date,"&end=",f_date,"?bbox=",loc,"?status=open"))
  # CAN ALSO BE USING v2.1: "https://eonet.sci.gsfc.nasa.gov/api/v2.1/events"
  # Check out "https://developers.arcgis.com/python/sample-notebooks/mapping-recent-natural-disasters/"
  tmp<-NULL
  for (i in 1:length(events$features)){tmp<-c(tmp,events$features[[i]]$properties$categories[[1]]$title)}
  disasters<-events$features[grep(d_type, tmp, ignore.case=TRUE)]
  
  #lapply(disasters, grep...)
  
  if(plotty){
    # source('http://www.sthda.com/upload/rquery_wordcloud.r')
    # for(i in 1:length(disasters)){tmp<-disasters[[i]]$properties$title}
    # res<-rquery.wordcloud(tmp, type ="text", lang = "english",excludeWords = d_type)
    # Sys.sleep(2)
    library(OpenStreetMap)
    library(osmdata)
    q<-opq(bbox = bbox)%>%add_osm_feature("amenity",c("house","apartments","cabins","bungalow"))
    homes<-osmdata_sf(q)
    q<-opq(bbox = bbox)%>%add_osm_feature("amenity",c("static_caravan","hotel"))
    tourists<-osmdata_sf(q)
    q<-opq(bbox = bbox)%>%add_osm_feature("amenity",c(""))
    transport<-osmdata_sf(q)
    
    openmap(upperLeft = c(bbox[1],bbox[2]),lowerRight = c(bbox[3],bbox[4]),minNumTiles=3L,type = "osm")
    
    #res<-rquery.wordcloud(tmp, type ="text", lang = "english",excludeWords = c("sea","lake","ice"))
  }
  return(disasters)
  # https://www.gdacs.org/datareport/resources/VO/273070/geojson_273070_2.geojson
}



# library(tidyRSS)
# tidyfeed(feed = 'http://gdacs.org/rss.aspx?profile=ARCHIVE&fromarchive=true&from=2019-01-01&to=2020-01-01')
# tidyfeed('http://www.gdacs.org/xml/rss_7d.xml')
# 
# stringy<-'https://www.gdacs.org/datareport/resources/VO/273070/geojson_273070_2.geojson'
# 
# library(geojsonR)
# GDACS<-FROM_GeoJson(url_file_string = stringy)
# str(GDACS)
# 
# library(geojsonio)
# data_park <- geojson_read(stringy, what = "sp")