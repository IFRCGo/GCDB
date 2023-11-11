
# Create an object of the required form from the USGS data
formUSGSobject<-function(meanhaz,sdhaz,I0=NULL){
  
  sgdf <- as(meanhaz, 'SpatialPixelsDataFrame') ; rm(meanhaz)
  tmp<- as(sdhaz, 'SpatialPixelsDataFrame') ; rm(sdhaz)
  sgdf$mmi_std<-tmp$mmi_std ; rm(tmp)
  projection(sgdf)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  
  colnames(sgdf@coords)<-rownames(sgdf@bbox)<-c("Longitude","Latitude")
  
  if(!is.null(I0)) sgdf<-sgdf[sgdf$mmi_mean>I0,]
  
  return(sgdf)
  
}

# Extract the raster object from an API call to USGS and extract the zip folder
ExtractUSGS<-function(url,namer,I0=NULL,plotty=F){
  
  temp<-paste0(namer,".zip")
  # Download the raster file to the location 'temp'
  download.file(url,temp)
  # Unpack the files in the zip document
  unzip(paste0(temp),exdir = paste0(namer,"/"))
  # Extract the mean hazard intensity from raster
  meanhaz<-tryCatch(raster(file.path(namer,"mmi_mean.flt")), error=function(e) NULL)
  if(is.null(meanhaz)) meanhaz<-raster(file.path(namer,"mi.fit"))
  # Extract the variance of the hazard intensity from raster
  sdhaz<-tryCatch(raster(file.path(namer,"mmi_std.flt")), error=function(e) NULL)
  if(is.null(sdhaz)) sdhaz<-raster(file.path(namer,"mi_std.fit"))
  unlink(temp)
  
  # Form a standard USGS object
  sgdf<-formUSGSobject(meanhaz,sdhaz,I0) ; rm(meanhaz,sdhaz)
  
  return(sgdf)
  
}

# Using an API call, search through USGS database for a specific EQ & pre/aftershocks
SearchUSGSbbox<-function(bbox,sdate,fdate=NULL,minmag=5,exdays=c(5,14)){
  
  debut<-"https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson"
  geojsonR::FROM_GeoJson(paste0(debut,"&starttime=",as.Date(sdate)-exdays[1],"&endtime=",as.Date(fdate)+exdays[1],
                                "&minlongitude=",bbox[1],"&minlatitude=",bbox[2],
                                "&maxlongitude=",bbox[3],"&maxlatitude=",bbox[4],
                                "&minmagnitude=",minmag,"&orderby=magnitude",
                                "&producttype=shakemap"))%>%return
  
}

USGSskelly<-as.data.frame(matrix(NA,1,14))
colnames(USGSskelly)<-c("USGSid","date","PAGER","dataURL","visURL","minDepth","feltSc","magnitude","magunit","intensity","intunit","tsunami","centLon","centLat")

# Make sure the USGS data wasn't empty or entirely outside of the specified boundary box
check_hazsdf<-function(hazsdf=NULL,minmag,bbox=NULL){
  if(is.null(hazsdf)) return(F)
  # Check if the bounding box of the hazard lies within the specified search area
  if(!is.null(bbox)){
    if (all(hazsdf@bbox[c(1,3)]<bbox[1]) | all(hazsdf@bbox[c(1,3)]>bbox[3]) |
        all(hazsdf@bbox[c(2,4)]<bbox[2]) | all(hazsdf@bbox[c(2,4)]>bbox[4])) {
      return(F)
    }
  }
  # Check that the maximum intensity of the earthquake is higher than our limit
  if(max(hazsdf@data$mmi_mean)<minmag) return(F)
  return(T)
}

GetUSGS_id<-function(USGSid,titlz="tmp",I0=4.5,minmag=5,earlysort=F){
  
  url<-paste0("https://earthquake.usgs.gov/fdsnws/event/1/query?eventid=",USGSid,"&format=geojson")
  tmp<-FROM_GeoJson(url)
  hazsdf<-tryCatch(ExtractUSGS(url = tmp$properties$products$shakemap[[1]]$contents$`download/raster.zip`$url,
                               namer = paste0("./RawData/MostlyHazardData/EQ/",USGSid),
                               I0=I0),
                   error=function(e) NULL)
  if(is.null(hazsdf)) {
    print(paste0("Error extracting USGS id ",USGSid))
    return(NULL)
  } else if(!check_hazsdf(hazsdf,I0)) {
    print(paste0(max(hazsdf@data$mmi_mean,na.rm = T),
                 ": Either the hazard doesn't exist in USGS, or the magnitude is below 4.5 MMI"))
    return(NULL)
  }
  
  if(earlysort) return(hazsdf)
  # Extract the date of the event
  sdate<-as.Date(tmp$properties$products$dyfi[[1]]$properties$eventtime)
  
  if(length(sdate)==0){
    sdate <- as.Date(tmp$properties$products$shakemap[[1]]$properties$eventtime)
  }
  
  print(sdate)
  
  return(new("HAZARD",
             obj=hazsdf,
             hazard="EQ",
             dater=as.Date(sdate),
             I0=I0,
             alertlevel=ifelse(is.null(tmp$properties$alert),"green",tmp$properties$alert),
             #alertscore=ifelse(i<=length(alertscores),alertscores[i],0))
             alertscore=NA_real_))
  
}

# WHY IS USGS SO DIFFICULT? GIVE ME A DATE!
GetUSGSdatetime<-function(USGSid){
  url<-paste0("https://earthquake.usgs.gov/fdsnws/event/1/query?eventid=",USGSid,"&format=geojson")
  tmp<-geojsonR::FROM_GeoJson(url)
  eventtime <- tmp$properties$products$dyfi[[1]]$properties$eventtime
  if(is.null(eventtime)){
    eventtime <- tmp$properties$products$shakemap[[1]]$properties$eventtime
  }
  return(as.Date(eventtime))
}

metaUSGS<-function(featie){
  
  date<-tryCatch(GetUSGSdatetime(featie$id),error=function(e) NA_Date_)
  
  data.frame(USGSid=featie$id,
             date=date,
             PAGER=ifelse(is.null(featie$properties$alert),NA,featie$properties$alert),
             dataURL=ifelse(is.null(featie$properties$detail),NA,featie$properties$detail),
             visURL=ifelse(is.null(featie$properties$url),NA,featie$properties$url),
             minDepth=ifelse(is.null(featie$properties$dmin),NA,featie$properties$dmin),
             feltSc=ifelse(is.null(featie$properties$felt),NA,featie$properties$felt),
             magnitude=ifelse(is.null(featie$properties$mag),NA,featie$properties$mag),
             magunit=ifelse(is.null(featie$properties$magType),NA,featie$properties$magType),
             intensity=ifelse(is.null(featie$properties$mmi),NA,featie$properties$mmi),
             intunit="MMI",
             tsunami=ifelse(is.null(featie$properties$tsunami),NA,featie$properties$tsunami),
             centLon=ifelse(is.null(featie$geometry$coordinates[1]),NA,featie$geometry$coordinates[1]),
             centLat=ifelse(is.null(featie$geometry$coordinates[2]),NA,featie$geometry$coordinates[2]))
}

MatchUSGS<-function(impies,noextract=F){
  # Check for duplicated entries
  inds<-!duplicated(impies); inds[is.na(inds)]<-F
  # Don't unecessarily spam USGS
  indind<-!impies%>%dplyr::select(event_ID)%>%duplicated & inds
  # Extended boundary boxes of countries
  bbies<-GenerateExpBBOX(unique(impies$ISO3[indind]),
                         expPartin=T,reducer=T,expFact=5)
  # Filter out the countries that don't have boundary boxes
  impies%<>%filter(ISO3%in%bbies$ISO3CD)
  # Extract the boundary
  out<-do.call(rbind,lapply(which(indind),function(i) {
    print(impies$event_ID[i])
    subbb<-bbies%>%filter(ISO3CD==impies$ISO3[i])%>%dplyr::select(-c(ISO3CD,i))
    
    outin<-do.call(rbind,lapply(1:nrow(subbb),function(j){
      # Try to find the event using the USGS search function
      tmp<-tryCatch(SearchUSGSbbox(subbb[j,],impies$ev_sdate[i],impies$ev_fdate[i],minmag=5,exdays = c(2,2)),
                    error=function(e) NA)
      # Check for fails
      if(all(is.na(tmp))) return(cbind(USGSskelly,impies[i,],
                                       data.frame(i=i,
                                                  mnlo=subbb$mnlo[j], 
                                                  mnla=subbb$mnla[j], 
                                                  mxlo=subbb$mxlo[j], 
                                                  mxla=subbb$mxla[j])))
      if(length(tmp$features)==0) return(cbind(USGSskelly,impies[i,],
                                               data.frame(i=i,
                                                          mnlo=subbb$mnlo[j], 
                                                          mnla=subbb$mnla[j], 
                                                          mxlo=subbb$mxlo[j], 
                                                          mxla=subbb$mxla[j])))
      # Extract all the important detail that we need
      usinf<-do.call(rbind,lapply(1:length(tmp$features),
                                  function(j) tryCatch(metaUSGS(tmp$features[[j]]),
                                                       error=function(e) USGSskelly)))
      usinf$ISO3<-impies$ISO3[i]; usinf$i<-i
      usinf$mnlo<-subbb$mnlo[j]; usinf$mnla<-subbb$mnla[j]; usinf$mxlo<-subbb$mxlo[j]; usinf$mxla<-subbb$mxla[j]
      
      return(merge(usinf,impies[i,],by="ISO3"))
    }))
    
    if(sum(!is.na(outin$USGSid))>0) print("success")
    # outin%>%distinct()%>%filter(!is.na(USGSid))
    outin
    
  }))
  # Prioritise extracting the events with the largest impact first
  out%<>%arrange(desc(imp_value))
  # Save out, just in case!
  saveRDS(out,"./RawData/MatchedEQ_hazimp_0D_20230627.RData")
  # If this was all you needed...
  if(noextract) return(out)
  # Make sure to get rid of anything that was likely to have a small impact
  out%<>%filter(intensity>4.5 & !is.na(USGSid))
  # Now download the hell out of everythiiiiing! Thanks USGS, spam away!
  out$downloaded<-sapply(1:nrow(out),function(i){
    print(out$event_ID[i])
    if(file.exists(paste0("./CleanedData/MostlyHazardData/EQ/",out$event_ID[i],"_",out$USGSid[i],".RData"))){
      print("Already there!")
      return(T)
    }
    hazzy<-tryCatch(GetUSGS_id(out$USGSid[i],titlz=paste0("./RawData/MostlyHazardData/EQ/"),I0=4.5,minmag=5,earlysort=T),error=function(e) NULL)
    if(is.null(hazzy)) return(F)
    print("success")
    saveRDS(hazzy,paste0("./CleanedData/MostlyHazardData/EQ/",out$event_ID[i],"_",out$USGSid[i],".RData"))
    return(T)
  })
  # Save out, just in case!
  saveRDS(out,"./RawData/MatchedEQ_hazimp_2D_20230627.RData")
  # out$overlap<-parallel::mclapply((1:nrow(out))[out$downloaded],function(i){
  #   print(out$event_ID[i])
  #   if(!file.exists(paste0("./CleanedData/MostlyHazardData/EQ/",out$event_ID[i],"_",out$USGSid[i],".RData"))){
  #     print("File not found")
  #     return(F)
  #   }
  #   hazzy<-readRDS(paste0("./CleanedData/MostlyHazardData/EQ/",out$event_ID[i],"_",out$USGSid[i],".RData"))
  #   bbox<-hazzy@bbox; bbox[]
  #   return(bbox_overlap(bbox,out[i,c("mnlo","mnla","mxlo","mxla")]))
  # },mc.cores = ceiling(parallel::detectCores()/2))
  # 
  # saveRDS(out,"./RawData/MatchedEQ_hazimp_2D_overlayed.RData")
  
  return(out)
}













# out$inBBOX<-out$centLon>out$mnlo & out$centLon<out$mxlo &
#   out$centLat>out$mnla & out$centLat<out$mxla
# 
# out$Dmnlo<-abs(out$mnlo-out$centLon)
# out$Dmxlo<-abs(out$mxlo-out$centLon)
# out$Dmnla<-abs(out$mnla-out$centLat)
# out$Dmxla<-abs(out$mxla-out$centLat)
# 
# out$minDlon<-sapply(1:nrow(out),function(i) min(out$Dmnlo[i],out$Dmxlo[i]))
# out$minDlat<-sapply(1:nrow(out),function(i) min(out$Dmnla[i],out$Dmxla[i]))
# 
# out$distance<-sqrt(out$minDlat^2+out$minDlon^2)
# 
# out$Dmnlo<-out$Dmnla<-out$Dmxlo<-out$Dmxla<-NULL

# Extract EQ data from USGS for a specified event
GetUSGS<-function(USGSid=NULL,bbox,sdate,fdate=NULL,titlz="tmp",I0=4.5,minmag=5){
  
  if(!is.null(USGSid)) {
    hazsdf<-GetUSGS_id(USGSid)
    if(is.null(hazsdf)) return(NULL)
    bbox<-hazsdf@bbox
    USGS<-SearchUSGSbbox(expandBbox(hazsdf@bbox,f = 200,scaling = F),
                         hazsdf@eventdate-7,hazsdf@eventdate+14,minmag)  
    sdate<-fdate<-hazsdf@eventdate
    lenny<-length(USGS$features)
    # Check that the original USGS id shakemap is contained inside the USGS extracted files
    ids<-unlist(sapply(USGS$features,function(x) x$id))
    if(lenny<=1 | !any(ids==USGSid)) {
      lhazdat<-list(hazard_info=list(bbox=bbox,sdate=sdate,fdate=fdate,NumEvents=1,
                                     hazard="EQ",I0=I0,eventdates=sdate),hazsdf)
      # lhazdat<-c(list(bbox=bbox,sdate=sdate,fdate=fdate,
      #               NumEvents=1,hazard="EQ",I0=I0,eventdates=sdate,hazsdf))
      return(lhazdat)
    } else{
      for (i in 1:lenny){
        USGSdate<-GetUSGSdatetime(USGS$features[[i]]$id)
        sdate<-min(sdate,USGSdate)
        fdate<-max(fdate,USGSdate)  
      }
    }
    
  } else {
    # Automatically assign end date if not specified (or badly specified)
    if(is.null(fdate)) {
      fdate=min(Sys.Date(),(as.Date(sdate)+10))
    } else fdate=min(Sys.Date(),as.Date(fdate)+1)
    # Search through the USGS events
    USGS<-SearchUSGSbbox(bbox,sdate,fdate,minmag)  
    lenny<-length(USGS$features)
  }
  
  # if no events are found:
  if(lenny==0) return(NULL)
  
  # lhazdat<-c(list(bbox=bbox,sdate=sdate,fdate=fdate,NumEvents=lenny,hazard="EQ",I0=I0,eventdates=NULL))
  lhazdat<-list(hazard_info=list(bbox=bbox,sdate=sdate,fdate=fdate,NumEvents=lenny,hazard="EQ",I0=I0,eventdates=c()))
  tbbox<-rep(NA,4)
  for (i in 1:lenny){
    # Find the details of the raster file for the EQ in the USGS database
    tmp<-FROM_GeoJson(USGS$features[[i]]$properties$detail)
    # Extract EQ raster of hazard intensity
    hazsdf<-tryCatch(ExtractUSGS(url = tmp$properties$products$shakemap[[1]]$contents$`download/raster.zip`$url,
                                 namer = paste0(directory,"Disaster_Data/USGS/",titlz,i),
                                 I0=I0),
                     error=function(e) NULL)
    # Check that this extracted event is in the correct form
    if(!check_hazsdf(hazsdf,minmag,bbox)){
      lhazdat$hazard_info$NumEvents<-lhazdat$hazard_info$NumEvents-1
      next
    }
    # Create HAZARD object
    hazsdf<-new("HAZARD",
                obj=hazsdf,
                hazard="EQ",
                dater=GetUSGSdatetime(USGS$features[[i]]$id),
                I0=I0,
                alertlevel=ifelse(is.null(tmp$properties$alert),"green",tmp$properties$alert),
                #alertscore=ifelse(i<=length(alertscores),alertscores[i],0))
                alertscore=0)
    # Add to the list of hazards
    lhazdat[[length(lhazdat)+1]]<-hazsdf
    # Extend the bounding box to account for this earthquake
    tbbox[c(1,2)]<-apply(cbind(tbbox[c(1,2)],hazsdf@bbox[c(1,2)]),1,min,na.rm=T)
    tbbox[c(3,4)]<-apply(cbind(tbbox[c(3,4)],hazsdf@bbox[c(3,4)]),1,max,na.rm=T)
    # Extract dates for each hazard event
    lhazdat$hazard_info$eventdates%<>%c(as.character(hazsdf@eventdate))
  }
  if(any(is.na(tbbox))) return(NULL)
  # Modify the bounding box to fit around all hazards extracted
  lhazdat$hazard_info$bbox<-tbbox
  lhazdat$hazard_info$eventdates%<>%as.Date()
  lhazdat$hazard_info$fdate<-max(lhazdat$hazard_info$eventdates)
  return(lhazdat)
  
}

