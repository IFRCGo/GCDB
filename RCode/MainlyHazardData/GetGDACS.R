
GDACSconvDis<-function(d_type){
  
  if(d_type=="Mass movement") stop("GetGDACS.R Error: GDACS has no mass movement data [03/2020]")
  if(d_type=="Wildfire") stop("GetGDACS.R Error: GDACS has no wildfire data [03/2020]")
  if(d_type=="Extreme temperature") stop("GetGDACS.R Error: GDACS has no extreme temperature data [03/2020]")
  if(is.na(d_type)) stop("GetGDACS.R Error: disaster type provided is not recognised by GDACS")
  
  if(d_type=="Storm"){
    print("Warning: for IDMC 'Storm', GDACS has only Tropical Cyclones or Violent Wind [03/2020]")
    return(c("TC","VW"))
  }
  
  d_choice<-c(
    "Drought"="DR",
    "Tropical Cyclones"="TC",
    "Violent Wind"="VW",
    "Flood"="FL",
    "Volcanic eruption"="VO",
    "Earthquake"="EQ"
  )

  if (is.na(d_choice[d_type])){stop("GetGDACS.R Error: input disaster type (e.g. 'Severe Storms') does not exist")}  
  
  return(d_choice[d_type])
  
}

GetGDACS_API<-function(haz=NULL,syear=2016,fyear=2020,alertlist=NULL){
  
  # stop("Check first whether the file exists on the computer")
  
  loc<-"https://www.gdacs.org/gdacsapi/api/events/geteventlist/SEARCH?eventlist="
  
  if(is.null(haz)) haz<-c("DR","TC","TS","FL","VO","EQ")
  if(is.null(alertlist)) alertlist<-c("red","orange","green")
  
  GDACS_yrs<-syear:fyear
  # ev<-paste(l_ev, collapse = ";")
  list_GDACS<-list()
  for (alert in alertlist){
    GDACS<-data.frame()
    for (ev in haz){
      eGDACS<-data.frame()
      for (year in GDACS_yrs){
        
        url<-paste0(loc,ev,"&year=",year,"&alertlevel=",alert)
        tGDACS<-try(FROM_GeoJson(url_file_string = url),silent = T) 
        if(!typeof(tGDACS)=="list"){
          print(paste0("Warning: GetGDACSsummary data API GET for : ",url))
          next
        }
        tGDACS<-tGDACS$features
        print(paste0("Retrieved ",length(tGDACS)," events from ",url))
        list_GDACS<-c(list_GDACS,tGDACS)

      }

    }
    
  }
  
  #saveRDS(list_GDACS,file = paste0(directory,"Disaster_Data/GDACS/GDACS_",haz,"_",syear,"-",fyear,"_events.Rdata"))
  return(list_GDACS)
  
} 

### Function to sort out expected country values used by GDACS ###
SortGDACSiso<-function(country){
  
  country<-trimws(country, "b")
  country<-str_squish(country)
  if(country=="") return(data.frame(country=NA,ISO3=NA))
  
  #@@@ EXCEPTIONS @@@#
  tdf<-data.frame()
  
  country<-gsub("Virgin Islands, U.S., British,","Virgin Islands, U.S., Virgin Islands, British,",country,fixed = TRUE)
  
  cexcept<-c("Korea, Republic of","Korea, Democratic People's Republic of",
             "Democratic People's Republic of, Korea",
             "Virgin Islands, U.S.","Virgin Islands, British",
             "E. Coast Of N. Island, N.Z.","East Of North Island, N.Z.",
             "W. Caroline Islands, Micronesia","E. Caroline Islands, Micronesia",
             "Minahassa Peninsula, Sulawesi","Andreanof Islands, Aleutian Is.",
             "Fox Islands, Aleutian Islands","Admiralty Islands Region, P.N.G.",
             "Santiago Del Estero Prov., Arg.","Miscellaneous (French) Indian Ocean Islands",
             "Rat Islands, Aleutian Islands", "Netherlands Antilles","Türkiye","Kosovo")
  
  rexcept<-c(NA,NA,NA,NA,NA, "New Zealand","New Zealand",
             "Micronesia, Federated States of","Micronesia, Federated States of",
             "Indonesia", "United States of America","United States of America",
             "Papa New Guinea","Argentina","France","United States of America",NA,NA,NA)
  
  iexcept<-c("KOR","PRK","PRK","VIR","VGB","NZL","NZL","FSM","FSM","IDN","USA","USA","PNG","ARG","FRA","USA","ANT","TUR","XXK")
  
  for (i in 1:length(cexcept)){
    if (any(grepl(cexcept[i],country,fixed = TRUE))){
      if(is.na(rexcept[i])) {
        tdf<-rbind(tdf,data.frame(country=cexcept[i],ISO3=iexcept[i]))
      } else {
        tdf<-rbind(tdf,data.frame(country=rexcept[i],ISO3=iexcept[i]))
      }
      country<-gsub(cexcept[i],"",country,fixed = TRUE)
    }
    
  }
  
  if(str_squish(country)=="") return(tdf)
  
  # Check and sort if '|' is used to mention multiple countries
  ct<-str_squish(trimws(unlist(strsplit(country,"|", fixed=TRUE)), "b"))
  ct<-ct[!ct==""]
  
  # if it doesn't split up using '|'
  if(length(ct)==1){
    
    # Check and sort if ',' is used to mention multiple countries
    ct<-str_squish(trimws(unlist(strsplit(country,",", fixed=TRUE)), "b"))
    ct<-ct[!ct==""]
    
    # no checks are possible if country string doesn't split up, rely on countrycode to find value.
    if(length(ct)==1){
      ttt<-countrycode(ct, origin ='country.name', destination ='iso3c',warn = FALSE)
      return(rbind(tdf,data.frame(country=country,ISO3=ttt)))
    }
    
    # If it splits by ',' make checks
    ttt<-countrycode(ct, origin ='country.name', destination ='iso3c',warn = FALSE)
    
    if (length(ttt[!is.na(ttt)])<=1){
      # Check that the comma wasn't for another country, e.g. Congo, Democratic Republic of
      ttt2<-countrycode(country, origin ='country.name', destination ='iso3c',warn = FALSE)
      if(!is.na(ttt2)) {return(rbind(tdf,data.frame(country=country,ISO3=ttt2)))}
      
      # Checks are over
      print(paste0("Warning: GDACS country name ",country," might not be translated properly to iso3"))
      return(rbind(tdf,data.frame(country=ct,ISO3=ttt)))      
    }
    
    # Check for duplicated values 
    ct<-ct[!duplicated(ttt)]
    ttt<-ttt[!duplicated(ttt)]
    
    if (!anyNA(ttt)) {return(rbind(tdf,data.frame(country=ct,ISO3=ttt)))}
    
    # last resort: cancel splitting for ','
    print(paste0("Warning: GDACS country name ",country," might not be properly translated to iso3"))
    print(ct)
    print(ttt)
    return(rbind(tdf,data.frame(country=ct,ISO3=ttt)))
    
  }
  
  # Country splits via '|'
  ttt<-countrycode(ct, origin ='country.name', destination ='iso3c',warn = FALSE)
  ct<-ct[!duplicated(ttt)]
  ttt<-ttt[!duplicated(ttt)]
  if(anyNA(ttt)){
    print(paste0("Warning: iso not found for country: "))
    print(cbind(ct[is.na(ttt)],ttt[is.na(ttt)]))
  }
  return(rbind(tdf,data.frame(country=ct,ISO3=ttt)))
  
}

severitysplitter<-function(haz,txt){
  if(haz=="EQ") {
    sev<-as.numeric(gsub("[^0-9.]", "",  strsplit(txt$severitytext,split = ",")[[1]]))
    return(list(haz_sev=sev[1],haz_sev_add=sev[2],haz_unit="M",haz_add_unit="km"))
  } 
  
  # if(!is.null(txt$severity) & str_remove(txt$severity," ")!="" & 
  #    !is.null(txt$severityunit) & str_remove(txt$severityunit," ")!="") 
    return(list(haz_sev=txt$severity,haz_sev_add=NA_character_,
                haz_unit=txt$severityunit,haz_add_unit=NA_character_))
}

GetIntMap<-function(hazard="EQ"){
  if(hazard=="EQ"){
    return(seq(from = 5,to = 9,by = 0.5))
  } else stop("IIDIPUS NOT READY FOR ALTERNATIVE HAZARDS THAN EARTHQUAKE")
}

FilterGDACS<-function(haz=NULL,syear=2016L,fyear=AsYear(Sys.Date()),list_GDACS=NULL,red=F){
  
  if(is.null(list_GDACS)) list_GDACS<-GetGDACS_API(haz,syear,fyear)
  
  # Filter countries
  dfGDACS<-data.frame()
  for (i in 1:length(list_GDACS)){
    
    tmp<-list_GDACS[[i]]
    
    # Use the function built to filter GDACS naming conventions and map to iso3    
    if (is.na(tmp$properties$country)|is.null(tmp$properties$country)|tmp$properties$country %in% c(""," ","  ")){
      # use NA and iso3 values if country is empty:
      if (is.null(tmp$properties$iso3)) tmp$properties$iso3<-NA
      dfct<-data.frame(country=rep(NA,length(tmp$properties$iso3)),ISO3=tmp$properties$iso3)
    } else {
      dfct<-SortGDACSiso(tmp$properties$country)
      if (!is.null(tmp$properties$iso3) & !all(tmp$properties$iso3%in%dfct$ISO3))
        print(paste0("Diff ISOs: ",
                     paste0(tmp$properties$iso3,collapse = ","),
                     "!=",paste0(dfct$ISO3,collapse = ",")))
      if (!is.null(tmp$properties$iso3) & !all(is.na(dfct$ISO3))) dfct$ISO3<-tmp$properties$iso3
    }
    
    len<-length(dfct$country)
    txt<-severitysplitter(tmp$properties$eventtype,tmp$properties$severitydata)
    
    tmp$properties$glide<-ifelse(str_remove_all(tmp$properties$glide," ")=="",NA_character_,tmp$properties$glide)
    
    if(len>1 & !is.na(tmp$properties$glide) & str_remove_all(tmp$properties$glide," ")!="" &
       nchar(tmp$properties$glide)!=18) print(paste0("Check the GLIDE: ",tmp$properties$glide))
    
    for (j in 1:length(tmp$properties$episodealertlevel)){
      
      dfGDACS<-rbind(dfGDACS,data.frame(alert=rep(trimws(tolower(tmp$properties$episodealertlevel[j]), "b"),len),
                                        alertscore=rep(tmp$properties$episodealertscore[j],len),
                                        eventid=rep(tmp$properties$eventid,len),
                                        ev_name_orig=rep(tmp$properties$eventname,len),
                                        episodeid=rep(tmp$properties$episodeid,len),
                                        link=rep(tmp$properties$url$details,len),
                                        ISO3=dfct$ISO3,
                                        country=dfct$country,
                                        ev_sdate=rep(as.Date(as.POSIXct(tmp$properties$fromdate),format = "%Y%m%d"),len),
                                        ev_fdate=rep(as.Date(as.POSIXct(tmp$properties$todate),format = "%Y%m%d"),len),
                                        haz_Ab=rep(tmp$properties$eventtype,len),
                                        hazard_severity=rep(tmp$properties$severitydata$severity,len),
                                        txt,
                                        GLIDE=rep(tmp$properties$glide,len),
                                        geom_type=rep(tmp$geometry$type,len),
                                        cent_lon=rep(tmp$geometry$coordinates[1],len),
                                        cent_lat=rep(tmp$geometry$coordinates[2],len),
                                        src_URL=rep(tmp$properties$url$geometry,len)))
    }
    
  }  
  
  dfGDACS$alertscore[dfGDACS$alertscore<0]<-0
  
  if(red) dfGDACS%>%dplyr::select(c(alertscore,hazard_severity,ISO3,sdate,fdate,long,lat))%>%return
  
  dfGDACS$GCDB_ID<-GetGCDB_ID(dfGDACS)
  
  return(dfGDACS)
  
}

convGDACS_GCDB<-function(GDACS){
  
  GDACS$imp_sdate<-GIDD$imp_unitdate<-GDACS$ev_sdate
  GDACS$imp_fdate<-GDACS$ev_fdate
  GDACS$ev_name_en<-GDACS$ev_name_orig
  # Add the continent, then remove the unnecesary layers
  GDACS%<>%mutate(Continent=convIso3Continent(ISO3))%>%
    filter(!is.na(Continent))
  # Add alertscore as the impact value
  GDACS$imp_value<-GDACS$alertscore
  # This estimate is modelled
  GDACS$imp_est_type<-"esttype_model"
  GDACS$haz_est_type<-"esttype_second"
  # Organisation
  GDACS$imp_src_db<-"GDACS"
  GDACS$imp_src_org<-"European Commission"
  GDACS$imp_src_orgtype<-"orgtyperio"
  colnames(GDACS)[colnames(GDACS)=="link"]<-"src_URL"
  
  c(
    
    "impsub_ID"="character", # ID of each impact element in the overall event
    "subhaz_ID"="character", # ID of each hazard of each impact element in the overall event
    "imp_cats"="character", # Impact category
    "imp_subcats"="character", # Impact subcategory
    "imp_det"="character", # Impact subsubcategory
    "imp_type"="character", # Impact units
    "imp_units"="character", # Impact unit (e.g. stock or currency)
    
    "haz_type"="character", # Impacting hazard type
    "haz_cluster"="character", # Impacting hazard cluster
    "haz_spec"="character", # Impacting specific hazard
    "haz_link"="character", # Associated impactful-hazards to the specific hazard
    "haz_potlink"="character", # Potential other impactful-hazards that may be associated to the specific hazard
    "spat_ID"="character", # ID of the spatial object
    "spat_type"="character", # Spatial object type
    "spat_res"="character", # Spatial resolution of impact estimate
    "spat_srcorg"="character") # Source organisation from where the spatial object comes from
}

GetGDACS_GCDB<-function(){
  # Extract the data
  GDACS<-FilterGDACS()
  # Store it out as a imp_GCDB object
  GDACS%<>%convGDACS_GCDB()
}

GetGDACSalertscore<-function(dfGDACS=NULL,haz,bbox,sdater,fdater=NULL,isos=NULL){
  
  if(any(is.null(c(haz,sdater,bbox)))) stop("Please provide hazard type, start date and bounding box to extract GDACS alertscore")
  
  if(is.null(fdater)) {
    fdater=min(Sys.Date(),(as.Date(sdater)+30))}
  else fdater=min(Sys.Date(),(as.Date(fdater)+10))
  
  syear<-AsYear(sdater); fyear<-AsYear(fdater)
  if(is.null(dfGDACS)) dfGDACS<-FilterGDACS(haz="EQ",syear=syear,fyear=fyear)
  
  dfGDACS%>%filter(sdate>=sdater & fdate<=fdater & 
           long>=bbox[1] & long<=bbox[3] & lat>=bbox[2] & lat<=bbox[4])%>%
    arrange(desc(alertscore))
  if(!is.null(isos)) dfGDACS%<>%filter(ISO3 %in% isos)
  
  dfGDACS%>%arrange(desc(alertscore))%>%pull(alertscore)%>%return
  
}

poly_ccodes<-c("eventid","episodeid","polygonlabel","Class","forecast","fromdate","todate","geometry")

GetGDACSPoly<-function(GDACS){
  
  BigPoly<-data.frame()
  for(ev in unique(GDACS$eventid)){
    # Find the appropriate elements of the GDACS database
    ind<-GDACS$eventid==ev
    # Get the shapefile
    poly<-suppressWarnings(geojsonsf::geojson_sf(unique(GDACS$geom_link[ind])))
    # Modify per hazard
    if(unique(GDACS$haz_Ab[ind])=="TC"){
      # Take only the three-level wind-speed risk boundaries
      poly%<>%filter(Class%in%c("Poly_Red","Poly_Orange","Poly_Green"))%>%
        dplyr::select(any_of(poly_ccodes))
    } else if(unique(GDACS$haz_Ab[ind])=="EQ"){
      # Check to see that there is some actual shakemap data contained in the polygon
      if(!any(grepl("intensity",colnames(poly)))) next
      # Take all the shakemap intensity boundaries available, except those below 4.5
      poly%<>%filter(grepl("Poly_SMP",Class) & intensity>=4.5)%>%
        mutate(polygonlabel=paste0("Intensity-",intensity),forecast=NA)%>%
        dplyr::select(any_of(poly_ccodes))
    } else if(unique(GDACS$haz_Ab[ind])=="FL"){
      # Take the 'affected' area... I have absolutely no idea what the definition is, nor the difference with 'global area'
      poly%<>%filter(Class%in%c("Poly_Affected"))%>%
        dplyr::select(any_of(poly_ccodes))%>%
        mutate(forecast=NA)
    } else if(unique(GDACS$haz_Ab[ind])=="DR"){
      # Take the 'affected' area to the drought, whatever that means
      poly%<>%filter(Class%in%c("Poly_area"))%>%
        dplyr::select(any_of(poly_ccodes))%>%
        mutate(forecast=NA)
    } else if(unique(GDACS$haz_Ab[ind])=="VO"){
      # Take both the forecast ("FCST") and observed ("OBS") cones
      poly%<>%filter(grepl("Poly_Cones_",Class) | grepl("Poly_Circle",Class))%>%
        dplyr::select(any_of(poly_ccodes))%>%
        mutate(forecast=NA)
    } else stop("Hazard not recognised in GDACS available hazards")
    
    
    
    
    BigPoly$hazsub_ID<-BigPoly$spat_ID<-paste0(unique())
    
    
    GetGCDB_ID(Dessie,haz=unique(GDACS$haz_Ab[ind]))
    
    
    
    
    
    # Join to the big motherbase
    BigPoly%<>%rbind(poly)
  }
  # Make sure to set the forecast variable as logical
  ind<-is.na(BigPoly$forecast)
  BigPoly$forecast[ind]<-F; BigPoly$forecast[!ind]<-T
  # Create a unique ID per shapefile
  BigPoly$hazsub_ID<-
  # convert to sp spatial class
  BigPoly%<>%as("Spatial")
  
  
  
  BigPoly@crs<-
    
    
    
    
    
  # Now left-join to the full GDACS database by 
  GDACS%<>%left_join(poly@data,by = "eventid")
  
  return(list(GDACS=GDACS,poly=BigPoly))
}

ShakeURL2Poly<-function(eventid,sid=1L,sil=T){
  
  st_url<-"https://www.gdacs.org/gdacsapi/api/shakemap/getgeometry?eventid="
  fn_url<-"&shakeid="
  url<-paste0(st_url,eventid,fn_url,sid)
  
  shake<-try(FROM_GeoJson(url_file_string = url),silent = sil)
  
  if(class(shake) == "try-error") stop(paste0("Warning: no GDACS SHAKE data found for GDACS event - ",eventid))
  
  # url2<-paste0("https://www.gdacs.org/gdacsapi/api/events/geteventdata?eventtype=EQ&eventid=",eventid)
  # info<-try(FROM_GeoJson(url_file_string = url2),silent = T)
  # 
  # if(class(info) == "try-error") stop(paste0("Warning: no GDACS INFO data found for GDACS event - ",eventid))
  # 
  # alertscore<-info$properties$episodealertscore
  # sdate<-as.Date(info$properties$fromdate)
  
  len<-length(shake$features)
  poly<-data.frame()
  
  for (i in 1:len){
    # Loop over multiple polygons of same hazard intensity
    for (j in 1:length(shake$features[[i]]$geometry$coordinates)){
      # THANKS GDACS... VDM
      check<-tryCatch(shake$features[[i]]$geometry$coordinates[[j]][[1]][,1],error = function(e) NULL)
      if(!is.null(check)) shake$features[[i]]$geometry$coordinates[[j]]<-shake$features[[i]]$geometry$coordinates[[j]][[1]]
      
      long<-shake$features[[i]]$geometry$coordinates[[j]][,1]
      lat<-shake$features[[i]]$geometry$coordinates[[j]][,2]
      intensity<-shake$features[[i]]$properties$intensity
      if(length(intensity)>1) print(paste0("check intensity Shake2Poly ",intensity))
      
      poly<-rbind(poly,data.frame(eventid=rep(eventid,length(long)),Intensity=rep(intensity,length(long)),
                                  Longitude=long,Latitude=lat,ncontour=rep(j,length(long))))      
      
      # poly<-rbind(poly,data.frame(eventid=rep(eventid,length(long)),Intensity=rep(intensity,length(long)),
      #                             Longitude=long,Latitude=lat,date=rep(sdate,length(long)),
      #                             alertscore=rep(alertscore,length(long)),ncontour=rep(j,length(long))))
    }
  }
  
  return(poly)
  
}

GetShakeGDACS_ev<-function(GDB){
  
  qq<-1
  poly<-data.frame()
  
  if(is.na(GDB$eventid[1])) return(NULL)
  
  for (sid in 1:3){
  
    tp<-NULL
    k<-0
    tp<-tryCatch(ShakeURL2Poly(GDB$eventid[1], sid = sid, sil=T),error = function(e) NULL)
    while(is.null(tp)&k<5){
      tp<-tryCatch(ShakeURL2Poly(GDB$eventid[1], sid = sid),error = function(e) NULL)
      k<-k+1
    }
    # Check the output is not empty
    if(is.null(tp)&length(poly)==0L) {return(NULL)} else if (is.null(tp)) {return(poly)}
    
    ll<-length(tp$Longitude)
    poly<-rbind(poly,cbind(tp,alertscore=rep(GDB$alertscore[1],ll),
                           date=rep(GDB$sdate[1],ll),
                           id=rep(qq,ll)))
    
    if(GDB$haz_Ab[1]=="EQ"){
      poly%<>%filter(Intensity<GDB$hazard_severity[1])%>%
        rbind(data.frame(eventid=GDB$eventid[1],Intensity=GDB$hazard_severity[1],
                         Longitude=GDB$long[1],Latitude=GDB$lat[1],ncontour=0,
                         alertscore=GDB$alertscore[1],date=GDB$sdate[1],id=qq))
    }
    
    for (i in 2:length(GDB$alert)){
      
      if(is.na(GDB$eventid[i])) next
      
      tp<-tryCatch(ShakeURL2Poly(GDB$eventid[i], sid = sid),error = function(e) NULL)
      if(is.null(tp)&GDB$alertscore[i]<=1.5) {
        next
      } else {
        k<-0
        while(is.null(tp)&k<5){
          tp<-tryCatch(ShakeURL2Poly(GDB$eventid[i], sid = sid),error = function(e) NULL)
          k<-k+1
        }
        if(is.null(tp)) next
      }
      
      qq<-qq+1
      ll<-length(tp$Longitude)
      
      tpoly<-cbind(tp,alertscore=rep(GDB$alertscore[i],ll),
                   date=rep(GDB$sdate[i],ll),
                   id=rep(qq,ll))
      
      if(GDB$haz_Ab[i]=="EQ"){
        poly%<>%rbind(tpoly%>%
                        rbind(data.frame(eventid=GDB$eventid[i],Intensity=GDB$hazard_severity[i],
                                         Longitude=GDB$long[i],Latitude=GDB$lat[i],ncontour=0,
                                         alertscore=GDB$alertscore[i],date=GDB$sdate[i],id=qq)))
      }
      
    }
  
  }
  
  # poly$ncontour<-poly$ncontour+1L
  
  return(poly)
  
}

# Modified Omori - Gutenberg–Richter combined equations derived by Reasenberg and Jones (1989, 1994),
# Evaluated with parameters taken from 
# Hardebeck J.L, A.L. Llenos, A.J. Michael, M.T. Page and N. van der Elst. (2018). 
# 'Updated California Aftershock Parameters', Seismological Research Letters, vol. 90, pp. 262-270.
ModOmori<-function(M0){
  # Worst case scenario parameters given by:
  # https://earthquake.usgs.gov/data/oaf/background.php
  # MLE Rate, R, is Less than one earthquake per month
  R<-1/30
  # p and a are spatially dependent variables, we take somewhere inbetween worst case and median values
  p<-0.8
  a<--1.8
  # b is more or less a standard decay rate of aftershock magnitudes
  b<-1.1
  # Look only at 
  Mth<-5.0
  c<-0.03
  mnlim<-3
  mxlim<-15
  
    
  # Add a lower limit just to ensure that everything has been captured by the data.
  # if(M0<Mth) c<- -1
  # Return the time until the MLE rate R drops to one earthquake (of magnitude >= Mth) per month
  return(min(c(mxlim,max(c(mnlim,(10^(a+b*(M0-Mth))/R)^(1/p))))))
}

GetShakeGDACS<-function(dfGDACS,hazard="EQ",directory,plotty=FALSE){
  
  # url<-"https://www.gdacs.org/gdacsapi/api/shakemap/getdetails?id=9187"
  # url taken from dfGDACS$link
  
  dfGDACS%<>%filter(haz_Ab==hazard)%>%arrange(desc(hazard_severity))
  
  # Remove earthquakes that are unlikely to cause damage - they probably won't be in Helix
  if(hazard=="EQ") {
    Mc<-5
    # dfGDACS%<>%filter(hazard_severity>Mc)
    sub<-5
    sup<-15
  }
  
  qq<-1
  poly<-data.frame()
  
  # for (url in unique(dfGDACS$link)){
  while (length(dfGDACS$link)>0){
    
    # Read in worst intensity event shakemap
    url<-as.character(dfGDACS$link[1])
    tp<-tryCatch(Shake2Poly(url),error = function(e) NULL)
    
    # Check the output is not empty
    if(is.null(tp)) {
      tp<-cbind(tp,id=rep(NA,length(tp$date)))
      poly<-rbind(poly,tp)
      dfGDACS%<>%filter(link!=url)
      next
    }
    poly<-rbind(poly,cbind(tp,id=rep(qq,length(tp$date))))
    # Event ID & date of the worst (main) event
    mev<-unique(tp$eventid)
    mdate<-unique(tp$date)
    
    print(paste0(hazard,": ",unique(tp$Intensity)))
    
    # Find other hazards for this event
    evs<-dfGDACS %>% filter(sdate>(mdate-sub) & sdate<(mdate+sup))
    # Filter out any event outside of the radius of the larger earthquake
    long<-tp$Longitude
    lat<-tp$Latitude
    ids<-evs$eventid[point.in.polygon(evs$long,evs$lat,long,lat)>0]
    evs%<>%filter(eventid %in% ids)
    
    if(plotty){    
      # library("rnaturalearth")
      # library("rnaturalearthdata")
      # world <- ne_countries(scale = "medium", returnclass = "sf")
      # Make some pretty pictures
      p<-ggplot(evs,aes(x=sdate,y=hazard_severity)) + geom_point() + xlab("Date") + 
        ylab(paste0("Hazard Intensity [",evs$hazard_sev_unit[1],"]")) + 
        ggtitle(paste0(hazard," event ",mev))
      ggsave(paste0(mev,"_Aftershock.eps"), plot=p,path = paste0(directory,'Plots/GDACS/'),width = 5,height = 5)
      
      # p<-GetMapObj(bbox<-c(min(tp$Longitude), min(tp$Latitude), max(tp$Longitude), max(tp$Latitude)),world)
      for (j in unique(tp$ncontour)){
        p<-p+geom_polygon(data = filter(tp,ncontour==j),aes(x=Longitude,y=Latitude,group=Intensity,colour=Intensity),
                          alpha=0,na.rm = T,size=2)
      }
      p<-p+ggtitle(paste0(hazard," ",as.character(evs$country[1])," eventid ",mev))
      #p<-p+scale_color_gradient(low="mistyrose2", high="red")
      ggsave(paste0(mev,"_Shakemap.eps"), plot=p,path = paste0(directory,'Plots/GDACS/'),width = 5,height = 5)
    }
    
    evs%<>%filter(hazard_severity>Mc & eventid!=mev)
    
    for (j in 1:length(evs$eventid)){
      
      url<-as.character(evs$link[j])
      tp<-tryCatch(Shake2Poly(url),error = function(e) NULL)
      
      if(is.null(tp)) next
      
      poly<-rbind(poly,cbind(tp,id=rep(qq,length(tp$date))))  
      print(paste0(hazard,": (subset) ",unique(tp$Intensity)))
      
    }  
    
    save(poly,paste0(directory,"Disaster_Data/GDACS/GDACS_",hazard,"_polygons.Rdata"))
    
    dfGDACS%<>%filter(!(eventid %in% c(mev,evs$eventid)))
    
    qq<-qq+1
    
  }
  
  return(poly)
  
}

PolyDuplicates<-function(dfpoly){
  
  dfpoly<-distinct(dfpoly)
  return(rbind(dfpoly,dfpoly[1,]))
  
}

ReducePolyEvent<-function(polys){
  
  namer<-c()
  
  fpoly<-list()
  
  for (iii in sort(unique(polys$Intensity))){
    
    ipolys<-filter(polys,Intensity==iii)
    tlpoly<-NULL
    
    if(max(ipolys$ncontour)==0) next
    
    for (n in 1:max(ipolys$ncontour)){
      
      nipolys<-filter(ipolys, ncontour==n)
      
      minnie<-min(nipolys$id)
      maxxie<-max(nipolys$id)
      
      ttt<-filter(nipolys, id==minnie)
      ttt<-PolyDuplicates(ttt)
      tmp<-st_polygon(list(as.matrix(dplyr::select(ttt,c(Longitude,Latitude)))))
      
      if(is.null(tlpoly)){tlpoly<-tmp} else {tlpoly<-st_union(tlpoly,tmp,by_feature = T)}
      if(!st_is_valid(tlpoly)) stop(paste0("Non-valid POLYGON for event ",polys$eventid,". iii,n: ",iii,n))
      
      if(maxxie-minnie!=0L){
        for (j in (minnie+1):maxxie){
          
          ttt<-filter(nipolys, id==minnie)
          ttt<-PolyDuplicates(ttt)
          tmp<-st_polygon(list(as.matrix(dplyr::select(ttt,c(Longitude,Latitude)))))
          
          if(is.null(tlpoly)){tlpoly<-tmp} else {tlpoly<-st_union(tlpoly,tmp,by_feature = T)}
          if(!st_is_valid(tlpoly)) stop(paste0("Non-valid POLYGON for event ",polys$eventid,". iii,n: ",iii,n))
          
        }
        
      }
      
    }
    
    if(is.null(tlpoly)) next
    # combine to make one multipolygon list
    tlpoly<-st_sfc(tlpoly,crs = "+proj=longlat +datum=WGS84")
    if(!st_is_valid(tlpoly)) stop(paste0("Non-valid MULTIPOLYGON for event ",ev,". iii: ",iii))
    # a<-st_area(stmpoly)
    # set_units(a,km^2)
    namer<-c(namer,as.character(iii))  
    fpoly<-c(fpoly,list(tlpoly))
    
  }
  
  names(fpoly)<-namer
  return(fpoly)
}

ReducePolyALL<-function(polys){
  
  ename<-list()
  totalpoly<-list()
  
  for (ev in unique(polys$helix_id)){
    
    epoly<-polys%>%filter(helix_id==ev)
    
    fpoly<-ReducePolyEvent(epoly)
    
    ename<-c(ename,as.character(ev))
    totalpoly<-c(totalpoly,list(fpoly))
    
  }
  
  names(totalpoly)<-ename
  return(totalpoly)
  
}

PolyIntegrateData<-function(melty,polys,IntMap=NULL,func=NULL){
  
  if(is.null(IntMap)) IntMap<-GetIntMap()
  if(is.null(func)) {func<-match.fun(mean)} else func<-match.fun(func)
  
  labs<-as.numeric(names(polys))
  
  # Filter data according to polygon form
  tmp<-st_coordinates(polys[[which.min(labs)]])
  melty%<>%filter(X<=max(tmp[,1]) &
                    X>=min(tmp[,1]) &
                    Y<=max(tmp[,2]) &
                    Y>=min(tmp[,2]))
  
  outDF<-data.frame()
  # for(Int in unique(polys$Intensity)){
  maxlab<-max(labs,na.rm = T)
  for(Int in IntMap){
    
    if(!Int%in%labs & Int<maxlab) {outDF<-rbind(outDF,data.frame(Intensity=Int,area=NA,value=NA)); next}
    if(!Int%in%labs & Int>maxlab) {outDF<-rbind(outDF,data.frame(Intensity=Int,area=0,value=0)); next}
    
    area<-st_area(polys[[as.character(Int)]])
    coords<-ExtractPolyCoords(polys[[as.character(Int)]])
    lennie<-max(unique(coords[,3]))
    
    tmp<-melty%>%filter(X<=max(coords[,1]) &
                          X>=min(coords[,1]) &
                          Y<=max(coords[,2]) &
                          Y>=min(coords[,2]))
    
    vals<-c()
    
    for(i in 1:lennie){
      
      # Calculate the sum of the value
      ind<-point.in.polygon(tmp$X,tmp$Y,coords[coords[,3]==i,1],coords[coords[,3]==i,2])
      # We include boundary points
      ind[ind>1]<-1
      ind<-as.logical(ind)
      
      vals<-c(vals,tmp$data[ind])
      
    }
    
    if(is.null(vals)) {value<-0} else {value<-func(vals,na.rm=T)}
    
    outDF<-rbind(outDF,data.frame(Intensity=Int,area=area,value=value))
    # dist<-rbind(dist,data.frame(Intensity=rep(Int,length(melty$data[ind])),value=melty$data[ind]))
  }
  
  return(outDF)
  
}

PolyIntegrateData_old<-function(data,poly,Ldist=FALSE,av=FALSE){
  
  long<-as.numeric(rownames(data))
  nlong<-length(long)
  lat<-as.numeric(colnames(data))
  nlat<-length(lat)
  
  melty<-melt(data);colnames(melty)<-c("X","Y","data")
  
  DF<-dist<-data.frame()
  for(Int in unique(poly$Intensity)){
    tmp<-poly%>%filter(Intensity==Int)
    area<-value<-0
    #for(cont in unique(tmp$ncontour)){
    cont<-1
    tmp2<-tmp%>%filter(ncontour==cont)
    # Calculate the polygon area
    sp1<-spPolygons(cbind(lon=tmp2$Longitude,lat=tmp2$Latitude), crs="+proj=longlat +datum=WGS84")
    mp1 <- makePoly(sp1, interval=1000)
    print(area(sp1))
    print(area(mp1))
    asub<-areaPolygon(mp1)*1e-3
    # Calculate the sum of the value
    ind<-point.in.polygon(melty$X,melty$Y,tmp2$Longitude,tmp2$Latitude)
    # We include boundary points
    ind[ind>1]<-1
    ind<-as.logical(ind)
    
    if(av) {
      value<-value+median(melty$data[ind],na.rm=T)/length(melty$data[ind])
    } else {
      value<-value+sum(melty$data[ind],na.rm=T) 
    }
    area<-area+asub
    #}
    DF<-rbind(DF,data.frame(Intensity=Int,area=area,value=value))
    dist<-rbind(dist,data.frame(Intensity=rep(Int,length(melty$data[ind])),value=melty$data[ind]))
  }
  
  if(Ldist) return(dist)
  return(DF)
  
}

# longData<-melt(PHL_GDP)
# longData<-longData[longData$value!=0,]
# cities<-maps::world.cities%>%filter(lat>bbox[2]&lat<bbox[4]&long>bbox[1]&long<bbox[3])%>%arrange(desc(pop))
# if(ncity>1){wordcloud::wordcloud(words=cities$name,freq = cities$pop,max.words = 30,scale = c(2.5,0.2))}
# cities<-slice(cities,1:ncity)
# p<-ggplot(longData, aes(x = Var1, y = Var2)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient(low="grey90", high="red",name = "GDP (PPP) [$]") +
#   labs(x="Longitude", y="Latitude", title="Philippines Earthquake 15 December 2019") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))
# for (j in unique(poly$ncontour)){
#   p<-p+geom_polygon(data = filter(poly,ncontour==j),aes(x=Longitude,y=Latitude,group=Intensity,colour=Intensity),
#                     alpha=0,na.rm = T,size=2)
# }
# p<-p+geom_label(data = cities, aes(long, lat, label = name), size = 4, fontface = "bold", nudge_x = 0.15,nudge_y = -0.15)
# p

# 
# p<-ggplot(PGDP,aes(value,group=Intensity)) + geom_density(aes(colour=Intensity,fill=Intensity),alpha=0.1,size=2) +
#   scale_x_log10() + ggtitle("Earthquake Philippines 15-12-2019") + xlab("GDP-PPP") + ylab("Density")


# Intensity     Sum [$]
#      3.5    17836855.02
#      4.0    12206917.67
#      4.5    7775080.93
#      5.0    5799973.74
#      5.5    2914321.12
#      6.0    1049755.59
#      6.5    520233.32

#      7.0    122006.5    71593.56

# Intensity     #People
#      3.5   82570376421
#      4.0   56801475832
#      4.5   36427972284
#      5.0   27448741808
#      5.5   13939262498
#      6.0   4844595755
#      6.5   2364353644

#      7.0    122006.5   349463808

  # Helix Names : 
  # unique(helix$haz_Ab)
  # [1] "Flood"               "Storm"               NA                   
  # [4] "Wildfire"            "Earthquake"         "Extreme temperature"
  # [7] "Volcanic eruption"   "Drought"            "Mass movement"  
  
  # d_choice<-c(
  #   "Drought"="DR",
  #   "Tropical Cyclones"="TC",
  #   "Tornadoes"="TO",
  #   "Severe Local Storms"="SL",
  #   "Heat Wave"="HT",
  #   "Extratropical Cyclone"="EC",
  #   "Violent Wind"="VW",
  #   "Flood"="FL",
  #   "Flash Flood"="FF",
  #   "Snow Avalanche"="AV",
  #   "Land Slide"="LS",
  #   "Mud Slide"="MS",
  #   "Volcano"="VO",
  #   "Earthquake"="EQ",
  #   "Fire"="FR",
  #   "Tsunami"="TS",
  #   "Storm Surge"="SS",
  #   "Wild Fire"="WF"
  # )

ExtractTC<-function(url){
  
  tmp<-try(FROM_GeoJson(url_file_string = url),silent = T)
  
  polydata<-data.frame()
  for (i in 1:length(tmp$features)){
    
    if(!(tmp$features[[i]]$geometry$type%in%c("Polygon") & tmp$features[[i]]$properties$Class%in%c("Poly_Green","Poly_Orange","Poly_Red"))) next
    
    lennie<-length(tmp$features[[i]]$geometry$coordinates[,2])
    
    polydata<-rbind(polydata,data.frame(Longitude=tmp$features[[i]]$geometry$coordinates[,1],
                                        Latitude=tmp$features[[i]]$geometry$coordinates[,2], 
                                        Severity=rep(tmp$features[[i]]$properties$polygonlabel,lennie),
                                        Type=rep(tmp$features[[i]]$geometry$type,lennie)))
    
    
  }
  
  return(polydata)
  
}

# Shake2Poly<-function(murl){
#   
#   tmp<-try(FROM_GeoJson(url_file_string = murl),silent = T)
#   if(class(tmp) == "try-error") {
#     print("Warning: no GDACS data found for ")
#     print(murl)
#     return(data.frame(eventid=NA,Intensity=NA,Longitude=NA,Latitude=NA,
#                       date=NA,alertscore=NA,ncontour=NA))
#   }
#   
#   if(length(tmp$properties$shakemap)>0) {
#     url<-c()
#     for (j in 1:length(tmp$properties$shakemap)) {
#       url<-c(url,tmp$properties$shakemap[[j]]$url)
#       tshake<-try(FROM_GeoJson(url_file_string = url),silent = T)
#       url<-tshake$properties$geometrydetails
#       if(!is.null(url)) {
#         shake<-try(FROM_GeoJson(url_file_string = url),silent = T)
#         if(class(shake) != "try-error") break
#       }
#     }
#   } else {
#     url<-tmp$properties$geometrydetails
#     shake<-try(FROM_GeoJson(url_file_string = tmp$properties$geometrydetails),silent = T)
#   }
#   
#   alertscore<-tmp$properties$alertscore
#   sdate<-as.Date(tmp$properties$fromdate)
#   eventid<-tmp$properties$eventid
#   
#   if(class(shake) == "try-error") {
#     print("Warning: no GDACS data found for ")
#     print(murl)
#     return(data.frame(eventid=eventid,Intensity=NA,Longitude=NA,Latitude=NA,
#                       date=sdate,alertscore=alertscore,ncontour=NA))
#   }
#   
#   rm(tmp)
#   
#   len<-length(shake$features)
#   poly<-data.frame()
#   
#   # Loop over different hazard intensities
#   for (i in 1:len){
#     # Loop over multiple polygons of same hazard intensity
#     for (j in 1:length(shake$features[[i]]$geometry$coordinates)){
#       # THANKS GDACS... VDM
#       check<-tryCatch(shake$features[[i]]$geometry$coordinates[[j]][[1]][,1],error = function(e) NULL)
#       if(!is.null(check)) shake$features[[i]]$geometry$coordinates[[j]]<-shake$features[[i]]$geometry$coordinates[[j]][[1]]
#       
#       long<-shake$features[[i]]$geometry$coordinates[[j]][,1]
#       lat<-shake$features[[i]]$geometry$coordinates[[j]][,2]
#       intensity<-shake$features[[i]]$properties$intensity
#       if(length(intensity)>1) print(paste0("check intensity Shake2Poly ",intensity))
#       
#       poly<-rbind(poly,data.frame(eventid=rep(eventid,length(long)),Intensity=rep(intensity,length(long)),
#                                   Longitude=long,Latitude=lat,date=rep(sdate,length(long)),
#                                   alertscore=rep(alertscore,length(long)),ncontour=rep(j,length(long))))
#     }
#   } 
#   
#   return(poly)
#   
# }
# 
# GetShakeGDACS_red<-function(dfGDACS){
#   
#   # url<-"https://www.gdacs.org/gdacsapi/api/shakemap/getdetails?id=9187"
#   # url taken from dfGDACS$link
#   
#   qq<-1
#   poly<-data.frame()
#   st_url<-"https://www.gdacs.org/gdacsapi/api/shakemap/getgeometry?eventid="
#   fn_url<-"&shakeid=1"
#   # Read in worst intensity event shakemap
#   # url<-as.character(dfGDACS$link[1])
#   #if(is.na(url)) return(NULL)
#   if(is.na(dfGDACS$eventid[1])) return(NULL)
#   url<-paste0(st_url,dfGDACS$eventid[1],fn_url)
#   
#   tp<-NULL
#   k<-0
#   while(is.null(tp)|k<100){
#     tp<-tryCatch(Shake2Poly(url),error = function(e) NULL)
#     k<-k+1
#   }
#   # Check the output is not empty
#   if(is.null(tp)) return(NULL)
#   
#   poly<-rbind(poly,cbind(tp,id=rep(qq,length(tp$date))))
#   
#   for (i in 2:length(dfGDACS$alert)){
#     
#     if(is.na(dfGDACS$eventid[i])) next
#     url<-paste0(st_url,dfGDACS$eventid[i],fn_url)
#     # url<-as.character(dfGDACS$link[i])
#     # if(is.na(url)) next
#     
#     tp<-tryCatch(Shake2Poly(url),error = function(e) NULL)
#     if(is.null(tp)) next
#     
#     qq<-qq+1
#     poly<-rbind(poly,cbind(tp,id=rep(qq,length(tp$date))))  
#     
#   }
#   
#   return(poly)
#   
# }