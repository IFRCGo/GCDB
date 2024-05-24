
ExtractDFOdata<-function(){
  # Ensure the repositories exist to save out the data
  dir.create("./RawData/MostlyHazardData/DFO",showWarnings = F); dir.create("./CleanedData/MostlyHazardData/DFO",showWarnings = F)
  # Extract the data from DFO
  out<-lapply(c("dbf","prj","shp","shx"),function(chch){
    download.file(paste0("https://floodobservatory.colorado.edu/temp/FloodArchive_region.",chch),
                  paste0("./RawData/MostlyHazardData/DFO/DFO.",chch),
                  quiet = T)
  })
  # Now return the geospatial elements
  sf::read_sf("./RawData/MostlyHazardData/DFO/DFO.shp")
}

DFOhazards<-function(DFO){
  # First convert all to lower case
  DFO$MAINCAUSE%<>%str_to_lower()%>%str_replace_all("  "," ")
  # Remove all NAs or empty hazards
  DFO%<>%filter(!is.na(MAINCAUSE) & MAINCAUSE!="0")
  # Setup empty hazard classifications (other than 'FL' for flooding...)
  haz_Ab<-rep("FL",nrow(DFO))
  haz_spec<-rep("",nrow(DFO))
  
  # Heavy rain is flash flood and/or riverine
  i<-grepl("rain",DFO$MAINCAUSE) | grepl("monsoon",DFO$MAINCAUSE) |
    grepl("torrential",DFO$MAINCAUSE) | grepl("heavy",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : FF")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0006")
    
  # Tropical cyclone
  i<-(grepl("cyclone",DFO$MAINCAUSE) |
        grepl("cylone",DFO$MAINCAUSE) |
        grepl("cycl",DFO$MAINCAUSE) |
        grepl("typhoon",DFO$MAINCAUSE) |
        grepl("hurricane",DFO$MAINCAUSE)) &
    !(grepl("extra-tropical",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i]," : TC")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0057")
  
  # Extra-tropical cyclone
  i<-grepl("extra-tropical",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : TC")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0031")
  
  # Tropical storm
  i<-grepl("tropical storm",DFO$MAINCAUSE) | 
    grepl("tropial storm",DFO$MAINCAUSE) |
    grepl("tropical stom",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ST")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0058")
  
  # Tropical depression
  i<-grepl("tropical",DFO$MAINCAUSE) & grepl("depression",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ST")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0030")
  
  # It's a flash flood if it's dam-release related
  i<-(grepl("dam ",DFO$MAINCAUSE) | grepl("dam\\/",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i]," : FF")
  haz_spec[i]<-paste0(haz_spec[i]," : TL0009")
  
  # Glacial lake-related
  i<-grepl("glacial",DFO$MAINCAUSE) | grepl("glacier",DFO$MAINCAUSE)
  # haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0013")
  
  # Coastal flooding
  i<-grepl("surge",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : SS")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0027")
  
  # Storm tides
  i<-grepl("tide",DFO$MAINCAUSE) | 
    grepl("tidal",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : SS")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0028")
  
  # Ice-melts & jams
  i<-grepl("ice",DFO$MAINCAUSE) & 
    (grepl("melt",DFO$MAINCAUSE) | grepl("jam",DFO$MAINCAUSE))
  # haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0009")
  
  # Avalanche
  i<-grepl("avalanche",DFO$MAINCAUSE) | grepl("avalance",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : AV")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0050")
  
  # Winter storms == blizzard, snowstorm
  i<-grepl("winter storm",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ST")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0034")
  
  # Snowmelts
  i<-grepl("snow",DFO$MAINCAUSE) & grepl("melt",DFO$MAINCAUSE)
  # haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0011")
  
  # Snow-related but not snowmelt
  i<-grepl("snow",DFO$MAINCAUSE) & !(grepl("melt",DFO$MAINCAUSE))
  # haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0038")
  
  # Tsunami
  i<-grepl("tsunami",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : TS")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0029")
  
  # Mudslides
  i<-grepl("mudslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : LS_HM")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0051")
  
  # Landslides
  i<-grepl("landslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : LS_HM")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0052")
  
  # General flood- & wind-related (include thunder or any of the others? NAH)
  i<-grepl("storm",DFO$MAINCAUSE) & 
    !(grepl("surge",DFO$MAINCAUSE) |
        grepl("tropical",DFO$MAINCAUSE) |
        grepl("tropial",DFO$MAINCAUSE) |
        grepl("winter",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i]," : ST")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0060 : MH0054")
  
  # If we didn't manage to label the event, assume it was either riverine or pluvial
  i<- !(grepl("MH0006",haz_spec) | grepl("MH0007",haz_spec) |
    grepl("MH0009",haz_spec) | grepl("MH0011",haz_spec) |
        grepl("MH0013",haz_spec))
  haz_Ab[i]<-paste0(haz_Ab[i]," : FF")
  haz_spec[i]<-paste0(haz_spec[i]," : MH0006 : MH0007")
  
  # Get rid of all " : " at the start
  haz_spec<-gsub("^ : ","",haz_spec)
  haz_Ab<-gsub("^ : ","",haz_Ab)
  
  
  
  
  
  
  
  stop("TC shouldn't have riverine")
  
  data.frame(haz_Ab=haz_Ab,haz_spec=haz_spec,MAINCAUSE=DFO$MAINCAUSE)%>%
    distinct()%>%View()
  
  
  
  
  
  
  haz_Ab%<>%str_split(" : ")
  haz_spec%<>%str_split(" : ")
  
  DFO$allhaz_class<-lapply(1:nrow(DFO), function(i){
    list(haz_Ab=unique(haz_Ab[[i]]),
               haz_spec=unique(haz_spec[[i]]))
  })
  
  return(DFO)
}

GetDFO<-function(){
  # Extract the data
  DFO<-ExtractDFOdata()
  # Rename things
  DFO%<>%rename("ext_ID"="ID",
                "GLIDE"="GLIDENUMBE",
                "imp_lon"="LONG",
                "imp_lat"="LAT",
                "imp_sdate"="BEGAN",
                "imp_fdate"="ENDED")
  # Add some repeated columns
  DFO%<>%mutate(ev_sdate=imp_sdate,haz_sdate=imp_sdate,
                ev_fdate=imp_fdate,haz_fdate=imp_fdate,
                exp_spec="expspec_allpeop",imp_units="unitscountnum",
                imp_est_type="esttype_prim")
  # Adding the ISO3C codes
  imp_ISOs<-convCountryIso3(DFO$COUNTRY)
  tmp<-convCountryIso3(DFO$OTHERCOUNT)
  # Combine them
  DFO$imp_ISOs<-DFO$ev_ISOs<-lapply(1:nrow(DFO),function(i){
    isos<-c(imp_ISOs[i],tmp[i])
    isos<-isos[!is.na(isos)]
    if(length(isos)==0) return(NA_character_) else return(isos)
  })
  # Add the hazards
  DFO%<>%DFOhazards()
  
  DFO%<>%reshape2::melt(measure.vars=c("DEAD","DISPLACED"))%>%
    rename("imp_type"="variable","imp_value"="value")
  DFO$imp_type[DFO$imp_type=="DEAD"]<-"imptypdeat"
  DFO$imp_type[DFO$imp_type=="DISPLACED"]<-"imptypedisp"
  
  stop("Add ev_name as MAINCAUSE and COUNTRY and OTHERCOUNT")
  stop("Define haz_... stuff as well as imp_... and ev_...")
  
  return(DFO)
}