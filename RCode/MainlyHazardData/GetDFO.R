
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
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
    
  # Tropical cyclone
  i<-(grepl("cyclone",DFO$MAINCAUSE) |
      grepl("cylone",DFO$MAINCAUSE) |
      grepl("cycl",DFO$MAINCAUSE) |
      grepl("hurricane",DFO$MAINCAUSE)) &
    !(grepl("extra-tropical",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Typhoon
  i<-grepl("typhoon",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Extra-tropical cyclone
  i<-grepl("extra-tropical",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Tropical storm
  i<-grepl("tropical storm",DFO$MAINCAUSE) | 
    grepl("tropial storm",DFO$MAINCAUSE) |
    grepl("tropical stom",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Tropical depression
  i<-grepl("tropical storm",DFO$MAINCAUSE) & grepl("depression",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Dam-related (flash)
  i<-grepl("dam ",DFO$MAINCAUSE) | grepl("dam\\/",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Levee-related (flash & riverine)
  i<-grepl("levee",DFO$MAINCAUSE) | grepl("levy",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # It's a flash flood if it's dam-release related
  i<-(grepl("dam ",DFO$MAINCAUSE) | grepl("dam\\/",DFO$MAINCAUSE)) & grepl("release",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Glacial lake-related
  i<-grepl("glacial",DFO$MAINCAUSE) | grepl("glacier",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Coastal flooding
  i<-grepl("surge",DFO$MAINCAUSE) | 
    grepl("tide",DFO$MAINCAUSE) | 
    grepl("tidal",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Ice-jams
  i<-grepl("ice",DFO$MAINCAUSE) & grepl("jam",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Ice-melts
  i<-grepl("ice",DFO$MAINCAUSE) & grepl("melt",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Avalanche
  i<-grepl("avalanche",DFO$MAINCAUSE) | grepl("avalance",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Winter storms == blizzard, snowstorm
  i<-grepl("winter storm",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Snowmelts
  i<-grepl("snow",DFO$MAINCAUSE) & grepl("melt",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Snow-related but not snowmelt
  i<-grepl("snow",DFO$MAINCAUSE) & !(grepl("melt",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Tsunami
  i<-grepl("tsunami",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Mudslides
  i<-grepl("mudslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # Landslides
  i<-grepl("landslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  # General flood- & wind-related (include thunder or any of the others? NAH)
  i<-grepl("storm",DFO$MAINCAUSE) & 
    !(grepl("surge",DFO$MAINCAUSE) |
        grepl("tropical",DFO$MAINCAUSE) |
        grepl("tropial",DFO$MAINCAUSE) |
        grepl("winter",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i]," : ")
  haz_spec[i]<-paste0(haz_spec[i]," : ")
  
  haz_Ab%<>%str_split(" : ")
  haz_spec%<>%str_split(" : ")
  
  DFO$allhaz_class<-lapply(1:nrow(DFO), function(i){
    data.frame(haz_Ab=haz_Ab[[i]],
               haz_spec=haz_spec[[i]])
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