
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
  # Add the hazard cluster: floods!
  DFO$haz_cluster<-"hazhmflood"
  # Setup empty hazard classifications (other than 'FL' for flooding...)
  haz_Ab<-rep("FL",nrow(DFO))
  haz_spec<-rep("",nrow(DFO))
  
  # Heavy rain is flash flood and/or riverine
  i<-grepl("rain",DFO$MAINCAUSE) | grepl("monsoon",DFO$MAINCAUSE) |
    grepl("torrential",DFO$MAINCAUSE) | grepl("heavy",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0006:MH0007")
    
  # Tropical cyclone
  i<-(grepl("cyclone",DFO$MAINCAUSE) |
        grepl("cylone",DFO$MAINCAUSE) |
        grepl("cycl",DFO$MAINCAUSE) |
        grepl("typhoon",DFO$MAINCAUSE) |
        grepl("hurricane",DFO$MAINCAUSE)) &
    !(grepl("extra-tropical",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i],":TC:FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0057:MH0006")
  
  # Extra-tropical cyclone
  i<-grepl("extra-tropical",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":TC:FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0031:MH0006")
  
  # Tropical storm
  i<-grepl("tropical storm",DFO$MAINCAUSE) | 
    grepl("tropial storm",DFO$MAINCAUSE) |
    grepl("tropical stom",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":ST:FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0058:MH0006")
  
  # Tropical depression
  i<-grepl("tropical",DFO$MAINCAUSE) & grepl("depression",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":ST:FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0030: MH0006")
  
  # It's a flash flood if it's dam-release related
  i<-(grepl("dam ",DFO$MAINCAUSE) | grepl("dam\\/",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i],":FF")
  haz_spec[i]<-paste0(haz_spec[i],":TL0009")
  
  # Glacial lake-related - flash flood
  i<-grepl("glacial",DFO$MAINCAUSE) | grepl("glacier",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0013:MH0006")
  
  # Coastal flooding
  i<-grepl("surge",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":SS")
  haz_spec[i]<-paste0(haz_spec[i],":MH0027")
  
  # Storm tides
  i<-grepl("tide",DFO$MAINCAUSE) | 
    grepl("tidal",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":SS")
  haz_spec[i]<-paste0(haz_spec[i],":MH0028")
  
  # Ice-melts & jams
  i<-grepl("ice",DFO$MAINCAUSE) & 
    (grepl("melt",DFO$MAINCAUSE) | grepl("jam",DFO$MAINCAUSE))
  # haz_Ab[i]<-paste0(haz_Ab[i],":")
  haz_spec[i]<-paste0(haz_spec[i],":MH0009")
  
  # Avalanche
  i<-grepl("avalanche",DFO$MAINCAUSE) | grepl("avalance",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":AV")
  haz_spec[i]<-paste0(haz_spec[i],":MH0050")
  
  # Winter storms == blizzard, snowstorm
  i<-grepl("winter storm",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":ST")
  haz_spec[i]<-paste0(haz_spec[i],":MH0034")
  
  # Snowmelts
  i<-grepl("snow",DFO$MAINCAUSE) & grepl("melt",DFO$MAINCAUSE)
  # haz_Ab[i]<-paste0(haz_Ab[i],":")
  haz_spec[i]<-paste0(haz_spec[i],":MH0011")
  
  # Snow-related but not snowmelt
  i<-grepl("snow",DFO$MAINCAUSE) & !(grepl("melt",DFO$MAINCAUSE))
  # haz_Ab[i]<-paste0(haz_Ab[i],":")
  haz_spec[i]<-paste0(haz_spec[i],":MH0038")
  
  # Tsunami
  i<-grepl("tsunami",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":TS")
  haz_spec[i]<-paste0(haz_spec[i],":MH0029")
  
  # Mudslides
  i<-grepl("mudslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":LS_HM")
  haz_spec[i]<-paste0(haz_spec[i],":MH0051")
  
  # Landslides
  i<-grepl("landslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],":LS_HM")
  haz_spec[i]<-paste0(haz_spec[i],":MH0052")
  
  # General flood- & wind-related (include thunder or any of the others? NAH)
  i<-grepl("storm",DFO$MAINCAUSE) & 
    !(grepl("surge",DFO$MAINCAUSE) |
        grepl("tropical",DFO$MAINCAUSE) |
        grepl("tropial",DFO$MAINCAUSE) |
        grepl("winter",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i],":ST")
  haz_spec[i]<-paste0(haz_spec[i],":MH0060:MH0054")
  
  # If we didn't manage to label the event, assume it was either riverine or pluvial
  i<- !(grepl("MH0006",haz_spec) | grepl("MH0007",haz_spec) |
    grepl("MH0009",haz_spec) | grepl("MH0011",haz_spec) |
        grepl("MH0013",haz_spec))
  haz_Ab[i]<-paste0(haz_Ab[i],":FF")
  haz_spec[i]<-paste0(haz_spec[i],":MH0006:MH0007")
  
  # Get rid of all ":" at the start and split by element
  DFO$haz_spec<-gsub("^:","",haz_spec)#%>%str_split(":")
  DFO$haz_Ab<-gsub("^:","",haz_Ab)#%>%str_split(":")
  
  # DFO$allhaz_class<-lapply(1:nrow(DFO), function(i){
  #   list(haz_Ab=unique(haz_Ab[[i]]),
  #              haz_spec=unique(haz_spec[[i]]))
  # })
  
  return(DFO)
}

# Function to extract the exception countries from DFO
DFOcountryExc<-function(isos,COUNTRY){
  isos[COUNTRY=="Phillipines"]<-"PHL"
  isos[COUNTRY=="Columbia"]<-"COL"
  isos[COUNTRY=="Azores Islands"]<-"PRT"
  isos[COUNTRY=="Alaska"]<-"USA"
  isos[COUNTRY=="Yugoslavia"]<-"YUG"
  isos[COUNTRY=="Guatamala"]<-"GTM"
  isos[COUNTRY=="Czechoslovakia"]<-"CSK"
  isos[COUNTRY=="El Savador"]<-"SLV"
  isos[COUNTRY=="Scotland"]<-"GBR"        
  isos[COUNTRY=="Bangledesh"]<-"BGD"           
  isos[COUNTRY=="Myanamar"]<-"MMR"         
  isos[COUNTRY=="England"]<-"GBR"
  isos[COUNTRY=="Britain, Ireland"]<-"GBR:IRL"      
  isos[COUNTRY=="Philippine"]<-"PHL"            
  # isos[COUNTRY=="Caribbean"]
  isos[COUNTRY=="Philipines"]<-"PHL"
  isos[COUNTRY=="Moldava"]<-"MDA"
  isos[COUNTRY=="Siberia"]<-"RUS"              
  isos[COUNTRY=="Bangaldesh"]<-"BGD"            
  isos[COUNTRY=="Tibet"]<-"CHN"
  isos[COUNTRY=="Timor"]<-"TLS"                
  isos[COUNTRY=="Canary Islands"]<-"ESP"
  # isos[COUNTRY=="Africa"]
  # isos[COUNTRY=="Europe"]
  isos[COUNTRY=="Texas"]<-"USA"
  isos[COUNTRY=="Sudan and Eritrea"]<-"SDN:ERI"
  isos[COUNTRY=="Canada and USA"]<-"CAN:USA" 
  isos[COUNTRY=="Boliva"]<-"BOL"                
  isos[COUNTRY=="Malayasia"]<-"MYS"             
  isos[COUNTRY=="Venezulea"]<-"VEN"            
  isos[COUNTRY=="Serbia-Montenegro"]<-"CSK"     
  isos[COUNTRY=="Phillippines"]<-"PHL"          
  isos[COUNTRY=="Serbia and Montenegro"]<-"CSK"
  isos[COUNTRY=="Tasmania"]<-"AUS"              
  isos[COUNTRY=="Zimbawe"]<-"ZWE"               
  isos[COUNTRY=="Kazahkstan"]<-"KAZ"           
  isos[COUNTRY=="Camaroun"]<-"CMR" 
  isos[COUNTRY=="Northern Ireland"]<-"GBR"
  isos[COUNTRY=="Unitd Kingdom"]<-"GBR"        
  isos[COUNTRY=="Madascar"]<-"MDG"              
  isos[COUNTRY=="Cote D'Iavoir"]<-"CIV"  
  isos[COUNTRY=="Kazahkistan"]<-"KAZ"
  isos[COUNTRY=="Gutamala"]<-"GTM"
  isos[COUNTRY=="Virgin Islands"]<-"GBR"
  isos[COUNTRY=="Camaroon"]<-"CMR"
  
  return(isos)
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
  DFO%<>%mutate(COUNTRY=str_replace_all(COUNTRY,"\xa0",""),
                OTHERCOUNT=str_replace_all(OTHERCOUNT,"\xa0",""),
                gen_location=COUNTRY,
                ev_sdate=imp_sdate,haz_sdate=imp_sdate,
                ev_fdate=imp_fdate,haz_fdate=imp_fdate,
                exp_spec="expspec_allpeop",imp_units="unitscountnum",
                imp_est_type="esttype_prim", haz_est_type="esttype_prim",
                imp_src_db="DFO",
                imp_src_org="UniColumbia",
                imp_src_orgtype="orgtypeacad",
                imp_spat_covcode="spat_polygon",
                imp_spat_res=0,
                imp_spat_resunits="adminlevel",
                imp_spat_crs="EPSG:4326",
                imp_spat_srcorg="IFRC",
                imp_spat_srcdb="GO-Maps",
                imp_spat_URL="https://go-user-library.ifrc.org/maps",
                imp_spat_ID=NA_character_,
                haz_src_db="DFO",
                haz_src_org="UniColumbia",
                haz_src_URL="https://floodobservatory.colorado.edu",
                haz_lon=imp_lon,
                haz_lat=imp_lat,
                haz_units="unitsna",
                haz_spat_covcode="spat_polygon",
                haz_spat_res=NA_real_,
                haz_spat_resunits="spatresother",
                haz_spat_fileloc="https://floodobservatory.colorado.edu/temp/FloodArchive_region.shp",
                haz_spat_crs="EPSG:4326",
                haz_spat_srcorg="UniColumbia",
                haz_spat_srcdb="DFO",
                haz_spat_URL="https://floodobservatory.colorado.edu/temp/FloodArchive_region.shp")
  # Make the unstructured text value of general location
  DFO$gen_location[!(is.na(DFO$OTHERCOUNT) | DFO$OTHERCOUNT==0)]<-
    paste0(DFO$gen_location[!(is.na(DFO$OTHERCOUNT) | DFO$OTHERCOUNT==0)],", ",
           DFO$OTHERCOUNT[!(is.na(DFO$OTHERCOUNT) | DFO$OTHERCOUNT==0)])
  # Adding the ISO3C codes
  ev_ISO3s<-imp_ISO3s<-haz_ISO3s<-convCountryIso3(DFO$COUNTRY)%>%DFOcountryExc(DFO$COUNTRY)
  tmp<-convCountryIso3(DFO$OTHERCOUNT)%>%DFOcountryExc(DFO$COUNTRY)
  # Which countries didn't work?
  print(paste0("Getting rid of the following countries: ",
               paste0(unique(DFO$COUNTRY[is.na(ev_ISO3s) & is.na(tmp)]),collapse = ", ")))
  # If no country is present, get rid of the event
  DFO%<>%filter(!(is.na(ev_ISO3s) & is.na(tmp)))
  # Combine them
  DFO$imp_ISO3s<-DFO$ev_ISO3s<-DFO$haz_ISO3s<-unlist(lapply(1:nrow(DFO),function(i){
    isos<-c(imp_ISO3s[i],tmp[i])
    isos<-isos[!is.na(isos)]
    if(length(isos)==0) return(NA_character_) else return(paste0(isos,collapse = ":"))
  }))
  # If no country is present, get rid of the event
  DFO%<>%filter(!is.na(ev_ISO3s))
  # Add the hazards
  DFO%<>%DFOhazards()
  # Event ID
  DFO%<>%mutate(event_ID=GetMonty_ID(DFO%>%st_drop_geometry()),
                ev_name=paste0(MAINCAUSE," in ",gen_location," beginning  on ",ev_sdate))
  # Clear up issues with GLIDE numbers
  DFO$GLIDE[DFO$GLIDE=="0"]<-NA_character_
  # External IDs
  DFO$haz_ext_IDs<-DFO$all_ext_IDs<-lapply(1:nrow(DFO), function(i){
    # First extract EM-DAT event ID
    out<-data.frame(ext_ID=DFO$ext_ID[i],
                    ext_ID_db="DFO",
                    ext_ID_org="UniColumbia")
    # If no other external IDs are provided, return only the Em-DAT ID
    if(is.na(DFO$GLIDE[i])) return(out) else 
      return(rbind(out,data.frame(ext_ID=DFO$GLIDE[i],
                                  ext_ID_db="GLIDE",
                                  ext_ID_org="ADRC")))
  })
  
  # Separate into hazard and impact
  hDFO<-DFO%>%dplyr::select(colnames(DFO)[!grepl("imp_",colnames(DFO))])
  iDFO<-DFO%>%dplyr::select(c(colnames(DFO)[!grepl("haz_",colnames(DFO))],
                              "haz_Ab","haz_spec"))%>%st_drop_geometry()
  
  # Sort the impact estimates
  iDFO%<>%reshape2::melt(measure.vars=c("DEAD","DISPLACED"))%>%
    rename("imp_type"="variable","imp_value"="value")%>%
    mutate(imp_type=as.character(imp_type))
  iDFO$imp_type[iDFO$imp_type=="DEAD"]<-"imptypdeat"
  iDFO$imp_type[iDFO$imp_type=="DISPLACED"]<-"imptypedisp"
  # Create an impact-specific ID
  iDFO%<>%GetGCDB_impID()
  iDFO$imp_spat_ID<-GetGCDB_imp_spatID(iDFO)
  # Now for hazards
  hgeom<-hDFO%>%st_drop_geometry%>%GetGCDB_hazID(); hDFO$haz_sub_ID<-hgeom$haz_sub_ID; rm(hgeom)
  hDFO$haz_spat_ID<-GetGCDB_haz_spatID(hDFO%>%st_drop_geometry())
  
  # Add missing columns & reorder the dataframe to fit imp_GCDB & haz_GCDB object
  return(list(hazards=hDFO%>%dplyr::select(any_of(MontyJSONnames()))%>%distinct(),
              impacts=iDFO%>%dplyr::select(any_of(MontyJSONnames()))%>%distinct()))
}


stop("SPLIT haz_Ab + haz:spec + ev_ISO3s + ...")


convDFO_Monty<-function(){
  # Extract raw Dessie data
  DFO<-GetDFO()
  # Get rid of repeated entries
  Dessie%<>%distinct()%>%arrange(ev_sdate)
  # Extract the Monty JSON schema template
  dMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(Dessie),function(i) {
      Dessie$all_ext_IDs[[i]]%>%mutate(event_ID=Dessie$event_ID[i],
                                       ev_name=Dessie$ev_name[i])
    },mc.cores=ncores))
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    Dessie%>%dplyr::select(event_ID, ev_ISO3s, gen_location)
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    Dessie%>%dplyr::select(event_ID,ev_sdate,ev_fdate)
  )
  # Hazards
  hazs<-Dessie%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  allhaz_class<-Add_EvHazTax_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(hazs),function(i){
      specs<-c(str_split(hazs$haz_spec[i],":",simplify = T))
      outsy<-hazs[rep(i,length(specs)),]
      outsy$haz_spec<-specs
      return(outsy)
    },mc.cores=ncores))
  )
  # Gather it all and store it in the template!
  dMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  dMonty$event_Level$ID_linkage<-ID_linkage
  dMonty$event_Level$temporal<-temporal
  dMonty$event_Level$spatial<-spatial
  dMonty$event_Level$allhaz_class<-allhaz_class
  dMonty$event_Level$ev<-NULL
  
  #@@@@@ Hazard-level data @@@@@#
  # Nothing to put here as we haven't linked any hazard data yet
  dMonty$hazard_Data<-list()
  
  #@@@@@ Impact-level data @@@@@#
  # First need to ensure that any impacts with zero impacts estimated are removed to prevent bias
  Dessie%<>%filter(!is.na(haz_spec) | !is.na(imp_value) | imp_value>0)
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(Dessie),function(i) {
      Dessie$all_ext_IDs[[i]]%>%mutate(event_ID=Dessie$event_ID[i],
                                       imp_sub_ID=Dessie$imp_sub_ID[i],
                                       haz_sub_ID=NA_character_)
    },mc.cores=ncores))
  )
  # Sources for impact data
  srcy<-do.call(rbind,parallel::mclapply(unique(Dessie$imp_sub_ID),function(ID){
    return(Dessie[Dessie$imp_sub_ID==ID,]%>%
             dplyr::select(imp_src_db,imp_src_URL,imp_src_org)%>%
             slice(1))
  },mc.cores=ncores))
  # impact estimates
  impact_detail<-Dessie%>%distinct(imp_sub_ID,.keep_all = T)%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-Dessie%>%distinct(imp_sub_ID,.keep_all = T)%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_ISO3s,imp_spat_res
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=Dessie%>%dplyr::select(imp_sub_ID,imp_spat_ID,imp_spat_fileloc),
    spatial_info=Dessie%>%dplyr::select(
      imp_ISO3s,
      imp_lon,
      imp_lat,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_crs
    ),
    source=Dessie%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  dMonty$impact_Data<-data.frame(imp_sub_ID=unique(Dessie$imp_sub_ID))
  dMonty$impact_Data$ID_linkage=ID_linkage
  dMonty$impact_Data$source=srcy
  dMonty$impact_Data$impact_detail=impact_detail
  dMonty$impact_Data$temporal=temporal
  dMonty$impact_Data$spatial=spatial
  dMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  dMonty$response_Data<-list()
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  dMonty$taxonomies$src_info<-readxl::read_xlsx("./Taxonomies/Monty_DataSources.xlsx")%>%distinct()
  
  #@@@@@ Checks and validation @@@@@#
  dMonty%<>%checkMonty()
  
  # Create the path for the output
  dir.create("./CleanedData/MostlyHazardData/UNDRR",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(dMonty,pretty = T,auto_unbox=T,na = 'null'),
        paste0("./CleanedData/MostlyHazardData/UNDRR/Desinventar_",Sys.Date(),".json"))
  
  return(dMonty)
}














