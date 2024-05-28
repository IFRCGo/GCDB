
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
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0006  :  MH0007")
    
  # Tropical cyclone
  i<-(grepl("cyclone",DFO$MAINCAUSE) |
        grepl("cylone",DFO$MAINCAUSE) |
        grepl("cycl",DFO$MAINCAUSE) |
        grepl("typhoon",DFO$MAINCAUSE) |
        grepl("hurricane",DFO$MAINCAUSE)) &
    !(grepl("extra-tropical",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  TC  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0057  :  MH0006")
  
  # Extra-tropical cyclone
  i<-grepl("extra-tropical",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  TC  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0031  :  MH0006")
  
  # Tropical storm
  i<-grepl("tropical storm",DFO$MAINCAUSE) | 
    grepl("tropial storm",DFO$MAINCAUSE) |
    grepl("tropical stom",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  ST  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0058  :  MH0006")
  
  # Tropical depression
  i<-grepl("tropical",DFO$MAINCAUSE) & grepl("depression",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  ST  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0030  :   MH0006")
  
  # It's a flash flood if it's dam-release related
  i<-(grepl("dam ",DFO$MAINCAUSE) | grepl("dam\\/",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  TL0009")
  
  # Glacial lake-related - flash flood
  i<-grepl("glacial",DFO$MAINCAUSE) | grepl("glacier",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0013  :  MH0006")
  
  # Coastal flooding
  i<-grepl("surge",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  SS")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0027")
  
  # Storm tides
  i<-grepl("tide",DFO$MAINCAUSE) | 
    grepl("tidal",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  SS")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0028")
  
  # Ice-melts & jams
  i<-grepl("ice",DFO$MAINCAUSE) & 
    (grepl("melt",DFO$MAINCAUSE) | grepl("jam",DFO$MAINCAUSE))
  # haz_Ab[i]<-paste0(haz_Ab[i],"  :  ")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0009")
  
  # Avalanche
  i<-grepl("avalanche",DFO$MAINCAUSE) | grepl("avalance",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  AV")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0050")
  
  # Winter storms == blizzard, snowstorm
  i<-grepl("winter storm",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  ST")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0034")
  
  # Snowmelts
  i<-grepl("snow",DFO$MAINCAUSE) & grepl("melt",DFO$MAINCAUSE)
  # haz_Ab[i]<-paste0(haz_Ab[i],"  :  ")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0011")
  
  # Snow-related but not snowmelt
  i<-grepl("snow",DFO$MAINCAUSE) & !(grepl("melt",DFO$MAINCAUSE))
  # haz_Ab[i]<-paste0(haz_Ab[i],"  :  ")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0038")
  
  # Tsunami
  i<-grepl("tsunami",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  TS")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0029")
  
  # Mudslides
  i<-grepl("mudslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  LS_HM")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0051")
  
  # Landslides
  i<-grepl("landslide",DFO$MAINCAUSE)
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  LS_HM")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0052")
  
  # General flood- & wind-related (include thunder or any of the others? NAH)
  i<-grepl("storm",DFO$MAINCAUSE) & 
    !(grepl("surge",DFO$MAINCAUSE) |
        grepl("tropical",DFO$MAINCAUSE) |
        grepl("tropial",DFO$MAINCAUSE) |
        grepl("winter",DFO$MAINCAUSE))
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  ST")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0060  :  MH0054")
  
  # If we didn't manage to label the event, assume it was either riverine or pluvial
  i<- !(grepl("MH0006",haz_spec) | grepl("MH0007",haz_spec) |
    grepl("MH0009",haz_spec) | grepl("MH0011",haz_spec) |
        grepl("MH0013",haz_spec))
  haz_Ab[i]<-paste0(haz_Ab[i],"  :  FF")
  haz_spec[i]<-paste0(haz_spec[i],"  :  MH0006  :  MH0007")
  
  # Get rid of all "  :  " at the start and split by element
  DFO$haz_spec<-gsub("^  :  ","",haz_spec)#%>%str_split("  :  ")
  DFO$haz_Ab<-gsub("^  :  ","",haz_Ab)#%>%str_split("  :  ")
  
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
  isos[COUNTRY=="Britain, Ireland"]<-"GBR  :  IRL"      
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
  isos[COUNTRY=="Sudan and Eritrea"]<-"SDN  :  ERI"
  isos[COUNTRY=="Canada and USA"]<-"CAN  :  USA" 
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
                imp_unitdate=imp_sdate,
                imp_src_db="DFO",
                imp_src_URL="https://floodobservatory.colorado.edu",
                imp_src_org="UniColumbia",
                imp_src_orgtype="orgtypeacad",
                imp_spat_covcode="spat_polygon",
                imp_spat_res=0,
                imp_spat_resunits="adminlevel",
                imp_spat_crs="EPSG:4326",
                imp_spat_srcorg="IFRC",
                imp_spat_srcdb="GO-Maps",
                imp_spat_URL="https://go-user-library.ifrc.org/maps",
                imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
                imp_spat_ID=NA_character_,
                haz_src_db="DFO",
                haz_src_org="UniColumbia",
                haz_src_URL="https://floodobservatory.colorado.edu",
                haz_lon=imp_lon,
                haz_lat=imp_lat,
                haz_maxvalue=AREA,
                haz_maxunits="unitsna",
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
  tmp1<-convCountryIso3(DFO$COUNTRY)%>%DFOcountryExc(DFO$COUNTRY)
  tmp2<-convCountryIso3(DFO$OTHERCOUNT)%>%DFOcountryExc(DFO$COUNTRY)
  # Which countries didn't work?
  print(paste0("Getting rid of the following countries: ",
               paste0(unique(DFO$COUNTRY[is.na(tmp1) & is.na(tmp2)]),collapse = ", ")))
  # If no country is present, get rid of the event
  DFO%<>%filter(!(is.na(tmp1) & is.na(tmp2)))
  tmp1<-tmp1[!is.na(tmp1)]; tmp2<-tmp2[!is.na(tmp2)]
  # Combine them
  DFO$imp_ISO3s<-DFO$ev_ISO3s<-DFO$haz_ISO3s<-unlist(lapply(1:nrow(DFO),function(i){
    isos<-c(tmp1[i],tmp2[i])
    isos<-isos[!is.na(isos)]
    if(length(isos)==0) return(NA_character_) else return(paste0(isos,collapse = "  :  "))
  }))
  # If no country is present, get rid of the event
  DFO%<>%filter(!is.na(ev_ISO3s))
  # Add the hazards
  DFO%<>%DFOhazards()
  # Event ID
  DFO%<>%mutate(event_ID=GetMonty_ID(DFO%>%st_drop_geometry()),
                ev_name=paste0(MAINCAUSE," in ",gen_location," beginning on ",ev_sdate))
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
  
  # Damn sf geometries
  hDFO%<>%dplyr::select(any_of(MontyJSONnames()))
  hDFO<-hDFO[!duplicated(hDFO),]
  # Add missing columns & reorder the dataframe to fit imp_GCDB & haz_GCDB object
  return(list(hazards=hDFO,
              impacts=iDFO%>%dplyr::select(any_of(MontyJSONnames()))%>%distinct()))
}

# Wrangle into Monty JSON schema
convDFO_Monty<-function(){
  # Extract raw DFO data
  DFO<-GetDFO()
  # Get rid of repeated entries
  DFO$impacts%<>%arrange(ev_sdate)
  DFO$hazards%<>%arrange(ev_sdate)
  # Extract the Monty JSON schema template
  dMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(DFO$impacts),function(i) {
      DFO$impacts$all_ext_IDs[[i]]%>%mutate(event_ID=DFO$impacts$event_ID[i],
                                       ev_name=DFO$impacts$ev_name[i])
    },mc.cores=ncores))%>%distinct(event_ID,.keep_all = T)
  )
  # Spatial
  spatial_info<-DFO$impacts%>%dplyr::select(event_ID, gen_location)
  spatial_info$ev_ISO3s<-parallel::mclapply(1:nrow(DFO$hazards),function(i) {
    # First get the length of the required DF
    unique(c(str_split(DFO$hazards$ev_ISO3s[i],"  :  ",simplify = T)))
  },mc.cores=ncores)
  # Add it into the events object
  spatial<-Add_EvSpat_Monty(spatial_info)
  # temporal
  temporal<-Add_EvTemp_Monty(
    DFO$impacts%>%dplyr::select(event_ID,ev_sdate,ev_fdate)
  )
  # Hazards
  allhaz_class<-Add_EvHazTax_Monty(
    DFO$impacts%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  )
  # Gather it all and store it in the template!
  dMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  dMonty$event_Level$ID_linkage<-ID_linkage
  dMonty$event_Level$temporal<-temporal
  dMonty$event_Level$spatial<-spatial
  dMonty$event_Level$allhaz_class<-allhaz_class
  dMonty$event_Level$ev<-NULL
  
  #@@@@@ Hazard-level data @@@@@#
  # The ID linkage stuff is the same as for the event_Level element
  ID_linkage<-Add_hazIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(DFO$hazards),function(i) {
      DFO$impacts$all_ext_IDs[[i]]%>%mutate(event_ID=DFO$hazards$event_ID[i],
                                            haz_sub_ID=DFO$hazards$haz_sub_ID[i])
    },mc.cores=ncores))
  )
  # Sources for impact data
  srcs<-DFO$hazards%>%dplyr::select(haz_src_db,haz_src_URL,haz_src_org)
  # hazard taxonomy
  hazard_detail<-Add_HazTax_Monty(
    DFO$hazards%>%dplyr::select(haz_sub_ID, haz_Ab, haz_spec, 
                          haz_maxvalue,haz_maxunits,haz_est_type)%>%
      st_drop_geometry()%>%
      rename(event_ID=haz_sub_ID)
  )
  # Concurrent hazard info:
  hazard_detail$concur_haz<-lapply(1:nrow(hazard_detail),function(i) list())
  # Add temporal information
  temporal<-DFO$hazards%>%dplyr::select(haz_sdate,haz_fdate)%>%st_drop_geometry()
  # Spatial instance
  spatial_info<-DFO$hazards%>%
    dplyr::select(all_of(c("haz_lon","haz_lat","haz_spat_covcode",
                           "haz_spat_res","haz_spat_resunits","haz_spat_crs")))
  spatial_info$haz_ISO3s<-parallel::mclapply(1:nrow(DFO$hazards),function(i) {
    # First get the length of the required DF
    unique(c(str_split(DFO$hazards$haz_ISO3s[i],"  :  ",simplify = T)))
  },mc.cores=ncores)
  # Form the object
  spatial<-Add_hazSpatAll_Monty(
    ID_linkage=DFO$hazards%>%st_drop_geometry()%>%
      dplyr::select(
      haz_sub_ID,
      haz_spat_ID,
      haz_spat_fileloc
    ),
    spatial_info=DFO$hazards%>%st_drop_geometry()%>%
      dplyr::select(
      haz_ISO3s,
      haz_lon,
      haz_lat,
      haz_spat_covcode,
      haz_spat_res,
      haz_spat_resunits,
      haz_spat_crs
    ),
    source=DFO$hazards%>%st_drop_geometry()%>%
      dplyr::select(
      haz_spat_srcdb,
      haz_spat_URL,
      haz_spat_srcorg
    )
  )
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  dMonty$hazard_Data<-data.frame(haz_sub_ID=DFO$hazards$haz_sub_ID)
  dMonty$hazard_Data$ID_linkage=ID_linkage
  dMonty$hazard_Data$source=srcs
  dMonty$hazard_Data$hazard_detail=hazard_detail
  dMonty$hazard_Data$temporal=temporal
  dMonty$hazard_Data$spatial=spatial #cbind(DFO$hazards%>%dplyr::select(geometry),spatial)
  dMonty$hazard_Data$haz_sub_ID<-NULL
  
  #@@@@@ Impact-level data @@@@@#
  # First need to ensure that any impacts with zero impacts estimated are removed to prevent bias
  DFO$impacts%<>%filter(!is.na(haz_spec) | !is.na(imp_value) | imp_value>0)
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(DFO$impacts),function(i) {
      DFO$impacts$all_ext_IDs[[i]]%>%mutate(event_ID=DFO$impacts$event_ID[i],
                                       imp_sub_ID=DFO$impacts$imp_sub_ID[i])%>%
        left_join(DFO$hazards[,c("haz_sub_ID","event_ID")],
                  by="event_ID",relationship="many-to-many")
    },mc.cores=ncores))
  )
  # Sources for impact data
  srcy<-do.call(rbind,parallel::mclapply(unique(DFO$impacts$imp_sub_ID),function(ID){
    return(DFO$impacts[DFO$impacts$imp_sub_ID==ID,]%>%
             dplyr::select(imp_src_db,imp_src_URL,imp_src_org)%>%
             slice(1))
  },mc.cores=ncores))
  # impact estimates
  impact_detail<-DFO$impacts%>%distinct(imp_sub_ID,.keep_all = T)%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-DFO$impacts%>%distinct(imp_sub_ID,.keep_all = T)%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # Create the spatial_info first
  spatial_info<-DFO$impacts%>%
    dplyr::select(all_of(c("imp_lon","imp_lat","imp_spat_covcode",
                           "imp_spat_res","imp_spat_resunits","imp_spat_crs")))
  spatial_info$imp_ISO3s<-parallel::mclapply(1:nrow(DFO$impacts),function(i) {
    # First get the length of the required DF
    unique(c(str_split(DFO$impacts$imp_ISO3s[i],"  :  ",simplify = T)))
  },mc.cores=ncores)
  # multiple-entry rows: imp_ISO3s
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=DFO$impacts%>%dplyr::select(imp_sub_ID,imp_spat_ID,imp_spat_fileloc),
    spatial_info=spatial_info,
    source=DFO$impacts%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  dMonty$impact_Data<-data.frame(imp_sub_ID=unique(DFO$impacts$imp_sub_ID))
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
  dir.create("./CleanedData/MostlyHazardData/UniColumbia",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(dMonty,pretty = T,auto_unbox=T,na = 'null'),
        paste0("./CleanedData/MostlyHazardData/UniColumbia/DFO_",Sys.Date(),".json"))
  
  return(dMonty)
}














