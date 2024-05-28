
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

GetGDACS_API<-function(haz=NULL,syear=2016,fyear=NULL,alertlist=NULL){
  
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # stop("Check first whether the file exists on the computer")
  
  loc<-"https://www.gdacs.org/gdacsapi/api/events/geteventlist/SEARCH?eventlist="
  
  if(is.null(haz)) haz<-c("DR","TC","TS","FL","VO","EQ")
  if(is.null(alertlist)) alertlist<-c("red","orange","green")
  
  if(fyear>=AsYear(Sys.Date())) fdate<-paste0(Sys.Date(),"+00:00") else fdate<-paste0(fyear,"-12-31+00:00")
  
  list_GDACS<-list()
  for (alert in alertlist){
    GDACS<-data.frame()
    for (ev in haz){
      eGDACS<-data.frame()
      url<-paste0(loc,ev,"&fromdate=",syear,"-01-01+00:00&todate=",fdate,"&alertlevel=",alert)
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
  # if(haz=="EQ") {
  #   sev<-as.numeric(gsub("[^0-9.]", "",  strsplit(txt$severitytext,split = ",")[[1]]))
  #   return(list(haz_sev=sev[1],haz_sev_add=sev[2],haz_unit="M",haz_add_unit="km"))
  # } 
  if(haz=="EQ") {
    return(list(haz_maxvalue=txt$severity,
                haz_maxunits="unitsrichter"))
  } else if(haz=="TC") {
    return(list(haz_maxvalue=txt$severity,
                haz_maxunits="unitskph"))
  } else if(haz=="DR") {
    return(list(haz_maxvalue=txt$severity,
                haz_maxunits="unitskm2"))
  } else return(list(haz_maxvalue=NA_real_,
                     haz_maxunits=NA_character_))
}

GetIntMap<-function(hazard="EQ"){
  if(hazard=="EQ"){
    return(seq(from = 5,to = 9,by = 0.5))
  } else stop("Unknown hazard in GetIntMap")
}

FilterGDACS<-function(haz=NULL,syear=2016L,fyear=NULL,list_GDACS=NULL,red=F){
  
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
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
                                        GDACS_ID=rep(tmp$properties$eventid,len),
                                        ev_name=rep(tmp$properties$name,len),
                                        ev_name_lang=rep("lang_eng",len),
                                        location=rep(tmp$properties$name,len),
                                        episodeid=rep(tmp$properties$episodeid,len),
                                        link=rep(tmp$properties$url$details,len),
                                        imp_ISO3s=dfct$ISO3,
                                        ev_ISO3s=dfct$ISO3,
                                        country=dfct$country,
                                        ev_sdate=rep(as.Date(as.POSIXct(tmp$properties$fromdate),format = "%Y%m%d"),len),
                                        ev_fdate=rep(as.Date(as.POSIXct(tmp$properties$todate),format = "%Y%m%d"),len),
                                        haz_Ab=rep(tmp$properties$eventtype,len),
                                        hazard_severity=rep(tmp$properties$severitydata$severity,len),
                                        txt,
                                        haz_src_db=tmp$properties$source,
                                        haz_src_org=tmp$properties$source,
                                        ext_IDs=rep(tmp$properties$glide,len),
                                        ext_ID_dbs="GLIDE",
                                        ext_ID_orgs="ADRC",
                                        geom_type=rep(tmp$geometry$type,len),
                                        cent_lon=rep(tmp$geometry$coordinates[1],len),
                                        cent_lat=rep(tmp$geometry$coordinates[2],len),
                                        src_URL=rep(tmp$properties$url$geometry,len)))
    }
    
  }  
  
  # Get rid of any GLIDE numbers that are not in the correct format
  dfGDACS$ext_IDs[sum(!is.na(dfGDACS$ext_IDs) &
                      !grepl(dfGDACS$ext_IDs,pattern = "^[A-Z]{2}-\\d{4}-\\d{6}-[A-Z]{3}$"))]<-NA_character_
  
  dfGDACS$alertscore[dfGDACS$alertscore<0]<-0
  
  if(red) dfGDACS%>%dplyr::select(c(alertscore,hazard_severity,imp_ISO3s,sdate,fdate,long,lat))%>%return
  
  dfGDACS$event_ID<-GetMonty_ID(dfGDACS)
  
  return(dfGDACS%>%distinct())
  
}

GDACSHazards<-function(GDACS){
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/GDACS-HIP.xlsx")
  # Reduce the translated vector and merge
  GDACS%<>%left_join(colConv,by = c("haz_Ab"),
                     relationship="many-to-one")
}

restructGDACS<-function(GDACS){
  # Form the ID for the event
  GDACS$event_ID<-GetMonty_ID(GDACS)
  # Make the dates the correct type
  GDACS%<>%mutate_at(vars(ev_sdate,ev_fdate),as.character)
  # Date shifts
  GDACS$imp_sdate<-GDACS$imp_unitdate<-GDACS$haz_sdate<-GDACS$ev_sdate
  GDACS$imp_fdate<-GDACS$haz_fdate<-GDACS$ev_fdate
  # Add alertscore as the impact value
  GDACS%<>%mutate(
    imp_value=GDACS$alertscore,
    exp_spec="expspec_alert",
    imp_type="imptypalert",
    imp_units="unitsgdacsalert",
    # This estimate is modelled
    imp_est_type="esttype_model",
    haz_est_type="esttype_second",
    # Organisation
    imp_src_db="GDACS",
    imp_src_org="EC-JRC",
    imp_src_orgtype="orgtyperio",
    imp_spat_covcode="spat_polygon",
    imp_spat_res=0,
    imp_spat_resunits="adminlevel",
    imp_spat_crs="EPSG:4326",
    imp_spat_srcorg="IFRC",
    imp_spat_srcdb="GO-Maps",
    imp_spat_URL="https://go-user-library.ifrc.org/maps",
    imp_spat_ID=NA_character_,
    haz_ISO3s=imp_ISO3s,
    haz_spat_covcode="spat_polygon",
    haz_spat_res=NA_real_,
    haz_spat_resunits="spatresother",
    haz_spat_fileloc=src_URL,
    haz_spat_crs="EPSG:4326",
    haz_spat_srcorg="EC-JRC",
    haz_spat_fileloc=src_URL,
    haz_spat_srcdb="GDACS",
    haz_spat_URL=src_URL)
  
  colnames(GDACS)[colnames(GDACS)=="src_URL"]<-"haz_src_URL"
  colnames(GDACS)[colnames(GDACS)=="link"]<-"imp_src_URL"
  # Convert to the UNDRR-ISC hazard taxonomy
  GDACS%<>%GDACSHazards()
  # Create the impact and hazard sub-ID for the speciic level, not event level
  GDACS%<>%GetGCDB_impID()
  GDACS$imp_spat_ID<-GetGCDB_imp_spatID(GDACS)
  # Now for hazards
  GDACS%<>%GetGCDB_hazID()
  GDACS$haz_spat_ID<-GetGCDB_haz_spatID(GDACS)

  GDACS%>%distinct()
}

GetGDACS_GCDB<-function(){
  # Extract the data
  GDACS<-FilterGDACS()
  # Store it out as a imp_GCDB object
  GDACS%>%restructGDACS()
}

convGDACS_Monty<-function(){
  # Extract raw GDACS data
  GDACS<-GetGDACS_GCDB()
  # Get rid of repeated entries
  GDACS%<>%arrange(ev_sdate)
  # Extract the Monty JSON schema template
  gdacsMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  #@@@@@ Impact-level data @@@@@#
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    rbind(GDACS%>%mutate(ext_ID_db="GDACS",ext_ID_org="EC-JRC")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, GDACS_ID,
                          ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=GDACS_ID)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org),
          GDACS%>%filter(!is.na(ext_IDs))%>%mutate(ext_ID_db="GDACS",ext_ID_org="EC-JRC")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, ext_IDs, ext_ID_dbs, ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org)
    )
  )
  # Sources for impact data
  source<-GDACS%>%dplyr::select(imp_src_db,imp_src_URL,imp_src_org)
  # impact estimates
  impact_detail<-GDACS%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-GDACS%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=data.frame(
      imp_sub_ID=GDACS$imp_sub_ID,
      imp_spat_ID="GO-ADM0-World-shp",
      imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
      imp_spat_colname="iso3",
      imp_spat_rowname=GDACS$imp_ISO3s
    ),
    spatial_info=GDACS%>%dplyr::select(
      imp_ISO3s,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_crs
    ),
    source=GDACS%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  gdacsMonty$impact_Data<-data.frame(imp_sub_ID=unique(GDACS$imp_sub_ID))
  gdacsMonty$impact_Data$ID_linkage=ID_linkage
  gdacsMonty$impact_Data$source=source
  gdacsMonty$impact_Data$impact_detail=impact_detail
  gdacsMonty$impact_Data$temporal=temporal
  gdacsMonty$impact_Data$spatial=spatial
  gdacsMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    # By default, only GDACS eventIDs are used
    rbind(GDACS%>%mutate(ext_ID_db="GDACS",ext_ID_org="EC-JRC")%>%
            dplyr::select(event_ID, ev_name, GDACS_ID,ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=GDACS_ID),
          GDACS%>%filter(!is.na(ext_IDs))%>%
            dplyr::select(event_ID, ev_name, ext_IDs,ext_ID_dbs,ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)
      )
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    GDACS%>%dplyr::select(event_ID,imp_ISO3s,location)%>%
      rename(ev_ISO3s=imp_ISO3s,gen_location=location)
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    GDACS%>%dplyr::select(event_ID,imp_sdate,imp_fdate,ev_sdate,ev_fdate)
  )
  # Hazards
  allhaz_class<-Add_EvHazTax_Monty(
    GDACS%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  )
  # Gather it all and store it in the template!
  gdacsMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  gdacsMonty$event_Level$ID_linkage<-ID_linkage
  gdacsMonty$event_Level$temporal<-temporal
  gdacsMonty$event_Level$spatial<-spatial
  gdacsMonty$event_Level$allhaz_class<-allhaz_class
  gdacsMonty$event_Level$ev<-NULL
  
  
  #@@@@@ Hazard-level data @@@@@#
  # The ID linkage stuff is the same as for the event_Level element
  ID_linkage%<>%cbind(GDACS["haz_sub_ID"])%>%
    dplyr::select(event_ID,haz_sub_ID,all_ext_IDs)%>%rename(haz_ext_IDs=all_ext_IDs)
  # <-Add_hazIDlink_Monty(
  #   GDACS%>%
  #     dplyr::select(event_ID,haz_sub_ID,ext_IDs,ext_ID_dbs,ext_ID_orgs)%>%
  #     rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)
  # )
  
  # Sources for impact data
  source<-GDACS%>%dplyr::select(haz_src_db,haz_src_URL,haz_src_org)%>%mutate(haz_src_db="GDACS")
  # hazard taxonomy
  hazard_detail<-Add_HazTax_Monty(
    GDACS%>%dplyr::select(haz_sub_ID, haz_Ab, haz_spec, 
                  haz_maxvalue,haz_maxunits,haz_est_type)%>%
      rename(event_ID=haz_sub_ID)
  )
  # Concurrent hazard info:
  hazard_detail$concur_haz<-lapply(1:nrow(hazard_detail),function(i) list())
  # Add temporal information
  temporal<-GDACS%>%dplyr::select(haz_sdate,haz_fdate)
  # Spatial instance
  spatial<-Add_hazSpatAll_Monty(
    ID_linkage=GDACS%>%dplyr::select(
      haz_sub_ID,
      haz_spat_ID,
      haz_spat_fileloc
    ),
    spatial_info=GDACS%>%dplyr::select(
      haz_ISO3s,
      haz_lon,
      haz_lat,
      haz_spat_covcode,
      haz_spat_res,
      haz_spat_resunits,
      haz_spat_crs
    ),
    source=GDACS%>%dplyr::select(
      haz_spat_srcdb,
      haz_spat_URL,
      haz_spat_srcorg
    )
  )
  
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  gdacsMonty$hazard_Data<-data.frame(imp_sub_ID=GDACS$imp_sub_ID)
  gdacsMonty$hazard_Data$ID_linkage=ID_linkage
  gdacsMonty$hazard_Data$source=source
  gdacsMonty$hazard_Data$hazard_detail=hazard_detail
  gdacsMonty$hazard_Data$temporal=temporal
  gdacsMonty$hazard_Data$spatial=spatial
  gdacsMonty$hazard_Data$imp_sub_ID<-NULL
  
  
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  gdacsMonty$response_Data<-list()
  
  
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  gdacsMonty$taxonomies$src_info<-data.frame(
    src_org_code="EC-JRC",
    src_org_lab="European Commission - Joint Research Center",
    src_org_typecode="orgtyperio",
    src_org_typelab="Regional Intergovernmental Organisation",
    src_org_email="coordination@gdacs.org",
    src_db_code="GDACS",
    src_db_lab="Global Disaster Alert and Coordination System (GDACS)",
    src_db_attr="mediator",
    src_db_lic="unknown",
    src_db_URL="www.gdacs.org",
    src_addinfo=""
  )
  # And the impact modelling spatial data
  gdacsMonty$taxonomies$src_info%<>%rbind(data.frame(
    src_org_code="IFRC",
    src_org_lab="International Federation of Red Cross and Red Crescent Societies (IFRC)",
    src_org_typecode="orgtypengo",
    src_org_typelab="Non Governmental Organisation",
    src_org_email="im@ifrc.org",
    src_db_code="GO-Maps",
    src_db_lab="IFRC-GO ADM-0 Maps",
    src_db_attr="custodian",
    src_db_lic="Creative Commons Attribution 3.0 International License",
    src_db_URL="https://go-user-library.ifrc.org/maps",
    src_addinfo=""
  ))
  # Create the path for the output
  dir.create("./CleanedData/MostlyHazardData/GDACS",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(gdacsMonty,pretty = T,auto_unbox=T,na = 'null'),
        paste0("./CleanedData/MostlyHazardData/GDACS/GDACS_",Sys.Date(),".json"))
  
  return(gdacsMonty)
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
  if(!is.null(isos)) dfGDACS%<>%filter(imp_ISO3s %in% isos)
  
  dfGDACS%>%arrange(desc(alertscore))%>%pull(alertscore)%>%return
  
}

poly_ccodes<-c("eventid","episodeid","polygonlabel","Class","haz_severity","haz_sev_units","forecast","fromdate","todate","geometry")

GetGDACSspatial<-function(GDACS){
  
  BigPoly<-data.frame()
  for(ev in unique(GDACS$event_ID)){
    print(ev)
    # Find the appropriate elements of the GDACS database
    ind<-GDACS$event_ID==ev
    # Get the shapefile
    poly<-suppressWarnings(geojsonsf::geojson_sf(unique(GDACS$haz_spat_fileloc[ind])))
    # poly<-as(sf::st_read(URL),"Spatial")
    poly$haz_severity<-suppressWarnings(as.numeric(gsub("[^0-9]", "",  poly$polygonlabel)))
    poly$haz_sev_units<-sapply(1:nrow(poly),function(i) jsonlite::fromJSON(poly$severitydata[i])$severityunit,simplify = T)
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
    
    poly$event_ID<-ev
    
    # Here we need to modify the GDACS object to put in the fromdate and todate 
    # in the form of the haz_sdate and haz_fdate
    
    
    # Modify the spatial object to allow columns for hazard mag/int/coverage 
    # and then also the units of measurement too 
    
    
    # Join to the big motherbase
    BigPoly%<>%rbind(poly)
  }
  # Make sure to set the forecast variable as logical
  ind<-is.na(BigPoly$forecast)
  BigPoly$forecast[ind]<-F; BigPoly$forecast[!ind]<-T
  # Create a unique ID per shapefile
  # convert to sp spatial class
  BigPoly%<>%as("Spatial")
  
  
  
  # BigPoly@crs<-
    
    
  BigPoly@data%<>%left_join(GDACS,by="event_ID")
  return(BigPoly)
  
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
    
    if(!(tmp$features[[i]]$geometry$type%in%c("spat_polygon") & tmp$features[[i]]$properties$Class%in%c("Poly_Green","Poly_Orange","Poly_Red"))) next
    
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