GLIDEHazards<-function(GLIDE){
  # Read in the GLIDE-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/GLIDE-HIP.xlsx")
  # Reduce the translated vector and merge
  GLIDE%>%left_join(colConv,by = c("haz_Ab"),
                     relationship="many-to-one")
} 

GetGLIDEnum<-function(DF,numonly=T){
  # Names of the output of the GLIDEcols that we want to keep
  GLIDEcols<-c("comments","year","docid","latitude","homeless","source","idsource",
               "killed","affected","duration","number","injured","month","geocode",
               "location","magnitude","time","id","event","day","status","longitude")
  # Skeleton for empty output
  glide_skel<-data.frame(matrix(NA_character_,nrow = 1,ncol = length(GLIDEcols))); colnames(glide_skel)<-GLIDEcols
  # Needs to contain the columns: ev_sdate, ev_ISO3s & haz
  # Make sure dates are not in character format
  DF$ev_sdate%<>%as.Date()
  # Abbreviated hazard taxonomy
  haz_Ab<-DF$haz_Ab; 
  # Fetch the different GLIDE numbers, individually
  glides<-do.call(rbind,lapply(1:nrow(DF),function(i){
    baseurl<-"https://www.glidenumber.net/glide/jsonglideset.jsp?level1="
    URL<-paste0(baseurl,DF$ev_ISO3s[i],
           "&fromyear=",AsYear(DF$ev_sdate[i]-5),
           "&frommonth=",AsMonth(DF$ev_sdate[i]-5),
           "&fromday=",AsDay(DF$ev_sdate[i]-5),
           "&toyear=",AsYear(DF$ev_sdate[i]+5),
           "&tomonth=",AsMonth(DF$ev_sdate[i]+5),
           "&today=",AsDay(DF$ev_sdate[i]+5),
           "&events=",haz_Ab)
    out<-rjson::fromJSON(file = URL)[[1]]; 
    if(length(out)==0) return(glide_skel)
    # Just in case there are multiple GLIDE numbers, take the first one
    ind<-which.max(sapply(1:length(out),function(i) extractnumbers(out[[i]]$magnitude)))
    tryCatch(as.data.frame(out[[ind]]),error=function(e) glide_skel)
  }))
  # Convert to the proper GLIDE number, including the ISO3 country code
  glides$ext_IDs<-apply(glides[,c("geocode","number")],1,function(x) paste0(x,collapse = "-"))
  # For those that returned an error...
  inds<-glides$ext_IDs=="NA-NA"
  # Replace them with 
  glides$ext_IDs[inds]<-DF[inds,]%>%GetMonty_ID()
  # and let it be known when it is a glide number, too!
  glides$ext_IDs[!inds]<-paste0(glides$ext_IDs[!inds],"-GLIDE")
  # Let it be known that this is a GLIDE ID number
  glides$ext_ID_dbs<-"GLIDE"
  glides$ext_ID_orgs<-"Asian Disaster Reduction Center (ADRC)"
  # If we only care about the GLIDE number
  if(numonly) return(glides$ext_IDs)
  
  return(glides)
}

modGLIDEmagunits<-function(DF){
  # Remove any entries that don't contain any numbers
  DF$haz_maxvalue[DF$haz_maxvalue=="" | str_detect(DF$haz_maxvalue,"[0-9]",T)]<-NA_character_
  # Ready to store the units of each measurement
  DF$haz_units<-NA_character_
  
  # Let's first work on earthquakes & tsunamis (TS also measured in Richter scale)
  inds<-!is.na(DF$haz_maxvalue) & DF$haz_Ab%in%c("EQ","TS")
  # Sometimes they mention the dates of the EQ '5.9 on 12th March and 5.2 on 14th March'
  tmp<-str_split(DF$haz_maxvalue[inds],"and",simplify = T)
  for(j in 1:ncol(tmp)) tmp[,j]<-parse_number(str_split(tmp[,j],"on",simplify = T)[,1])
  # Sometimes they order the numbers in smaller first, we only care about the max
  DF$haz_maxvalue[inds]<-apply(tmp,1,max,na.rm=T)
  # And modify the units
  DF$haz_units[inds]<-"unitsrichter"
  # Zero magnitude earthquakes make no sense, remove to avoid errors
  DF$haz_maxvalue[inds & DF$haz_maxvalue=="0"]<-NA
  
  # Now tropical cyclones
  inds<-!is.na(DF$haz_maxvalue) & DF$haz_Ab=="TC"
  # Anything mentioning category
  tinds<-inds & ( grepl(paste(c("cat.","category"), 
                              collapse='|'), DF$haz_maxvalue,ignore.case = T))
  # Substitute them back in
  DF$haz_maxvalue[tinds]<-extractnumbers(DF$haz_maxvalue[tinds])
  # And add the units
  DF$haz_units[tinds]<-"unitssaffsimp"
  # Any referring to mph or kph separate now
  tinds<-inds & ( grepl(paste(c("km/h","kph","m/h","mph","mile/h"), 
                              collapse='|'), DF$haz_maxvalue,ignore.case = T))
  # Substitute them back in
  DF$haz_maxvalue[tinds]<-extractnumbers(DF$haz_maxvalue[tinds])
  # And add the units
  DF$haz_units[tinds & ( grepl(paste(c("m/h","mph","mile/h"), 
                                     collapse='|'), DF$haz_maxvalue,ignore.case = T))]<-"unitsmph"
  DF$haz_units[tinds & ( grepl(paste(c("km/h","kph"), 
                                     collapse='|'), DF$haz_maxvalue,ignore.case = T))]<-"unitskph"
  
  # Now tornados
  inds<-!is.na(DF$haz_maxvalue) & DF$haz_Ab=="TO"
  # Split by '-'
  tmp<-str_split(DF$haz_maxvalue[inds],"-",simplify = T)
  # Replace it in the vector
  DF$haz_maxvalue[inds]<-apply(tmp,1,function(x) max(extractnumbers(x),na.rm=T))
  # Now the units
  DF$haz_units[inds]<-"unitsenhfuj"
  
  # Now get rid of the rest
  inds<-is.na(DF$haz_maxvalue) | !DF$haz_Ab%in%c("EQ","TS","TO","TC")
  DF$haz_maxvalue[inds]<-NA_character_; DF$haz_units[inds]<-NA_character_
  
  return(DF)
}

GetGLIDEimps<-function(){
  # If you give GLIDE a request that they don't have, they give you the entire database!
  baseurl<-"https://www.glidenumber.net/glide/jsonglideset.jsp?glide=2008-000056"
  GLIDE<-rjson::fromJSON(file = baseurl)[[1]]; 
  GLIDE<-do.call(rbind,lapply(1:length(GLIDE),function(i) as.data.frame(GLIDE[[i]])))
  # Now let's treat this as a source of impact estimates!
  colnames(GLIDE)<-c("ev_name",
                   "Year",
                   "docid", # Nope!
                   "imp_lat", # Nope!
                   "imptyphomles",
                   "imp_src_org",
                   "idsource", # Nope!
                   "imptypdeat",
                   "imptypaffe",
                   "duration",
                   "GLIDE",
                   "imptypinju",
                   "month",
                   "imp_ISO3s",
                   "location",
                   "haz_maxvalue",
                   "time", # Nope!
                   "id", # Nope!
                   "haz_Ab",
                   "day",
                   "status", # Nope!
                   "imp_lon")
  # The source reference isn't well structured, use NLP to extract organisation name
  GLIDE$imp_src_db<-"GLIDE"
  # Set database to be the same as the organisation as we don't know better. Also, housekeeping
  GLIDE$imp_spat_ID<-NA
  # Sort out the ISO3 values to remove the NaNs
  GLIDE$imp_ISO3s[GLIDE$imp_ISO3s=="---"]<-NA_character_
  # Make sure the start date is 2 characters
  GLIDE$day[nchar(GLIDE$day)==1 & !is.na(GLIDE$day)]<-
    paste0("0",GLIDE$day[nchar(GLIDE$day)==1 & !is.na(GLIDE$day)])
  # Make sure the start month is 2 characters
  GLIDE$month[nchar(GLIDE$month)==1 & !is.na(GLIDE$month)]<-
    paste0("0",GLIDE$month[nchar(GLIDE$month)==1 & !is.na(GLIDE$month)])
  # Start date of event
  GLIDE$ev_sdate<-GLIDE$imp_sdate<-GLIDE$haz_sdate<-
    sapply(1:nrow(GLIDE),function(i) paste0(c(GLIDE$Year[i],GLIDE$month[i],GLIDE$day[i]),
                                          collapse = "-"),simplify = T)
  # End date of event
  GLIDE$ev_fdate<-GLIDE$imp_fdate<-GLIDE$haz_fdate<-as.character(as.Date(GLIDE$ev_sdate)+GLIDE$duration)
  # Modify the GLIDE number to be what it is known by
  GLIDE$GLIDE<-sapply(1:nrow(GLIDE),function(i) trimws(paste0(c(GLIDE$haz_Ab[i],GLIDE$GLIDE[i],ifelse(is.na(GLIDE$imp_ISO3s[i]),"",GLIDE$imp_ISO3s[i])),
                                                   collapse = "-"),
                                                   whitespace = "-",
                                                   which = 'right'),simplify = T)
  # Get the event_ID number
  GLIDE$event_ID<-GetMonty_ID(GLIDE)
  # Add the UNDRR-ISC hazard taxonomy
  GLIDE%<>%GLIDEHazards()%>%filter(!is.na(haz_cluster))
  # Take the impact estimate columns and convert into the imp_type feature
  GLIDE%<>%reshape2::melt(measure.vars=c("imptyphomles","imptypdeat","imptypaffe","imptypinju"),
                        variable.name="imp_type",value.name="imp_value")
  # Instead of as a factor
  GLIDE$imp_type%<>%as.character()
  # Get the continent name & add on the impact taxonomy layers
  GLIDE%<>%mutate(imp_cat="impcatpop",
                imp_subcat="imptypepopcnt",
                imp_det="impdetallpeop",
                imp_units="unitscount",
                imp_est_type="esttype_prim")
  # Try to extract as much as possible from the estimated magnitude and its units
  GLIDE%<>%modGLIDEmagunits()
  # Get rid of all zero values as we can't be sure that they are actual estimates
  stop("Shouldn't do this... we should create events even if impacts don't exist. Should filter impacts database after events has been created")
  GLIDE%<>%filter(imp_value>0)
  # And the sub IDs
  GLIDE%<>%GetGCDB_impID()
  # Make it into a GCDB_table-like object
  GLIDE%>%dplyr::select(any_of(MontyJSONnames()))
}


convGLIDE_Monty<-function(){
  # Extract raw GLIDE data
  GLIDE<-GetGLIDEimps()
  # Get rid of repeated entries
  GLIDE%<>%distinct(imp_sub_ID,.keep_all = TRUE)%>%
    arrange(ev_sdate)
  # Extract the Monty JSON schema template
  glideMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  #@@@@@ Impact-level data @@@@@#
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    rbind(GLIDE%>%mutate(ext_ID_db="GLIDE",ext_ID_org="ADRC")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, GLIDE,
                          ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=GLIDE)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org),
          GLIDE%>%filter(!is.na(ext_IDs))%>%mutate(ext_ID_db="GLIDE",ext_ID_org="ADRC")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, ext_IDs, ext_ID_dbs, ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org)
    )
  )
  # Sources for impact data
  source<-GLIDE%>%dplyr::select(imp_src_db,imp_src_URL,imp_src_org)
  # impact estimates
  impact_detail<-GLIDE%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-GLIDE%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=data.frame(
      imp_sub_ID=GLIDE$imp_sub_ID,
      imp_spat_ID="GO-ADM0-World-shp",
      imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
      imp_spat_colname="iso3",
      imp_spat_rowname=GLIDE$imp_ISO3s
    ),
    spatial_info=GLIDE%>%dplyr::select(
      imp_ISO3s,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_crs
    ),
    source=GLIDE%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  glideMonty$impact_Data<-data.frame(imp_sub_ID=unique(GLIDE$imp_sub_ID))
  glideMonty$impact_Data$ID_linkage=ID_linkage
  glideMonty$impact_Data$source=source
  glideMonty$impact_Data$impact_detail=impact_detail
  glideMonty$impact_Data$temporal=temporal
  glideMonty$impact_Data$spatial=spatial
  glideMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    # By default, only GLIDE eventIDs are used
    rbind(GLIDE%>%mutate(ext_ID_db="GLIDE",ext_ID_org="ADRC")%>%
            dplyr::select(event_ID, ev_name, GLIDE,ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=GLIDE),
          GLIDE%>%filter(!is.na(ext_IDs))%>%
            dplyr::select(event_ID, ev_name, ext_IDs,ext_ID_dbs,ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)
    )
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    GLIDE%>%dplyr::select(event_ID,imp_ISO3s,location)%>%
      rename(ev_ISO3s=imp_ISO3s,gen_location=location)
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    GLIDE%>%dplyr::select(event_ID,imp_sdate,imp_fdate,ev_sdate,ev_fdate)
  )
  # Hazards
  hazs<-GLIDE%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  allhaz_class<-Add_EvHazTax_Monty(
    do.call(rbind,lapply(1:nrow(hazs),function(i){
      specs<-c(str_split(hazs$haz_spec[i],":",simplify = T))
      outsy<-hazs[rep(i,length(specs)),]
      outsy$haz_spec<-specs
      return(outsy)
    }))
  )
  # Gather it all and store it in the template!
  glideMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  glideMonty$event_Level$ID_linkage<-ID_linkage
  glideMonty$event_Level$temporal<-temporal
  glideMonty$event_Level$spatial<-spatial
  glideMonty$event_Level$allhaz_class<-allhaz_class
  glideMonty$event_Level$ev<-NULL
  
  
  #@@@@@ Hazard-level data @@@@@#
  GLIDE%<>%distinct(haz_sub_ID,.keep_all = T)
  # The ID linkage stuff is the same as for the event_Level element
  ID_linkage%<>%cbind(GLIDE["haz_sub_ID"])%>%
    dplyr::select(event_ID,haz_sub_ID,all_ext_IDs)%>%rename(haz_ext_IDs=all_ext_IDs)
  # <-Add_hazIDlink_Monty(
  #   GLIDE%>%
  #     dplyr::select(event_ID,haz_sub_ID,ext_IDs,ext_ID_dbs,ext_ID_orgs)%>%
  #     rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)
  # )
  
  # Sources for impact data
  source<-GLIDE%>%dplyr::select(haz_src_db,haz_src_URL,haz_src_org)%>%mutate(haz_src_db="GLIDE")
  # hazard taxonomy
  hazard_detail<-Add_HazTax_Monty(
    GLIDE%>%dplyr::select(haz_sub_ID, haz_Ab, haz_spec, 
                          haz_maxvalue,haz_maxunits,haz_est_type)%>%
      rename(event_ID=haz_sub_ID)
  )
  # Concurrent hazard info:
  hazard_detail$concur_haz<-lapply(1:nrow(hazard_detail),function(i) list())
  # Add temporal information
  temporal<-GLIDE%>%dplyr::select(haz_sdate,haz_fdate)
  # Spatial instance
  spatial<-Add_hazSpatAll_Monty(
    ID_linkage=GLIDE%>%dplyr::select(
      haz_sub_ID,
      haz_spat_ID,
      haz_spat_fileloc,
      haz_spat_colname,
      haz_spat_rowname
    ),
    spatial_info=GLIDE%>%dplyr::select(
      haz_ISO3s,
      haz_spat_covcode,
      haz_spat_res,
      haz_spat_resunits,
      haz_spat_crs
    ),
    source=GLIDE%>%dplyr::select(
      haz_spat_srcdb,
      haz_spat_URL,
      haz_spat_srcorg
    )
  )
  
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  glideMonty$hazard_Data<-data.frame(imp_sub_ID=GLIDE$imp_sub_ID)
  glideMonty$hazard_Data$ID_linkage=ID_linkage
  glideMonty$hazard_Data$source=source
  glideMonty$hazard_Data$hazard_detail=hazard_detail
  glideMonty$hazard_Data$temporal=temporal
  glideMonty$hazard_Data$spatial=spatial
  glideMonty$hazard_Data$imp_sub_ID<-NULL
  
  
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  glideMonty$response_Data<-list()
  
  
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  glideMonty$taxonomies$src_info<-data.frame(
    src_org_code="ADRC",
    src_org_lab="Asian Disaster Reduction Center (ADRC)",
    src_org_typecode="orgtyperio",
    src_org_typelab="Regional Intergovernmental Organisation",
    src_org_email="gliderep@adrc.asia",
    src_db_code="GLIDE",
    src_db_lab="GLobal IDEntifier numbers (GLIDE)",
    src_db_attr="custodian",
    src_db_lic="unknown",
    src_db_URL="https://glidenumber.net",
    src_addinfo=""
  )
  # And the impact modelling spatial data
  glideMonty$taxonomies$src_info%<>%rbind(data.frame(
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
  dir.create("./CleanedData/MostlyHazardData/GLIDE",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(glideMonty,pretty = T,auto_unbox=T),
        paste0("./CleanedData/MostlyHazardData/GLIDE/GLIDE_",Sys.Date(),".json"))
  
  return(glideMonty)
}

