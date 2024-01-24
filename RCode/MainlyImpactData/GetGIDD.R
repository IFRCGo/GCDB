IDMCfolder<-"./CleanedData/MostlyImpactData/IDMC/"

ExtractGIDD<-function(){
  # Create the folder for the data
  dir.create(IDMCfolder,showWarnings = F,recursive = T); 
  # Download the data directly from IDMC
  rety<-tryCatch(download.file("https://helix-tools-api.idmcdb.org/external-api/gidd/disasters/disaster-export/?iso3__in=&start_year=2000&end_year=2022&hazard_type__in=&client_id=IDMCWSHSOLO009&release_environment=RELEASE",
                paste0(IDMCfolder,"GIDD-IDMC.xlsx")),error=function(e) NA)
  !is.na(rety)
}


# PostModGIDD<-function(colConv){
#   # hazard Types
#   colConv$haz_type[colConv$hazG%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
#   colConv$haz_type[colConv$hazG%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
#   colConv$haz_type[colConv$hazG=="WF"]<-"haztypeenviron"
#   colConv$haz_type[colConv$hazG=="EP"]<-"haztypebio"
#   
#   # Hazard clusters
#   colConv$haz_cluster[colConv$hazG=="DR"]<-"hazhmprecip,hazhmtemp"
#   colConv$haz_cluster[colConv$hazG=="FL"]<-"hazhmflood"
#   colConv$haz_cluster[colConv$hazG=="ST"]<-"hazhmconv,hazhmwind,hazhmpress,hazhmflood"
#   colConv$haz_cluster[grepl("rain",colConv$HazardSubType,ignore.case = T)]<-"hazhmprecip"
#   colConv$haz_cluster[grepl("wind",colConv$HazardSubType,ignore.case = T)]<-"hazhmwind,hazhmpress"
#   colConv$haz_cluster[grepl("lightning",colConv$HazardSubType,ignore.case = T)]<-"hazhmconv"
#   colConv$haz_cluster[colConv$hazG=="ET"]<-"hazhmtemp"
#   colConv$haz_cluster[colConv$hazG=="TC"]<-"hazhmwind,hazhmpress,hazhmconv,hazhmflood"
#   colConv$haz_cluster[colConv$hazG=="TS"]<-"hazgeoother,hazhmmarine,hazhmflood"
#   colConv$haz_cluster[colConv$hazG=="EQ"]<-"hazgeoseis"
#   colConv$haz_cluster[colConv$hazG=="VO"]<-"hazgeovolc"
#   colConv$haz_cluster[colConv$hazG=="WF"]<-"hazenvenvdeg"
#   colConv$haz_cluster[grepl("hail",colConv$HazardSubType,ignore.case = T)]<-"hazhmprecip"
#   colConv$haz_cluster[colConv$hazG=="LS"]<-"hazgeoseis,hazenvenvdeg,hazgeovolc,hazgeoother"
#   colConv$haz_cluster[grepl("rock",colConv$HazardSubType,ignore.case = T)]<-"hazhmterr"
#   colConv$haz_cluster[grepl("mud",colConv$HazardSubType,ignore.case = T)]<-"hazhmterr"
#   colConv$haz_cluster[grepl("liquefaction",colConv$HazardSubType,ignore.case = T)]<-"hazgeoseis,hazgeoother"
#   colConv$haz_cluster[colConv$hazG=="AV"]<-"hazhmterr"
#   colConv$haz_cluster[grepl("tidal",colConv$HazardSubType,ignore.case = T)]<-"hazhmmarine,hazhmflood"
#   colConv$haz_cluster[grepl("coastal flood",colConv$HazardSubType,ignore.case = T)]<-"hazhmflood,hazhmmarine"
#   colConv$haz_cluster[grepl("wave",colConv$HazardSubType,ignore.case = T)]<-"hazhmmarine,hazhmflood"
#   colConv$haz_cluster[grepl("surge",colConv$HazardSubType,ignore.case = T)]<-"hazhmmarine,hazhmflood,hazhmwind"
#   colConv$haz_cluster[grepl("hail",colConv$HazardSubType,ignore.case = T)]<-"hazhmprecip"
#   colConv$haz_cluster[grepl("tropical storm",colConv$HazardSubType,ignore.case = T)]<-"hazhmwind"
#   colConv$haz_cluster[grepl("convective storm",colConv$HazardSubType,ignore.case = T)]<-"hazhmconv"
#   colConv$haz_cluster[grepl("cold wave",colConv$HazardSubType,ignore.case = T)]<-"hazhmtemp"
#   
#   # Specific Hazards
#   colConv$haz_spec[colConv$hazG=="EQ"]<-"GH0001,GH0002"
#   colConv$haz_potlink[colConv$hazG=="EQ"]<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
#   
#   # Save it out
#   openxlsx::write.xlsx(colConv,"./Taxonomies/MostlyImpactData/GIDD-HIP.xlsx")
#   
#   return(colConv)
# }

GIDDHazards<-function(GIDD){
  GIDD$HazardCategory%<>%str_to_lower()
  GIDD$HazardType%<>%str_to_lower()
  GIDD$HazardSubType%<>%str_to_lower()
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/GIDD-HIP.xlsx")
  colConv$HazardCategory%<>%str_to_lower()
  colConv$HazardType%<>%str_to_lower()
  colConv$HazardSubType%<>%str_to_lower()
  # Reduce the translated vector and merge
  GIDD%<>%left_join(colConv,by = c("HazardCategory","HazardType","HazardSubType"),
                    relationship="many-to-one")%>%
    dplyr::select(-c("HazardCategory","HazardType","HazardSubType"))
  # if(haz=="EQ"){
  #   GIDD$haz_potlink<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
  # } 
  
  return(GIDD)
}


GetGIDD<-function(){
  # File storage
  filez<-paste0(IDMCfolder,"GIDD-IDMC.xlsx")
  # If it doesn't exist, extract it
  ExtractGIDD()
  # Load data
  GIDD<-readxl::read_xlsx(filez)
  # Change the column names to something more user friendly
  colnames(GIDD)<-str_remove_all(str_split(str_split(colnames(GIDD),"\\(",simplify = T)[,1],"\\/",simplify = T)[,1]," ")
  colnames(GIDD)[7]<-paste0(colnames(GIDD)[7],"Raw")
  # Hazard taxonomy - HIPS
  GIDD%<>%GIDDHazards()
  # Modify date names
  GIDD$imp_sdate<-GIDD$imp_fdate<-GIDD$ev_sdate<-GIDD$ev_fdate<-GIDD$imp_unitdate<-as.character(GIDD$DateofEvent)
  # Rename the event name
  colnames(GIDD)[colnames(GIDD)=="EventName"]<-"ev_name"
  GIDD$ev_name_lang<-"lang_eng"
  # Rename ISO3 variable
  colnames(GIDD)[colnames(GIDD)=="ISO3"]<-imp_ISO3s
  # Add the continent, then remove the unnecesary layers
  GIDD%<>%mutate(region=convIso3Continent(imp_ISO3s))%>%
    filter(!is.na(region))
  # Generate GCDB event ID
  GIDD$event_ID<-GetMonty_ID(GIDD)
  # Add some of the extra details that are Desinventar-specific
  GIDD%<>%mutate(imp_est_type="esttype_prim",
  imp_src_URL="https://helix-tools-api.idmcdb.org/external-api/gidd/disasters/disaster-export/",
  imp_src_org="IDMC",
  imp_src_db="GIDD",
  imp_src_orgtype="orgtypengo",
  imp_spat_srcorg="IFRC",
  imp_spat_srcdb="GO",
  imp_spat_URL="https://go-user-library.ifrc.org/maps",
  imp_spat_res=0,
  imp_spat_resunits="adminlevel",
  imp_spat_fileread="spatfstanshp",
  imp_spat_crs="EPSG:4326",
  imp_spat_covcode="spat_polygon",
  imp_spat_ID=NA_character_)
  # Correct the labels of the impacts, melting by impact detail
  GIDD%<>%ImpLabs(nomDB = "GIDD")
  # Create an impact-specific ID
  GIDD%<>%GetGCDB_impID()
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  GIDD%>%AddEmptyColImp()
}

convGIDD_Monty<-function(){
  # Extract raw GIDD data
  GIDD<-GetGIDD()
  # Get rid of repeated entries
  GIDD%<>%distinct(imp_sub_ID,.keep_all = TRUE)%>%
    arrange(ev_sdate)
  # Extract the Monty JSON schema template
  gMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  #@@@@@ Impact-level data @@@@@#
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    rbind(GIDD%>%mutate(ext_ID_db="GIDD",ext_ID_org="IDMC")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, GIDD_ID,
                          ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=GIDD_ID)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org),
          GIDD%>%filter(!is.na(ext_IDs))%>%mutate(ext_ID_db="GIDD",ext_ID_org="IDMC")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, ext_IDs, ext_ID_dbs, ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org)
    )
  )
  # Sources for impact data
  source<-GIDD%>%dplyr::select(imp_src_db,imp_src_URL,imp_src_org)
  # impact estimates
  impact_detail<-GIDD%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-GIDD%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res,imp_spat_fileread
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=data.frame(
      imp_sub_ID=GIDD$imp_sub_ID,
      imp_spat_ID="GO-ADM0-World-shp",
      imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
      imp_spat_colname="iso3",
      imp_spat_rowname=GIDD$imp_ISO3s
    ),
    spatial_info=GIDD%>%dplyr::select(
      imp_ISO3s,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_fileread,
      imp_spat_crs
    ),
    source=GIDD%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  gMonty$impact_Data<-data.frame(imp_sub_ID=unique(GIDD$imp_sub_ID))
  gMonty$impact_Data$ID_linkage=ID_linkage
  gMonty$impact_Data$source=source
  gMonty$impact_Data$impact_detail=impact_detail
  gMonty$impact_Data$temporal=temporal
  gMonty$impact_Data$spatial=spatial
  gMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    # By default, only GIDD eventIDs are used
    rbind(GIDD%>%mutate(ext_ID_db="GIDD",ext_ID_org="IDMC")%>%
            dplyr::select(event_ID, ev_name, GIDD_ID,ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=GIDD_ID),
          GIDD%>%filter(!is.na(ext_IDs))%>%
            dplyr::select(event_ID, ev_name, ext_IDs,ext_ID_dbs,ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)
    )
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    GIDD%>%dplyr::select(event_ID,imp_ISO3s,location)%>%
      rename(ev_ISO3s=imp_ISO3s,gen_location=location)
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    GIDD%>%dplyr::select(event_ID,imp_sdate,imp_fdate,ev_sdate,ev_fdate)
  )
  # Hazards
  hazs<-GIDD%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  allhaz_class<-Add_EvHazTax_Monty(
    do.call(rbind,lapply(1:nrow(hazs),function(i){
      specs<-c(str_split(hazs$haz_spec[i],":",simplify = T))
      outsy<-hazs[rep(i,length(specs)),]
      outsy$haz_spec<-specs
      return(outsy)
    }))
  )
  # Gather it all and store it in the template!
  gMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  gMonty$event_Level$ID_linkage<-ID_linkage
  gMonty$event_Level$temporal<-temporal
  gMonty$event_Level$spatial<-spatial
  gMonty$event_Level$allhaz_class<-allhaz_class
  gMonty$event_Level$ev<-NULL
  
  
  #@@@@@ Hazard-level data @@@@@#
  gMonty$hazard_Data<-list()
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  gMonty$response_Data<-list()
  
  
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  gMonty$taxonomies$src_info<-data.frame(
    src_org_code="IDMC",
    src_org_lab="Internal Displacement Monitoring Centre (IDMC)",
    src_org_typecode="orgtypengo",
    src_org_typelab="Non Governmental Organisation",
    src_org_email="info@idmc.ch",
    src_db_code="GIDD",
    src_db_lab="Global Internal Displacement Database (GIDD)",
    src_db_attr="curator",
    src_db_lic="unknown",
    src_db_URL="www.internal-displacement.org",
    src_addinfo=""
  )
  # And the impact modelling spatial data
  gMonty$taxonomies$src_info%<>%rbind(data.frame(
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
  dir.create("./CleanedData/MostlyHazardData/GIDD",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(gMonty,pretty = T,auto_unbox=T),
        paste0("./CleanedData/MostlyHazardData/GIDD/GIDD_",Sys.Date(),".json"))
  
  return(gMonty)
}





# 
# out%<>%arrange(desc(imp_value))
# 
# checker<-sapply(1:nrow(out),function(i){
#   print(out$event_ID[i])
#   hazzy<-GetUSGS_id(out$USGSid[i],titlz=paste0("./RawData/MostlyHazardData/EQ/"),I0=4.5,minmag=5,earlysort=T)
#   if(is.null(hazzy)) return(F)
#   print("success")
#   saveRDS(hazzy,paste0("./CleanedData/MostlyHazardData/EQ/",out$event_ID[i],"_",out$USGSid[i],".RData"))
#   return(T)
# })
# # 










