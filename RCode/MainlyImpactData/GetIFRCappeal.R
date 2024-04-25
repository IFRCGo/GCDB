
token <- paste("Token", go_token)

getGOurl<-function(db="GO-App",token=NULL, results=T){
  
  if(db=="GO-App") {db_code<-"appeal"
  } else if(db=="GO-FR") {db_code<-"field_report"
  } else if(db=="GO-DREF") {db_code<-"dref-final-report"
  } else stop("this IFRC-GO database does not exist")
  
  # Base URL
  url<-paste0("https://goadmin.ifrc.org/api/v2/",db_code,"/?format=json&limit=100000000000") 
  # It's easier if we only want to access the public-facing data...
  if(!is.null(token)){
    # Make a request using a token
    req <- httr::GET(url, httr::add_headers(Authorization = token),httr::timeout(10000))
    # Extract the content
    json <- httr::content(req, as = "text", encoding = "UTF-8")
    # Convert from JSON to R list or tibble output
    json%<>%jsonlite::fromJSON()
  } else json<-jsonlite::fromJSON(url)
  
  if(results) return(json$results) else return(json)
}

ExtractGOdata<-function(db="GO-App", token = NULL, results=T){
  options(timeout = max(10000, getOption("timeout")))
  getGOurl(db=db,token,results)
}

PostModGO<-function(colConv){
  # hazard Types
  colConv$haz_type[colConv$haz_Ab%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
  colConv$haz_type[colConv$haz_Ab%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
  colConv$haz_type[colConv$haz_Ab=="WF"]<-"haztypeenviron"
  colConv$haz_type[colConv$haz_Ab=="EP"]<-"haztypebio"
  
  # Hazard clusters
  colConv$haz_cluster[colConv$haz_Ab=="DR"]<-"hazhmprecip,hazhmtemp"
  colConv$haz_cluster[colConv$haz_Ab=="FL"]<-"hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="ST"]<-"hazhmconv,hazhmwind,hazhmpress,hazhmflood"
  colConv$haz_cluster[grepl("rain",colConv$dtype,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("wind",colConv$dtype,ignore.case = T)]<-"hazhmwind,hazhmpress"
  colConv$haz_cluster[grepl("lightning",colConv$dtype,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[colConv$haz_Ab=="ET"]<-"hazhmtemp"
  colConv$haz_cluster[colConv$haz_Ab=="TC"]<-"hazhmwind,hazhmpress,hazhmconv,hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="TS"]<-"hazgeoother,hazhmmarine,hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="EQ"]<-"hazgeoseis"
  colConv$haz_cluster[colConv$haz_Ab=="VO"]<-"hazgeovolc"
  colConv$haz_cluster[colConv$haz_Ab=="WF"]<-"hazenvenvdeg"
  colConv$haz_cluster[grepl("hail",colConv$dtype,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[colConv$haz_Ab=="LS"]<-"hazgeoseis,hazenvenvdeg,hazgeovolc,hazgeoother"
  colConv$haz_cluster[grepl("rock",colConv$dtype,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("mud",colConv$dtype,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("liquefaction",colConv$dtype,ignore.case = T)]<-"hazgeoseis,hazgeoother"
  colConv$haz_cluster[colConv$haz_Ab=="AV"]<-"hazhmterr"
  colConv$haz_cluster[grepl("tidal",colConv$dtype,ignore.case = T)]<-"hazhmmarine,hazhmflood"
  colConv$haz_cluster[grepl("wave",colConv$dtype,ignore.case = T)]<-"hazhmmarine,hazhmflood"
  colConv$haz_cluster[grepl("coastal flood",colConv$dtype,ignore.case = T)]<-"hazhmflood,hazhmmarine"
  colConv$haz_cluster[grepl("surge",colConv$dtype,ignore.case = T)]<-"hazhmmarine,hazhmflood,hazhmwind"
  colConv$haz_cluster[grepl("hail",colConv$dtype,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("tropical storm",colConv$dtype,ignore.case = T)]<-"hazhmwind"
  colConv$haz_cluster[grepl("convective storm",colConv$dtype,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("cold wave",colConv$dtype,ignore.case = T)]<-"hazhmtemp"
  
  # Specific Hazards
  colConv$haz_spec[colConv$haz_Ab=="EQ"]<-"GH0001,GH0002"
  colConv$haz_potlink[colConv$haz_Ab=="EQ"]<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
  
  # Save it out
  openxlsx::write.xlsx(colConv,"./Taxonomies/MostlyImpactData/IFRC_HIP.xlsx")
  
  return(colConv)
}

GOHazards<-function(impies){
  
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/IFRC_HIP.xlsx")
  # Make sure spelling is a little less error prone
  colConv$dtype%<>%str_to_lower();impies$dtype%<>%str_to_lower()
  # Reduce the translated vector and merge
  impies%<>%left_join(colConv,by = "dtype")
  
  return(impies)
  
}

CleanGO_app<-function(appeal){
  
  appeal$dtype<-appeal$dtype$name
  
  appeal%<>%GOHazards(); appeal$dtype<-NULL
  
  appeal$imp_ISO3s<-appeal$country$iso3
  appeal$region<-convIso3Continent_alt(appeal$imp_ISO3s)

  appeal$country<-NULL
  
  appeal$start_date<-str_split(appeal$start_date,"T",simplify = T)[,1]
  appeal$end_date<-str_split(appeal$end_date,"T",simplify = T)[,1]
  
  appeal%<>%mutate(imp_ISO3s=imp_ISO3s,ev_ISO3s=imp_ISO3s,region=region,
                   ev_name=str_replace_all(name,'"',""),location=str_replace_all(name,'"',""),
                   ev_name_lang="lang_eng",
                   imp_sdate=as.character(as.Date(start_date)),imp_fdate=as.character(as.Date(end_date)),
                   ev_sdate=as.character(as.Date(start_date)),ev_fdate=as.character(as.Date(end_date)),
                   # ev_sdate=as.character(as.Date(created_at)),ev_fdate=as.character(as.Date(created_at)),
                   imp_unitdate=as.character(as.Date(modified_at)),
                   imp_src_URL="https://goadmin.ifrc.org/api/v2/appeal",
                   imp_src_orglab="International Federation of Red Cross and Red Crescent Societies (IFRC)",
                   imp_src_org="IFRC",
                   imp_src_db=atype_display,
                   imp_src_orgtype="orgtypengo",
                   imp_spat_covcode="spat_polygon",
                   imp_spat_res=0,
                   imp_spat_resunits="adminlevel",
                   imp_spat_crs="EPSG:4326",
                   imp_spat_srcorg="IFRC",
                   imp_spat_srcdb="GO-Maps",
                   imp_spat_URL="https://go-user-library.ifrc.org/maps",
                   imp_spat_ID=NA_character_)
  
  appeal$event_ID<-GetMonty_ID(appeal,haz=appeal$haz_Ab)
  
  appeal%<>%ImpLabs(nomDB = "GO-App", dropName = T)
  
  # Create an impact-specific ID
  appeal%<>%GetGCDB_impID()
  appeal$imp_spat_ID<-GetGCDB_imp_spatID(appeal)
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  # appeal%<>%AddEmptyColImp()
  
  # appeal$GLIDE<-GetGLIDEnum(appeal)
  
  # Make sure to remove repeated entries for when they update them
  appeal%<>%mutate(aid=as.integer(aid))%>%arrange(desc(aid))%>%
    filter(!duplicated(imp_sub_ID))%>%arrange(aid)
  
  appeal$imp_src_db[appeal$imp_src_db=="Emergency Appeal"]<-"GO-EA"
  appeal$imp_src_db[appeal$imp_src_db=="DREF"]<-"GO-DREF"
  appeal$imp_src_db[appeal$imp_src_db=="Forecast Based Action"]<-"GO-FBA"
  
  return(appeal)
}

CleanGO_field<-function(fieldr){
  
  fieldr$dtype<-fieldr$dtype$name
  
  fieldr%<>%GOHazards(); fieldr$dtype<-NULL
  
  fieldr$num_affected<-sapply(1:nrow(fieldr), function(i){
    ifelse(is.na(fieldr$num_affected[i]) & !is.na(fieldr$num_potentially_affected[i]),
           fieldr$num_potentially_affected[i],
           fieldr$num_affected[i])
  },simplify = T)
  
  fieldr$gov_num_affected<-sapply(1:nrow(fieldr), function(i){
    ifelse(is.na(fieldr$gov_num_affected[i]) & !is.na(fieldr$gov_num_potentially_affected[i]),
           fieldr$gov_num_potentially_affected[i],
           fieldr$gov_num_affected[i])
  },simplify = T)
  
  fieldr$other_num_affected<-sapply(1:nrow(fieldr), function(i){
    ifelse(is.na(fieldr$other_num_affected[i]) & !is.na(fieldr$other_num_potentially_affected[i]),
           fieldr$other_num_potentially_affected[i],
           fieldr$other_num_affected[i])
  },simplify = T)
  
  fieldr$imp_ISO3s<-sapply(1:length(fieldr$countries), function(i) paste0(fieldr$countries[[i]]$iso3,collapse = ","), simplify = T)
  fieldr%<>%filter(imp_ISO3s!="")
  fieldr$region<-sapply(1:length(fieldr$countries), function(i) median(convIso3Continent(fieldr$countries[[i]]$iso3)), simplify = T)
  
  fieldr$ev_name<-fieldr$location<-
    sapply(1:length(fieldr$countries), function(i) paste0(fieldr$countries[[i]]$name,collapse = ","), simplify = T)
  fieldr$ev_name=str_replace_all(fieldr$ev_name,'"',"")
  
  fieldr$ev_name_lang="lang_eng"
  
  fieldr$country<-fieldr$region<-NULL
  
  fieldr%<>%mutate(imp_ISO3s=imp_ISO3s,region=region,
                   imp_sdate=as.character(as.Date(start_date)),imp_fdate=as.character(as.Date(report_date)),
                   ev_sdate=as.character(as.Date(start_date)),ev_fdate=as.character(as.Date(report_date)),
                   imp_unitdate=as.character(as.Date(report_date)),
                   imp_src_URL="https://goadmin.ifrc.org/api/v2/field_reports",
                   imp_src_orglab="International Federation of Red Cross and Red Crescent Societies (IFRC)",
                   imp_src_org="IFRC",
                   imp_src_db="GO-FR",
                   imp_src_orgtype="orgtypengo",
                   imp_spat_covcode="spat_polygon",
                   imp_spat_res=0,
                   imp_spat_resunits="adminlevel",
                   imp_spat_crs="EPSG:4326",
                   imp_spat_srcorg="IFRC",
                   imp_spat_srcdb="GO-Maps",
                   imp_spat_URL="https://go-user-library.ifrc.org/maps",
                   imp_spat_ID=NA_character_)
  
  districts<-do.call(rbind,lapply(fieldr$districts,function(x) paste0(x,collapse = ",")))
  fieldr$imp_spat_ID[districts!=""]<-districts[districts!=""]
  fieldr$spat_res[districts!=""]<-"ADM-1"
  
  fieldr$event_ID<-GetMonty_ID(fieldr,haz=fieldr$haz_Ab)
  
  fieldr$countries<-fieldr$event<-fieldr$actions_taken<-fieldr$districts<-fieldr$regions<-fieldr$external_partners<-fieldr$supported_activities<-NULL
  
  fieldr%<>%ImpLabs(nomDB = "GO-FR", dropName = F)
  # Correct for some entries being government or 'other' estimates
  # Make sure that government estimates are saved separately
  stop("What's all this crap?")
  inds<-grepl(fieldr$VarName,pattern = "gov_num")
  fieldr$imp_src_orgtype[inds]<-"orgtypegov"
  fieldr$imp_src_org[inds]<-"IFRC-Curated Government"
  # And the 'other' column of estimates
  inds<-grepl(fieldr$VarName,pattern = "other_num")
  fieldr$imp_src_orgtype[inds]<-"orgtypeun,orgtyperio,orgtypengo,orgtypeacad,orgtypepriv,orgtypenews,orgtypeother"
  fieldr$imp_src_org[inds]<-"IFRC-Curated Other"
  
  # Create an impact-specific ID
  fieldr%<>%GetGCDB_impID()
  fieldr$imp_spat_ID<-GetGCDB_imp_spatID(fieldr)
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  fieldr%<>%AddEmptyColImp()
  
  fieldr%<>%filter(!is.na(imp_value))
  # fieldr$GLIDE<-GetGLIDEnum(fieldr)
  
  return(fieldr)
  
}

cleanGO_dref_excel<-function(dref){
  # Firstly, drop all irrelevant columns
  dref%<>%dplyr::select("Appeal.ID","Appeal.Type","Country","Disaster.Definition",
                        "Disaster.Name","Total.Approved.(CHF)","Date.of.Disaster/Trigger.Date",
                        "Date.of.Appeal.request.from.NS","Date.of.Appeal.request.from.Regions",
                        "Date.of.Approval.in.HQ.(start.date)","End.Date.of.Operation",
                        "Affected.People", "Targeted.People", "Average.cost.per.person", 
                        "Beneficiaries.Asssisted","Female.Assisted","Male.Asissted")  
  # Sort all date information:
  dref%<>%mutate_at(c("Date.of.Disaster/Trigger.Date","Date.of.Appeal.request.from.NS",
                      "Date.of.Appeal.request.from.Regions","Review.start.date",
                      "Date.of.Approval.in.HQ.(start.date)","End.Date.of.Operation"),
                    ConvDateExcel)
  
  return(dref)
}

CleanGO_dref<-function(dref){
  # # Firstly, drop all irrelevant columns
  # dref%<>%dplyr::select("Appeal.ID","Appeal.Type","Country","Disaster.Definition",
  #                "Disaster.Name","Total.Approved.(CHF)","Date.of.Disaster/Trigger.Date",
  #                "Date.of.Appeal.request.from.NS","Date.of.Appeal.request.from.Regions",
  #                "Date.of.Approval.in.HQ.(start.date)","End.Date.of.Operation",
  #                "Affected.People", "Targeted.People", "Average.cost.per.person", 
  #                "Beneficiaries.Asssisted","Female.Assisted","Male.Asissted")  
  # # Sort all date information:
  # dref%<>%mutate_at(c("Date.of.Disaster/Trigger.Date","Date.of.Appeal.request.from.NS",
  #                     "Date.of.Appeal.request.from.Regions","Review.start.date",
  #                     "Date.of.Approval.in.HQ.(start.date)","End.Date.of.Operation"),
  #                   ConvDateExcel)
  
  dref$dtype<-dref$disaster_type_details$id
  dref$dtype_disp<-dref$disaster_type_details$name
  dref$disaster_type_details<-NULL

  dref%<>%dplyr::select(-c(national_society_actions,needs_identified,
                           modified_by_details,event_map_file,
                           images_file,created_by_details,users_details,
                           cover_image_file,
                           country_details))

  tmp%<>%dplyr::select(-c(planned_interventions))

  colnames(tmp)
}

convGOApp_Monty<-function(){
  # Get the Emergency Appeal data from GO
  appeal<-ExtractGOdata(db = "GO-App", token = token)
  # Clean using the old GCDB structure
  appeal%<>%CleanGO_app()%>%filter(!is.na(haz_spec) & imp_value>0)
  # Get rid of repeated entries
  appeal%<>%distinct()%>%
    arrange(ev_sdate)
  # Load the Monty JSON template
  appMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    appeal%>%dplyr::select(event_ID, ev_name, code, imp_src_db, imp_src_org)%>%
      rename(ext_ID=code,ext_ID_db=imp_src_db,ext_ID_org=imp_src_org)
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    appeal%>%dplyr::select(event_ID,imp_ISO3s,location)%>%
    rename(ev_ISO3s=imp_ISO3s,gen_location=location)
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    appeal%>%dplyr::select(event_ID,ev_sdate,ev_fdate)
  )
  # Hazards
  hazs<-appeal%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  allhaz_class<-Add_EvHazTax_Monty(
    do.call(rbind,lapply(1:nrow(hazs),function(i){
      specs<-c(str_split(hazs$haz_spec[i],":",simplify = T))
      outsy<-hazs[rep(i,length(specs)),]
      outsy$haz_spec<-specs
      return(outsy)
    }))
  )
  # Gather it all and store it in the template!
  appMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  appMonty$event_Level$ID_linkage<-ID_linkage
  appMonty$event_Level$temporal<-temporal
  appMonty$event_Level$spatial<-spatial
  appMonty$event_Level$allhaz_class<-allhaz_class
  appMonty$event_Level$ev<-NULL
  #@@@@@ Hazard-level data @@@@@#
  # Nothing to put here as we haven't linked any hazard data yet
  appMonty$hazard_Data<-list()
  
  #@@@@@ Impact-level data @@@@@#
  # First need to ensure that any impacts with zero impacts estimated are removed to prevent bias
  appeal%<>%filter(!is.na(haz_spec) | is.na(imp_value) | imp_value>0)
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    appeal%>%mutate(ext_ID_db="GO-App",ext_ID_org="IFRC",haz_sub_ID=NA_character_)%>%
      dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, code,
                    ext_ID_db,ext_ID_org)%>%
      rename(ext_ID=code)
  )
  # Sources for impact data
  srcy<-appeal%>%dplyr::select(imp_src_db,imp_src_URL,imp_src_org)
  # impact estimates
  impact_detail<-appeal%>%distinct(imp_sub_ID,.keep_all = T)%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-appeal%>%distinct(imp_sub_ID,.keep_all = T)%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=data.frame(
      imp_sub_ID=appeal$imp_sub_ID,
      imp_spat_ID="GO-ADM0-World-shp",
      imp_spat_fileloc="https://go-user-library.ifrc.org/maps"
    ),
    spatial_info=appeal%>%dplyr::select(
      imp_ISO3s,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_crs
    )%>%mutate(imp_lon=NA_real_,imp_lat=NA_real_),
    source=appeal%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  appMonty$impact_Data<-data.frame(imp_sub_ID=unique(appeal$imp_sub_ID))
  appMonty$impact_Data$ID_linkage=ID_linkage
  appMonty$impact_Data$source=srcy
  appMonty$impact_Data$impact_detail=impact_detail
  appMonty$impact_Data$temporal=temporal
  appMonty$impact_Data$spatial=spatial
  appMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  appMonty$response_Data<-list()
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  appMonty$taxonomies$src_info<-readxl::read_xlsx("./Taxonomies/Monty_DataSources.xlsx")%>%distinct()
  
  #@@@@@ Checks and validation @@@@@#
  appMonty%<>%checkMonty()
  
  dir.create("./CleanedData/MostlyImpactData/IFRC/",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(appMonty,pretty = T,auto_unbox=T),
        paste0("./CleanedData/MostlyImpactData/IFRC/Appeal_",Sys.Date(),".json"))
    
  return(appMonty)
}

GetGO<-function(token=NULL){
  # Get the Emergency Appeal data from GO
  appeal<-ExtractGOdata(db = "GO-App", token = token)
  # Clean it up!
  appeal%<>%CleanGO_app()
  # Get the Field Reports data from GO
  fieldr<-ExtractGOdata(db = "GO-FR") #, token = token)
  # Clean it up!
  fieldr%<>%CleanGO_field()
  # Get the DREF data from GO
  # dref<-ExtractGOdata(db="GO-DREF", token = token)
  # dref<-openxlsx::read.xlsx("./RawData/MostlyImpactData/IFRC/DREF_MasterDataset_v1.1 2.xlsx","ALL_DATA")
  # Clean it up!
  # dref%<>%CleanGO_dref()
  # Combine both datasets and output
  rbind(appeal,fieldr)
}

# ifrcgo<-GetGO(haz="EQ")
# 
# fieldr<-getGOurl(db="GO-FR")
# fieldr<-getGOurl(db="GO-FR",token)
# 
# dref<-getGOurl(db="GO-DREF",token)
# dref$dtype<-dref$disaster_type_details$id
# dref$dtype_disp<-dref$disaster_type_details$name
# dref$disaster_type_details<-NULL
# 
# tmp<-dref%>%dplyr::select(-c(national_society_actions,needs_identified,
#                          modified_by_details,event_map_file,
#                          images_file,created_by_details,users_details,
#                          budget_file_details,cover_image_file,
#                          operational_update_details,country_details,
#                          dref_final_report_details))
# 
# tmp%<>%dplyr::select(-c(planned_interventions))
# 
# colnames(tmp)



