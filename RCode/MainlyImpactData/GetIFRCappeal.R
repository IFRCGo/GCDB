
token <- paste("Token", go_token)

getGOurl<-function(db="GO-App",token=NULL){
  
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
    return(jsonlite::fromJSON(json)$results)
  } else return(jsonlite::fromJSON(url)$results)
}

ExtractGOdata<-function(db="GO-App", token = NULL){
  options(timeout = max(10000, getOption("timeout")))
  getGOurl(db=db,token)
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
  
  appeal$ISO3<-appeal$country$iso3
  appeal$region<-convIso3Continent(appeal$ISO3)

  appeal$country<-appeal$region<-NULL
  
  appeal$created_at<-str_split(appeal$created_at," ",simplify = T)[,1]
  appeal$modified_at<-str_split(appeal$modified_at," ",simplify = T)[,1]
  appeal$start_date<-str_split(appeal$start_date,"T",simplify = T)[,1]
  appeal$end_date<-str_split(appeal$end_date,"T",simplify = T)[,1]
  
  appeal%<>%mutate(ISO3=ISO3,region=region,
                   ev_name=name,location=name,
                   ev_name_lang="lang_eng",
                   imp_sdate=as.character(as.Date(created_at)),imp_fdate=as.character(as.Date(modified_at)),
                   ev_sdate=as.character(as.Date(start_date)),ev_fdate=as.character(as.Date(end_date)),
                   # ev_sdate=as.character(as.Date(created_at)),ev_fdate=as.character(as.Date(created_at)),
                   imp_unitdate=as.character(as.Date(modified_at)),
                   imp_est_type="esttype_prim",
                   src_URL="https://goadmin.ifrc.org/api/v2/appeal",
                   imp_src_org="International Federation of Red Cross and Red Crescent Societies (IFRC)",
                   imp_src_db="GO-App",
                   imp_src_orgtype="orgtypengo",
                   imp_spat_type="Polygon",
                   imp_spat_srcorg="IFRC",
                   imp_spat_ID=NA_character_,
                   spat_res="ADM-0")
  
  appeal$event_ID<-GetMonty_ID(appeal,haz=appeal$haz_Ab)
  
  appeal%<>%ImpLabs(nomDB = "GO-App", dropName = T)
  
  # Create an impact-specific ID
  appeal%<>%GetGCDB_impID()
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  appeal%<>%AddEmptyColImp()
  
  # appeal$GLIDE<-GetGLIDEnum(appeal)
  
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
  
  fieldr$ISO3<-sapply(1:length(fieldr$countries), function(i) paste0(fieldr$countries[[i]]$iso3,collapse = ","), simplify = T)
  fieldr%<>%filter(ISO3!="")
  fieldr$region<-sapply(1:length(fieldr$countries), function(i) median(convIso3Continent(fieldr$countries[[i]]$iso3)), simplify = T)
  
  fieldr$ev_name<-fieldr$location<-
    sapply(1:length(fieldr$countries), function(i) paste0(fieldr$countries[[i]]$name,collapse = ","), simplify = T)
  
  fieldr$ev_name_lang="lang_eng"
  
  fieldr$country<-fieldr$region<-NULL
  
  fieldr%<>%mutate(ISO3=ISO3,region=region,
                   imp_sdate=as.character(as.Date(created_at)),imp_fdate=as.character(as.Date(updated_at)),
                   ev_sdate=as.character(as.Date(start_date)),ev_fdate=as.character(as.Date(report_date)),
                   imp_unitdate=as.character(as.Date(report_date)),
                   imp_est_type="Primary",
                   src_URL="https://goadmin.ifrc.org/api/v2/field_reports",
                   imp_src_org="International Federation of Red Cross and Red Crescent Societies (IFRC)",
                   imp_src_db="GO-FR",
                   imp_src_orgtype="orgtypengo",
                   imp_spat_type="Polygon",
                   imp_spat_srcorg="IFRC",
                   imp_spat_ID=NA_character_,
                   spat_res="ADM-0")
  
  districts<-do.call(rbind,lapply(fieldr$districts,function(x) paste0(x,collapse = ",")))
  fieldr$imp_spat_ID[districts!=""]<-districts[districts!=""]
  fieldr$spat_res[districts!=""]<-"ADM-1"
  
  fieldr$event_ID<-GetMonty_ID(fieldr,haz=fieldr$haz_Ab)
  
  fieldr$countries<-fieldr$event<-fieldr$actions_taken<-fieldr$districts<-fieldr$regions<-fieldr$external_partners<-fieldr$supported_activities<-NULL
  
  fieldr%<>%ImpLabs(nomDB = "GO-FR", dropName = F)
  # Correct for some entries being government or 'other' estimates
  # Make sure that government estimates are saved separately
  inds<-grepl(fieldr$VarName,pattern = "gov_num")
  fieldr$imp_src_orgtype[inds]<-"orgtypegov"
  fieldr$imp_src_org[inds]<-"IFRC-Curated Government"
  # And the 'other' column of estimates
  inds<-grepl(fieldr$VarName,pattern = "other_num")
  fieldr$imp_src_orgtype[inds]<-"orgtypeun,orgtyperio,orgtypengo,orgtypeacad,orgtypepriv,orgtypenews,orgtypeother"
  fieldr$imp_src_org[inds]<-"IFRC-Curated Other"
  
  # Create an impact-specific ID
  fieldr%<>%GetGCDB_impID()
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  fieldr%<>%AddEmptyColImp()
  
  fieldr%<>%filter(!is.na(imp_value))
  # fieldr$GLIDE<-GetGLIDEnum(fieldr)
  
  return(fieldr)
  
}

CleanGO_dref<-function(dref){
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



