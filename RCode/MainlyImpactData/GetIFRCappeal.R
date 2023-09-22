
token <- paste("Token", go_token)

getGOurl<-function(db="GO-App",token=NULL){
  
  if(db=="GO-App") {db_code<-"appeal"
  } else if(db=="GO-FR") {db_code<-"field_report"
  } else if(db=="GO-DREF") {db_code<-"dref"
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
  colConv$haztype[colConv$hazG%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
  colConv$haztype[colConv$hazG%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
  colConv$haztype[colConv$hazG=="WF"]<-"haztypeenviron"
  colConv$haztype[colConv$hazG=="EP"]<-"haztypebio"
  
  # Hazard clusters
  colConv$hazcluster[colConv$hazG=="DR"]<-"hazhmprecip,hazhmtemp"
  colConv$hazcluster[colConv$hazG=="FL"]<-"hazhmflood"
  colConv$hazcluster[colConv$hazG=="ST"]<-"hazhmconv,hazhmwind,hazhmpress,hazhmflood"
  colConv$hazcluster[grepl("rain",colConv$dtype,ignore.case = T)]<-"hazhmprecip"
  colConv$hazcluster[grepl("wind",colConv$dtype,ignore.case = T)]<-"hazhmwind,hazhmpress"
  colConv$hazcluster[grepl("lightning",colConv$dtype,ignore.case = T)]<-"hazhmconv"
  colConv$hazcluster[colConv$hazG=="ET"]<-"hazhmtemp"
  colConv$hazcluster[colConv$hazG=="TC"]<-"hazhmwind,hazhmpress,hazhmconv,hazhmflood"
  colConv$hazcluster[colConv$hazG=="TS"]<-"hazgeoother,hazhmmarine,hazhmflood"
  colConv$hazcluster[colConv$hazG=="EQ"]<-"hazgeoseis"
  colConv$hazcluster[colConv$hazG=="VO"]<-"hazgeovolc"
  colConv$hazcluster[colConv$hazG=="WF"]<-"hazenvenvdeg"
  colConv$hazcluster[grepl("hail",colConv$dtype,ignore.case = T)]<-"hazhmprecip"
  colConv$hazcluster[colConv$hazG=="LS"]<-"hazgeoseis,hazenvenvdeg,hazgeovolc,hazgeoother"
  colConv$hazcluster[grepl("rock",colConv$dtype,ignore.case = T)]<-"hazhmterr"
  colConv$hazcluster[grepl("mud",colConv$dtype,ignore.case = T)]<-"hazhmterr"
  colConv$hazcluster[grepl("liquefaction",colConv$dtype,ignore.case = T)]<-"hazgeoseis,hazgeoother"
  colConv$hazcluster[colConv$hazG=="AV"]<-"hazhmterr"
  colConv$hazcluster[grepl("tidal",colConv$dtype,ignore.case = T)]<-"hazhmmarine,hazhmflood"
  colConv$hazcluster[grepl("wave",colConv$dtype,ignore.case = T)]<-"hazhmmarine,hazhmflood"
  colConv$hazcluster[grepl("coastal flood",colConv$dtype,ignore.case = T)]<-"hazhmflood,hazhmmarine"
  colConv$hazcluster[grepl("surge",colConv$dtype,ignore.case = T)]<-"hazhmmarine,hazhmflood,hazhmwind"
  colConv$hazcluster[grepl("hail",colConv$dtype,ignore.case = T)]<-"hazhmprecip"
  colConv$hazcluster[grepl("tropical storm",colConv$dtype,ignore.case = T)]<-"hazhmwind"
  colConv$hazcluster[grepl("convective storm",colConv$dtype,ignore.case = T)]<-"hazhmconv"
  colConv$hazcluster[grepl("cold wave",colConv$dtype,ignore.case = T)]<-"hazhmtemp"
  
  # Specific Hazards
  colConv$hazspec[colConv$hazG=="EQ"]<-"GH0001,GH0002"
  colConv$hazpotlink[colConv$hazG=="EQ"]<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
  
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
  colnames(impies)[colnames(impies)=="hazG"]<-"hazAb"
  
  return(impies)
  
}

CleanGO_app<-function(appeal){
  
  appeal$dtype<-appeal$dtype$name
  
  appeal%<>%GOHazards(); appeal$dtype<-NULL
  
  appeal$ISO3<-appeal$country$iso3
  appeal$Continent<-convIso3Continent(appeal$ISO3)

  appeal$country<-appeal$region<-NULL
  
  appeal$created_at<-str_split(appeal$created_at," ",simplify = T)[,1]
  appeal$modified_at<-str_split(appeal$modified_at," ",simplify = T)[,1]
  appeal$start_date<-str_split(appeal$start_date,"T",simplify = T)[,1]
  appeal$end_date<-str_split(appeal$end_date,"T",simplify = T)[,1]
  
  appeal%<>%mutate(ISO3=ISO3,Continent=Continent,
                   ev_name_en=name,location=name,
                   imp_sdate=as.character(as.Date(created_at)),imp_fdate=as.character(as.Date(modified_at)),
                   ev_sdate=as.character(as.Date(start_date)),ev_fdate=as.character(as.Date(end_date)),
                   # ev_sdate=as.character(as.Date(created_at)),ev_fdate=as.character(as.Date(created_at)),
                   unitdate=as.character(as.Date(modified_at)),
                   imp_est_type="esttype_prim",
                   src_URL="https://goadmin.ifrc.org/api/v2/appeal",
                   src_org="International Federation of Red Cross and Red Crescent Societies (IFRC)",
                   src_db="GO-App",
                   src_orgtype="orgtypengo",
                   spat_type="Polygon",
                   spat_srcorg="IFRC",
                   spat_ID=NA_character_,
                   spat_res="ADM-0")
  
  appeal$GCDB_ID<-GetGCDB_ID(appeal,haz=appeal$hazAb)
  
  appeal%<>%ImpLabs(nomDB = "GO-App", dropName = T)
  
  # Create an impact-specific ID
  appeal$impsub_ID<-appeal%>%dplyr::select(c(GCDB_ID,src_db,hazspec,impactdetails))%>%
    mutate(src_db=stringr::str_remove(stringi::stri_trans_totitle(src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
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
  fieldr$Continent<-sapply(1:length(fieldr$countries), function(i) median(convIso3Continent(fieldr$countries[[i]]$iso3)), simplify = T)
  
  fieldr$ev_name_en<-fieldr$location<-
    sapply(1:length(fieldr$countries), function(i) paste0(fieldr$countries[[i]]$name,collapse = ","), simplify = T)
  
  fieldr$country<-fieldr$region<-NULL
  
  fieldr%<>%mutate(ISO3=ISO3,Continent=Continent,
                   imp_sdate=as.character(as.Date(created_at)),imp_fdate=as.character(as.Date(updated_at)),
                   ev_sdate=as.character(as.Date(start_date)),ev_fdate=as.character(as.Date(report_date)),
                   unitdate=as.character(as.Date(report_date)),
                   est_type="Primary",
                   src_URL="https://goadmin.ifrc.org/api/v2/field_reports",
                   src_org="International Federation of Red Cross and Red Crescent Societies (IFRC)",
                   src_db="GO-FR",
                   src_orgtype="orgtypengo",
                   spat_type="Polygon",
                   spat_srcorg="IFRC",
                   spat_ID=NA_character_,
                   spat_res="ADM-0")
  
  districts<-do.call(rbind,lapply(fieldr$districts,function(x) paste0(x,collapse = ",")))
  fieldr$spat_ID[districts!=""]<-districts[districts!=""]
  fieldr$spat_res[districts!=""]<-"ADM-1"
  
  fieldr$GCDB_ID<-GetGCDB_ID(fieldr,haz=fieldr$hazAb)
  
  fieldr$countries<-fieldr$event<-fieldr$actions_taken<-fieldr$districts<-fieldr$regions<-fieldr$external_partners<-fieldr$supported_activities<-NULL
  
  fieldr%<>%ImpLabs(nomDB = "GO-FR", dropName = F)
  # Correct for some entries being government or 'other' estimates
  # Make sure that government estimates are saved separately
  inds<-grepl(fieldr$VarName,pattern = "gov_num")
  fieldr$src_orgtype[inds]<-"orgtypegov"
  fieldr$src_org[inds]<-"IFRC-Curated Government"
  # And the 'other' column of estimates
  inds<-grepl(fieldr$VarName,pattern = "other_num")
  fieldr$src_orgtype[inds]<-"orgtypeun,orgtyperio,orgtypengo,orgtypeacad,orgtypepriv,orgtypenews,orgtypeother"
  fieldr$src_org[inds]<-"IFRC-Curated Other"
  
  # Create an impact-specific ID
  fieldr$impsub_ID<-fieldr%>%dplyr::select(c(GCDB_ID,src_db,hazspec,impactdetails))%>%
    mutate(src_db=stringr::str_remove(stringi::stri_trans_totitle(src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  fieldr%<>%AddEmptyColImp()
  
  fieldr%<>%filter(!is.na(impvalue))
  # fieldr$GLIDE<-GetGLIDEnum(fieldr)
  
  return(fieldr)
  
}

CleanGO_dref<-function(dref){
  dref$dtype<-dref$disaster_type_details$id
  dref$dtype_disp<-dref$disaster_type_details$name
  dref$disaster_type_details<-NULL

  tmp<-dref%>%dplyr::select(-c(national_society_actions,needs_identified,
                           modified_by_details,event_map_file,
                           images_file,created_by_details,users_details,
                           budget_file_details,cover_image_file,
                           operational_update_details,country_details,
                           dref_final_report_details))

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
  # fieldr%<>%CleanGO_dref()
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



