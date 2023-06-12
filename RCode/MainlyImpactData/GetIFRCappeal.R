
token <- paste("Token", go_token)

getGOurl<-function(db="appeal",token=NULL){
  # Base URL
  url<-paste0("https://goadmin.ifrc.org/api/v2/",db,"/?format=json&limit=100000000000")
  # It's easier if we only want to access the public-facing data...
  if(!is.null(token)){
    # Make a request using a token
    req <- httr::GET(url, httr::add_headers(Authorization = token),httr::timeout(1000))
    # Extract the content
    json <- httr::content(req, as = "text", encoding = "UTF-8")
    # Convert from JSON to R list or tibble output
    return(jsonlite::fromJSON(json)$results)
  } else return(jsonlite::fromJSON(url)$results)
}


appeal<-getGOurl(db="appeal",token)
saveRDS(appeal,"./RawData/MostlyImpactData/IFRC/appeal.Rdata")

appeal%<>%filter(dtype$name=="Earthquake")

appeal$ISO3<-appeal$country$iso3
appeal$Continent<-convIso3Continent(appeal$country$iso3)

appeal%<>%reshape2::melt(measure.vars=c("num_beneficiaries",
                                        "amount_requested",
                                        "amount_funded"))%>%
  transmute(ISO3=ISO3,Continent=Continent,
            ev_name_en=name,location=name,
            imp_sdate=as.Date(created_at),imp_fdate=as.Date(modified_at),
            ev_sdate=as.Date(start_date),ev_fdate=as.Date(end_date),
            unitdate=as.Date(modified_at),
            est_type="Primary",
            src_URL="https://goadmin.ifrc.org/api/v2/appeal/?format=json",
            spat_srcorg=NA_character_,
            src_org="International Federation of Red Cross and Red Crescent Societies (IFRC)",
            src_db="IFRC-GO",
            src_orgtype="orgtypengo",
            spat_type="Polygon",
            spat_ID=NA_character_,
            spat_res="ADM-0",
            variable=variable,value=as.numeric(value))

appeal$GCDB_ID<-GetGCDB_ID(appeal)
appeal$hazspec<-"GH0001,GH0002"
appeal$haztype<-"haztypegeohaz"
appeal$hazcluster<-"hazgeoseis"
appeal$hazpotlink<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
appeal$hazlink<-NA_character_

appeal%<>%ImpLabs(nomDB = "GO")

# Create an impact-specific ID
appeal$impsub_ID<-appeal%>%dplyr::select(c(GCDB_ID,src_db,hazspec,impactdetails))%>%
  mutate(src_db=stringr::str_remove(stringi::stri_trans_totitle(src_db),pattern = " "))%>%
  apply(1,function(x) paste0(x,collapse = "-"))
# Add missing columns & reorder the dataframe to fit imp_GCDB object
appeal%<>%AddEmptyColImp()

appeal$GLIDE<-GetGLIDEnum(appeal)

hazzies<-MatchUSGS(appeal,noextract = T)



fieldr<-getGOurl(db="field_report")
fieldr<-getGOurl(db="field_report",token)

dref<-getGOurl(db="dref",token)








