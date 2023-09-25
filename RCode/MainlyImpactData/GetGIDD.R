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
#   colConv$haz_type[colConv$hazG%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haz_typehydromet"
#   colConv$haz_type[colConv$hazG%in%c("EQ","LS","TS","VO","AV")]<-"haz_typegeohaz"
#   colConv$haz_type[colConv$hazG=="WF"]<-"haz_typeenviron"
#   colConv$haz_type[colConv$hazG=="EP"]<-"haz_typebio"
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
  colnames(GIDD)[colnames(GIDD)=="EventName"]<-"ev_name_en"
  # Add the continent, then remove the unnecesary layers
  GIDD%<>%mutate(Continent=convIso3Continent(ISO3))%>%
    filter(!is.na(Continent))
  # Generate GCDB event ID
  GIDD$GCDB_ID<-GetGCDB_ID(GIDD)
  # Add some of the extra details that are Desinventar-specific
  GIDD$imp_est_type<-"esttype_prim"
  GIDD$src_URL<-"https://helix-tools-api.idmcdb.org/external-api/gidd/disasters/disaster-export/"
  GIDD$spat_srcorg<-GIDD$imp_src_org<-"Internal Displacement Monitoring Centre (IDMC)"
  GIDD$imp_src_db<-"GIDD"
  GIDD$imp_src_orgtype<-"orgtypengo"
  GIDD$spat_type<-"Polygon"
  GIDD$spat_ID<-NA_character_
  # Admin level resolution
  GIDD$spat_res<-"ADM-0"
  # Correct the labels of the impacts, melting by impact detail
  GIDD%<>%ImpLabs(nomDB = "GIDD")
  # Create an impact-specific ID
  GIDD$impsub_ID<-GIDD%>%dplyr::select(c(GCDB_ID,imp_src_db,haz_spec,imp_det))%>%
    mutate(imp_src_db=stringr::str_remove(stringi::stri_trans_totitle(imp_src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  GIDD%>%AddEmptyColImp()
}

# GIDD<-GetGIDD()





# 
# out%<>%arrange(desc(imp_value))
# 
# checker<-sapply(1:nrow(out),function(i){
#   print(out$GCDB_ID[i])
#   hazzy<-GetUSGS_id(out$USGSid[i],titlz=paste0("./RawData/MostlyHazardData/EQ/"),I0=4.5,minmag=5,earlysort=T)
#   if(is.null(hazzy)) return(F)
#   print("success")
#   saveRDS(hazzy,paste0("./CleanedData/MostlyHazardData/EQ/",out$GCDB_ID[i],"_",out$USGSid[i],".RData"))
#   return(T)
# })
# # 










