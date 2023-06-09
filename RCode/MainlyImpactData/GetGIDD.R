IDMCfolder<-"./CleanedData/MostlyImpactData/IDMC/"

ExtractGIDD<-function(){
  # Create the folder for the data
  dir.create(IDMCfolder,showWarnings = F,recursive = T); 
  # Download the data directly from IDMC
  download.file("https://helix-tools-api.idmcdb.org/external-api/gidd/disasters/disaster-export/?iso3__in=&start_year=2000&end_year=2022&hazard_type__in=&client_id=IDMCWSHSOLO009&release_environment=RELEASE",
                paste0(IDMCfolder,"GIDD-IDMC.xlsx"))
  return(T)
}

GIDDHazards<-function(GIDD,haz="EQ"){
  if(haz=="EQ"){
    # Filter for only this hazard
    GIDD%<>%filter(HazardSubType=="Earthquake")
    # Add hazard taxonomy
    GIDD$hazspec<-"GH0001,GH0002"
    GIDD$haztype<-"haztypegeohaz"
    GIDD$hazcluster<-"hazgeoseis"
    GIDD$hazpotlink<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
    
    GIDD%>%dplyr::select(-c(HazardCategory,HazardType,HazardSubType))%>%return()
    
  } else if(haz=="FL"){
    stop("This hazard isn't ready for Desinventar yet")
  } else if(haz=="TC"){
    stop("This hazard isn't ready for Desinventar yet")
  } else if(haz=="ST"){
    stop("This hazard isn't ready for Desinventar yet")
  } else stop("Hazard not recognised for Desinventar data")
  
}


GetGIDD<-function(haz="EQ"){
  # File storage
  filez<-paste0(IDMCfolder,"GIDD-IDMC.xlsx")
  # If it doesn't exist, extract it
  if(!file.exists(filez)) ExtractGIDD()
  # Load data
  GIDD<-readxl::read_xlsx(filez)
  # Change the column names to something more user friendly
  colnames(GIDD)<-str_remove_all(str_split(str_split(colnames(GIDD),"\\(",simplify = T)[,1],"\\/",simplify = T)[,1]," ")
  colnames(GIDD)[7]<-paste0(colnames(GIDD)[7],"Raw")
  # Hazard taxonomy - HIPS
  GIDD%<>%GIDDHazards(haz=haz)
  # Modify date names
  GIDD$imp_sdate<-GIDD$imp_fdate<-GIDD$ev_sdate<-GIDD$ev_fdate<-GIDD$unitdate<-as.character(GIDD$DateofEvent)
  # Rename the event name
  colnames(GIDD)[colnames(GIDD)=="EventName"]<-"ev_name_en"
  # Add the continent, then remove the unnecesary layers
  GIDD%<>%mutate(Continent=convIso3Continent(ISO3))%>%
    filter(!is.na(Continent))
  # Generate GCDB event ID
  GIDD$GCDB_ID<-GetGCDB_ID(GIDD,haz=haz)
  # Add some of the extra details that are Desinventar-specific
  GIDD$est_type<-"Primary"
  GIDD$src_URL<-"https://helix-tools-api.idmcdb.org/external-api/gidd/disasters/disaster-export/"
  GIDD$spat_srcorg<-GIDD$src_org<-"Internal Displacement Monitoring Centre (IDMC)"
  GIDD$src_db<-"HELIX"
  GIDD$src_orgtype<-"orgtypengo"
  GIDD$spat_type<-"Polygon"
  GIDD$spat_ID<-NA_character_
  # Admin level resolution
  GIDD$spat_res<-"ADM-0"
  # Correct the labels of the impacts, melting by impact detail
  GIDD%<>%ImpLabs(nomDB = "GIDD")
  # Create an impact-specific ID
  GIDD$impsub_ID<-GIDD%>%dplyr::select(c(GCDB_ID,src_db,hazspec,impactdetails))%>%
    mutate(src_db=stringr::str_remove(stringi::stri_trans_totitle(src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  GIDD%>%AddEmptyColImp()
}

# GIDD<-GetGIDD()





# 
# out%<>%arrange(desc(impvalue))
# 
checker<-sapply(1:nrow(out),function(i){
  print(out$GCDB_ID[i])
  hazzy<-GetUSGS_id(out$USGSid[i],titlz=paste0("./RawData/MostlyHazardData/EQ/"),I0=4.5,minmag=5,earlysort=T)
  if(is.null(hazzy)) return(F)
  print("success")
  saveRDS(hazzy,paste0("./CleanedData/MostlyHazardData/EQ/",out$GCDB_ID[i],"_",out$USGSid[i],".RData"))
  return(T)
})
# # 










