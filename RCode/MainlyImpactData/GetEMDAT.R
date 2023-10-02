
PairEMDATspatial<-function(EMDAT,haz="EQ",GAULexist=F){
  
  if(sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Adm.Level)) &
     sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin1.Code)) & 
     sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin2.Code))) stop("EMDAT has irregular admin locations")
  # Get all the iso codes that we need admin data for
  isoGAUL<-unique(EMDAT$ISO[!is.na(EMDAT$Geo.Locations)])
  # If the GAUL data is already there, link it and exit
  if(GAULexist){
    
  }
  # Load all required shapefiles
  checkers<-GetGAUL(isoGAUL)
  # Let's patch over the ones that didn't properly work
  GaulInc<-isoGAUL[!isoGAUL%in%list.files("./CleanedData/SocioPoliticalData/EMDAT/")]
  # Make sure to save which ones didn't fully work
  outer<-data.frame(ISO3C=isoGAUL,Status="Complete")
  # Set those that didn't work
  outer$Status[outer$ISO3C%in%GaulInc]<-"Some Elements Missing"
  # Write out
  write_csv(outer,paste0("./CleanedData/SocioPoliticalData/EMDAT/fully_complete_",haz,".csv"))
  
  EMDAT$spat_ID<-checkers$ID
  EMDAT$spat_type<-"polygon"
  EMDAT$spat_orig<-"GAUL"
  
  # If some spatial objects aren't found, try accessing ADM level 1
  if(sum(is.na(EMDAT$spat_ID))!=0) {
    # Which countries to go back over
    inds<-is.na(EMDAT$spat_ID)
    # Try, try and try, try and tryyyyyyyyy, you'll succeed at last
    checkers<-GetGAUL(isoGAUL[inds],lADM=1)
    EMDAT$spat_ID[inds]<-checkers$ID
  }
  # If some spatial objects aren't found, try accessing ADM level 0
  if(sum(is.na(EMDAT$spat_ID))!=0) {
    # Which countries to go back over
    inds<-is.na(EMDAT$spat_ID)
    # Try, try and try, try and tryyyyyyyyy, you'll succeed at last
    checkers<-GetGAUL(isoGAUL[inds],lADM=0)
    EMDAT$spat_ID[inds]<-checkers$ID
  }
  
  return(EMDAT)
  
}

# PostModEMDAT<-function(colConv){
#   # hazard Types
#   colConv$haz_type[colConv$hazEM%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haz_typehydromet"
#   colConv$haz_type[colConv$hazEM%in%c("EQ","LS","TS","VO","AV")]<-"haz_typegeohaz"
#   colConv$haz_type[colConv$hazEM=="WF"]<-"haz_typeenviron"
#   colConv$haz_type[colConv$hazEM=="EP"]<-"haz_typebio"
#   colConv$haz_type[grepl("cyclone & flood",colConv$Disaster.Subtype,ignore.case = T)]<-"haz_typehydromet"
#   
#   # Hazard clusters
#   colConv$haz_cluster[colConv$hazEM=="DR"]<-"hazhmprecip,hazhmtemp"
#   colConv$haz_cluster[colConv$hazEM=="FL"]<-"hazhmflood"
#   colConv$haz_cluster[colConv$hazEM=="ST"]<-"hazhmconv,hazhmwind,hazhmpress,hazhmflood"
#   colConv$haz_cluster[grepl("rain",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmprecip"
#   colConv$haz_cluster[grepl("wind",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmwind,hazhmpress"
#   colConv$haz_cluster[grepl("lightning",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmconv"
#   colConv$haz_cluster[colConv$hazEM=="ET"]<-"hazhmtemp"
#   colConv$haz_cluster[colConv$hazEM=="TC"]<-"hazhmwind,hazhmpress,hazhmconv,hazhmflood"
#   colConv$haz_cluster[colConv$hazEM=="TS"]<-"hazgeoother,hazhmmarine,hazhmflood"
#   colConv$haz_cluster[colConv$hazEM=="EQ"]<-"hazgeoseis"
#   colConv$haz_cluster[colConv$hazEM=="VO"]<-"hazgeovolc"
#   colConv$haz_cluster[colConv$hazEM=="WF"]<-"hazenvenvdeg"
#   colConv$haz_cluster[grepl("hail",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmprecip"
#   colConv$haz_cluster[colConv$hazEM=="LS"]<-"hazgeoseis,hazenvenvdeg,hazgeovolc,hazgeoother"
#   colConv$haz_cluster[grepl("rock",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmterr"
#   colConv$haz_cluster[grepl("mud",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmterr"
#   colConv$haz_cluster[grepl("liquefaction",colConv$Disaster.Subtype,ignore.case = T)]<-"hazgeoseis,hazgeoother"
#   colConv$haz_cluster[colConv$hazEM=="AV"]<-"hazhmterr"
#   colConv$haz_cluster[grepl("tidal",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmmarine,hazhmflood"
#   colConv$haz_cluster[grepl("wave",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmmarine,hazhmflood"
#   colConv$haz_cluster[grepl("coastal flood",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmflood,hazhmmarine"
#   colConv$haz_cluster[grepl("surge",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmmarine,hazhmflood,hazhmwind"
#   colConv$haz_cluster[grepl("hail",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmprecip"
#   colConv$haz_cluster[grepl("tropical storm",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmwind"
#   colConv$haz_cluster[grepl("convective storm",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmconv"
#   colConv$haz_cluster[grepl("cold wave",colConv$Disaster.Subtype,ignore.case = T)]<-"hazhmtemp"
#   
#   # Specific Hazards
#   colConv$haz_spec[colConv$hazEM=="EQ"]<-"GH0001,GH0002"
#   colConv$haz_potlink[colConv$hazEM=="EQ"]<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
#   
#   # Save it out
#   openxlsx::write.xlsx(colConv,"./Taxonomies/MostlyImpactData/EMDAT_HIP.xlsx")
#   
#   return(colConv)
# }

EMDATHazards<-function(EMDAT){
  EMDAT$Disaster.Subgroup%<>%str_to_lower()
  EMDAT$Disaster.Type%<>%str_to_lower()
  EMDAT$Disaster.Subtype%<>%str_to_lower()
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/EMDAT_HIP.xlsx")
  colConv$Disaster.Subgroup%<>%str_to_lower()
  colConv$Disaster.Type%<>%str_to_lower()
  colConv$Disaster.Subtype%<>%str_to_lower()
  # Reduce the translated vector and merge
  EMDAT%<>%left_join(colConv,by = c("Disaster.Subgroup","Disaster.Type","Disaster.Subtype","Disaster.Subsubtype"),
                     relationship="many-to-one")
  # Convert EMDAT hazard categorisation to the HIPS!
  # if(haz=="EQ"){
  #   # Actual linked hazards
  #   EMDAT$haz_link<-NA_character_
  #   # Modify EMDAT dataframe, line by line
  #   for(i in 1:nrow(colConv)){
  #     # Primary associated hazard
  #     ind<-!is.na(EMDAT$Associated.Dis) & EMDAT$Associated.Dis==colConv$Disaster.Subtype[i]
  #     EMDAT$haz_link[ind]<-colConv$haz_spec[i]
  #     # Secondary associated hazard
  #     ind<-!is.na(EMDAT$Associated.Dis2) & EMDAT$Associated.Dis2==colConv$Disaster.Subtype[i]
  #     EMDAT$haz_link[ind]<-paste0(EMDAT$haz_link[ind],paste0(",",colConv$haz_spec[i]))
  #   }
  #   # Potential linked hazards
  #   EMDAT$haz_potlink<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
  #   EMDAT$haz_potlink[EMDAT$Disaster.Subtype=="Tsunami"]<-paste0(c("GH0001","GH0002","GH0003","GH0004","GH0005","GH0007"),collapse = ",")
  EMDAT%<>%dplyr::select(-c(Disaster.Subtype,Disaster.Subsubtype,
                            Disaster.Subgroup,Disaster.Type,Disaster.Group,
                            Associated.Dis,Associated.Dis2))
    
} 
  

CleanEMDAT<-function(EMDAT){
  # Replace empty values
  EMDAT[EMDAT=="Source:"]<-NA
  # Some of the column names are messed up due to presence of non-letters
  colnames(EMDAT)[c(22,40:45)]<-
    c("AID.Contribution","Reconstruction.Costs","Reconstruction.Costs.Adjusted",
      "Insured.Damages","Insured.Damages.Adjusted","Total.Damages","Total.Damages.Adjusted")
  # Also, make sure to convert to the full value in US dollars
  for(i in c(22,40:45)) EMDAT[,i]<-1000*as.numeric(EMDAT[,i])
  # For dates with no start day, make it the middle of the month
  EMDAT$Start.Day[is.na(EMDAT$Start.Day)]<-15
  # Make sure the start date is 2 characters
  EMDAT$Start.Day[nchar(EMDAT$Start.Day)==1 & !is.na(EMDAT$Start.Day)]<-
    paste0("0",EMDAT$Start.Day[nchar(EMDAT$Start.Day)==1 & !is.na(EMDAT$Start.Day)])
  # Make sure the start month is 2 characters
  EMDAT$Start.Month[nchar(EMDAT$Start.Month)==1 & !is.na(EMDAT$Start.Month)]<-
    paste0("0",EMDAT$Start.Month[nchar(EMDAT$Start.Month)==1 & !is.na(EMDAT$Start.Month)])
  # Make sure the end date is 2 characters
  EMDAT$End.Day[nchar(EMDAT$End.Day)==1 & !is.na(EMDAT$End.Day)]<-
    paste0("0",EMDAT$End.Day[nchar(EMDAT$End.Day)==1 & !is.na(EMDAT$End.Day)])
  # Make sure the end month is 2 characters
  EMDAT$End.Month[nchar(EMDAT$End.Month)==1 & !is.na(EMDAT$End.Month)]<-
    paste0("0",EMDAT$End.Month[nchar(EMDAT$End.Month)==1 & !is.na(EMDAT$End.Month)])
  # Start date in one
  EMDAT$imp_sdate<-EMDAT$ev_sdate<-EMDAT$imp_unitdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$Start.Year[i],
                                                         EMDAT$Start.Month[i],
                                                         EMDAT$Start.Day[i]),collapse = "-"),simplify = T)
  # End date in one
  EMDAT$imp_fdate<-EMDAT$ev_fdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$End.Year[i],
                                                         EMDAT$End.Month[i],
                                                         EMDAT$End.Day[i]),collapse = "-"),simplify = T)
  # Remove everything we dont need
  EMDAT%<>%dplyr::select(-c(Start.Day,Start.Month,Start.Year,
                            End.Day,End.Month,End.Year,
                            Year,Country,Region))
  # Column renaming
  colnames(EMDAT)[colnames(EMDAT)=="Event.Name"]<-"ev_name_en"; colnames(EMDAT)[colnames(EMDAT)=="Location"]<-"location"; colnames(EMDAT)[colnames(EMDAT)=="ISO"]<-"ISO3"
  # Add some of the extra details that are Desinventar-specific
  EMDAT$imp_est_type<-"esttype_prim"
  EMDAT$src_URL<-"https://public.emdat.be/"
  EMDAT$spat_srcorg<-EMDAT$imp_src_org<-"CRED - Uni. Louvain"
  EMDAT$imp_src_db<-"EM-DAT"
  EMDAT$imp_src_orgtype<-"orgtypeacad"
  EMDAT$spat_type<-"Polygon"
  EMDAT$spat_ID<-apply(EMDAT[,c("Admin1.Code","Admin2.Code")],1,function(x) {
    if(all(is.na(x))) return(NA_character_)
    if(any(is.na(x))) return(x[!is.na(x)])
    paste0(c(ifelse(is.na(x[1]),"",x[1]),
             ifelse(is.na(x[2]),"",x[2])),
           collapse = ",")
  },simplify = T)
  # Admin level resolution
  EMDAT$spat_res<-"ADM-0"; EMDAT$spat_res[!is.na(EMDAT$Admin1.Code)]<-"ADM-1"; EMDAT$spat_res[!is.na(EMDAT$Admin2.Code)]<-"ADM-2"
  # Link to the hazard taxonomy from HIPS
  EMDAT%<>%EMDATHazards()
  
  if(nrow(EMDAT)==0) return(EMDAT)
  # Some GLIDE numbers don't have an associated hazard...
  ind<-!is.na(EMDAT$Glide) & nchar(EMDAT$Glide)==11
  EMDAT$Glide[ind]<-paste0(EMDAT$haz_Ab[ind],"-",EMDAT$Glide[ind])
  # Ensure column name aligns with imp_GCDB object
  colnames(EMDAT)[colnames(EMDAT)=="Glide"]<-"GLIDE"
  # Generate the GCDB ID
  EMDAT$GCDB_ID<-GetGCDB_ID(EMDAT)
  # Melt the columns and apply the impact categorisation
  EMDAT%<>%ImpLabs(nomDB = "EM-DAT")
  # Now get rid of the extra columns of data
  EMDAT%<>%dplyr::select(-which(!colnames(EMDAT)%in%names(col_impGCDB)))
  # Create an impact-specific ID
  EMDAT$imp_sub_ID<-EMDAT%>%dplyr::select(c(GCDB_ID,imp_src_db,haz_spec,imp_det,spat_ID))%>%
    mutate(imp_src_db=stringr::str_remove(stringi::stri_trans_totitle(imp_src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  # Make sure to remove all NA impact estimates
  EMDAT%<>%filter(!is.na(imp_value))
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  EMDAT%>%AddEmptyColImp()
}

GetEMDAT<-function(){
  # EMDAT file
  # filez<-paste0("./RawData/MostlyImpactData/EMDAT/emdat_public_",haz,"_20230526.xlsx")
  filez<-paste0("./CleanedData/MostlyImpactData/EMDAT/emdat_public_2023_09_22_query_uid-tUnheR.xlsx")
  # If nothing found
  if(!file.exists(filez)) return(data.frame())
  # Extract the hazard-specific EMDAT data
  EMDAT<-openxlsx::read.xlsx(filez,startRow = 7)
  # Clean it up and get it in the right format
  EMDAT%<>%CleanEMDAT()
  # Make sure that the spatial data required actually exists
  # EMDAT%<>%PairEMDATspatial(haz=haz)
  # Form a GCDB impacts object from EMDAT data (if there is a problem, return an empty impGCDB object)
  # tryCatch(new("impGCDB",EMDAT,type="EMDAT",haz=haz),error=function(e) new("impGCDB"))
  
  return(EMDAT)
}

# Get the EMDAT data
# filez<-list.files("../../CleanedData/MostlyImpactData/EMDAT/",include.dirs = T,all.files = T,recursive = T,ignore.case = T)
# EMDAT<-do.call(rbind,lapply(filez,function(fff) {openxlsx::read.xlsx(paste0("../../CleanedData/MostlyImpactData/EMDAT/",fff),startRow = 7)}))

