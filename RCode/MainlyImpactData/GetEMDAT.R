
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
  
  EMDAT$imp_spat_ID<-checkers$ID
  EMDAT$imp_spat_covcode<-"spat_polygon"
  EMDAT$spat_orig<-"GAUL"
  
  # If some spatial objects aren't found, try accessing ADM level 1
  if(sum(is.na(EMDAT$imp_spat_ID))!=0) {
    # Which countries to go back over
    inds<-is.na(EMDAT$imp_spat_ID)
    # Try, try and try, try and tryyyyyyyyy, you'll succeed at last
    checkers<-GetGAUL(isoGAUL[inds],lADM=1)
    EMDAT$imp_spat_ID[inds]<-checkers$ID
  }
  # If some spatial objects aren't found, try accessing ADM level 0
  if(sum(is.na(EMDAT$imp_spat_ID))!=0) {
    # Which countries to go back over
    inds<-is.na(EMDAT$imp_spat_ID)
    # Try, try and try, try and tryyyyyyyyy, you'll succeed at last
    checkers<-GetGAUL(isoGAUL[inds],lADM=0)
    EMDAT$imp_spat_ID[inds]<-checkers$ID
  }
  
  return(EMDAT)
  
}

EMDATHazards_API<-function(EMDAT){
  EMDAT$subgroup%<>%str_to_lower()
  EMDAT$type%<>%str_to_lower()
  EMDAT$subtype%<>%str_to_lower()
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/ConvertFromDatabases/EMDAT_HIP_API.xlsx")
  colConv$subgroup%<>%str_to_lower()
  colConv$type%<>%str_to_lower()
  colConv$subtype%<>%str_to_lower()
  # Reduce the translated vector and merge
  EMDAT%<>%left_join(colConv,by = c("subgroup","type","subtype"),
                     relationship="many-to-one")
  
  EMDAT%>%dplyr::select(-c(group,subgroup,type,subtype,associated_types))
} 

EMDATHazards<-function(EMDAT){
  EMDAT$Disaster.Subgroup%<>%str_to_lower()
  EMDAT$Disaster.Type%<>%str_to_lower()
  EMDAT$Disaster.Subtype%<>%str_to_lower()
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/ConvertFromDatabases/EMDAT_HIP_new.xlsx")
  colConv$Disaster.Subgroup%<>%str_to_lower()
  colConv$Disaster.Type%<>%str_to_lower()
  colConv$Disaster.Subtype%<>%str_to_lower()
  # Reduce the translated vector and merge
  EMDAT%<>%left_join(colConv,by = c("Disaster.Subgroup","Disaster.Type","Disaster.Subtype"),
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
  EMDAT%<>%dplyr::select(-c(Disaster.Subtype,Disaster.Subgroup,Disaster.Type,
                            Disaster.Group,Associated.Types))
    
} 
  
EMDATHazards_old<-function(EMDAT){
  EMDAT$Disaster.Subgroup%<>%str_to_lower()
  EMDAT$Disaster.Type%<>%str_to_lower()
  EMDAT$Disaster.Subtype%<>%str_to_lower()
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/ConvertFromDatabases/EMDAT_HIP.xlsx")
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
  EMDAT%<>%dplyr::select(-c(Disaster.Subtype,Disaster.Subgroup,Disaster.Type,
                            Disaster.Group,Associated.Dis,Associated.Dis2))
  
} 

CleanEMDAT_API<-function(EMDAT){
  # Some of the column names are messed up due to presence of non-letters
  EMDAT%<>%rename("AID.Contribution"="aid_contribution",
                  "Reconstruction.Costs"="reconstr_dam",
                  "Reconstruction.Costs.Adjusted"="reconstr_dam_adj",
                  "Insured.Damages"="insur_dam",
                  "Insured.Damages.Adjusted"="insur_dam_adj",
                  "Total.Damages"="total_dam",
                  "Total.Damages.Adjusted"="total_dam_adj",
                  "Total.Deaths"="total_deaths",
                  "No.Injured"="no_injured",
                  "No.Affected"="no_affected",
                  "No.Homeless"="no_homeless",
                  "Total.Affected"="total_affected",
                  "imp_credate"="entry_date",
                  "imp_moddate"="last_update")
  # Also, make sure to convert to the full value in US dollars
  EMDAT[,c("AID.Contribution","Reconstruction.Costs","Reconstruction.Costs.Adjusted",
           "Insured.Damages","Insured.Damages.Adjusted","Total.Damages",
           "Total.Damages.Adjusted")]<-1000*EMDAT[,c("AID.Contribution","Reconstruction.Costs","Reconstruction.Costs.Adjusted",
                                                                "Insured.Damages","Insured.Damages.Adjusted","Total.Damages",
                                                                "Total.Damages.Adjusted")]
  # For dates with no start day, make it the middle of the month
  EMDAT$start_day[is.na(EMDAT$start_day)]<-15
  # Make sure the start date is 2 characters
  EMDAT$start_day[nchar(EMDAT$start_day)==1 & !is.na(EMDAT$start_day)]<-
    paste0("0",EMDAT$start_day[nchar(EMDAT$start_day)==1 & !is.na(EMDAT$start_day)])
  # Make sure the start month is 2 characters
  EMDAT$start_month[nchar(EMDAT$start_month)==1 & !is.na(EMDAT$start_month)]<-
    paste0("0",EMDAT$start_month[nchar(EMDAT$start_month)==1 & !is.na(EMDAT$start_month)])
  # Make sure the end date is 2 characters
  EMDAT$end_day[nchar(EMDAT$end_day)==1 & !is.na(EMDAT$end_day)]<-
    paste0("0",EMDAT$end_day[nchar(EMDAT$end_day)==1 & !is.na(EMDAT$end_day)])
  # Make sure the end month is 2 characters
  EMDAT$end_month[nchar(EMDAT$end_month)==1 & !is.na(EMDAT$end_month)]<-
    paste0("0",EMDAT$end_month[nchar(EMDAT$end_month)==1 & !is.na(EMDAT$end_month)])
  # If the start month is NA, we take the start to be the start of the year
  EMDAT$start_day[is.na(EMDAT$start_month)]<-"01" 
  EMDAT$start_month[is.na(EMDAT$start_month)]<-"01"
  # If the end month is NA, we take the end to be the end of the year
  EMDAT$end_day[is.na(EMDAT$end_month)]<-"31" 
  EMDAT$end_month[is.na(EMDAT$end_month)]<-"12"
  # If the end date is NA, assume end of the month
  EMDAT$end_day[is.na(EMDAT$end_day) & !is.na(EMDAT$end_month)]<-
    sapply((1:nrow(EMDAT))[is.na(EMDAT$end_day) & !is.na(EMDAT$end_month)], function(i){
      # Go to the next month and then subtract one day to make it the end of the original month
      monthy<-as.character(as.numeric(EMDAT$end_month[i])+1)
      # Checks for the character
      if(nchar(monthy)==1) monthy<-paste0("0",monthy)
      if(monthy=="13") return("31")
      # Create the date variable, subtract one day from it then extract the date
      format(as.Date(paste0(c(EMDAT$end_year[i],monthy,
                              "01"),collapse = "-"))-1,"%d")
    },simplify = T)
  # Start date in one
  EMDAT$ev_sdate<-EMDAT$imp_unitdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$start_year[i],
                                                                                EMDAT$start_month[i],
                                                                                EMDAT$start_day[i]),collapse = "-"),simplify = T)
  # End date in one
  EMDAT$ev_fdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$end_year[i],
                                                            EMDAT$end_month[i],
                                                            EMDAT$end_day[i]),collapse = "-"),simplify = T)
  # imp_sdate and imp_fdate are not what you would expect, they are related to the impact record not the event start/ends dates:EMDAT$imp_sdate<-
  EMDAT%<>%mutate(imp_sdate=ev_sdate,imp_fdate=ev_fdate)
  # Remove everything we dont need
  EMDAT%<>%dplyr::select(-c(start_day,start_month,start_year,
                            end_day,end_month,end_year,
                            country,region))
  # Rename lots of the variables
  EMDAT%<>%rename("imp_ISO3s"="iso",
                  "ext_ID"="disno",
                  "gen_location"="location",
                  "imp_lon"="longitude",
                  "imp_lat"="latitude",
                  "ev_name"="name")%>%mutate(ev_ISO3s=imp_ISO3s)
  # Add some of the extra details that are EM-DAT specific
  EMDAT%<>%mutate(imp_src_URL="https://public.emdat.be/",
                 imp_src_org="CRED",
                 imp_spat_srcorg="FAO",
                 imp_spat_srcdb="GAUL",
                 imp_src_db="EMDAT",
                 imp_src_orgtype="orgtypeacad",
                 imp_spat_covcode="spat_polygon",
                 imp_spat_resunits="adminlevel",
                 imp_spat_crs="EPSG:4326")
  # Extract the admin level of each entry
  EMDAT$imp_spat_res<-0
  # extract which entries have adm level 1
  adm1s<-sapply(1:nrow(EMDAT),function(i) {
    ifelse(is.null(EMDAT$admin_units[[i]]),F,
           grepl(x = colnames(EMDAT$admin_units[[i]]),"adm1_"))
    },simplify = T)
  # extract which entries have adm level 2
  adm2s<-sapply(1:nrow(EMDAT),function(i) {
    ifelse(is.null(EMDAT$admin_units[[i]]),F,
           grepl(x = colnames(EMDAT$admin_units[[i]]),"adm2_"))
  },simplify = T)
  # Set the admin levels
  EMDAT$imp_spat_res[adm1s]<-1; EMDAT$imp_spat_res[adm2s]<-2
  # EMDAT, for now, uses GAUL ADM dataset
  EMDAT$imp_spat_ID<-lapply(1:nrow(EMDAT),function(i){
    # If no admin units are included, return simplest ID
    if(is.null(EMDAT$admin_units[[i]])) return(paste0("FAO-GAUL-ADM0-",EMDAT$imp_ISO3s[i]))
    # Otherwise, combine to make the imp_spat_ID
    return(paste0("FAO-GAUL-ADM",EMDAT$imp_spat_res[i],"-",
                  EMDAT$imp_ISO3s[i],"-",
                  (EMDAT$admin_units[[i]])[,grepl("_code",colnames(EMDAT$admin_units[[i]]))]))
  })
  # File location of the admin boundaries dataset
  EMDAT$imp_spat_fileloc<-EMDAT$imp_spat_URL<-"https://data.apps.fao.org/map/catalog/static/search?keyword=HiH_boundaries"
  # Link to the hazard taxonomy from HIPS
  EMDAT%<>%EMDATHazards_API()
  
  if(nrow(EMDAT)==0) return(EMDAT)
  
  # Generate the GCDB ID
  EMDAT$event_ID<-GetMonty_ID(EMDAT)
  # Sort the IDs variable:
  EMDAT$all_ext_IDs<-lapply(1:nrow(EMDAT), function(i){
    # First extract EM-DAT event ID
    out<-data.frame(ext_ID=EMDAT$ext_ID[i],
               ext_ID_db="EMDAT",
               ext_ID_org="CRED")
    # If no other external IDs are provided, return only the Em-DAT ID
    if(is.na(EMDAT$external_ids[i])) return(out)
    # Otherwise, add the others!
    exties<-str_split(EMDAT$external_ids[i],"\\|")
    # For each external ID, change into correct format
    do.call(rbind,lapply(exties,function(x){
      # Extract ID & org/db info from the string
      exex<-as.data.frame(str_split(x,":",simplify = T))%>%
        setNames(c("ext_ID_db","ext_ID"))%>%
        mutate(ext_ID_org=ext_ID_db)
      # Correct for the fact that EM-DAT confuses organisation and database names
      exex$ext_ID_org[exex$ext_ID_db=="USGS"]<-"USGS"
      exex$ext_ID_db[exex$ext_ID_db=="USGS"]<-"Atlas"
      exex$ext_ID_org[exex$ext_ID_db=="GLIDE"]<-"ADRC"
      exex$ext_ID_org[exex$ext_ID_db=="DFO"]<-"UniColumbia"
      
      rbind(out,exex)
    }))
  })
  # Check for any unknown/unprogrammed external IDs
  if(any(!(do.call(rbind,EMDAT$all_ext_IDs)%>%pull(ext_ID_db)%in%c("EMDAT","Atlas","GLIDE","DFO")))) warning("External IDs from unknown organisations found in EM-DAT database")
  # Melt the columns and apply the impact categorisation
  EMDAT%<>%ImpLabs(nomDB = "EMDAT",dropName = T)
  # Create an impact-specific ID
  EMDAT%<>%GetGCDB_impID()
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  EMDAT%>%dplyr::select(any_of(MontyJSONnames()))
}

CleanEMDAT<-function(EMDAT){
  # Replace empty values
  # EMDAT[EMDAT=="Source:"]<-NA
  # Some of the column names are messed up due to presence of non-letters
  colnames(EMDAT)[c(20,37:42)]<-
    c("AID.Contribution","Reconstruction.Costs","Reconstruction.Costs.Adjusted",
      "Insured.Damages","Insured.Damages.Adjusted","Total.Damages","Total.Damages.Adjusted")
  # Also, make sure to convert to the full value in US dollars
  for(i in c(20,37:42)) EMDAT[,i]<-1000*as.numeric(EMDAT[,i])
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
                            Country,Region))
  # Column renaming
  colnames(EMDAT)[colnames(EMDAT)=="Event.Name"]<-"ev_name"; colnames(EMDAT)[colnames(EMDAT)=="Location"]<-"location"; colnames(EMDAT)[colnames(EMDAT)=="ISO"]<-"imp_ISO3s"
  EMDAT$ev_name_lang<-"lang_eng"
  # Add some of the extra details that are Desinventar-specific
  EMDAT$imp_est_type<-"esttype_prim"
  EMDAT$src_URL<-"https://public.emdat.be/"
  EMDAT$imp_spat_srcorg<-EMDAT$imp_src_org<-"CRED - Uni. Louvain"
  EMDAT$imp_src_db<-"EMDAT"
  EMDAT$imp_src_orgtype<-"orgtypeacad"
  EMDAT$imp_spat_covcode<-"spat_polygon"
  # stop("EMDAT imp_spat_ID needs sorting out")
  # EMDAT$imp_spat_ID<-apply(EMDAT[,c("Admin1.Code","Admin2.Code")],1,function(x) {
  #   if(all(is.na(x))) return(NA_character_)
  #   if(any(is.na(x))) return(x[!is.na(x)])
  #   paste0(c(ifelse(is.na(x[1]),"",x[1]),
  #            ifelse(is.na(x[2]),"",x[2])),
  #          collapse = ",")
  # },simplify = T)
  # Admin level resolution
  EMDAT$spat_res<-"ADM-0"
  EMDAT$spat_res[grepl(x = EMDAT$Admin.Units,"adm1_")]<-"ADM-1" 
  EMDAT$spat_res[grepl(x = EMDAT$Admin.Units,"adm2_")]<-"ADM-2"
  # Link to the hazard taxonomy from HIPS
  EMDAT%<>%EMDATHazards()
  
  if(nrow(EMDAT)==0) return(EMDAT)
  # Some GLIDE numbers don't have an associated hazard...
  ind<-!is.na(EMDAT$Glide) & nchar(EMDAT$Glide)==11
  # stop("sort out EMDAT GLIDE numbers - 'External.IDs'")
  
  
  EMDAT$GLIDE<-EMDAT$External.IDs
  EMDAT$Glide[ind]<-paste0(EMDAT$haz_Ab[ind],"-",EMDAT$Glide[ind])
  # Ensure column name aligns with imp_GCDB object
  colnames(EMDAT)[colnames(EMDAT)=="Glide"]<-"GLIDE"
  
  
  
  
  
  # Generate the GCDB ID
  EMDAT$event_ID<-GetMonty_ID(EMDAT)
  # Melt the columns and apply the impact categorisation
  EMDAT%<>%ImpLabs(nomDB = "EMDAT")
  # Now get rid of the extra columns of data
  EMDAT%<>%dplyr::select(any_of(MontyJSONnames()))
  # Create an impact-specific ID
  EMDAT%<>%distinct()%>%GetGCDB_impID()
  # Make sure to remove all NA impact estimates
  EMDAT%<>%filter(!is.na(imp_value))
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  EMDAT%>%AddEmptyColImp()
}

CleanEMDAT_old<-function(EMDAT){
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
  colnames(EMDAT)[colnames(EMDAT)=="Event.Name"]<-"ev_name_en"; colnames(EMDAT)[colnames(EMDAT)=="Location"]<-"location"; colnames(EMDAT)[colnames(EMDAT)=="ISO"]<-"imp_ISO3s"
  # Add some of the extra details that are Desinventar-specific
  EMDAT%<>%mutate(ev_ISO3s=imp_ISO3s,
    imp_est_type="esttype_prim",
  imp_src_URL="https://public.emdat.be/",
  imp_src_org="CRED",
  imp_src_db="EMDAT",
  imp_src_orgtype="orgtypeacad",
  imp_spat_covcode="spat_polygon",
  imp_spat_ID=apply(EMDAT[,c("Admin1.Code","Admin2.Code")],1,function(x) {
    if(all(is.na(x))) return(NA_character_)
    if(any(is.na(x))) return(x[!is.na(x)])
    paste0(c(ifelse(is.na(x[1]),"",x[1]),
             ifelse(is.na(x[2]),"",x[2])),
           collapse = ",")
  },simplify = T),
  imp_spat_srcorg="FAO-GAUL",
  imp_spat_URL="https://owncloud.unepgrid.ch/index.php/s/Rh2hDmXY84VUrPb",
  imp_spat_res=0,
  imp_spat_resunits="adminlevel",
  imp_spat_crs="EPSG:4326")
  # Admin level resolution
  EMDAT$imp_spat_res[!is.na(EMDAT$Admin1.Code)]<-1; EMDAT$imp_spat_res[!is.na(EMDAT$Admin2.Code)]<-2
  # Link to the hazard taxonomy from HIPS
  EMDAT%<>%EMDATHazards_old()
  
  if(nrow(EMDAT)==0) return(EMDAT)
  # Some GLIDE numbers don't have an associated hazard...
  ind<-!is.na(EMDAT$Glide) & nchar(EMDAT$Glide)==11
  EMDAT$Glide[ind]<-paste0(EMDAT$haz_Ab[ind],"-",EMDAT$Glide[ind])
  # Ensure column name aligns with imp_GCDB object
  colnames(EMDAT)[colnames(EMDAT)=="Glide"]<-"ext_IDs"
  EMDAT$ext_ID_dbs<-"GLIDE"
  EMDAT$ext_ID_orgs<-"Asian Disaster Reduction Center (ADRC)"
  # Generate the GCDB ID
  EMDAT$event_ID<-GetMonty_ID(EMDAT)
  # Melt the columns and apply the impact categorisation
  EMDAT%<>%ImpLabs(nomDB = "EMDAT")
  # Now get rid of the extra columns of data
  EMDAT%<>%dplyr::select(any_of(MontyJSONnames()))
  # Create an impact-specific ID
  EMDAT%<>%GetGCDB_impID()
  # Make sure to remove all NA impact estimates
  EMDAT%<>%filter(!is.na(imp_value))
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  EMDAT%>%AddEmptyColImp()
}

API_EMDAT<-function(){
  query_str = 
    'query monty {
      api_version
      public_emdat(
        cursor: {limit: -1}
        filters: {
          from: 1900,
          to: 2019,
          classif: ["nat-*"],
          include_hist: true
        }
      ) {
        total_available
        info {
          timestamp
          filters
          cursor
        }
        data {
          disno
          classif_key
          group
          subgroup
          type
          subtype
          external_ids
          name
          iso
          country
          subregion
          region
          location
          origin
          associated_types
          ofda_response
          appeal
          declaration
          aid_contribution
          magnitude
          magnitude_scale
          latitude
          longitude
          river_basin
          start_year
          start_month
          start_day
          end_year
          end_month
          end_day
          total_deaths
          no_injured
          no_affected
          no_homeless
          total_affected
          reconstr_dam
          reconstr_dam_adj
          insur_dam
          insur_dam_adj
          total_dam
          total_dam_adj
          cpi
          admin_units
          entry_date
          last_update
        }
      }
    }'
  # setup the connection with the GraphQL database
  client <- ghql::GraphqlClient$new(
    url = "https://api.emdat.be/v1",
    headers = list(Authorization = emdat_token)
  )
  # Setup the query in GraphQL language
  q <- ghql::Query$new()
  q$query('monty',query_str)
  # Make the query to EM-DAT
  jsonlite::fromJSON(client$exec(q$queries$monty))$data$public_emdat$data%>%
    CleanEMDAT_API()
}

convEMDAT_Monty<-function(taby=F, fromdate=NULL){
  # Check the input date
  if(!is.null(fromdate)) {
    # Convert to date
    fromdate%<>%as.Date(format="%Y-%m-%d")
    # Checks
    if(is.na(fromdate)) {
      warning("date provided to EM-DAT to filter the recent data is not in the correct format of %Y-%m-%d")
      fromdate<-NULL
    }
  }
  # Get the Emergency Appeal data from GO
  EMDAT<-API_EMDAT()
  # Get rid of repeated entries
  EMDAT%<>%distinct()%>%
    arrange(ev_sdate)%>%filter(!is.na(haz_spec))
  
  if(!is.null(fromdate)) EMDAT%<>%filter(as.Date(imp_moddate)>=fromdate)
  
  if(taby) return(EMDAT)
  
  # Load the Monty JSON template
  emdMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  
  # Don't max out the RAM!
  s_ncores <- ncores
  ncores <<- min(ncores,20)
  
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(EMDAT),function(i) {
      EMDAT$all_ext_IDs[[i]]%>%mutate(event_ID=EMDAT$event_ID[i],
                                      ev_name=EMDAT$ev_name[i])
    },mc.cores=ncores))%>%
      distinct(ext_ID_db, ext_ID_org,event_ID,.keep_all=T)
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    EMDAT%>%dplyr::select(event_ID, ev_ISO3s, gen_location)%>%
      distinct()
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    EMDAT%>%dplyr::select(event_ID,ev_sdate,ev_fdate)%>%
      distinct()
  )
  # Hazards
  allhaz_class<-Add_EvHazTax_Monty(
    EMDAT%>%dplyr::select(event_ID, haz_Ab, haz_spec)%>%
      distinct()
  )
  # Gather it all and store it in the template!
  emdMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
  emdMonty$event_Level$ID_linkage<-ID_linkage
  emdMonty$event_Level$temporal<-temporal
  emdMonty$event_Level$spatial<-spatial
  emdMonty$event_Level$allhaz_class<-allhaz_class
  emdMonty$event_Level$ev<-NULL
  #@@@@@ Hazard-level data @@@@@#
  # Nothing to put here as we haven't linked any hazard data yet
  emdMonty$hazard_Data<-list()
  
  #@@@@@ Impact-level data @@@@@#
  # First need to ensure that any impacts with zero impacts estimated are removed to prevent bias
  EMDAT%<>%filter(!is.na(haz_spec) | !is.na(imp_value) | imp_value>0)
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    do.call(rbind,parallel::mclapply(1:nrow(EMDAT),function(i) {
      EMDAT$all_ext_IDs[[i]]%>%mutate(event_ID=EMDAT$event_ID[i],
                                      imp_sub_ID=EMDAT$imp_sub_ID[i],
                                      haz_sub_ID=NA_character_)
    },mc.cores=ncores))%>%
      distinct(ext_ID_db, ext_ID_org,event_ID,imp_sub_ID,.keep_all=T)
  )
  # Sources for impact data
  srcy<-do.call(rbind,parallel::mclapply(unique(EMDAT$imp_sub_ID),function(ID){
    return(EMDAT[EMDAT$imp_sub_ID==ID,]%>%
             dplyr::select(imp_src_db,imp_src_URL,imp_src_org)%>%
             slice(1))
  },mc.cores=ncores))
  # impact estimates
  impact_detail<-EMDAT%>%distinct(imp_sub_ID,.keep_all = T)%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-EMDAT%>%distinct(imp_sub_ID,.keep_all = T)%>%
    dplyr::select(imp_sdate,imp_fdate,imp_credate,imp_moddate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_ISO3s,imp_spat_res
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=EMDAT%>%dplyr::select(imp_sub_ID,imp_spat_ID,imp_spat_fileloc),
    spatial_info=EMDAT%>%dplyr::select(
      imp_ISO3s,
      imp_lon,
      imp_lat,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_crs
    ),
    source=EMDAT%>%dplyr::select(
      imp_spat_srcdb,
      imp_spat_URL,
      imp_spat_srcorg
    )
  )
  
  # Gather it all and store it in the template!
  # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
  emdMonty$impact_Data<-data.frame(imp_sub_ID=unique(EMDAT$imp_sub_ID))
  emdMonty$impact_Data$ID_linkage=ID_linkage
  emdMonty$impact_Data$source=srcy
  emdMonty$impact_Data$impact_detail=impact_detail
  emdMonty$impact_Data$temporal=temporal
  emdMonty$impact_Data$spatial=spatial
  emdMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  emdMonty$response_Data<-list()
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  emdMonty$taxonomies$src_info<-readxl::read_xlsx("./Taxonomies/Monty_DataSources.xlsx")%>%distinct()
  
  #@@@@@ Checks and validation @@@@@#
  emdMonty%<>%checkMonty()
  
  dir.create("./CleanedData/MostlyImpactData/CRED/",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(emdMonty,pretty = T,auto_unbox=T,na = 'null'),
        paste0("./CleanedData/MostlyImpactData/CRED/EMDAT_",Sys.Date(),".json"))
  
  # Don't max out the RAM!
  ncores <<- s_ncores
  
  return(emdMonty)
}

GetEMDAT<-function(new_format=T){
  # EMDAT file
  # filez<-paste0("./RawData/MostlyImpactData/EMDAT/emdat_public_",haz,"_20230526.xlsx")
  filez<-paste0("./CleanedData/MostlyImpactData/EMDAT/emdat_public_2023_09_22_query_uid-tUnheR.xlsx")
  
  
  # If nothing found
  if(!file.exists(filez)) return(data.frame())
  # Extract the hazard-specific EMDAT data
  if(new_format){
    # Read in the file
    EMDAT<-openxlsx::read.xlsx(filez)
    # Clean it up and get it in the right format
    EMDAT%<>%CleanEMDAT()
  } else {
    # Read in the file
    EMDAT<-openxlsx::read.xlsx(filez,startRow = 7)
    # Clean it up and get it in the right format
    EMDAT%<>%CleanEMDAT_old()
  }
  
  # Make sure that the spatial data required actually exists
  # EMDAT%<>%PairEMDATspatial(haz=haz)
  # Form a GCDB impacts object from EMDAT data (if there is a problem, return an empty impGCDB object)
  # tryCatch(new("impGCDB",EMDAT,type="EMDAT",haz=haz),error=function(e) new("impGCDB"))
  
  return(EMDAT)
}

# Get the EMDAT data
# filez<-list.files("../../CleanedData/MostlyImpactData/EMDAT/",include.dirs = T,all.files = T,recursive = T,ignore.case = T)
# EMDAT<-do.call(rbind,lapply(filez,function(fff) {openxlsx::read.xlsx(paste0("../../CleanedData/MostlyImpactData/EMDAT/",fff),startRow = 7)}))






# PostModEMDAT<-function(colConv){
#   # hazard Types
#   colConv$haz_type[colConv$hazEM%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
#   colConv$haz_type[colConv$hazEM%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
#   colConv$haz_type[colConv$hazEM=="WF"]<-"haztypeenviron"
#   colConv$haz_type[colConv$hazEM=="EP"]<-"haztypebio"
#   colConv$haz_type[grepl("cyclone & flood",colConv$Disaster.Subtype,ignore.case = T)]<-"haztypehydromet"
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
#   openxlsx::write.xlsx(colConv,"./Taxonomies/ConvertFromDatabases/EMDAT_HIP.xlsx")
#   
#   return(colConv)
# }


















































