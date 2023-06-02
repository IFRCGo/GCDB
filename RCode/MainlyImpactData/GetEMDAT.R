
PairEMDATspatial<-function(EMDAT,haz="EQ"){
  
  if(sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Adm.Level)) &
     sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin1.Code)) & 
     sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin2.Code))) stop("EMDAT has irregular admin locations")
  # Get all the iso codes that we need admin data for
  isoGAUL<-unique(EMDAT$ISO[!is.na(EMDAT$Geo.Locations)])
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

CleanEMDAT<-function(EMDAT){
  
  EMDAT[EMDAT=="Source:"]<-NA
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
  EMDAT$sdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$Start.Year[i],
                                                         EMDAT$Start.Month[i],
                                                         EMDAT$Start.Day[i]),collapse = "-"),simplify = T)
  # End date in one
  EMDAT$fdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$End.Year[i],
                                                         EMDAT$End.Month[i],
                                                         EMDAT$End.Day[i]),collapse = "-"),simplify = T)
  
  EMDAT%<>%dplyr::select(-c(Start.Day,Start.Month,Start.Year,End.Day,End.Month,End.Year))
  
  return(EMDAT)
}

GetEMDAT<-function(impies,haz="EQ"){
  # Extract the hazard-specific EMDAT data
  EMDAT<-openxlsx::read.xlsx(paste0("./RawData/MostlyImpactData/EMDAT/emdat_public_",haz,"_20230526.xlsx"),startRow = 7)
  # Make sure that the spatial data required actually exists
  EMDAT%<>%PairEMDATspatial(haz=haz)
  # Form a GCDB impacts object from EMDAT data (if there is a problem, return an empty impGCDB object)
  tryCatch(new("impGCDB",EMDAT,type="EMDAT",haz=haz),error=function(e) new("impGCDB"))
}

# Get the EMDAT data
# filez<-list.files("../../CleanedData/MostlyImpactData/EMDAT/",include.dirs = T,all.files = T,recursive = T,ignore.case = T)
# EMDAT<-do.call(rbind,lapply(filez,function(fff) {openxlsx::read.xlsx(paste0("../../CleanedData/MostlyImpactData/EMDAT/",fff),startRow = 7)}))

