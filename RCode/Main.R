# Read in all the necessary libraries and GCDB scripts
source("./RCode/Setup/GetPackages.R")

# Extract the impact databases, focussing specifically on the provided hazard
GetImpacts<-function(){
  # Desinventar
  print("Getting Desinventar data")
  impies<-GetDesinventar()
  # EM-DAT
  print("Getting EM-DAT data")
  impies%<>%rbind(GetEMDAT())
  # GIDD (IDMC)
  print("Getting GIDD data")
  impies%<>%rbind(GetGIDD())
  # IFRC GO
  print("Getting IFRC-GO data")
  impies%<>%rbind(GetGO())
  # GLIDE
  print("Getting GLIDE data")
  impies%<>%rbind(GetGLIDEimps())
  # GDACS
  print("Getting GDACS data")
  impies%<>%rbind(GetGDACS_GCDB())
  # Extras
  impies$Year<-AsYear(impies$ev_sdate)
  impies$haz_Ab[impies$haz_Ab%in%c("CW","HW")]<-"ET"
  impies$ev_ISO3s<-impies$imp_ISO3s
  return(impies)
}

# Match the impacts to hazard data
MatchImpHaz<-function(impies,haz="EQ"){
  # First check GDACS
  hazzies<-MatchGDACS(impies,haz)
  # Then match by hazard
  #@@@@@### EARTHQUAKES @@@@@@@@#
  if(haz=="EQ"){
    hazzies%<>%MatchUSGS(impies=impies)
  #@@@@@@@@@@ FLOODS @@@@@@@@@@@#
  } else if(haz=="FL"){
    stop("Not ready yet... sorry!")
    hazzies%<>%MatchCloud2Street(impies=impies)
  #@@@@@ TROPICAL CYCLONES @@@@@#
  } else if(haz=="TC"){
    stop("Not ready yet... sorry!")
    hazzies%<>%MatchNOAA_TC(impies=impies)
  #@@@@@@@@@@ STORMS @@@@@@@@@@@#
  } else if(haz=="ST"){
    stop("Not ready yet... sorry!")
    hazzies%<>%MatchNOAA_ST(impies=impies)
  } else stop("Hazard code not recognised, example: 'EQ'")
  
  return(hazzies)
}

saveGCDB<-function(GCDBy){
  saveRDS(GCDBy,paste0("./CleanedData/GCDB/",GCDBy$info$event_ID))
  return(T)
}

# Taking the wrangled impact, hazard, and their associated geospatial elements,
# this function forms the GCDB objects required
FormGCDBevents<-function(impies,cores=1){
  # Parallelise it and output the check directly
  do.call(rbind,mclapply(impies$event_ID, function(id){
    # Extract geospatial hazard data
    # (can be multiple impacts, multiple hazards, multiple impact types & multiple hazard types)
    GCDBy<-tryCatch(PairImpHaz(impies=impies[impies$event_ID==id,]),error=function(e) NA)
    # Check to see if all went well, if not, return fail
    if(class(GCDBy)!="GCDB") return(data.frame(event_ID=id,checker=F))
    # Extract geospatial impact data (e.g. aggregated admin boundaries)
    GCDBy<-tryCatch(PairImpPoly(GCDBy=GCDBy,impies=impies[impies$event_ID==id,]),error=function(e) NA)
    # All-or-nothing approach: the impact polygon data should not fail!!!
    if(class(GCDBy)!="GCDB") return(data.frame(event_ID=id,checker=F))
    # Store it out! Bespoke function, returning TRUE if not problems saving out
    checker<-saveGCDB(GCDBy)
    
    return(data.frame(event_ID=id,checker=checker))
  },mc.cores=cores))

}

GO_GCDB<-function(lhaz,cores=1){
  
  do.call(rbind,lapply(lhaz,function(haz){
    # Get the impacts for the specific hazard (inc. associated hazard-impacts)
    impies<-GetImpacts(haz=haz)
    # Get the matched
    hazzies<-MatchImpHaz(impies,haz="EQ")
    # Form the GCDB event objects and save out
    checker<-FormGCDBevents(impies,cores=cores); checker$hazard=haz
    
    return(checker)
  }))
  
}

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF","HW","CW")

GatherAllImps<-function(lhaz){
  do.call(rbind,lapply(lhaz,function(haz) {
    print(haz)
    impies<-GetImpacts(haz=haz)
    impies$haz_Ab<-haz
    return(impies)
  }))
}

# impies<-GatherAllImps(lhaz)

# Which hazards do we want to extract?
# lhaz<-c("EQ") # c("EQ","FL","TC","ST")
# # Level of parallelisation (modify accordingly)
# cores<-max(c(round(parallel::detectCores()/2),1))
# print(paste0("Number of cores to be used by GCDB = ",cores))
# # Extract the data!
# checker<-GO_GCDB(lhaz,cores=cores)


cnties<-c("MNG","SRI","NPL","TLS","IDN","PAK","FJI")


# Juicy juicy:
# Map between appeal and field reports via the hazard data
# Number of associated aftershocks & preshocks per matched event

    # 1) Wrangle in the IFRC ADM files
    # 2) Match earthquakes and impacts
    # 3) Download hazard data
# 4) Translate and wrangle all key hazards from all databases
# 5) Make the spatial impact maps of ADM-0 global comparison (doesn't need the impact polygon data) USING GADM NOT IFRC ADM
# 6) Extract the impact polygons
# 7) Make the spatial impact maps country-wise ADM-1, adding all ADM0 events as a start
# 8) Make individual GCDB objects and save out
# 9) Extract all hazard data, per country, and overlay it all (how to deal with the grid mis-match?)
# 10) Per country, plot spatial impact maps over the top of the hazard data to see where the reporting bias might be...








