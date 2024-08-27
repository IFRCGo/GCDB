# Read in all the necessary libraries and GCDB scripts
source("./RCode/Setup/GetPackages.R")

# Daily Monty
DailyMontyEDL<-function(){
  # From today!
  fromdate<-Sys.Date()-1
  # IFRC DREF-EA data
  Monty<-checkMonty(convGOApp_Monty(fromdate=fromdate))
  # EM-DAT
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convEMDAT_Monty(fromdate=fromdate)))),
                  error=function(e) {print("Error extracting CRED - EM-DAT data"); return(Monty)})
  
  # # IDMC GIDD
  # Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGIDD_Monty()))),
  #                 error=function(e) {print("Error extracting IDMC - GIDD data"); return(Monty)})
  # # IDMC IDU
  # Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convIDU_Monty()))),
  #                 error=function(e) {print("Error extracting IDMC - IDU data"); return(Monty)})
  # # GLIDE
  # Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGLIDE_Monty()))),
  #                 error=function(e) {print("Error extracting ADRC - GLIDE data"); return(Monty)})
  # # Desinventar
  # Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convDessie_Monty()))),
  #                 error=function(e) {print("Error extracting UNDRR - Desinventar data"); return(Monty)})
  # # DFO
  # Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convDFO_Monty()))),
  #                 error=function(e) {print("Error extracting University Columbia - DFO data"); return(Monty)})
  
  # If there is no data, there is nothing to return!
  if(nrow(Monty$event_Level$temporal)==0) return(NULL)
  # Merge with the current API data
  tryCatch(MergeMonty(list(Monty,GetCurrMonty())),
                  error=function(e) {stop("issues combining the Monty data with the recently updated data"); return(Monty)})
}

# 10-minute Monty
MinuteMontyEDL<-function(){
  
  stop("Nothing yet, to be done in the future!")
  
  
  # GDACS
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGDACS_Monty()))),
                  error=function(e) {print("Error extracting EC-JRC - GDACS data"); return(Monty)})
  # PDC Forecasts
  
  # ADAM Forecasts
  
  # USGS Atlas Shakemaps
  
  
  return(Monty)
}

# Code to extract the data from the Monty API
GetCurrMonty<-function(){
  # link to Monty
  url<-paste0("https://monty-api.ifrc.org/data/JSON?Mtoken=",monty_token,"&sdate=",as.character(Sys.Date()-1))
  # Download from the API
  jsonlite::fromJSON(url)
}














