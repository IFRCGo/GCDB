# Read in all the necessary libraries and GCDB scripts
source("./RCode/Setup/GetPackages.R")

# Code to extract the data from the Monty API
GetCurrMonty<-function(){
  # link to Monty
  url<-paste0("https://monty-api.ifrc.org/data/JSON?Mtoken=",monty_token,"&sdate=",as.character(Sys.Date()-1))
  # Download from the API
  jsonlite::fromJSON(url)
}

# Daily Monty - datasets that are updated at best once a day
DailyMontyEDL<-function(){
  # First extract the current Monty data
  Monty<-tryCatch(GetCurrMonty(),error=function(e) stop("problems extracting the current Monty data... API issues"))
  # Extract the lengths of the current data to save time later
  lennies<-c(nrow(Monty$event_Level$temporal),
             ifelse(is.null(nrow(Monty$impact_Data$temporal)),0,nrow(Monty$impact_Data$temporal)),
             ifelse(is.null(nrow(Monty$hazard_Data$temporal)),0,nrow(Monty$hazard_Data$temporal)))
  # From today!
  fromdate<-Sys.Date()-1
  # IFRC DREF-EA data
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGOApp_Monty(fromdate=fromdate)))),
                  error=function(e) {print("Error extracting IFRC DREF+EA (appeal) data"); return(Monty)})
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
  
  # Save time if no data is available
  finlen<-c(nrow(Monty$event_Level$temporal),
             ifelse(is.null(nrow(Monty$impact_Data$temporal)),0,nrow(Monty$impact_Data$temporal)),
             ifelse(is.null(nrow(Monty$hazard_Data$temporal)),0,nrow(Monty$hazard_Data$temporal)))
  # If the lengths are all the same, then no new data was loaded. 
  if(all(lennies-finlen==0)) return(NULL)
  
  # Write out in JSON format
  write(jsonlite::toJSON(Monty,pretty = T,auto_unbox=T,na = 'null'),
        paste0(monty_API_data,"/API/data/Monty.json"))
  # Also in a easier-to-load RData file
  saveRDS(Monty,paste0(monty_API_data,"/API/data/MontyAPI.RData"))
  # Finally, convert to tabular format and save out
  stop("output to tabular format here then save out")
  
  return(NULL)
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













