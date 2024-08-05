# Read in all the necessary libraries and GCDB scripts
source("./RCode/Setup/GetPackages.R")

# Daily Monty
DailyMontyEDL<-function(){
  # IFRC DREF-EA data
  Monty<-checkMonty(convGOApp_Monty())
  # EM-DAT
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convEMDAT_Monty()))),
                  error=function(e) {print("Error extracting CRED - EM-DAT data"); return(Monty)})
  # IDMC GIDD
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGIDD_Monty()))),
                  error=function(e) {print("Error extracting IDMC - GIDD data"); return(Monty)})
  # IDMC IDU
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convIDU_Monty()))),
                  error=function(e) {print("Error extracting IDMC - IDU data"); return(Monty)})
  # GLIDE
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGLIDE_Monty()))),
                  error=function(e) {print("Error extracting ADRC - GLIDE data"); return(Monty)})
  # Desinventar
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convDessie_Monty()))),
                  error=function(e) {print("Error extracting UNDRR - Desinventar data"); return(Monty)})
  # DFO
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convDFO_Monty()))),
                  error=function(e) {print("Error extracting University Columbia - DFO data"); return(Monty)})
  
  return(Monty)
}

# 10-minute Monty
MinuteMontyEDL<-function(){
  # GDACS
  Monty<-tryCatch(MergeMonty(list(Monty,checkMonty(convGDACS_Monty()))),
                  error=function(e) {print("Error extracting EC-JRC - GDACS data"); return(Monty)})
  # PDC Forecasts
  
  # ADAM Forecasts
  
  # USGS Atlas Shakemaps
  
  
  return(Monty)
}

# Difference Monty
DiffRealTimeMonty<-function(Monty){
  # Get the Monty data to compare to
  cMonty<-GetCurrMonty()
  # Calculate the differences between the two, starting with the events database
  
  
}













