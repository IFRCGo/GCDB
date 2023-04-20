# Code to extract and absorb GFD event into an ODD object
# Automatically extract data from GFD database using Google Earth Engine (you will almost certainly need either Python or Java wrapper functions)
GetGFDautoAPI<-function(bbox,sdate,fdate=NULL,I0=0){ # This should have the same form as the GetUSGS function
  
  # Search for all GFD events within this window via API access to Google Earth Engine
  # Look at the following links
  # 1) Google Earth Engine specific (Python & JavaScript): 
  # https://developers.google.com/earth-engine/guides
  # 2) Global Flood Database specific (JavaScript only):
  # https://developers.google.com/earth-engine/datasets/catalog/GLOBAL_FLOOD_DB_MODIS_EVENTS_V1
  
  return()
  
}

# If the data is downloaded to a certain location, import it and set it up as an ODD file
GFDdir<-"/home/patten/Downloads/DFO_4319_From_20151205_to_20160126/"
GFDfile<-"DFO_4319_From_20151205_to_20160126.tif"

GetGFD<-function(GFDdir,GFDfile){
  # Extract the gridded data
  hazsdf<-rgdal::readGDAL(paste0(GFDdir,GFDfile))
  # Select non-zero or non-NAN values
  inds <- !is.na(hazsdf$band2) & hazsdf$band2!=0
  # Extract the country per pixel
  hazsdf@data$ISO3C<-NA_character_
  hazsdf@data$ISO3C[inds]<-coords2country(hazsdf@coords[inds,])
  # Housekeeping
  hazard_info$began%<>%as.Date(); hazard_info$ended%<>%as.Date()
  # Now we extract the important metadata
  hazard_info<-rjson::fromJSON(file = paste0(GFDdir,list.files(GFDdir,"_properties.json")))
  # Create the IDMC data frame layout
  DispData<-data.frame(iso3=unique(hazsdf@data$ISO3C),
                       gmax=hazard_info$dfo_displaced,
                       hazard="FL",
                       sdate=hazard_info$began,
                       eventid=hazard_info$id,
                       qualifier="total", # This one is fairly redundant, but will be important for IDMC data in the future
                       fdate=hazard_info$ended,
                       GDACS=NA_real_,
                       PAGER=NA_character_
  )
  warning("You should check the IDMC GIDD against the GFD data for the observed displacement estimate. Use the GetGIDD function. This matching is done for EQs in the Match_HelixGDACS function.")
  # Extra bits we need
  I0<-ExtractParams("FL")
  #@@@@@@@@@@@@@@@@@@@ Look at GetUSGS.R for more information @@@@@@@@@@@@@@@@@@@#
  lhazdat<-list(hazard_info=list(bbox=hazsdf@bbox,
                                 sdate=hazard_info$began,
                                 fdate=hazard_info$ended,
                                 NumEvents=as.numeric(hazard_info$ended - hazard_info$began),
                                 hazard="FL",
                                 I0=I0))
  # Create HAZARD object
  hazsdf<-new("HAZARD",
              obj=hazsdf,
              hazard="FL",
              dater=lhazdat$sdate,
              I0=I0,
              alertlevel=hazard_info$dfo_main_cause,
              alertscore=hazard_info$dfo_severity)
  # Add the hazard intensity data to the hazard background info
  lhazdat[[length(lhazdat)+1]]<-hazsdf
  # Create the ODD object:
  ODDy<-new("ODD",lhazSDF=lhazdat,DispData=DispData)
  # For when you don't care about the errors anymore:
  ODDy<-tryCatch(new("ODD",lhazSDF=lhazdat,DispData=DispData),error=function(e) NULL)
  
  return(ODDy)
  
}
