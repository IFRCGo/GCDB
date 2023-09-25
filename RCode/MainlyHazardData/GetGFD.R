# 
ExtractGFDmeta<-function(metaGFD){
  # Extract the full list
  metties<-metaGFD$getInfo()
  # Now split by event and extract the important information
  out<-do.call(rbind,lapply(1:length(metties$features),function(i){
    minimet<-metties$features[[i]]
    as.data.frame(t(as.data.frame(unlist(minimet$properties[names(minimet$properties)[!names(minimet$properties)%in%c("countries","otsu_sample_res","system:footprint")]]))))
  }))
  row.names(out)<-NULL
  
  out%<>%mutate(hazsub_ID=`system:index`,haz_Ab="FL",haz_type="haz_typehydromet",haz_cluster="hazhmflood",
                haz_spec="",
                dfo_main_cause=str_to_lower(dfo_main_cause),
                ev_sdate=as.Date(str_split(str_split(hazsub_ID,"From_",simplify = T)[,2],"_to_",simplify = T)[,1],format = "%Y%m%d"),
                ev_fdate=as.Date(str_split(str_split(hazsub_ID,"From_",simplify = T)[,2],"_to_",simplify = T)[,2],format = "%Y%m%d"))
  
  
  
  # Dam-related incidents
  out$haz_type[grepl("dam",out$dfo_main_cause) & !grepl("damrey",out$dfo_main_cause)]<-"haz_typehydromet,haz_typetech"
  out$haz_cluster[grepl("dam",out$dfo_main_cause) & !grepl("damrey",out$dfo_main_cause)]<-"hazhmflood,haztechstrfail,haztechflood"
  # Now export only the variables we want
  out%>%transmute(GLIDE=glide_index,
                  ev_sdate=ev_sdate,ev_fdate=ev_fdate,
                  ev_name_en=paste0(dfo_main_cause," in ",cc,", ",AsYear(ev_sdate)))
}
# Code to extract and absorb GFD event into an ODD object
# Automatically extract data from GFD database using Google Earth Engine (you will almost certainly need either Python or Java wrapper functions)
GetGFDautoAPI<-function(bbox,sdate,fdate=NULL,I0=0){ # This should have the same form as the GetUSGS function
  # Setup Google Earth Engine library and objects
  SetupGEE()
  # Extract the metadata
  metaGFD<-ee$ImageCollection("GLOBAL_FLOOD_DB/MODIS_EVENTS/V1")
  # Let's have a look
  # ee_print(metaGFD)
  # Extract the event names
  metties<-ExtractGFDmeta(metaGFD)
  # Get the continents
  metties$continent<-sapply(1:nrow(metties),function(i){
    isos<-c(str_remove_all(str_split(metties$cc[i],",",simplify = T)," "))
    conties<-sort(unique(convIso3Continent(isos))) #; conties[conties!="Not Classified"]
    if(length(conties[!is.na(conties) & conties!="Not Classified"]>1)) conties<-conties[!is.na(conties) & conties!="Not Classified"]
    return(paste0(conties,collapse = ","))
  })
  
  eventNames<-metaGFD$toBands()$bandNames()$getInfo()
  
  
  
  
  metaGFD$filter()$select('flooded')$toBands()%>%
  # metaGFD$toBands()%>%
    ee_as_raster(via = "drive",quiet = T)
  
  
  dataset<-ee$ImageCollection("GLOBAL_FLOOD_DB/MODIS_EVENTS/V1")%>%
    ee_get(1:10)
  
  eventNames<-metaGFD$toBands()$bandNames()$getInfo()
  metaGFD$toBands()
  tmp<-ee_as_raster(metaGFD$toBands(),via = "drive",quiet = T)
  
  
  GDF<-metaGFD$select(c("flooded","duration","clear_views","clear_perc","jrc_perm_water"))
  
  GFD<-ee_as_raster(ee$ImageCollection(metaGDF))
  
  bandNames <- GFD$bandNames()
  cat("Band names: ",paste(bandNames$getInfo(),collapse=","))
  
  

  
  GFD<-ee_as_raster(GDF,via = "drive",quiet = T)
  
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
