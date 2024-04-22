
dir.create("./RawData/MostlyHazardData/IBTrACS",F)

DownloadIBTrACS<-function(){
  # Link to the dataset (in vector format - lines)
  URL<-"https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.ALL.list.v04r00.lines.zip"
  # Where to save it out to
  outloc<-"./RawData/MostlyHazardData/IBTrACS/AllEvents_Lines.zip"
  # Download it and save it out
  download.file(URL,outloc)
  # Unzip it
  unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])
}

unravelIBTRaCS<-function(){
  # First download the latest data
  DownloadIBTrACS()
  # Now extract it
  ibtracs<-as(sf::st_read("./RawData/MostlyHazardData/IBTrACS/AllEvents_Lines/IBTrACS.ALL.list.v04r00.lines.shp"),"Spatial")
  # The variables with wind speed data
  ibwcols<-grep("_WIND",names(ibtracs),value = T)
  # Find out which wind speed data is available, per row
  ibtracs@data$WIND<-sapply(1:nrow(ibtracs@data),function(i){
    paste0(ibwcols[!is.na(ibtracs@data[i,ibwcols])],collapse = " : ")
  })
  # The variables with pressure data
  ibpcols<-grep("_PRES",names(ibtracs),value = T)
  # Find out which pressure data is available, per row
  ibtracs@data$PRES<-sapply(1:nrow(ibtracs@data),function(i){
    paste0(ibpcols[!is.na(ibtracs@data[i,ibpcols])],collapse = " : ")
  })
    
  
}