
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

IBTrACS<-as(sf::st_read("./RawData/MostlyHazardData/IBTrACS/AllEvents_Lines/IBTrACS.ALL.list.v04r00.lines.shp"),"Spatial")
