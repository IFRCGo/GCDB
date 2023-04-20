library(ggplot2)
library(sf)
library(tidyverse)
library(sp)
library(magrittr)
library(dplyr)

ExtractParams<-function(haz="EQ"){
  if(haz=="EQ") return(list(I0=4.5,minmag=5))
  stop("Hazard type not recognised")
}

checkHAZARD<-function(object){

  if(!object@hazard%in%c("EQ","TC","FL")) stop("HAZARD object must be either TC, FL or EQ hazards")
  if(!object@alertlevel%in%c("red","orange","yellow","green")) stop("HAZARD object must have either red, orange, yellow or green alertlevels")
    if(!(is.na(object@alertscore) | (object@alertscore>=0 & object@alertscore<10))) stop("HAZARD object must have 0<=alertscores<10")
  if(!(object@eventdate>=as.Date("2008-01-01") & object@eventdate<=Sys.Date())) stop("HAZARD object must have dates between now and 2008")
  
  TRUE
}

setClass("HAZARD", 
         slots = c(hazard="character",
                   alertscore="numeric",
                   alertlevel="character",
                   I0="numeric",
                   eventdate="Date"),
         contains = "SpatialPixelsDataFrame")

setMethod(f="initialize", signature="HAZARD",
          # definition=function(.Object,bbox,hazSDF,dater=NULL,dir=directory,
          definition=function(.Object,obj=NULL,hazard=NULL,dater=NULL,I0=NULL,alertscore=NULL,alertlevel=NULL) {
            
            if(is.null(hazard)) {
              print("WARNING: no hazard type provided in HAZARD object initialisation, returning empty")
              return(.Object)
            }
            
            .Object@hazard<-hazard
            
            .Object@alertscore<-ifelse(is.null(alertscore),0,alertscore)
            .Object@alertlevel<-ifelse(is.null(alertlevel),"green",alertlevel)
            .Object@I0<-        ifelse(is.null(I0),ExtractParams(hazard),I0)
            if(!is.null(dater)) .Object@eventdate<-dater
            
            if(!is.null(obj)){
              # find bbox of entries within I>I0 polygon and crop object
              inI0<-obj@coords[obj@data$mmi_mean>.Object@I0,]
              # Take a bounding box a little larger than the I>I0 object but inside original (the 5 is obv. arbitrary)
              bbox<-c(max(obj@bbox[1],min(inI0[,1])-5*obj@grid@cellsize[1]),
                      max(obj@bbox[2],min(inI0[,2])-5*obj@grid@cellsize[2]),
                      min(obj@bbox[3],max(inI0[,1])+5*obj@grid@cellsize[1]),
                      min(obj@bbox[4],max(inI0[,2])+5*obj@grid@cellsize[2]))
              # Crop that barnet!
              e <- as(raster::extent(c(bbox[c(1,3,2,4)])), 'SpatialPolygons')
              proj4string(e) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
              obj%<>%raster::crop(e)
              obj %<>% SpatialPixelsDataFrame(obj@data) #raster::crop() now seems to be returning a spatial points data frame
              colnames(obj@coords) <- c('Longitude', 'Latitude')
              # Allocate the spatial data from a SpatialPixelsDataFrame object
              .Object@data <- obj@data
              .Object@coords.nrs <-obj@coords.nrs
              .Object@grid <-obj@grid
              .Object@grid.index <-obj@grid.index
              .Object@coords <-obj@coords
              .Object@bbox <-obj@bbox
            }
            .Object@proj4string <-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
            
            names(.Object)<-c("mean","sd")
            
            print("Checking HAZARD values")
            checkHAZARD(.Object)
            
            return(.Object)
          }
)

# Convert from ISO3C to country ("GBR"="United Kingdom")
setGeneric("transIso", function(ODD) 
  standardGeneric("transIso") )
setMethod("transIso", "ODD", function(ODD)
  return(countrycode::countrycode(sourcevar = ODD@iso3,
                                  origin = "iso3c",
                                  destination = "country.name")))