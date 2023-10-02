
returnX<-function(x,a=NULL,b=NULL) x
negexp <-function(x) -exp(x)
logneg <-function(x) log(-x)

AsYear<-function(date,red=F,limit=T){
  str_split(as.character(date),pattern = "-",simplify = T)[,1]%>%as.integer()%>%return()
  # date%<>%as.Date(optional=T)
  # if(!red) year<-as.numeric(format(date,"%Y"))
  # else year<-as.numeric(format(date,"%y"))
  # 
  # # if(limit&any(year>as.numeric(format(Sys.Date(),"%Y")))) 
  # #   year[year>as.numeric(format(Sys.Date(),"%Y"))]<-AsYear(Sys.Date())
  # 
  # return(year)
}
AsMonth<-function(date){
  str_split(as.character(date),pattern = "-",simplify = T)[,2]%>%as.integer()%>%return()
  # return(as.numeric(format(as.Date(date),"%m")))
}
AsDay<-function(date){
  str_split(as.character(date),pattern = "-",simplify = T)[,3]%>%as.integer()%>%return()
  # return(as.numeric(format(as.Date(date),"%d")))
}

# Used to save output files by date and time for model validation comparison in time
DateTimeString<-function(){
  return(gsub(gsub(Sys.time(),pattern = " ", replacement = "_"),pattern = ":",replacement = ""))
}

CheckBbox<-function(bbox,correct=TRUE){
  # bbox should be [min_longitude, min_latitude, max_longitude, max_latitude]
  if(abs(bbox[2])>90 | abs(bbox[4])>90 | abs(bbox[1])>180 | abs(bbox[3])>180) {
    stop("Error: non-physical bounding box values given to IIDIPUS")
  }
  
  if (bbox[1]>bbox[3]) {
    print("WARNING: bounding box range is long{-180,180}, lat{-90,90} with bbox[min_lon,min_lat,max_long,max_lat]")
    print("Found min_long>max_long")
    if(correct){
      tmp<-bbox[3]
      bbox[3]<-bbox[1]
      bbox[1]<-tmp
    }
  }
  if (bbox[2]>bbox[4]) {
    print("WARNING: bounding box range is long{-180,180}, lat{-90,90} with bbox[min_lon,min_lat,max_long,max_lat]")
    print("Found min_lat>max_lat")
    if(correct){
      tmp<-bbox[4]
      bbox[4]<-bbox[2]
      bbox[2]<-tmp
    }
  }
  
  return(bbox)
}


inbbox<-function(bbox,point){
  bbox[1]<point[1] &
  bbox[2]<point[2] &
  bbox[3]>point[1] &
  bbox[4]>point[2]
}

bbox_overlap<-function(bbox1,bbox2,buffer=0){
  bbox1%<>%as.matrix()%>%c()%>%unname(); bbox2%<>%as.matrix()%>%c()%>%unname()
  # Check if one bounding box lies inside the other
  (inbbox(bbox1,bbox2[c(1,2)]) |
  inbbox(bbox1,bbox2[c(1,4)]) |
  inbbox(bbox1,bbox2[c(3,2)]) |
  inbbox(bbox1,bbox2[c(3,4)])) |
    # Or the other way round!
    (inbbox(bbox2,bbox1[c(1,2)]) |
       inbbox(bbox2,bbox1[c(1,4)]) |
       inbbox(bbox2,bbox1[c(3,2)]) |
       inbbox(bbox2,bbox1[c(3,4)]))
}

bbox_inside<-function(bbox1,bbox2,buffer=0){
  bbox1%<>%as.matrix()%>%c()%>%unname(); bbox2%<>%as.matrix()%>%c()%>%unname()
  # Check if one bounding box lies inside the other
  (inbbox(bbox1,bbox2[c(1,2)]) &
      inbbox(bbox1,bbox2[c(1,4)]) &
      inbbox(bbox1,bbox2[c(3,2)]) &
      inbbox(bbox1,bbox2[c(3,4)])) |
    # Or the other way round!
    (inbbox(bbox2,bbox1[c(1,2)]) &
       inbbox(bbox2,bbox1[c(1,4)]) &
       inbbox(bbox2,bbox1[c(3,2)]) &
       inbbox(bbox2,bbox1[c(3,4)]))
}

ExtractBboxPoly<-function(polygon){
  c(min(polygon@coords[,1]),
    min(polygon@coords[,2]),
    max(polygon@coords[,1]),
    max(polygon@coords[,2]))
}

BboxArea<-function(ADM) {
  bbox<-c(apply(st_coordinates(st_as_sf(ADM)),2,min)[1:2],
          apply(st_coordinates(st_as_sf(ADM)),2,max)[1:2])
  geosphere::distHaversine(bbox[3:4],bbox[1:2])/1e3
}

BboxLengths<-function(ADM) {
  bbox<-c(apply(st_coordinates(st_as_sf(ADM)),2,min)[1:2],
          apply(st_coordinates(st_as_sf(ADM)),2,max)[1:2])
  abs(c(diff(bbox[c(1,3)]),diff(bbox[c(2,4)])))/2
}

DistPoly<-function(ADM){
  # Check for nothing weird... such as extra polygons popping out of nowhere
  if(length(ADM@polygons[[1]])>1) stop("SPDF has length of 1st polygon layer more than one...")
  # Go polygon by polygon
  centies<-do.call(rbind,lapply(1:length(ADM@polygons[[1]]@Polygons),function(i){
    ADM@polygons[[1]]@Polygons[[i]]@labpt
  }))
  # Now calculate the median distance of each polygon to all others
  do.call(rbind,lapply(1:length(ADM@polygons[[1]]@Polygons),function(i) 
    median(geosphere::distHaversine(ADM@polygons[[1]]@Polygons[[i]]@labpt,centies)/1e3)))
}

DistMatPoly<-function(ADM, indie=T){
  # Number of polygons to work over
  n<-length(ADM@polygons[[1]]@Polygons)
  # Find the bounding boxes
  bbies<-ExtractBBOXpoly(ADM)[,c(2:5)]
  # Bullshit because rbind requires colnames... damn this ugly!
  tmp1<-bbies[,c(1,2)]; colnames(tmp1)<-c("x","y")
  tmp2<-bbies[,c(1,4)]; colnames(tmp2)<-c("x","y")
  tmp3<-bbies[,c(3,2)]; colnames(tmp3)<-c("x","y")
  tmp4<-bbies[,c(3,4)]; colnames(tmp4)<-c("x","y")
  # Calculate the Haversine distance between the different bounding box corners 
  distmat<-geodist::geodist(rbind(tmp1,tmp2,tmp3,tmp4), 
                            measure = "haversine")/1e3; rm(tmp1,tmp2,tmp3,tmp4)
  # Calculate the distances between bounding box edges
  do.call(rbind,lapply(1:n,function(nnn){
    # Which polygon had the smallest distance to all of the individual edges?
    dissie<-apply(cbind(distmat[,nnn+n*0],
                        distmat[,nnn+n*1],
                        distmat[,nnn+n*2],
                        distmat[,nnn+n*3]),
                  1,min,na.rm=T)
    # Remove distances between bounding box with itself
    dissie[(nnn+n*(0:3))]<-NA
    # Find the corresponding closest polygon (considering that the vector length is 3*n)
    indie<-which.min(dissie)-(which.min(dissie)%/%n)*n
    
    return(data.frame(min=min(dissie,na.rm=T),
                      ind=ifelse(indie==0,n,indie)))
  }))
}

ExtractIndArea<-function(ADM){
  # Check for nothing weird... such as extra polygons popping out of nowhere
  if(length(ADM@polygons[[1]])>1) stop("SPDF has length of 1st polygon layer more than one...")
  # Go polygon by polygon
  sapply(1:length(ADM@polygons[[1]]@Polygons),function(i){
    tADM<-ADM
    tADM@polygons[[1]]@Polygons<-tADM@polygons[[1]]@Polygons[i]
    as.numeric(st_area(st_as_sf(tADM))/1e6)
  })
}

ExtractBBOXpoly<-function(ADM){
  out<-do.call(rbind,lapply(1:length(ADM@polygons[[1]]@Polygons),function(i){
    as.data.frame(t(c(i,ExtractBboxPoly(ADM@polygons[[1]]@Polygons[[i]]))))
  }))
  colnames(out)<-c("i","mnlo","mnla","mxlo","mxla")
  rownames(out)<-NULL
  return(out)
}

longDists<-function(bbox){
  suppressMessages(c(geodist::geodist(matrix(bbox[c(1,2)],nrow = 1,dimnames = list(c("z"),c("x","y"))),
                                      matrix(bbox[c(3,2)],nrow = 1,dimnames = list(c("z"),c("x","y"))),
                     measure = "haversine")/1e3,
    geodist::geodist(matrix(bbox[c(1,4)],nrow = 1,dimnames = list(c("z"),c("x","y"))),
                     matrix(bbox[c(3,4)],nrow = 1,dimnames = list(c("z"),c("x","y"))),
                     measure = "haversine")/1e3))
}

expBBOX_const<-function(bbox,factie,limmie=1000){
  # Check the value isn't outside the limit
  latval<-min(c(limmie/111,factie))
  # Latitude is easy-peasy!
  bbox[2]<-bbox[2]-latval
  bbox[4]<-bbox[4]+latval
  # For longitude values, we expand it by factie*111km, which is what the latitude uses
  distie<-longDists(bbox)
  # Cost function
  fncy<-function(li){
    # Temporary bounding box - modify directly
    btmp<-bbox; btmp[1]<-btmp[1]-li; btmp[3]<-btmp[3]+li
    # Take the difference to find the factor
    diffie<-longDists(btmp)-distie
    # put an upper limit on the difference possible
    if(any(diffie>limmie)) return(20^(1+sqrt(li)))
    # 111 km is one latitude degree, so make the longitude distance difference a function of that
    sqrt((max(diffie) - latval*111)^2)
  }
  # Optimise to find the ideal longitude shift
  lonval<-optimize(fncy,interval = c(0.01,latval*5),tol = 0.001)$minimum
  # Modify then output!
  bbox[1]<-bbox[1]-lonval/2
  bbox[3]<-bbox[3]+lonval/2
  
  return(bbox)
}

expBBOX_warp<-function(bbox,factie,limmie=1000){
  # Check the value isn't outside the limit
  latval<-min(c(limmie/111,factie))
  # Latitude is easy-peasy!
  bbox[2]<-bbox[2]-latval
  bbox[4]<-bbox[4]+latval
  # For longitude values, we expand it by factie*111km, which is what the latitude uses
  distie<-longDists(bbox)
  # Estimate distance from using latval directly
  btmp<-bbox; btmp[1]<-btmp[1]-latval; btmp[3]<-btmp[3]+latval
  # Take the difference to find the factor
  diffie<-longDists(btmp)-distie
  # put an upper limit on the difference possible
  if(!any(diffie>limmie)) return(btmp)
  # If the longitude distance with latval is more than limmie
  # Optimise over lonval until you reach limmie, increasing lenval in range (0.1,latval)
  # Cost function
  fncy<-function(li){
    # Temporary bounding box - modify directly
    btmp<-bbox; btmp[1]<-btmp[1]-li; btmp[3]<-btmp[3]+li
    # Take the difference to find the factor
    diffie<-longDists(btmp)-distie
    # put an upper limit on the difference possible
    if(any(diffie>limmie)) return(20^(1+sqrt(li)))
    # 111 km is one latitude degree, so make the longitude distance difference a function of that
    1/(1+li)
  }
  # Optimise to find the ideal longitude shift
  lonval<-optimize(fncy,interval = c(0.01,latval),tol = 0.001)$minimum
  # Modify then output!
  bbox[1]<-bbox[1]-lonval
  bbox[3]<-bbox[3]+lonval
  
  return(bbox)
}

FindBigPolys<-function(ADM,expPartin=T,reducer=T,expFact=5){
  # Area of each polygon
  areas<-ExtractIndArea(ADM)
  # Finding bounding box of all ADM polygons
  bbies<-ExtractBBOXpoly(ADM)
  # Initialise the ADM polygons to be output
  bbout<-data.frame()
  # For the biggest polygons, swallow up those that lie within or partly within
  while(nrow(bbies)>0){
    print(nrow(bbies))
    # Find the biggest element remaining
    maxxie<-which.max(areas[bbies$i])
    # Which is the biggest element of the country?
    bigBBOX<-bbies[maxxie,]
    # Expand it by a few extra long-lat values
    bigBBOX[,-1]<-expBBOX_warp(bigBBOX[,-1],factie=expFact); bbies[maxxie,]<-bigBBOX
    # Find and swallow up the polygons that lie within, or partly within and expand the bbox
    # Do until the number of polygons unmatched is zero
    remain<-T
    while (remain){
      # How many polygons lie entirely within the mainland?
      allin<-sapply(1:nrow(bbies),function(i) bbox_inside(bbies[i,-1],bigBBOX[,-1]))
      if(expPartin){
        # Find those polygons that part lie within the largest-polygon bounding box
        partin<-sapply(1:nrow(bbies),function(i) bbox_overlap(bbies[i,-1],bigBBOX[,-1]))
        # Expand the bbox to be at the edge of all the polygons that were partially contained within main-polygon bbox 
        if(sum(partin)>0)  bigBBOX[,2:5]<-
            c(bbies%>%filter(partin)%>%rbind(bigBBOX)%>%dplyr::select(mnlo,mnla)%>%apply(2,min),
              bbies%>%filter(partin)%>%rbind(bigBBOX)%>%dplyr::select(mxlo,mxla)%>%apply(2,max))
        # Add the partin polygons to filter out later
        allin<-partin
      }
      # Get rid of those that matched, or exit!
      bbies%<>%filter(!allin)
      # Finished yet?
      remain<-!all(!allin)
    }
    bbout%<>%rbind(bigBBOX)
    bbies<-bbies[bbies$i!=bigBBOX$i,]
  }
  # Neaten me up!
  row.names(bbout)<-NULL
  # For large countries, remove the polygons less than a certain size
  if(reducer) {if(max(areas)>300) bbout%<>%filter(areas[bbout$i]>10)}
  # Filter out the ADM boundary file to leave only the important polygons
  # ADM@polygons[[1]]@Polygons<-ADM@polygons[[1]]@Polygons[bbout$i]
  # ADM@polygons[[1]]@plotOrder<-ADM@polygons[[1]]@plotOrder[bbout$i]
  
  return(bbout)
}

GenerateExpBBOX<-function(isos,expPartin=T,reducer=T,expFact=5){
  # Just in case we already did it
  if(file.exists("./CleanedData/SocioPoliticalData/ExpandBBOX.xlsx")){
    bbies<-openxlsx::read.xlsx("./CleanedData/SocioPoliticalData/ExpandBBOX.xlsx")
    # Get the remaining countries to expand
    isy<-isos[!isos%in%bbies$ISO3CD]
    # if we all good, spit it out!
    if(length(isy)==0 & nrow(bbies)>0) bbies%>%filter(ISO3CD%in%isos)%>%return()
  } else bbies<-data.frame()
  # Bounding box expansion, per country
  bbies%<>%rbind(do.call(rbind,lapply(isy,function(is){
    # Get the admin boundaries for the country
    ADM<-tryCatch(GetIFRCADM(is,level=1),error=function(e) NULL)
    # Check nothing went wrong
    if(is.null(ADM)) return(data.frame())
    # Expand out the bounding boxes
    out<-FindBigPolys(ADM,expPartin,reducer,expFact)
    # Add the country element to store out
    out$ISO3CD<-is

    return(out)
  })))
  # Save it out!
  openxlsx::write.xlsx(bbies,"./CleanedData/SocioPoliticalData/ExpandBBOX.xlsx")
  
  bbies%>%filter(ISO3CD%in%isos)%>%return()
}

FullPolyOverlap<-function(ADM,poly){
  
}

CheckArgs<-function(args){
  # INDEX - TYPE - NAME - DESCRIPTION
  # 1:4 - numeric   - bbox[min lon, max lat, max lon, min lat] - region bounding box
  # 5   - character - country - ISO format only
  # 6   - character - hazard  - using IDMC definition: {"Flood","Storm","Mass movement","Wildfire","Earthquake","Extreme temperature","Volcanic eruption","Drought"}
  # 7   - date (%Y%m%d) - sdate - start date of event
  # 8   - date (%Y%m%d) - fdate - start date of event
  bbox<-as.numeric(args[1:4])
  bbox<-CheckBbox(bbox)
  
  iso3<-as.character(args[5])
  if(nchar(args[5])>3) {
    print(paste0("Warning: detected country name instead of iso3: ",args[5]))
    args[5]%<>%countrycode(origin ='country.name', destination ='iso3c')
  } else if (nchar(args[5])==2){
    print(paste0("Warning: detected iso2 instead of iso3 for country: ",args[5]))
    args[5]%<>%countrycode(origin ='iso2c', destination ='iso3c')
  } else if (nchar(args[5])<2) stop("Error in country input, try using iso3 value")
  args[5]%<>%countrycode(origin ='iso3c', destination ='iso3c')
  
  # list of possible IDMC hazards
  hazard_type<-as.character(args[6])
  hazards<-c("Flood","Storm","Mass movement","Wildfire","Earthquake","Extreme temperature","Volcanic eruption","Drought")
  if(!(hazard_type %in% hazards)) stop("Error: hazard not found among possible IDMC hazard types")
  
  sdate<-args[7]%>%as.POSIXct()%>%as.Date(format = "%Y%m%d")
  fdate<-args[8]%>%as.POSIXct()%>%as.Date(format = "%Y%m%d")
  
  if(!any(grepl("20",c(sdate,fdate)))) stop("Error: date input requires full year e.g. 2019")
  if(fdate<sdate) stop("Error: hazard end date must preceed start date")
  if(any(c(sdate,fdate)>Sys.Date()+10)) stop("Error: hazard cannot be in the future")
  if(any(c(sdate,fdate)<"2017-01-01")) stop("Error: hazard cannot be before 2017")
  year<-format(sdate,"%Y")
  return(list(bbox=bbox,iso3=iso3,hazard_type=hazard_type,sdate=sdate,fdate=fdate,year=year))
  
}

areaBbox<-function(bbox){
  s1<-cbind(lon=c(bbox[1],bbox[3],bbox[1],bbox[3]),lat=c(bbox[2],bbox[2],bbox[4],bbox[4]))
  sp1 <- spPolygons(s1, crs="+proj=longlat +datum=WGS84")
  mp1 <- makePoly(sp1, interval=100000)
  return(areaPolygon(mp1)*1e-3)
  # R<-6378.137
  # return((pi/180)*R^2 *abs(sin(bbox[2])-sin(bbox[4]))*abs(bbox[1]-bbox[3]))
}

# convDF2SPDF<-function(DF,name=NULL,crs="WGS84"){
#   
#   
#   
#   if(is.null(name)) name<-"Value"
#   
#   0
#   
#   if(crs=="WGS84") {crs(DF)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
#   } else {stop("ERROR: Unknown coordinate system in convMat2SPDF, see Functions.R")}
#   
#   return(DF)
# }
checkMatlonglat<-function(array){
  
  long<-as.numeric(rownames(array))
  lat<-as.numeric(colnames(array))
  
  if(!(any(is.na(long)) | any(is.na(lat)))) {
    colnames(array)<-lat
    rownames(array)<-long
    
    array%<>%reshape2::melt()
    
    return(array)
  }
  
  ladiff<-median(diff(lat),na.rm = T)
  lodiff<-median(diff(long),na.rm = T)
  
  # Unfortunately sometimes the GetPopDemo function returns a character in col/rownames
  # Additionally, sometimes diff(long/lat) is not unique
  # Let's fix that!
  if(is.na(lat[1])) lat[1]<-lat[2]-ladiff
  if(is.na(lat[length(lat)])) lat[length(lat)]<-lat[length(lat)-1]+ladiff 
  if(is.na(long[1])) long[1]<-long[2]-lodiff 
  if(is.na(long[length(long)])) long[length(long)]<-long[length(long)-1]+lodiff 
  
  if(any(is.na(long)) | any(is.na(lat))) stop("nan values in longitude/latitude values of array col/row names")
  
  colnames(array)<-lat
  rownames(array)<-long
  
  array%<>%reshape2::melt()
  # if(array$Var1!=long | array$Var2!=lat) {
  #   
  # }
  
  return(array)
}

convRaster2SPDF<-function(raster,name=NULL){
  
  raster%<>%as("SpatialPixelsDataFrame")
  colnames(raster@coords) <- c("Longitude","Latitude")
  rownames(raster@bbox) <- c("Longitude","Latitude")
  if(!is.null(name)) colnames(raster@data)<-name
  
  return(raster)
  
}

convRaster2SP<-function(raster,name=NULL){
  
  raster%<>%as("SpatialPointsDataFrame")
  colnames(raster@coords) <- c("Longitude","Latitude")
  rownames(raster@bbox) <- c("Longitude","Latitude")
  if(!is.null(name)) colnames(raster@data)<-name
  
  return(raster)
  
}

convMat2DF<-function(array,name=NULL){
  
  array%<>%checkMatlonglat()
  
  if(is.null(name)) name<-"Value"
  colnames(array)<-c("Longitude","Latitude",name)
  
  return(array)
  
}

convMat2raster<-function(array,name=NULL,crs="WGS84"){
  
  array%<>%checkMatlonglat()
  if(is.null(name)) name<-"Value"
  colnames(array)<-c("Longitude","Latitude",name)
  array %<>%raster
  
  if(crs=="WGS84") {crs(array)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  } else {stop("ERROR: Unknown coordinate system in convMat2SPDF, see Functions.R")}
  
  return(array)
  
}

convMat2SPDF<-function(array,name=NULL,crs="WGS84"){
  
  array%<>%checkMatlonglat()
  
  if(is.null(name)) name<-"Value"
  colnames(array)<-c("Longitude","Latitude",name)
  array <- SpatialPixelsDataFrame(points = array[c("Longitude","Latitude")],
                                  data = array[name])
  
  if(crs=="WGS84") {crs(array)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  } else {stop("ERROR: Unknown coordinate system in convMat2SPDF, see Functions.R")}
  
  ##### THIS SECTION IS TO ORDER THE VECTOR VALUES OF THE POPULATION MATRIX
  ##### OTHERWISE THE HAZARD INTERPOLATION IS SPLIT IN THE HORIZONTAL PLANE
  
  xo<-array@coords[1:array@grid@cells.dim[1],1]
  yo<-array@coords[1:array@grid@cells.dim[2]*array@grid@cells.dim[1]-array@grid@cells.dim[1]+1,2]
  
  if(!any(sort(yo)==yo)) {
    
    # find index to split data.frame
    ind<-which.min(array@coords[,2])-1L
    array@data[[name]]<-c(array@data[[name]][(ind+1):nrow(array)],array@data[[name]][1:ind])
    array@grid.index<-c(array@grid.index[(ind+1):nrow(array)],array@grid.index[1:ind])
    array@coords[,1]<-c(array@coords[(ind+1):nrow(array),1],array@coords[1:ind,1])
    array@coords[,2]<-c(array@coords[(ind+1):nrow(array),2],array@coords[1:ind,2])
    
  }
  # 
  # if(!any(sort(xo)==xo)) {
  # 
  #   # find index to split data.frame
  #   ind<-which.min(array@coords[1:array@grid@cells.dim[1],1])-1
  #   pop<-xco<-yco<-gind<-array(NA,nrow(array))
  #   for(i in 1:array@grid@cells.dim[2]){
  #     pop[(1:array@grid@cells.dim[1])*i]<-c(array@data[[name]][((ind+1):array@grid@cells.dim[1])*i],array@data[[name]][(1:ind)*i])
  #     gind[(1:array@grid@cells.dim[1])*i]<-c(array@grid.index[((ind+1):array@grid@cells.dim[1])*i],array@grid.index[(1:ind)*i])
  #     xco[(1:array@grid@cells.dim[1])*i]<-c(array@coords[((ind+1):array@grid@cells.dim[1])*i,1],array@coords[(1:ind)*i,1])
  #     yco[(1:array@grid@cells.dim[1])*i]<-c(array@coords[((ind+1):array@grid@cells.dim[1])*i,2],array@coords[(1:ind)*i,2])
  #   }
  #   array@data[[name]]<-pop
  #   array@grid.index<-gind
  #   array@coords[,1]<-xco
  #   array@coords[,2]<-yco
  # 
  # }
  
  return(array)
  
}

# Function to extract the coordinates of all the polygons
extractPolyCoords<-function(ADM){
  coords<-data.frame()
  for(i in 1:length(ADM@polygons)){
    for(j in 1:length(ADM@polygons[[i]]@Polygons)){
      coords%<>%rbind(data.frame(LONGITUDE=ADM@polygons[[1]]@Polygons[[1]]@coords[,1],
                                 LATITUDE=ADM@polygons[[1]]@Polygons[[1]]@coords[,2],
                                 i=i,j=j))
    }
  }
  return(coords)
}
# Find which coordinates of an S4 spatial object lie inside (or on the boundary of) a spatial polygon file
inPoly<-function(poly,pop,iii=1,sumFn="sum",reducer=NULL){
  
  Ifin<-rep(F,nrow(pop))
  if(is.null(reducer)) reducer<-!Ifin
  
  pop<-pop[reducer,]
  
  if(any(class(pop)=="SpatialPointsDataFrame") | any(class(pop)=="SpatialPixelsDataFrame")){
    coords<-pop@coords
    data<-pop@data
  } else {
    coords<-as.data.frame(pop[,c("Longitude","Latitude")])
    data<-as.data.frame(pop)
  }
  
  insidepoly<-rep(FALSE,nrow(pop))
  
  for (i in 1:length(poly@Polygons)){
    # Get rid of values outside the bounding box first
    minipoly<-rep(FALSE,length(insidepoly))
    indies<-coords[,1]>=min(poly@Polygons[[i]]@coords[,1]) &
      coords[,1]<=max(poly@Polygons[[i]]@coords[,1]) &
      coords[,2]>=min(poly@Polygons[[i]]@coords[,2]) &
      coords[,2]<=max(poly@Polygons[[i]]@coords[,2])
    # Now we only need to calculate a few points that lie inside the polygon!
    minipoly[indies]<-sp::point.in.polygon(coords[indies,1],
                                           coords[indies,2],
                                           poly@Polygons[[i]]@coords[,1],
                                           poly@Polygons[[i]]@coords[,2])>0
    # Add to the total
    insidepoly<- insidepoly | minipoly
  }
  
  outer<-match.fun(sumFn)(data[insidepoly,iii],na.rm=T)
  
  Ifin[reducer]<-insidepoly
  
  return(list(vals=outer,indies=Ifin))
}

pval <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Assumes 9 income distribution percentiles
SplitSamplePop<-function(Pop,n=1){
  k<-length(Pop)
  return(array(vapply(Pop,function(tPop) rmultinom(n=n,
                                                   size=(tPop + rbinom(n=1,p=tPop%%1,size=1)), #LOOSEEND: same size for all Np
                                                   prob=rep(1/8,8)),FUN.VALUE = numeric(8L*n)),dim = c(8,k*n)))
}

rgammaM<-function(n,mu,sig_percent){
  # rgamma(n shape = alpha, scale = theta)
  # Note that the sig_percent is the constant coefficient of variation
  # Therefore, it is like a percentage of the mean
  ssq<-sig_percent*sig_percent
  rgamma(n,shape=1./ssq,scale=mu*ssq)
}
dgammaM<-function(x,mu,sig_percent,log=T){
  # rgamma(n shape = alpha, scale = theta)
  # Note that the sig_percent is the constant coefficient of variation
  # Therefore, it is like a percentage of the mean
  ssq<-sig_percent*sig_percent
  dgamma(x,shape=1./ssq,scale=mu*ssq,log=log)
}

extractnumbers<-function(str){
  return(as.numeric(unlist(regmatches(str,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",str, perl=TRUE)))))
}

expandBbox<-function(bbox,f,scaling=T){
  Dx<-bbox[3]-bbox[1]
  Dy<-bbox[4]-bbox[2]
  A<-Dx*Dy
  if(scaling) dx<-Dx*(-1+sqrt(f))
  else dx<-Dx*(-1+sqrt(1+(f-A)/A)) # use f as as the resulting area
  dy<-dx*Dy/Dx
  return(bbox+0.5*c(-dx,-dy,dx,dy))
}

convIso2Iso3<-function(iso2){
  countrycode::countrycode(sourcevar = iso2,
                           origin = "iso2c",
                           destination = "iso3c",warn = F)
}

convIso3Country<-function(iso3){
  countrycode::countrycode(sourcevar = iso3,
                           origin = "iso3c",
                           destination = "country.name",warn = F)
}

convIso3Continent<-function(iso3){
  # Access the file that contains all the relevant continent taxonomies
  filer<-"./Taxonomies/IsoContinentRegion.xlsx"
  if(!file.exists(filer)){
    url<-"https://data.unicef.org/wp-content/uploads/2018/05/JME_Regional-Classifications.xlsx"
    download.file(url,filer)
  }
  # continents<-countrycode::countrycode(sourcevar = iso3,
  #                                      origin = "iso3c",
  #                                      destination = "continent",warn = F)
  left_join(data.frame(ISO3=iso3),readxl::read_xlsx(filer)%>%transmute(ISO3=`ISO Code`,continent=`UN Region`),by="ISO3")$continent
}

convIso3Continent_alt<-function(iso3){
  # continents<-countrycode::countrycode(sourcevar = iso3,
  #                                      origin = "iso3c",
  #                                      destination = "continent",warn = F)
  left_join(data.frame(ISO3=iso3),raster::ccodes()[,c("ISO3","continent")],by="ISO3")$continent
}

convCountryIso3<-function(iso3){
  countrycode::countrycode(sourcevar = iso3,
                           origin = "country.name",
                           destination = "iso3c",warn = F)
}

# GetISObbox<-function(ISO3C){
#   filez<-"./CleanedData/SocioPoliticalData/ISO_BBOX.json"
#   if(!file.exists(filez)) write(rjson::toJSON(rjson::fromJSON(file = "https://gist.github.com/botzill/fc2a1581873200739f6dc5c1daf85a7d/raw/002372a57a40f299a463122c039faf9f927b13fe/countries_bbox.json")),filez)
#   bboxs<-rjson::fromJSON(file=filez)
#   
#   out<-do.call(rbind,lapply(1:length(ISO3C),function(is){
#     tmp<-tryCatch(unlist(bboxs[[is]])[c(2,1,4,3)],error=function(e) NA)
#     if(any(is.na(tmp))) return(data.frame(mnlo=NA,mnla=NA,mxlo=NA,mxla=NA))
#     names(tmp)<-c("mnlo","mnla","mxlo","mxla")
#     return(t(as.data.frame(tmp)))
#   }))
#   row.names(out)<-NULL
#   return(out)
# }

InterpDay<-function(ndata,day){
  val<-data.frame()
  for (iso3c in unique(ndata$iso3)){
    nd<-filter(ndata,iso3==iso3c)
    if(all(is.na(nd$value))|length(nd$value)<=1) {
      print(paste0("Not enough data found for country ",iso3c," for normalisation spline for country indicators"))
      val%<>%rbind(data.frame(iso3=iso3c,value=NA))
      next
    }
    func = tryCatch(splinefun(x=nd$day,y=nd$value),error = function(e) NULL)
    if(is.null(func)) {
      print(paste0("No spline function possible for country ",iso3c," values: ",nd$value))
      value<-nd$value[which.min(abs(nd$day-day))]
      val%<>%rbind(data.frame(iso3=iso3c,value=value))
    } else val%<>%rbind(data.frame(iso3=iso3c,value=func(day)))
  }
  return(val)
}

library(rworldmap)
library(rworldxtra)
library(sp)
coords2country = function(points,iso=T)
{  
  if(dim(points)[2]!=2) stop("coords2country Error: long/lat coords are invalid")
  if(dim(points)[1]==1) points=rbind(points,points)
  
  countriesSP <- rworldmap::getMap(resolution='high')
  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)
  
  # return the ISO3 of country
  if(iso) return(as.character(indices$ISO3))
  # return the ADMIN names of country
  return(as.character(indices$ADMIN))
}

countriesbbox<-function(iso3){
  
  countriesSP <- rworldmap::getMap(resolution='low')
  indies<-which(countriesSP$ISO3%in%iso3)
  
  mnlo<-mxlo<-mnla<-mxla<-c()
  for(c in 1:length(indies)){
    indy<-indies[c]
    for (i in 1:length(countriesSP@polygons[[indy]]@Polygons)){
      mnlo<-min(c(mnlo,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,1]))
      mxlo<-max(c(mxlo,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,1])) 
      mnla<-min(c(mnla,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,2]))
      mxla<-max(c(mxla,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,2]))
    }
  }
  
  return(c(mnlo,mnla,mxlo,mxla))
  
}

PlotDisaster<-function(pop,dfpoly,bbox=NULL,map=FALSE,ncity=1,namer="Disaster",filer="./"){
  
  if(is.null(bbox)) bbox<-as.numeric(c(min(rownames(pop)),min(colnames(pop)),max(rownames(pop)),max(colnames(pop))))
  longData<-reshape2::melt(pop)
  longData<-longData[longData$value!=0,]
  
  cities<-maps::world.cities%>%filter(lat>bbox[2]&lat<bbox[4]&long>bbox[1]&long<bbox[3])%>%arrange(desc(pop))
  if(ncity>1){wordcloud::wordcloud(words=cities$name,freq = cities$pop,max.words = 30,scale = c(2.5,0.2))}
  cities<-slice(cities,1:ncity)
  
  p<-ggplot(longData, aes(x = Var1, y = Var2)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="gray80", high="black") +
    labs(x="Longitude", y="Latitude", title=namer) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  for (j in unique(dfpoly$ncontour)){
    tp<-filter(dfpoly,ncontour==j)
    p<-p+geom_polygon(data = tp,aes(x=Longitude,y=Latitude,group=Intensity,colour=Intensity),alpha=0,na.rm = T,size=2)+
      scale_color_gradient(low="mistyrose2", high="red")
  }
  p<-p+geom_label(data = cities, aes(long, lat, label = name), size = 4, fontface = "bold", nudge_x = 0.05*(bbox[3]-bbox[1]))
  
  print(p)
  if(!is.null(filer)) ggsave(paste0(namer,".eps"), plot=p,path = filer,width = 9,height = 7.)
  return(p)
}

# GetMapObj<-function(bbox,world=NULL){
#   
#   if(is.null(world)){
#     library("rnaturalearth")
#     library("rnaturalearthdata")
#     world <- ne_countries(scale = "medium", returnclass = "sf")
#   }
#   
#   aj<-c(abs(bbox[1]-bbox[3])*0.05,abs(bbox[4]-bbox[2])*0.05)
#   p<- ggplot(data = world) + geom_sf(fill= "antiquewhite") + 
#     coord_sf(xlim = c(bbox[1]-aj[1],bbox[3]+aj[1]), ylim = c(bbox[2]-aj[2],bbox[4]+aj[2]), expand = FALSE) + 
#     xlab("Longitude") + ylab("Latitude") +
#     theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
#   
#   return(p)
# }

GetWeightedRoR<-function(x,y){
  
  len<-length(y)
  w<-rep(1,len)
  w[len]<-len
  
  fit<-lm(log(y) ~ x,weights = w)
  sumz<-summary(fit)
  # Extract gradient, intercept and p-value of gradient
  return(c(sumz$coefficients[2, 1],exp(sumz$coefficients[1, 1]),sumz$coefficients[2, 4]))
}

ggmap_bbox <- function(map,bbox) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  # map_bbox <- setNames(unlist(attr(map, "bb")), 
  # c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  # bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox["ymin"]
  attr(map, "bb")$ll.lon <- bbox["xmin"]
  attr(map, "bb")$ur.lat <- bbox["ymax"]
  attr(map, "bb")$ur.lon <- bbox["xmax"]
  map
}

GetExtent<-function(ADM,expander=NULL){
  bbox<-ADM@bbox
  # Expand the bounding box, useful for the interpolation
  if(!is.null(expander)) bbox%<>%expandBbox(1.1)
  ext<-as(extent(bbox[c(1,3,2,4)]), 'SpatialPolygons')
  crs(ext) <- "+proj=longlat +datum=WGS84 +no_defs"  
  return(ext)
}

# Remove unnecessary columns that arise with the R st_multipolygon format
ExtractPolyCoords<-function(polys){
  coords<-st_coordinates(polys)
  ind<-c()
  for (j in 3:ncol(coords)){
    if(max(coords[,j])==1) ind<-c(ind,j)
  }
  if(!is.null(ind)) coords<-coords[,-ind]
  if(ncol(coords)==2) {
    coords<-cbind(coords,rep(1,dim(coords)[1]))
  } else if(ncol(coords)>3) {
    
    # filter per L3 column by L2 value. Add a value per unique value in L2
    coords%<>%as.data.frame()
    L1<-coords%>%filter(L1==unique(coords$L1)[1])
    L2<-max(L1$L2)
    for (j in unique(coords$L1)[-1]){
      L1<-coords%>%filter(L1==j)
      coords$L2[coords$L1==j]<-L1$L2+L2-min(L1$L2)+1
      L2<-max(coords$L2)
    }
    coords<-coords[,-3]
    coords%<>%arrange(L2)%>%as.matrix()
    
  } else if(ncol(coords)>5) {
    stop("Error in extracting polygon coordinates, see ExtractPolyCoords in GetGDACS.R")
  }
  
  return(coords)
  
}

ImpactAggADM0<-function(impies, haz="EQ"){
  # Extract Global ADM
  ADM <- rworldmap::getMap(resolution='high')
  ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
  
  impies%<>%filter(!(is.na(imp_det) | is.na(imp_type))) 
  
  impies$impact<-sapply(1:nrow(impies),function(i) paste0(impies$imp_subcats[i],"-",impies$imp_type[i]),simplify = T)
  
  ADM@data$N<-sapply(ADM@data$ISO3, function(is){
    length(unique(impies$GCDB_ID[impies$ISO3==is]))
  },simplify = T)
  
  for(imp in unique(impies$impact)){
    # Aggregated per country
    ADM@data$tmp<-sapply(ADM@data$ISO3, function(is){
      sum(impies$imp_value[impies$ISO3==is & impies$impact==imp])
    },simplify = T)
    # Remove all zero counts
    ADM@data$tmp[ADM@data$tmp==0]<-NA
    # Set the column name
    colnames(ADM@data)[ncol(ADM@data)]<-imp
  }
  
  return(ADM)
}

PlotImpAgg<-function(ADM,impact="imp_typepopcnt-imptypdeat",loggie=T,bks=NULL,lbs=NULL,guidie="colourbar"){
  # Filter only the data we need
  ADM@data$tmp<-ADM@data[,impact]
  # Exception if we're interested in plotting only the number of impact records
  if(impact=="N"){
    # Keep as is
    labeller<-"No. Events"
  } else {
    # Extract the correct label for the legend
    taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
    # 
    labeller<-paste0(taxies%>%filter(list_name=="imp_subcats" &
                                       name==str_split(impact,"-",simplify = T)[1])%>%
                       pull(label)," ",
                     taxies%>%filter(list_name=="imp_type" &
                                       name==str_split(impact,"-",simplify = T)[2])%>%
                       pull(label))  
  }
  # Plot it out!
  q<-ggplot()+geom_sf(data=st_as_sf(ADM),aes(fill=tmp))
  # Specific the fill style of the plot
  if(loggie){
    q+scale_fill_gradient(name = labeller, trans = "log", guide = guidie,
                          breaks=bks,labels=lbs)
  } else {
    q
  }
  
}

# # 2D DENSITY PLOT WITH MODIFIED COLOUR AXIS
# ggplot(as.data.frame(buildings),aes(Longitude,Latitude))+
#   stat_density_2d(aes(fill = ..level..),breaks=c(0,0.5,1,3,5,10,30,50,100,300), 
#                   geom = "polygon")
# p1<-ggplot(as.data.frame(BDy),aes(Longitude,Latitude))+
#   stat_density_2d_filled(aes(fill=..level..),breaks=c(0,0.5,1,3,5,10,30,50,100,300)) +
#   ggtitle("Building Damage Data, EQ2015-04-25NPL")+theme(plot.title = element_text(hjust = 0.5))
# p2<-ggplot(as.data.frame(buildings),aes(Longitude,Latitude))+
#   stat_density_2d_filled(aes(fill=..level..),breaks=c(0,0.5,1,3,5,10,30,50,100,300)) +
#   ggtitle("OSM Building Data")+theme(plot.title = element_text(hjust = 0.5))
# gridExtra::grid.arrange(p1, p2, ncol=2)
# 
# tmp<-data.frame(PopDens=c(buildings$Pdensity,BDy$Population),ID=c(rep("OSM",nrow(buildings)),rep("Damaged",nrow(BDy))))
# ggplot(tmp,aes(x=PopDens,group=ID))+ stat_ecdf(aes(colour=ID),geom = "step",size=2) + scale_x_log10() + xlab("Population Density") +
#   ylab("Cumulative Distribution Function") + ggtitle("Nepal 2015 Earthquake") +theme(plot.title = element_text(hjust = 0.5))



# ggplot(tmp,aes(OSM,Damaged))+
#   stat_density_2d_filled(aes(fill=..level..),breaks=c(0,10,30,50,100,300,500,10000), geom="tile") +
#   ggtitle("Population Density of Damaged vs OSM Data")+theme(plot.title = element_text(hjust = 0.5))