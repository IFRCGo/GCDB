# Names of the output of the GLIDEcols that we want to keep
GLIDEcols<-c("comments","year","docid","latitude","homeless","source","idsource","killed","affected","duration","number","injured","month","geocode","location","magnitude","time","id","event","day","status","longitude")
# Skeleton for empty output
glide_skel<-data.frame(matrix(NA_character_,nrow = 1,ncol = 22)); colnames(glide_skel)<-GLIDEcols

GetGLIDEnum<-function(DF,numonly=T){
  # Needs to contain the columns: ev_sdate, ISO3 & haz
  # Make sure dates are not in character format
  DF$ev_sdate%<>%as.Date()
  # Abbreviated hazard taxonomy
  hazAb<-DF$hazAb; 
  # Fetch the different GLIDE numbers, individually
  glides<-do.call(rbind,lapply(1:nrow(DF),function(i){
    baseurl<-"https://www.glidenumber.net/glide/jsonglideset.jsp?level1="
    URL<-paste0(baseurl,DF$ISO3[i],
           "&fromyear=",AsYear(DF$ev_sdate[i]-5),
           "&frommonth=",AsMonth(DF$ev_sdate[i]-5),
           "&fromday=",AsDay(DF$ev_sdate[i]-5),
           "&toyear=",AsYear(DF$ev_sdate[i]+5),
           "&tomonth=",AsMonth(DF$ev_sdate[i]+5),
           "&today=",AsDay(DF$ev_sdate[i]+5),
           "&events=",hazAb)
    out<-rjson::fromJSON(file = URL)[[1]]; 
    if(length(out)==0) return(glide_skel)
    # Just in case there are multiple GLIDE numbers, take the first one
    ind<-which.max(sapply(1:length(out),function(i) extractnumbers(out[[i]]$magnitude)))
    tryCatch(as.data.frame(out[[ind]]),error=function(e) glide_skel)
  }))
  # Convert to the proper GLIDE number, including the ISO3 country code
  glides$number<-apply(glides[,c("geocode","number")],1,function(x) paste0(x,collapse = "-"))
  # For those that returned an error...
  inds<-glides$number=="NA-NA"
  # Replace them with 
  glides$number[inds]<-DF[inds,]%>%GetGCDB_ID()
  # and let it be known when it is a glide number, too!
  glides$number[!inds]<-paste0(glides$number[!inds],"-GLIDE")
  # If we only care about the GLIDE number
  if(numonly) return(glides$number)
  
  return(glides)
}

