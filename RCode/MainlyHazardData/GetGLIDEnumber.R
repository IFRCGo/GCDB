# Names of the output of the GLIDEcols that we want to keep
GLIDEcols<-c("comments","year","docid","latitude","homeless","source","idsource",
             "killed","affected","duration","number","injured","month","geocode",
             "location","magnitude","time","id","event","day","status","longitude")
# Skeleton for empty output
glide_skel<-data.frame(matrix(NA_character_,nrow = 1,ncol = 22)); colnames(glide_skel)<-GLIDEcols

GLIDEHazards<-function(GLIDE){
  # Read in the GLIDE-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/GLIDE-HIP.xlsx")
  # Reduce the translated vector and merge
  GLIDE%>%left_join(colConv,by = c("haz_Ab"),
                     relationship="many-to-one")
} 

GetGLIDEnum<-function(DF,numonly=T){
  # Needs to contain the columns: ev_sdate, ISO3 & haz
  # Make sure dates are not in character format
  DF$ev_sdate%<>%as.Date()
  # Abbreviated hazard taxonomy
  haz_Ab<-DF$haz_Ab; 
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
           "&events=",haz_Ab)
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

modGLIDEmagunits<-function(DF){
  # Remove any entries that don't contain any numbers
  DF$haz_maxvalue[DF$haz_maxvalue=="" | str_detect(DF$haz_maxvalue,"[0-9]",T)]<-NA_character_
  # Ready to store the units of each measurement
  DF$haz_units<-NA_character_
  
  # Let's first work on earthquakes & tsunamis (TS also measured in Richter scale)
  inds<-!is.na(DF$haz_maxvalue) & DF$haz_Ab%in%c("EQ","TS")
  # Sometimes they mention the dates of the EQ '5.9 on 12th March and 5.2 on 14th March'
  tmp<-str_split(DF$haz_maxvalue[inds],"and",simplify = T)
  for(j in 1:ncol(tmp)) tmp[,j]<-parse_number(str_split(tmp[,j],"on",simplify = T)[,1])
  # Sometimes they order the numbers in smaller first, we only care about the max
  DF$haz_maxvalue[inds]<-apply(tmp,1,max,na.rm=T)
  # And modify the units
  DF$haz_units[inds]<-"unitsrichter"
  # Zero magnitude earthquakes make no sense, remove to avoid errors
  DF$haz_maxvalue[inds & DF$haz_maxvalue=="0"]<-NA
  
  # Now tropical cyclones
  inds<-!is.na(DF$haz_maxvalue) & DF$haz_Ab=="TC"
  # Anything mentioning category
  tinds<-inds & ( grepl(paste(c("cat.","category"), 
                              collapse='|'), DF$haz_maxvalue,ignore.case = T))
  # Substitute them back in
  DF$haz_maxvalue[tinds]<-extractnumbers(DF$haz_maxvalue[tinds])
  # And add the units
  DF$haz_units[tinds]<-"unitssaffsimp"
  # Any referring to mph or kph separate now
  tinds<-inds & ( grepl(paste(c("km/h","kph","m/h","mph","mile/h"), 
                              collapse='|'), DF$haz_maxvalue,ignore.case = T))
  # Substitute them back in
  DF$haz_maxvalue[tinds]<-extractnumbers(DF$haz_maxvalue[tinds])
  # And add the units
  DF$haz_units[tinds & ( grepl(paste(c("m/h","mph","mile/h"), 
                                     collapse='|'), DF$haz_maxvalue,ignore.case = T))]<-"unitsmph"
  DF$haz_units[tinds & ( grepl(paste(c("km/h","kph"), 
                                     collapse='|'), DF$haz_maxvalue,ignore.case = T))]<-"unitskph"
  
  
  # Now tornados
  inds<-!is.na(DF$haz_maxvalue) & DF$haz_Ab=="TO"
  
  
  DF%>%group_by(haz_Ab)%>%reframe(hazm=unique(haz_maxvalue))%>%View()
  
  
  
}

ExtractFromGLIDE<-function(DF){
  # If you give GLIDE a request that they don't have, they give you the entire database!
  baseurl<-"https://www.glidenumber.net/glide/jsonglideset.jsp?glide=2008-000056"
  out<-rjson::fromJSON(file = baseurl)[[1]]; 
  out<-do.call(rbind,lapply(1:length(out),function(i) as.data.frame(out[[i]])))
  # Now let's treat this as a source of impact estimates!
  colnames(out)<-c("ev_name_en",
                   "Year",
                   "docid", # Nope!
                   "Latitude", # Nope!
                   "imptyphomles",
                   "imp_src_org",
                   "idsource", # Nope!
                   "imptypdeat",
                   "imptypaffe",
                   "duration",
                   "GLIDE",
                   "imptypinju",
                   "month",
                   "ISO3",
                   "location",
                   "haz_maxvalue",
                   "time", # Nope!
                   "id", # Nope!
                   "haz_Ab",
                   "day",
                   "status", # Nope!
                   "Longitude")
  # Sort out the ISO3 values to remove the NaNs
  out$ISO3[out$ISO3=="---"]<-NA_character_
  # House keeping
  out$haz_Ab[out$haz_Ab=="HT"]<-"HW"
  # Make sure the start date is 2 characters
  out$day[nchar(out$day)==1 & !is.na(out$day)]<-
    paste0("0",out$day[nchar(out$day)==1 & !is.na(out$day)])
  # Make sure the start month is 2 characters
  out$month[nchar(out$month)==1 & !is.na(out$month)]<-
    paste0("0",out$month[nchar(out$month)==1 & !is.na(out$month)])
  # Start date of event
  out$ev_sdate<-out$imp_sdate<-out$haz_sdate<-
    sapply(1:nrow(out),function(i) paste0(c(out$Year[i],out$month[i],out$day[i]),
                                          collapse = "-"),simplify = T)
  # End date of event
  out$ev_fdate<-out$imp_fdate<-out$haz_fdate<-as.character(as.Date(out$ev_sdate)+out$duration)
  # Modify the GLIDE number to be what it is known by
  out$GLIDE<-sapply(1:nrow(out),function(i) trimws(paste0(c(out$haz_Ab[i],out$GLIDE[i],ifelse(is.na(out$ISO3[i]),"",out$ISO3[i])),
                                                   collapse = "-"),
                                                   whitespace = "-",
                                                   which = 'right'),simplify = T)
  # Get the GCDB_ID number
  out$GCDB_ID<-GetGCDB_ID(out)
  # And the sub IDs
  out%<>%GetGCDB_impID()
  # Take the impact estimate columns and convert into the imp_type feature
  out%<>%reshape2::melt(measure.vars=c("imptyphomles","imptypdeat","imptypaffe","imptypinju"),
                        variable.name="imp_type",value.name="imp_value")
  # Instead of as a factor
  out$imp_type%<>%as.character()
  # Get the continent name & add on the impact taxonomy layers
  out%<>%mutate(Continent=convIso3Continent(ISO3),
                imp_cats="impcatpop",
                imp_subcats="imptypepopcnt",
                imp_det="impdetallpeop",
                imp_units="unitscount",
                imp_est_type="esttype_prim")
  # Add the UNDRR-ISC hazard taxonomy
  out%<>%GLIDEHazards()%>%filter(!is.na(haz_cluster))
  # Try to extract as much as possible from the estimated magnitude and its units
  out%<>%modGLIDEmagunits()
  
  c(
    "haz_units"="character", # Units of the max intensity/magnitude value estimate
    ) # Source organisation from where the spatial object comes from
  
  return(out)
}


GetGLIDE<-function(DF){

  # Retrieve those that didn't have a GLIDE number
  DF%<>%ExtractFromGLIDE()
  # For all GLIDE numbers that already exist, retrieve impact estimates
  
  return(DF)
}

