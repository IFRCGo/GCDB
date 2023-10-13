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
  # Split by '-'
  tmp<-str_split(DF$haz_maxvalue[inds],"-",simplify = T)
  # Replace it in the vector
  DF$haz_maxvalue[inds]<-apply(tmp,1,function(x) max(extractnumbers(x),na.rm=T))
  # Now the units
  DF$haz_units[inds]<-"unitsenhfuj"
  
  # Now get rid of the rest
  inds<-is.na(DF$haz_maxvalue) | !DF$haz_Ab%in%c("EQ","TS","TO","TC")
  DF$haz_maxvalue[inds]<-NA_character_; DF$haz_units[inds]<-NA_character_
  
  return(DF)
}

GetGLIDEimps<-function(){
  # If you give GLIDE a request that they don't have, they give you the entire database!
  baseurl<-"https://www.glidenumber.net/glide/jsonglideset.jsp?glide=2008-000056"
  GLIDE<-rjson::fromJSON(file = baseurl)[[1]]; 
  GLIDE<-do.call(rbind,lapply(1:length(GLIDE),function(i) as.data.frame(GLIDE[[i]])))
  # Now let's treat this as a source of impact estimates!
  colnames(GLIDE)<-c("ev_name_en",
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
  # The source reference isn't well structured, use NLP to extract organisation name
  GLIDE$imp_src_db<-"GLIDE"
  # Set database to be the same as the organisation as we don't know better. Also, housekeeping
  GLIDE$imp_spat_ID<-NA
  # Sort out the ISO3 values to remove the NaNs
  GLIDE$ISO3[GLIDE$ISO3=="---"]<-NA_character_
  # House keeping
  GLIDE$haz_Ab[GLIDE$haz_Ab=="HT"]<-"HW"
  # Make sure the start date is 2 characters
  GLIDE$day[nchar(GLIDE$day)==1 & !is.na(GLIDE$day)]<-
    paste0("0",GLIDE$day[nchar(GLIDE$day)==1 & !is.na(GLIDE$day)])
  # Make sure the start month is 2 characters
  GLIDE$month[nchar(GLIDE$month)==1 & !is.na(GLIDE$month)]<-
    paste0("0",GLIDE$month[nchar(GLIDE$month)==1 & !is.na(GLIDE$month)])
  # Start date of event
  GLIDE$ev_sdate<-GLIDE$imp_sdate<-GLIDE$haz_sdate<-
    sapply(1:nrow(GLIDE),function(i) paste0(c(GLIDE$Year[i],GLIDE$month[i],GLIDE$day[i]),
                                          collapse = "-"),simplify = T)
  # End date of event
  GLIDE$ev_fdate<-GLIDE$imp_fdate<-GLIDE$haz_fdate<-as.character(as.Date(GLIDE$ev_sdate)+GLIDE$duration)
  # Modify the GLIDE number to be what it is known by
  GLIDE$GLIDE<-sapply(1:nrow(GLIDE),function(i) trimws(paste0(c(GLIDE$haz_Ab[i],GLIDE$GLIDE[i],ifelse(is.na(GLIDE$ISO3[i]),"",GLIDE$ISO3[i])),
                                                   collapse = "-"),
                                                   whitespace = "-",
                                                   which = 'right'),simplify = T)
  # Get the GCDB_ID number
  GLIDE$GCDB_ID<-GetGCDB_ID(GLIDE)
  # Add the UNDRR-ISC hazard taxonomy
  GLIDE%<>%GLIDEHazards()%>%filter(!is.na(haz_cluster))
  # Take the impact estimate columns and convert into the imp_type feature
  GLIDE%<>%reshape2::melt(measure.vars=c("imptyphomles","imptypdeat","imptypaffe","imptypinju"),
                        variable.name="imp_type",value.name="imp_value")
  # Instead of as a factor
  GLIDE$imp_type%<>%as.character()
  # Get the continent name & add on the impact taxonomy layers
  GLIDE%<>%mutate(Continent=convIso3Continent(ISO3),
                imp_cats="impcatpop",
                imp_subcats="imptypepopcnt",
                imp_det="impdetallpeop",
                imp_units="unitscount",
                imp_est_type="esttype_prim")
  # Try to extract as much as possible from the estimated magnitude and its units
  GLIDE%<>%modGLIDEmagunits()
  # Get rid of all zero values as we can't be sure that they are actual estimates
  GLIDE%<>%filter(imp_value>0)
  # And the sub IDs
  GLIDE%<>%GetGCDB_impID()
  # Make it into a GCDB_table-like object
  GLIDE%>%AddEmptyColImp()
}

GetGLIDE<-function(DF){

  # Retrieve those that didn't have a GLIDE number
  DF%<>%ExtractFromGLIDE()
  # For all GLIDE numbers that already exist, retrieve impact estimates
  
  return(DF)
}

