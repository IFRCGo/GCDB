# Names of the output of the GLIDEcols that we want to keep
GLIDEcols<-c("comments","year","docid","latitude","homeless","source","idsource",
             "killed","affected","duration","number","injured","month","geocode",
             "location","magnitude","time","id","event","day","status","longitude")
# Skeleton for empty output
glide_skel<-data.frame(matrix(NA_character_,nrow = 1,ncol = 22)); colnames(glide_skel)<-GLIDEcols

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
  out%<>%reshape2::melt(measure.vars=c("imptyphomles","imptypdeat","imptypaffe","imptypinju"),
                        variable.name="imp_type",value.name="imp_value")
  out$imp_type%<>%as.character()
  # Get the GCDB_ID number
  out$GCDB_ID<-GetGCDB_ID(out)
  # Get the continent name
  out%<>%mutate(Continent=convIso3Continent(ISO3))
  
  c(# Add triggering hazard details
    "prim_haz_Ab"="character", # Primary (triggering) hazard 2-letter abbreviation
    "prim_haz_type"="character", # Primary (triggering) hazard  type
    "prim_haz_cluster"="character", # Primary (triggering) hazard cluster 
    "prim_haz_spec"="character", # Primary (triggering) specific hazard 
    
    # Impact information
    "imp_sub_ID"="character", # ID of each impact element in the overall event
    "imp_sdate"="POSIXct", # Start date of the impact estimate (in case it aggregates over a range of dates)
    "imp_fdate"="POSIXct", # End date of the impact estimate (in case it aggregates over a range of dates)
    "imp_cats"="character", # Impact category
    "imp_subcats"="character", # Impact subcategory
    "imp_det"="character", # Impact subsubcategory
    "imp_value"="numeric", # Impact quantity
    "imp_type"="character", # Impact units
    "imp_units"="character", # Impact type (e.g. excess mortality, displacement stock)
    "imp_unitdate"="character", # date associated to the unit (for currencies almost exclusively)
    "imp_est_type"="character", # Estimate type: primary, secondary, modelled
    "imp_src_db"="character", # Source database name of impact estimate or the curated estimate
    "imp_src_org"="character", # Source organisation of impact estimate or the curated estimate
    "imp_src_orgtype"="character", # Source organisation type
    "imp_src_URL"="character", # URL of the impact estimate
    
    # Hazard information (can change from impact to impact for the same event)
    "haz_sub_ID"="character", # ID of each hazard event in the overall event, e.g. aftershocks or flash floods with cyclone
    "haz_sdate"="POSIXct", # Start date of the hazard estimate (in case it aggregates over a range of dates)
    "haz_fdate"="POSIXct", # End date of the hazard estimate (in case it aggregates over a range of dates)
    "haz_Ab"="character", # Abbreviated, simplified name of the hazard
    "haz_type"="character", # Impacting hazard type
    "haz_cluster"="character", # Impacting hazard cluster
    "haz_spec"="character", # Impacting specific hazard
    "haz_link"="character", # Associated impactful-hazards to the specific hazard
    "haz_potlink"="character", # Potential other impactful-hazards that may be associated to the specific hazard
    "haz_maxvalue"="numeric", # Maximum intensity or magnitude of the hazard, e.g.  
    "haz_units"="character", # Units of the max intensity/magnitude value estimate
    "haz_est_type"="character", # Estimate type: primary, secondary, modelled
    "haz_src_db"="character", # Source database name of impact estimate or the curated estimate
    "haz_src_org"="character", # Source organisation of impact estimate or the curated estimate
    "haz_src_orgtype"="character", # Source organisation type
    "haz_src_URL"="character", # URL of the impact estimate
    
    # Spatial info - impact
    "imp_spat_ID"="character", # ID of the spatial object
    "imp_spat_type"="character", # Spatial object type
    "imp_spat_res"="character", # Spatial resolution of impact estimate
    "imp_spat_srcorg"="character", # URL of the impact estimate
    # Spatial info - hazard
    "imp_spat_ID"="character", # ID of the spatial object
    "haz_spat_type"="character", # Spatial object type
    "haz_spat_res"="character", # Spatial resolution of impact estimate
    "haz_spat_srcorg"="character") # Source organisation from where the spatial object comes from
  
  
}


GetGLIDE<-function(DF){

  # Retrieve those that didn't have a GLIDE number
  DF%<>%ExtractFromGLIDE()
  # For all GLIDE numbers that already exist, retrieve impact estimates
  
  return(DF)
}

