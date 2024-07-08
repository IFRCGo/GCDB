RWebHazards<-function(rweb){
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/ReliefWeb_HIP.xlsx")
  # Reduce the translated vector and merge
  rweb%<>%left_join(colConv,by = c("haz_Ab"),
                     relationship="many-to-one")
}

ProcessReliefWeb<-function(json.content){
  # Extract everything directly from the json file of the API call
  rweb<-do.call(rbind,lapply(json.content, function(x) {
    out<-data.frame(
      ext_ID=x$id,
      ev_name=x$fields$name,
      GLIDE=ifelse(is.null(x$fields$glide),NA_character_,x$fields$glide),
      url=x$fields$url,
      description=ifelse(is.null(x$fields$description),NA_character_,x$fields$description),
      imp_credate=x$fields$date$created,
      imp_moddate=x$fields$date$changed,
      ev_sdate=x$fields$date$event
    )%>%mutate_at(c("imp_credate","imp_moddate","ev_sdate"),
                function(x) as.character(as.Date(x)))%>%
      mutate(imp_sdate=ev_sdate, imp_fdate=imp_credate,
             ev_fdate=ev_sdate)
    # Can be many countries per event, keep them all
    out$ev_ISO3s<-list(unlist(lapply(x$fields$country,function(xx) str_to_upper(xx$iso3))))
    # Can be many different hazards per event, keep them all
    out$hazard<-list(unlist(lapply(x$fields$type,function(xx) xx$name)))
    # Do the same with the abbreviated hazards
    out$haz_Ab<-paste0(unlist(lapply(x$fields$type,function(xx) xx$code)),collapse=delim)
    
    return(out)
  }))
  # Form the external ID object
  rweb$all_ext_IDs<-lapply(1:nrow(rweb), function(i){
    # First extract EM-DAT event ID
    out<-data.frame(ext_ID=rweb$ext_ID[i],
                    ext_ID_db="ReliefWeb",
                    ext_ID_org="UNOCHA")
    # If no other external IDs are provided, return only the Em-DAT ID
    if(is.na(rweb$GLIDE[i])) return(out) else 
      return(rbind(out,data.frame(ext_ID=rweb$GLIDE[i],
                                  ext_ID_db="GLIDE",
                                  ext_ID_org="ADRC")))
  })
  # Convert to the UNDRR-ISC hazard taxonomy
  rweb%<>%RWebHazards()%>%filter(!is.na(haz_spec))%>%
    mutate(all_hazs_spec=haz_spec)
  # Form the ID for the event
  rweb$event_ID<-GetMonty_ID(rweb)
  
  rweb%>%dplyr::select(any_of(MontyJSONnames()))%>%distinct()
}
# Function to call to ReliefWeb
CallReliefWeb<-function(maxdate=NULL){
  # Baseline url
  url<-"https://api.reliefweb.int/v1/disasters?appname=rwint-user-0&profile=list&slim=0&limit=1000&sort[]=date:desc&fields[include][]=country.iso3&fields[include][]=country.name&fields[include][]=date&fields[include][]=date.created&fields[include][]=date.event&fields[include][]=description&fields[include][]=glide&fields[include][]=id&fields[include][]=primary_type&fields[include][]=primary_type.id&fields[include][]=related_glide&fields[include][]=type&fields[include][]=type.code&fields[include][]=type.id&fields[include][]=type.name&fields[include][]=type.primary&fields[include][]=url"
  # With upper-date limit
  if(!is.null(maxdate)) datefilt<-paste0("&filter[field]=date&filter[value][to]=",maxdate,"T00:00:00%2B00:00") else datefilt <- ""
  # All in one
  url%<>%paste0(datefilt)
  # Make the call to ReliefWeb
  response <- httr::GET(url)
  # Extract the data
  jsonlite::fromJSON(httr::content(response, 
                                   "text", 
                                   encoding = "UTF-8"), 
                     simplifyVector = FALSE)$data%>%
    ProcessReliefWeb()
}
# To get around the API limit of 1000 records per call, 1000 calls per day
LoopReliefWeb<-function(rweb){
  # Find the earliest date
  minnie<-as.character(min(as.Date(unlist(rweb$ev_sdate)),na.rm = T))
  # Call the upper date limit to be the minimum existing date in the extracted data
  rweb%<>%rbind(CallReliefWeb(minnie))%>%distinct()
}

GetReliefWeb<-function(){
  # Extract direct from their API
  # (make sure to add the extra fields to the returned result)
  rweb<-CallReliefWeb()
  # Loop over 999 more times (API limit is 1000 records per call, 1000 calls per day)
  while(min(AsYear(unlist(rweb$ev_sdate)),na.rm = T)>1981) rweb%<>%LoopReliefWeb()
  
  return(rweb)
}

