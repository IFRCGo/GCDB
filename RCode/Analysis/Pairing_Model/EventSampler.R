UnbiasedSample<-function(Monty,maxsize=200){
  # How can we generate a sample size of roughly maxsize? Let's find the number of available events
  targnum<-Monty%>%mutate(year=AsYear(ev_sdate))%>%
    group_by(haz_Ab,ev_ISO3s,year)%>%
    reframe(Count=n())%>%nrow()
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  
  
  
  
  
  # Filter to leave only the important variables
  Monty%<>%dplyr::select()
  
  
  
  
  
  
  
  # Convert this into a factor of the maxsize
  targnum<-ceiling(maxsize/targnum)
  # Skeleton
  samsam<-data.frame()
  # For each hazard in
  hazzies <- unique(stringi::stri_trim_both(str_split(Monty$haz_Ab,";"))); hazzies<-hazzies[!is.na(hazzies)]
  # Sample
  for (haz in hazzies){
    # Filter out other hazards
    sMonty<-filter(Monty,grepl(haz,haz_Ab))
    if(nrow(sMonty)==0) next
    # For each country
    isos <- sort(unique(stringi::stri_trim_both(str_split(sMonty$ev_ISO3s,";")))); isos<-isos[!is.na(isos)]
    # Sample
    for (is in isos){
      # Filter out other countries
      ssMonty<-filter(sMonty,grepl(is,ev_ISO3s))
      if(nrow(ssMonty)==0) next
      # For each year
      yearies<-unique(ssMonty$year)
      # Sample
      for (yr in yearies){ 
        sssMonty<-filter(ssMonty,year==yr)
        if(nrow(sssMonty)==0) next
        # Sample targnum rows from the filtered dataframe
        samsam%<>%rbind(sssMonty[sample(1:nrow(sssMonty),pmin(targnum,nrow(sssMonty)),replace = F),])
      }
    }
  }
  return(samsam)
}

EventSampler<-function(Monty){
  
  # 0a) Find all GLIDE numbers, TC names, ext_IDs that have already been paired,
  #    then extract them and generate an unbiased sample using the method for (1), below.
  # 0b) Equally, for all obvious unpaired data, also generate an unbiased sample.
  # 0c) Remove all of these pre-paired events from the database after sampling some from non-paired databases for the same event
  # 
  # For each database,
  #   For each hazard
  #     For each country
  #       For each year
  #          1) Generate unbiased samples from db1, db2, ...
  #
  # 2) using (1a), sample possible pairs using:
  #   a) Cauchy-weighted year-difference
  #   b) Same country, with max 1-year difference
  #   c) Same hazard type, with max 1-year difference
  #   d)
  
  
  
  
}