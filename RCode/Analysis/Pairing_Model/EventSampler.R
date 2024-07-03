# Sample events from Monty tabular dataframe in an unbiased manner
UnbiasedSample<-function(Monty,maxsize=200){
  # How can we generate a sample size of roughly maxsize? Let's find the number of available events
  targnum<-Monty%>%mutate(year=AsYear(ev_sdate))%>%
    group_by(haz_Ab,ev_ISO3s,year)%>%
    reframe(Count=n())%>%nrow()
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  # Filter to leave only the important variables
  Monty%<>%dplyr::select(event_ID, ev_sdate, ev_fdate, year, 
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)
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

# Create the weightings of the date-difference based sampler
# y: destination dataframe to sample from
# sdate: target dataframe event start date, to make the date difference from
# caucgam: scale of the cauchy distribution, empirically estimated from paired events
# eqweigdays: number of days by which the probability is constant around the difference of zero
WeightDateDiff<-function(y,sdate,caucgam,eqweigdays=45){
  
}

# 
SimHazs<-function(hazs,targhaz){
  # Bring in the taxonomy file of overlapping hazards
  
  # Create the boolean from this matrix
  
}
  
# Given an unbiased sample of the target database, sample from the paired database
PairedSample<-function(samply,aMonty,yeardiff=1){
  # Keep only the variables we need
  aMonty%<>%dplyr::select(event_ID, ev_sdate, ev_fdate, year, 
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)
  samply%<>%dplyr::select(event_ID, ev_sdate, ev_fdate, year, 
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)
  # This will help later on
  aMonty%<>%mutate_at(c("ev_sdate","ev_fdate"),as.Date)
  samply%<>%mutate_at(c("ev_sdate","ev_fdate"),as.Date)
  # Change the column names of both dataframes for ease later on in merging
  colnames(aMonty)<-c("dest_evID","dest_evsdate","dest_evfdate","dest_yr",
                      "dest_hzAb","dest_hzsp","dest_evISOs")
  colnames(samply)<-c("targ_evID","targ_evsdate","targ_evfdate","targ_yr",
                      "targ_hzAb","targ_hzsp","targ_evISOs")
  
    
    
  # FOR EACH DATABASE IN AMONTY, SAMPLE SOME INDICES
  # for(){}
  
  
  
  # ALSO FILTER ALL NON-RELATED HAZARDS FROM SAMPLE SCAN
  
  
  
  
  
  # Create three lists of sampled indices of the target database
  # that will be used to sample in time, country and hazard type the destination database
  inds<-caret::createFolds(1:nrow(samply), k = 3, list = TRUE)
  
  # Using the unbiased target sample, sample possible pairs using:
  #   a) Cauchy-weighted year-difference
  pairsam<-do.call(rbind,parallel::mclapply(inds[[1]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff)]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Create the weighting for the sample
    www<-WeightDateDiff(y,x$targ_evsdate[1])
    # Sample from the destination dataframe, with the given weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = www)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores))
  # Filter out these values from the 
  aMonty%<>%filter(!event_ID%in%pairsam$dest_evid)
  
  
  # Checks in case nrow(aMonty)<length(inds[[2]])
  
  
  #   b) Same country, with max 1-year difference
  pairsam%<>%rbind(do.call(rbind,parallel::mclapply(inds[[2]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) & 
                aMonty$dest_evISOs==x$targ_evISOs[1]]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores)))
  # Filter out these values from the 
  aMonty%<>%filter(!event_ID%in%pairsam$dest_evid)
  
  
  
  # Checks in case nrow(aMonty)<length(inds[[3]])
  
  
  
  #   c) Same hazard type, with max 1-year difference
  pairsam%>%rbind(do.call(rbind,parallel::mclapply(inds[[3]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                aMonty$dest_hzAb==x$targ_hzAb]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores)))
}

EventSampler<-function(Monty){
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  
  
  
  
  # 0a) Find all GLIDE numbers, TC names, ext_IDs that have already been paired,
  #    then extract them and generate an unbiased sample using the method for (1), below.
  # 0b) Equally, for all obvious unpaired data, also generate an unbiased sample.
  # 0c) Remove all of these pre-paired events from the database after sampling some from non-paired databases for the same event
  
  
  
  
  # Sample from the target database, then sample from potential paired events from destination databases
  do.call(rbind,lapply(unique(Monty$imp_src_db), function(db){
    # Filter out only the database to be paired
    submon<-filter(Monty,imp_src_db==db)
    # Unbiased sample from the target database
    samply<-UnbiasedSample(submon); rm(submon)
    # Filter out the database to be paired to leave only the others
    antimon<-filter(Monty,imp_src_db!=db)
    # Unbiased sample from the destination database
    PairedSample(samply,antimon)
  }))
  
  
  
  
  
}