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
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)%>%distinct()
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

# Function to create the function of probabilities for sampling the dates
PYrDiffSampler <- function(x, eqwt=30, endwt=0.02) {
  # Ensure the function is symmetric
  x<-abs(x)
  # If within the time window then probability of 1 otherwise decaying to 1-in-30 at 365 days
  ifelse(x <= eqwt, 1, 1/exp(-log(endwt) / 335 * (x - eqwt)))
}

# Create the weightings of the date-difference based sampler
# y: destination dataframe to sample from
# sdate: target dataframe event start date, to make the date difference from
# caucgam: scale of the cauchy distribution, empirically estimated from paired events
# eqweigdays: number of days by which the probability is constant around the difference of zero
WeightDateDiff<-function(y,sdate,eqweigdays=30){
  # For now, make it symmetric and drop from probability of 1 within 1 month to 1/30 for 1 year difference
  PYrDiffSampler(abs(y-sdate),eqwt=eqweigdays)
}

# Function to filter out rows where the hazards could never correspond to the same event
# hazs: vector of abbreviated hazard types (e.g. EQ earthquake)
# targhaz: abbreviated hazard type to check against
# hazmat: matrix used to check the probability of occurrence between the hazards
MatHazFilter<-function(hazs,targhaz,hazmat){
  # Extract only target hazard and get into a dictionary-style variable
  bindy<-data.frame(hazard=colnames(hazmat)[2:ncol(hazmat)],
                    prob=hazmat[,c("hazard",targhaz)])
  # Create weighting from this and return it out
  out<-left_join(data.frame(hazard=hazs),bindy)$prob
  # Now do this for the inverse: triggering hazards
  bindy<-data.frame(hazard=colnames(hazmat)[2:ncol(hazmat)],
                    prob=c(hazmat[hazmat$hazard==targhaz,2:ncol(hazmat)]))
  
  return(out>0 | left_join(data.frame(hazard=hazs),bindy)$prob>0)
}
  
# Given an unbiased sample of the target database, sample from the paired database
PairedSample<-function(samply,aMonty,yeardiff=1){
  # Keep only the variables we need
  aMonty%<>%dplyr::select(event_ID, ev_sdate, ev_fdate, year, Database,
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)%>%distinct()
  samply%<>%dplyr::select(event_ID, ev_sdate, ev_fdate, year, Database,
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)%>%distinct()
  # This will help later on
  aMonty%<>%mutate_at(c("ev_sdate","ev_fdate"),as.Date)
  samply%<>%mutate_at(c("ev_sdate","ev_fdate"),as.Date)
  # Change the column names of both dataframes for ease later on in merging
  colnames(aMonty)<-c("dest_evID","dest_evsdate","dest_evfdate","dest_yr", "dest_db",
                      "dest_hzAb","dest_hzsp","dest_evISOs", "dest_URL", "dest_ID")
  colnames(samply)<-c("targ_evID","targ_evsdate","targ_evfdate","targ_yr", "targ_db",
                      "targ_hzAb","targ_hzsp","targ_evISOs", "targ_URL", "targ_ID")
  # Now let's make sure that the databases are sampled equally: create the weighting
  mfreq<-table(aMonty$dest_db)%>%as.data.frame()%>%
    setNames(c("dest_db","weight"))%>%mutate(weight=1/weight)
  # Merge with the database to get the weighting as a variable and normalise to max 1
  aMonty%<>%left_join(mfreq,by="dest_db")%>%mutate(weight=weight/max(weight))
  # Bring in the taxonomy file of overlapping hazards
  hazmat<-read_csv("./Taxonomies/Hazard_CrossProbability.csv")
  
  # Create three lists of sampled indices of the target database
  # that will be used to sample in time, country and hazard type the destination database
  inds<-caret::createFolds(1:nrow(samply), k = 3, list = TRUE)
  
  # NOTE: the three sections below allow the same events in the destination database
  #       to be resampled. In cases where lots of data is available, the probability 
  #       of resampling occurrence is low, but when the amount of data is low
  #       then we should resample anyway to avoid obviously unpaired events requiring validation.
  #       However, we do prevent resampling between each of the three blocks to ensure
  #       minimisation of bias.
  
  # Using the unbiased target sample, sample possible pairs using:
  #   a) Cauchy-weighted year-difference
  pairsam<-do.call(rbind,parallel::mclapply(inds[[1]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat)]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Create the weighting for the sample
    www<-WeightDateDiff(y$dest_evsdate,x$targ_evsdate[1])*y$weight
    # Scale it for computational efficiency
    www<-www/max(www)
    # Sample from the destination dataframe, with the given weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = www)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores))
  # Filter out these values from the destination database
  aMonty%<>%filter(!event_ID%in%pairsam$dest_evid)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[2]])) {
    print("post-yeardiff: trying to sample more than the destination database has to offer...")
    return(pairsam)
  }
  
  #   b) Same country, with max 1-year difference
  pairsam%<>%rbind(do.call(rbind,parallel::mclapply(inds[[2]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) & 
                aMonty$dest_evISOs==x$targ_evISOs[1] &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat)]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = y$weight)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores)))
  # Filter out these values from the 
  aMonty%<>%filter(!event_ID%in%pairsam$dest_evid)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[3]])) {
    print("post-ISOdiff: trying to sample more than the destination database has to offer...")
    return(pairsam)
  }
  
  #   c) Same hazard type, with max 1-year difference
  pairsam%>%rbind(do.call(rbind,parallel::mclapply(inds[[3]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                aMonty$dest_hzAb==x$targ_hzAb &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat)]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = y$weight)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores)))
}

AutomatedPairUnpairing<-function(Monty){
  # 0a) Find all GLIDE numbers, TC names, ext_IDs that have already been paired,
  #    then extract them and generate an unbiased sample using the method for (1), below.
  # 0b) Equally, for all obvious unpaired data, also generate an unbiased sample.
  # 0c) Remove all of these pre-paired events from the database after sampling some from non-paired databases for the same event
  
  return(list(Monty=Monty,out=out))
}

EventSampler<-function(Monty){
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  # Now make sure that the URL to the data is present. Prioritise the hazard link and if not, impact link
  Monty$URL<-Monty$haz_spat_fileloc
  Monty$URL[is.na(Monty$URL)]<-Monty$haz_src_URL[is.na(Monty$URL)]
  Monty$URL[is.na(Monty$URL)]<-Monty$imp_src_URL[is.na(Monty$URL)]
  # Store database information
  Monty%<>%mutate(Database=paste0(haz_src_db," - ",haz_src_org))
  ind<-is.na(Monty$haz_src_db)
  Monty$Database[ind]<-paste0(Monty$imp_src_db[ind]," - ",Monty$imp_src_org[ind])
  # For now, make this empty
  if(is.null(Monty$ext_ID)) Monty$ext_ID<-NA_character_
  # Automated pairing and unpairing of events based on things like GLIDE number
  out<-AutomatedPairUnpairing(Monty); Monty<-out$Monty; out<-out$out
  # Extract which databases to iterate over
  mondbs<-c(unique(Monty$imp_src_db),unique(Monty$haz_src_db)); mondbs<-mondbs[!is.na(mondbs)]
  
  out<-data.frame()
  # Sample from the target database, then sample from potential paired events from destination databases
  for (db in mondbs){
    # Filter out only the database to be paired
    submon<-filter(Monty,imp_src_db==db)
    # Unbiased sample from the target database
    samply<-UnbiasedSample(submon); rm(submon)
    # Filter out the database to be paired to leave only the others
    antimon<-filter(Monty,imp_src_db!=db)
    # Unbiased sample from the destination database
    out%<>%rbind(PairedSample(samply,antimon))
    # Remove this database from what will next be sampled
    Monty%<>%filter(imp_src_db!=db)
  }
  
  return(out)
}








































