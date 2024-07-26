# Sample events from Monty tabular dataframe in an unbiased manner
UnbiasedSample<-function(Monty,maxsize=200){
  # How can we generate a sample size of roughly maxsize? Let's find the number of available events
  targnum<-Monty%>%mutate(year=AsYear(ev_sdate))%>%
    group_by(haz_Ab,ev_ISO3s,year)%>%
    reframe(Count=n())%>%nrow()
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  # Filter to leave only the important variables
  Monty%<>%dplyr::select(m_id, event_ID, database, ev_sdate, ev_fdate, year, 
                         longitude, latitude, haz_Ab, ev_ISO3s, 
                         URL, ext_ID)%>%distinct()
  # Convert this into a factor of the maxsize
  targnum<-3L*ceiling(maxsize/targnum)
  # Skeleton
  samsam<-data.frame()
  # For each hazard in
  hazzies <- unique(stringi::stri_trim_both(str_split(Monty$haz_Ab,";"))); hazzies<-hazzies[!is.na(hazzies)]
  # Repeat sampling until we reach maxsize
  while(nrow(samsam) < maxsize) {
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
          # Sample the rows
          minisam<-sssMonty[sample(1:nrow(sssMonty),pmin(targnum,nrow(sssMonty)),replace = F),]
          # Add to the sampler dataframe
          samsam%<>%rbind(minisam)
          # Remove sampled rows from Monty
          Monty<-anti_join(Monty, minisam)
          # Check if we've reached maxsize
          if (nrow(samsam) >= maxsize) break
        }
        # Check if we've reached maxsize
        if (nrow(samsam) >= maxsize) break
      }
      # Check if we've reached maxsize
      if (nrow(samsam) >= maxsize) break
    }
  }
  # In case the number of entries is more than maxsize
  if(nrow(samsam)>maxsize) samsam<-samsam[sample(1:nrow(samsam),maxsize,F),]
  
  return(samsam)
}

# Function to create the function of probabilities for sampling the dates
PExpSampler <- function(x, eqwt, endwt, mxlim) {
  # Ensure the function is symmetric
  x<-abs(x)
  # If within the time window then probability of 1 otherwise decaying to 1-in-30 at 365 days
  ifelse(x <= eqwt, 1, 1/exp(-log(endwt) / mxlim*(x - eqwt)))
}

# Create the weightings of the date-difference based sampler
# y: destination dataframe to sample from
# sdate: target dataframe event start date, to make the date difference from
# eqweigdays: number of days by which the probability is constant around the difference of zero
WeightDateDiff<-function(y,sdate){
  # For now, make it symmetric and drop from probability of 1 within 1 month to 1/2000 for 6-month difference
  www<-PExpSampler(abs(as.numeric(y-sdate)),eqwt=14, endwt=0.005, mxlim=182)
  # Ensure NAs are correctly dealt with - set to probability of zero
  www[is.na(www)]<-0
  
  return(www)
}

# Create weightings of the distance between the longitude and latitude
WeightDistance<-function(x,y){
  # First calculate the geometric circumferential distance along the earth
  disty<-geosphere::distHaversine(as.matrix(x),as.matrix(y))/1000
  # Calculate the probability, in kilometres
  www<-PExpSampler(disty, eqwt=300, endwt=0.0001, mxlim=2500)
  # Ensure NAs are correctly dealt with - set to probability of zero
  www[is.na(www)]<-0
  
  return(www)
}


# Function to filter out rows where the hazards could never correspond to the same event
# hazs: vector of abbreviated hazard types (e.g. EQ earthquake)
# targhaz: abbreviated hazard type to check against
# hazmat: matrix used to check the probability of occurrence between the hazards
MatHazFilter <- function(hazs, targhaz, hazmat) {
  # Split the hazard strings by the delimiter
  split_hazs <- strsplit(hazs, delim)
  split_targhaz <- strsplit(targhaz, delim)[[1]]
  # Loop through each element in hazs
  unlist(lapply(split_hazs, function(haz_list){
    # Check for direct matches (unnecessary but a backup in case hazmat file is modified)
    match_targ <- any(haz_list %in% split_targhaz)
    # Now using the matrix
    match_hazmat <- any(sapply(haz_list, function(h1) {
      any(sapply(split_targhaz, function(h2) {
        hazmat[h1, h2]>0 || hazmat[h2, h1]>0
      }))
    }))
    
    # If any match is found, set the result to TRUE
    match_targ || match_hazmat
  }))
}

# Function to filter out rows where the countries could never correspond to the same event
# targISO: target country/territory ISO3C code
# destISO: destination list of country/territory ISO3C codes
# isomat: matrix used to check the centroid distance between the countries
MatISOFilter<-function(targISO, destISO, isomat, disty=5000){
  # To be safe, by default all of the countries are included
  innies<-!logical(length(destISO))
  # First check whether the 
  if(!targISO%in%colnames(isomat)) return(innies)
  # Check that the ISO code exists in the distance calculation
  ind<-destISO%in%colnames(isomat)
  # Evaluate the distance for the rest
  innies[ind]<-unname(sapply(destISO[ind],function(x) isomat[row.names(isomat)==x,targISO]<disty))
  
  return(innies)
}

# Function to create a boolean to filter out the distances below the threshold (current idea: 2500km)
DistFilter<-function(x,y,maxlim=2500){
  geosphere::distHaversine(as.matrix(y[,c("dest_lon","dest_lat")]),
                           x[,c("targ_lon","targ_lat")])/1000<maxlim
}
  
# Given an unbiased sample of the target database, sample from the paired database
PairedSample<-function(samply,aMonty,yeardiff=0.25){
  # Keep only the variables we need
  aMonty%<>%dplyr::select(m_id, event_ID, database, ev_sdate, ev_fdate, longitude, latitude, 
                         haz_Ab, ev_ISO3s, URL, ext_ID)%>%distinct()
  samply%<>%dplyr::select(m_id, event_ID, database, ev_sdate, ev_fdate, longitude, latitude, 
                         haz_Ab, ev_ISO3s, URL, ext_ID)%>%distinct()
  # Change the column names of both dataframes for ease later on in merging
  colnames(aMonty)<-c("dest_mid","dest_evID","dest_db","dest_evsdate","dest_evfdate",
                      "dest_lon","dest_lat","dest_hzAb","dest_evISOs", 
                      "dest_URL","dest_ID")
  colnames(samply)<-c("targ_mid","targ_evID","targ_db","targ_evsdate","targ_evfdate",
                      "targ_lon","targ_lat","targ_hzAb","targ_evISOs",
                      "targ_URL","targ_ID")
  # Bring in the taxonomy file of overlapping hazards and the country centroid distances
  hazmat<-read.csv("./Taxonomies/Hazard_CrossProbability.csv"); hazmat<-hazmat[,2:nrow(hazmat)]; rownames(hazmat)<-colnames(hazmat); 
  isomat<-openxlsx::read.xlsx("./Taxonomies/Country_DistanceProbability.xlsx"); isomat<-isomat[,2:ncol(isomat)]; isomat<-as.data.frame(apply(isomat,2,as.numeric)); rownames(isomat)<-colnames(isomat);
  # Just in case we have Desinventar database where only one country is present, skip the last sampling routine
  kk<-ifelse(length(unique(aMonty$dest_evISOs))==1 | 
     length(unique(samply$targ_evISOs))==1,3,4)
  # Create kk lists of sampled indices of the target database
  # that will be used to sample in time, country and hazard type the destination database
  inds<-caret::createFolds(1:nrow(samply), k = kk, list = TRUE)
  
  # NOTE: the three sections below allow the same events in the destination database
  #       to be resampled. In cases where lots of data is available, the probability 
  #       of resampling occurrence is low, but when the amount of data is low
  #       then we should resample anyway to avoid obviously unpaired events requiring validation.
  #       However, we do prevent resampling between each of the three blocks to ensure
  #       minimisation of bias.
  
  # Using the unbiased target sample, sample possible pairs using:
  #   a) exponential-weighted year-difference
  pairsam<-do.call(rbind,parallel::mclapply(inds[[1]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat) &
                MatISOFilter(aMonty$dest_evISOs, x$targ_evISOs[1], isomat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Check for distance values: default is TRUE if geolocation data is missing
    distfilts<-!logical(nrow(y))
    # Now check by distance
    if(!is.na(x$targ_lon) & !is.na(x$targ_lat) &
       any(!is.na(y$dest_lon) & !is.na(y$dest_lat))) {
      jj<-!is.na(y$dest_lon) & !is.na(y$dest_lat)
      distfilts[jj]<-DistFilter(x[,c("targ_lon","targ_lat")],
                            y[jj,c("dest_lon","dest_lat")])
    }
    # Filter by distance
    y%<>%filter(distfilts)
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Create the weighting for the sample & scaling it to max 1
    www<-WeightDateDiff(y$dest_evsdate,x$targ_evsdate[1])
    if(max(www,na.rm = T)==0) www[]<-1
    www<-www/max(www,na.rm = T)
    # Sample from the destination dataframe, with the given weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = www)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores))
  # Filter out these values from the destination database
  aMonty%<>%filter(!dest_evID%in%pairsam$dest_evID)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[2]])) {
    print("post-yeardiff: trying to sample more than the destination database has to offer...")
    return(pairsam)
  }
  
  #   b) Same hazard type, with max 1-year difference
  pairsam%<>%rbind(do.call(rbind,parallel::mclapply(inds[[2]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat) &
                MatISOFilter(aMonty$dest_evISOs, x$targ_evISOs[1], isomat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Identify same country ISO data
    hazs<-str_split(x$targ_hzAb[1],pattern = delim)
    samehaz<-sapply(y$dest_hzAb,
                    function(hzy) any(str_split(hzy,pattern = delim)%in%hazs))
    # Filter them out
    y<-y[samehaz,]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Check for distance values: default is TRUE if geolocation data is missing
    distfilts<-!logical(nrow(y))
    # Now check by distance
    if(!is.na(x$targ_lon) & !is.na(x$targ_lat) &
       any(!is.na(y$dest_lon) & !is.na(y$dest_lat))) {
      jj<-!is.na(y$dest_lon) & !is.na(y$dest_lat)
      distfilts[jj]<-DistFilter(x[,c("targ_lon","targ_lat")],
                                y[jj,c("dest_lon","dest_lat")])
    }
    # Filter by distance
    y%<>%filter(distfilts)
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores)))
  # Filter out these values from the 
  aMonty%<>%filter(!dest_evID%in%pairsam$dest_evID)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[3]])) {
    print("post-Hazdiff: trying to sample more than the destination database has to offer...")
    if(nrow(aMonty)==length(inds[[3]])) return(pairsam)
    inds[[3]]<-inds[[3]][1:nrow(aMonty)]
  }
  
  # Using the unbiased target sample, sample possible pairs using:
  #   c) exponential-weighted year-difference
  pairsam<-do.call(rbind,parallel::mclapply(inds[[3]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat) &
                MatISOFilter(aMonty$dest_evISOs, x$targ_evISOs[1], isomat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Check for distance values: default is TRUE if geolocation data is missing
    distfilts<-!logical(nrow(y))
    # Now check by distance
    if(!is.na(x$targ_lon) & !is.na(x$targ_lat) &
       any(!is.na(y$dest_lon) & !is.na(y$dest_lat))) {
      jj<-!is.na(y$dest_lon) & !is.na(y$dest_lat)
      distfilts[jj]<-DistFilter(x[,c("targ_lon","targ_lat")],
                                y[jj,c("dest_lon","dest_lat")])
    }
    # Filter by distance
    y%<>%filter(distfilts)
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Create the weighting for the sample & scaling it to max 1
    www<-WeightDistance(x[,c("targ_lon","targ_lat")],y[,c("dest_lon","dest_lat")])
    if(max(www,na.rm = T)==0) www[]<-1
    www<-www/max(www,na.rm = T)
    # Sample from the destination dataframe, with the given weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = www)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores))
  # Filter out these values from the destination database
  aMonty%<>%filter(!dest_evID%in%pairsam$dest_evID)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[4]])) {
    print("post-distance: trying to sample more than the destination database has to offer...")
    if(nrow(aMonty)==length(inds[[4]])) return(pairsam)
    inds[[4]]<-inds[[4]][1:nrow(aMonty)]
    
    return(pairsam)
  }
  
  # For single-country databases, such as Desinventar or Field Maps
  if(kk==3) return(pairsam)
  
  #   d) Same country, with max 1-year difference
  pairsam%>%rbind(do.call(rbind,parallel::mclapply(inds[[4]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) & 
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Check for distance values: default is TRUE if geolocation data is missing
    distfilts<-!logical(nrow(y))
    # Now check by distance
    if(!is.na(x$targ_lon) & !is.na(x$targ_lat) &
       any(!is.na(y$dest_lon) & !is.na(y$dest_lat))) {
      jj<-!is.na(y$dest_lon) & !is.na(y$dest_lat)
      distfilts[jj]<-DistFilter(x[,c("targ_lon","targ_lat")],
                                y[jj,c("dest_lon","dest_lat")])
    }
    # Filter by distance
    y%<>%filter(distfilts)
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Identify same country ISO data
    isos<-str_split(x$targ_evISOs[1],pattern = delim)
    sameiso<-sapply(y$dest_evISOs,
                    function(isy) any(str_split(isy,pattern = delim)%in%isos))
    # Filter them out
    y<-y[sameiso,]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  },mc.cores=ncores)))
}

AutomatedPairUnpairing<-function(Monty){
  # 0a) Find all GLIDE numbers, TC names, ext_IDs that have already been paired,
  #     also all events that share the same event_ID without being actually paired
  #    then extract them and generate an unbiased sample using the method for (1), below.
  # 0b) Equally, for all obvious unpaired data, also generate an unbiased sample.
  # 0c) Remove all of these pre-paired events from the database after sampling some from non-paired databases for the same event
  out<-data.frame()
  
  return(list(Monty=Monty,out=out))
}

# Code to consolidate the nested external ID list vector into a 1D character vector
PasteExtID<-function(x){
  paste0(x[,1]," = ",paste0(apply(x[,2:3],1,paste0,collapse="-")),collapse=delim)
}

# Code to consolidate the nested country ISO3C codes list vector into a 1D character vector
PasteISO3s<-function(x){
  unname(unlist(lapply(x,paste0,collapse=delim)))
}

# Function to sort external IDs
SortExtIDSampler<-function(Monty){
  # First check if the Monty instance has already been modified to have no nested lists
  if(is.character(Monty$all_ext_IDs)) return(Monty)
  # First extract IDs of the local database
  Monty%<>%cbind(do.call(rbind,parallel::mclapply(1:length(Monty$all_ext_IDs),function(i){
    # Extract list
    x<-Monty$all_ext_IDs[[i]]
    # Check for empties
    if(is.null(x)) return(data.frame(ext_ID=NA_character_,extext_ID=NA_character_))
    # Find the index that corresponds to the sources own id 
    j<-x$ext_ID_db==Monty$imp_src_db[i] |
      x$ext_ID_db==Monty$haz_src_db[i]
    # Bring out external ID and second external ID is a combination of all the others
    data.frame(ext_ID=PasteExtID(x[j,]),
               extext_ID=PasteExtID(x[!j,]))
  },mc.cores=ncores)))
  
  return(Monty)
}

# Function to sort ISO3s
SortISO3Sampler<-function(Monty){
  Monty%>%mutate_at(c("ev_ISO3s","haz_ISO3s","imp_ISO3s"),PasteISO3s)
}

# Function to prepare the ISO3C codes from a character vector into the unique country codes
PrepCondISOs<-function(is){
  isos<-unique(unlist(str_split(is,delim)))
  # NOTE WE CONVERT NAs TO "NA"
  isos[isos=="" | is.na(isos)]<-"NA"
  return(unique(isos))
}

PrepareMontySampler<-function(Monty){
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  # Now make sure that the URL to the data is present. Prioritise the hazard link and if not, impact link
  Monty$URL<-Monty$haz_spat_fileloc
  Monty$URL[is.na(Monty$URL)]<-Monty$haz_src_URL[is.na(Monty$URL)]
  Monty$URL[is.na(Monty$URL)]<-Monty$imp_src_URL[is.na(Monty$URL)]
  # Split all the Desinventar databases into individual country databases
  indy<-!is.na(Monty$imp_src_db) & Monty$imp_src_db=="Desinventar"
  if(sum(indy,na.rm = T)>0)
    Monty$imp_src_db[indy]<-
    paste0(Monty$imp_src_db[indy],"-",
           Monty$imp_ISO3s[indy])
  # The GO-EA, GO-DREF, GO-FBA databases are actually all from the same one and so shouldn't be paired between them
  Monty$imp_src_db[Monty$imp_src_db%in%c("GO-EA","GO-DREF","GO-FBA")]<-"GO-DREF"
  # Store database information
  Monty%<>%mutate(database=paste0(haz_src_db," - ",haz_src_org))
  ind<-is.na(Monty$haz_src_db)
  Monty$database[ind]<-paste0(Monty$imp_src_db[ind]," - ",Monty$imp_src_org[ind])
  # Sort the external IDs
  Monty%<>%dplyr::select(-any_of("ext_ID"))%>%SortExtIDSampler()
  # Sort the country ISO3C codes from list to character
  Monty%<>%SortISO3Sampler()
  # Convert dates from character to Date
  Monty%<>%mutate_at(c("ev_sdate","ev_fdate"),as.Date)
  # Sort also the longitude and latitude position
  Monty%<>%mutate(longitude=case_when(is.na(haz_lon)~imp_lon, TRUE~haz_lon),
                  latitude=case_when(is.na(haz_lat)~imp_lat, TRUE~haz_lat))
  # Modify the hazard types and make sure we only include what we have information on
  Monty%<>%filter(haz_Ab%in%colnames(read.csv("./Taxonomies/Hazard_CrossProbability.csv")))%>%
    mutate(haz_Ab=case_when(haz_Ab=="LS_HM" ~ "LS",
                            TRUE~haz_Ab))
  
  return(Monty%>%dplyr::select(m_id, event_ID, database, ev_sdate, ev_fdate, year, 
                               longitude, latitude, haz_Ab, ev_ISO3s, 
                               URL, ext_ID)%>%distinct())
}

# Sample from the records in the monty tabular instance,
# returns ssize_db number of samples between any two databases
EventSampler<-function(Monty,ssize_db=50){
  # Clean up and prepare Monty for the sampling
  Monty%<>%PrepareMontySampler()
  # Automated pairing and unpairing of events based on things like GLIDE number
  out<-AutomatedPairUnpairing(Monty); Monty<-out$Monty; out<-out$out
  # Extract which databases to iterate over
  mondbs<-unique(Monty$database); mondbs<-mondbs[!is.na(mondbs)]
  # Skeleton frame
  out<-data.frame()
  # Sample from the target database one at a time
  for (targ_db in mondbs){
    print(paste0("Target database = ",targ_db))
    # First make sure we know how many databases are currently present
    ndb<-length(unique(Monty$database))
    if(ndb<=1) next
    # Sample from potential paired events from destination databases
    for(dest_db in mondbs[mondbs!=targ_db]){
      print(paste0("Destination database = ",dest_db))
      # Filter out only the database to be paired
      submon<-filter(Monty,database==targ_db); if(nrow(submon)==0) next
      # Filter out the database to be paired to leave only the others
      antimon<-filter(Monty,database==dest_db); if(nrow(antimon)==0) next
      # Match the infimum and supremum of the event start dates
      dates<-c(max(min(submon$ev_sdate,na.rm=T),min(antimon$ev_sdate,na.rm=T)),
               min(max(submon$ev_sdate,na.rm=T),max(antimon$ev_sdate,na.rm=T)))
      # Filter out from both dataframes to leave what can be matched, including with ISO3C codes
      submon%<>%filter(ev_sdate>=dates[1] & ev_sdate<=dates[2] &
                         ev_ISO3s%in%PrepCondISOs(antimon$ev_ISO3s)); if(nrow(submon)==0) next
      antimon%<>%filter(ev_sdate>=dates[1] & ev_sdate<=dates[2] &
                          ev_ISO3s%in%PrepCondISOs(submon$ev_ISO3s)); if(nrow(antimon)==0) next
      # Checks
      if(nrow(submon)==0 | nrow(antimon)==0) next
      # How many values do we wish to sample?
      nsam<-min(c(as.integer(ndb*ssize_db),nrow(submon),nrow(antimon)))
      # Skeleton dataframe
      outsam<-data.frame(); mmm<-0
      # Loop over the sampling until we have enough samples
      while(nrow(outsam)<nsam | nrow(submon)!=0 | mmm<3){
        # Set the upper threshold for the number of re-samples
        mmm<-mmm+1
        # Unbiased sample from the target database
        samply<-UnbiasedSample(submon,min(nsam,nrow(submon)))
        # Sample the destination data
        outsam<-tryCatch(PairedSample(samply,antimon),error=function(e) return(NULL))
        # If an error occurred, leave this one
        if(is.null(outsam)) break
        # If we no longer have anymore data then try again
        if(nrow(outsam)==0) next
        # Unbiased sample from the destination database
        out%<>%rbind(outsam)
        # Remove the sampled values from the target dataframe
        submon%<>%filter(!m_id%in%outsam$targ_mid)
      }
    }
    # Remove this database from what will next be sampled
    Monty%<>%filter(database!=targ_db)
  }
  # Add the distance to the mix
  out$dist_km<-geosphere::distHaversine(out[,c("dest_lon","dest_lat")],
                                           out[,c("targ_lon","targ_lat")])/1000
  
  return(out)
}




# # Get ADAM data
# drv <- DBI::dbDriver("PostgreSQL")
# conn <- RPostgreSQL::dbConnect(drv, host = "localhost", port=5432,
#                   dbname = "risk", user = "postgres",password=" ")
# # Extract + clean ADAM data
# adam<-sf::st_read(conn,"imminent_adam")%>%CleanADAM()
# # # Extract PDC tabular data
# pdc<-readRDS("./CleanedData/MostlyHazardData/PDC-DFS_clean.RData")
# # pdc<-sf::st_read(conn,"imminent_pdc")
# # # Now the geospatial element, but only to get access to the country information
# # pdcdis<-sf::st_read(conn,"imminent_pdcdisplacement")
# # pdc%<>%filter()
# # pdc%<>%CleanPDC()
# # Get GDACS data
# gdacs<-convGDACS_Monty(T)
# # Merge them
# Monty<-dplyr::bind_rows(adam,gdacs,pdc)
# 
# # Store database information
# Monty%<>%mutate(database=paste0(haz_src_db," - ",haz_src_org))
# ind<-is.na(Monty$haz_src_db)
# Monty$database[ind]<-paste0(Monty$imp_src_db[ind]," - ",Monty$imp_src_org[ind])
# # Add some IDs to ensure we can replicate this later
# Monty$m_id<-paste0("monty_",sapply(paste0(Monty$event_ID,Monty$database,sep="_"), digest::digest, algo = "sha256"))
# # Which databases exist in the Monty sample?
# dbs<-paste0(str_replace_all(sort(unique(Monty$database))," ",""),collapse="_")
# saveRDS(Monty,paste0("Analysis_Results/Pairing/Monty_sample_",dbs,"_",Sys.Date(),".RData"))






# Other Monty data, pre-prepped from ARO workshop
# Monty<-readRDS("./CleanedData/Monty_2024-06-11_tab.RData")%>%
#   filter(imp_src_db!="Desinventar")%>%
#   mutate_at("haz_maxvalue",as.numeric)
# Monty$ev_ISO3s<-Monty$imp_ISO3s<-Monty$haz_ISO3s<-
#   lapply(Monty$ev_ISO3s,function(x){unlist(str_split(x,pattern = delim))})
# # Pre-prepped forecast data
# Monty%<>%dplyr::bind_rows(readRDS("Analysis_Results/Pairing/Monty_sample_ADAM-WFP_DisasterAWARE-PDC_GDACS-EC-JRC_2024-07-08.RData"))
# Monty$haz_src_db[Monty$imp_src_db=="GDACS"]<-"GDACS"
# Monty$haz_src_org[Monty$imp_src_db=="GDACS"]<-"EC-JRC"
# # Add ReliefWeb and USGS-Atlas. LEAVE DESINVENTAR OUT FOR NOW
# Monty%<>%dplyr::bind_rows(GetReliefWeb())
# Monty%<>%dplyr::bind_rows(GetAtlasTab())
# 
# 
# # Store database information
# Monty%<>%mutate(database=paste0(haz_src_db," - ",haz_src_org))
# ind<-is.na(Monty$haz_src_db)
# Monty$database[ind]<-paste0(Monty$imp_src_db[ind]," - ",Monty$imp_src_org[ind])
# # Add some IDs to ensure we can replicate this later
# Monty$m_id<-paste0("monty_",sapply(paste0(Monty$event_ID,Monty$database,sep="_"), digest::digest, algo = "sha256"))
# # Which databases exist in the Monty sample?
# dbs<-paste0(str_replace_all(sort(unique(Monty$database))," ",""),collapse="_")
# # Save it out!
# saveRDS(Monty,paste0("Analysis_Results/Pairing/Monty_sample_",dbs,"_",Sys.Date(),".RData"))


Monty<-readRDS("./Analysis_Results/Pairing/Monty_sample_ADAM-WFP_Atlas-USGS_DFO-UniColumbia_DisasterAWARE-PDC_EMDAT-CRED_GDACS-EC-JRC_GIDD-IDMC_GLIDE-ADRC_GO-DREF-IFRC_GO-EA-IFRC_GO-FBA-IFRC_IDU-IDMC_ReliefWeb-UNOCHA_2024-07-08.RData")
# Sample the data!
sam<-EventSampler(Monty,50)
# Add the pairing column
sam%<>%mutate(paired="",description="")
# Trim down the length of the numerical columns
sam%<>%mutate_if(is.numeric,round,digits=3)
# Which databases were present for this sample?
dbs<-paste0(str_replace_all(sort(unique(c(sam$targ_db,sam$dest_db)))," ",""),collapse="_")
# Write it out
openxlsx::write.xlsx(sam,paste0("./Analysis_Results/Pairing/Sampled_",dbs,"_",Sys.Date(),".xlsx"))

































