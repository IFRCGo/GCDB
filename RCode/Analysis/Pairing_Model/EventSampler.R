# Sample events from Monty tabular dataframe in an unbiased manner
UnbiasedSample<-function(Monty,maxsize=200){
  # How can we generate a sample size of roughly maxsize? Let's find the number of available events
  targnum<-Monty%>%mutate(year=AsYear(ev_sdate))%>%
    group_by(haz_Ab,ev_ISO3s,year)%>%
    reframe(Count=n())%>%nrow()
  # Add the year to the dataset
  if(is.null(Monty$year)) Monty$year<-AsYear(Monty$ev_sdate)
  # Filter to leave only the important variables
  Monty%<>%dplyr::select(event_ID, database, ev_sdate, ev_fdate, year, longitude, latitude,
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
  
  # In case the number of entries is more than maxsize
  if(nrow(samsam)>maxsize) samsam<-samsam[sample(1:nrow(samsam),maxsize,F),]
  
  return(samsam)
}

# Function to create the function of probabilities for sampling the dates
PYrDiffSampler <- function(x, eqwt=14, endwt=0.005) {
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
WeightDateDiff<-function(y,sdate,eqweigdays=14){
  # For now, make it symmetric and drop from probability of 1 within 1 month to 1/30 for 1 year difference
  PYrDiffSampler(abs(as.numeric(y-sdate)),eqwt=eqweigdays)
}

# Function to filter out rows where the hazards could never correspond to the same event
# hazs: vector of abbreviated hazard types (e.g. EQ earthquake)
# targhaz: abbreviated hazard type to check against
# hazmat: matrix used to check the probability of occurrence between the hazards
MatHazFilter<-function(hazs,targhaz,hazmat){
  # Extract only target hazard and get into a dictionary-style variable
  bindy<-data.frame(hazard=colnames(hazmat)[2:ncol(hazmat)],
                    prob=hazmat[,targhaz])
  # Create weighting from this and return it out
  out<-left_join(data.frame(hazard=hazs),bindy,by="hazard")$prob
  # Now do this for the inverse: triggering hazards
  bindy<-data.frame(hazard=colnames(hazmat)[2:ncol(hazmat)],
                    prob=as.numeric(hazmat[hazmat$hazard==targhaz,2:ncol(hazmat)]))
  
  return(out>0 | left_join(data.frame(hazard=hazs),bindy,by="hazard")$prob>0)
}
  
# Given an unbiased sample of the target database, sample from the paired database
PairedSample<-function(samply,aMonty,yeardiff=1){
  # Keep only the variables we need
  aMonty%<>%dplyr::select(event_ID, database, ev_sdate, ev_fdate, year, longitude, latitude, 
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)%>%distinct()
  samply%<>%dplyr::select(event_ID, database, ev_sdate, ev_fdate, year, longitude, latitude, 
                         haz_Ab, haz_spec, ev_ISO3s, URL, ext_ID)%>%distinct()
  # Change the column names of both dataframes for ease later on in merging
  colnames(aMonty)<-c("dest_evID","dest_db","dest_evsdate","dest_evfdate","dest_yr","dest_lon","dest_lat",
                      "dest_hzAb","dest_hzsp","dest_evISOs", "dest_URL", "dest_ID")
  colnames(samply)<-c("targ_evID","targ_db","targ_evsdate","targ_evfdate","targ_yr","targ_lon","targ_lat",
                      "targ_hzAb","targ_hzsp","targ_evISOs", "targ_URL", "targ_ID")
  # Bring in the taxonomy file of overlapping hazards
  hazmat<-read.csv("./Taxonomies/Hazard_CrossProbability.csv")
  # Just in case we have Desinventar database where only one country is present, skip the last sampling routine
  kk<-ifelse(length(unique(aMonty$dest_evISOs))==1 | 
     length(unique(samply$targ_evISOs))==1,2,3)
  # Create three lists of sampled indices of the target database
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
  pairsam<-do.call(rbind,lapply(inds[[1]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Create the weighting for the sample & scaling it to max 1
    www<-WeightDateDiff(y$dest_evsdate,x$targ_evsdate[1]); www<-www/max(www)
    # Sample from the destination dataframe, with the given weighting
    iii<-sample(1:nrow(y), 1, replace = F, prob = www)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  }))
  # Filter out these values from the destination database
  aMonty%<>%filter(!dest_evID%in%pairsam$dest_evID)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[2]])) {
    print("post-yeardiff: trying to sample more than the destination database has to offer...")
    return(pairsam)
  }
  
  #   b) Same hazard type, with max 1-year difference
  pairsam%<>%rbind(do.call(rbind,lapply(inds[[2]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) &
                aMonty$dest_hzAb==x$targ_hzAb &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  })))
  # Filter out these values from the 
  aMonty%<>%filter(!dest_evID%in%pairsam$dest_evID)
  # Insert checks to make sure we still have enough destination data
  if(nrow(aMonty)<length(inds[[3]])) {
    print("post-Hazdiff: trying to sample more than the destination database has to offer...")
    if(nrow(aMonty)==length(inds[[3]])) return(pairsam)
    inds[[3]]<-inds[[3]][1:nrow(aMonty)]
  }
  
  #   c) Same country, with max 1-year difference
  pairsam%>%rbind(do.call(rbind,lapply(inds[[3]],function(i){
    # reduce look-up costs
    x<-samply[i,]
    # Filter destination database to a reasonable dataset
    y<-aMonty[aMonty$dest_evsdate>(x$targ_evsdate[1]-365*yeardiff) & 
                aMonty$dest_evsdate<(x$targ_evsdate[1]+365*yeardiff) & 
                aMonty$dest_evISOs==x$targ_evISOs[1] &
                MatHazFilter(aMonty$dest_hzAb,x$targ_hzAb,hazmat),]
    # Checks
    if(nrow(y)==0) return(data.frame())
    # Sample from the destination dataframe, with equal weighting
    iii<-sample(1:nrow(y), 1, replace = F)
    # Merge the two samples into one dataframe
    cbind(x,y[iii,])
  })))
}

AutomatedPairUnpairing<-function(Monty){
  # 0a) Find all GLIDE numbers, TC names, ext_IDs that have already been paired,
  #    then extract them and generate an unbiased sample using the method for (1), below.
  # 0b) Equally, for all obvious unpaired data, also generate an unbiased sample.
  # 0c) Remove all of these pre-paired events from the database after sampling some from non-paired databases for the same event
  
  return(list(Monty=Monty,out=out))
}

# Code to consolidate the nested external ID list vector into a 1D character vector
PasteExtID<-function(x){
  paste0(x[,1]," = ",paste0(apply(x[,2:3],1,paste0,collapse="-")),collapse=delim)
}

# Code to consolidate the nested country ISO3C codes list vector into a 1D character vector
PasteISO3s<-function(x){
  unname(unlist(parallel::mclapply(x,paste0,collapse=delim,mc.cores=ncores)))
}

# Function to sort external IDs
SortExtIDSampler<-function(Monty){
  # First check if the Monty instance has already been modified to have no nested lists
  if(is.character(Monty$all_ext_IDs)) return(Monty)
  # First extract IDs of the local database
  Monty%<>%cbind(do.call(rbind,parallel::mclapply(1:length(Monty$all_ext_IDs),function(i){
    # Extract list
    x<-Monty$all_ext_IDs[[i]]
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
  Monty$imp_src_db[Monty$imp_src_db=="Desinventar"]<-
    paste0(Monty$imp_src_db[Monty$imp_src_db=="Desinventar"],"-",
           Monty$imp_ISO3s[Monty$imp_src_db=="Desinventar"])
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
  
  return(Monty)
}


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
    # Sample from potential paired events from destination databases
    for(dest_db in mondbs[mondbs!=targ_db]){
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
      # Unbiased sample from the target database
      samply<-UnbiasedSample(submon,min(c(ssize_db,nrow(submon),nrow(antimon))))
      # Unbiased sample from the destination database
      out%<>%rbind(PairedSample(samply,antimon))
    }
    # Remove this database from what will next be sampled
    Monty%<>%filter(database!=targ_db)
  }
  
  return(out)
}

# Get ADAM data
drv <- RPostgreSQL::dbDriver("PostgreSQL")
conn <- RPostgreSQL::dbConnect(drv, host = "localhost", port=5432,
                  dbname = "risk", user = "postgres",password=" ")
adam<-sf::st_read(conn,"imminent_adam")%>%CleanADAM()
# Get GDACS data
gdacs<-convGDACS_Monty(T)
# Merge the two
Monty<-dplyr::bind_rows(adam,gdacs)
# Sample
sam<-EventSampler(Monty,50)
dbs<-paste0(str_replace_all(sort(unique(c(sam$targ_db,sam$dest_db)))," ",""),collapse="_")
write_csv(sam,paste0("./Analysis_Results/Pairing/Sampled_",dbs,".csv"))







































