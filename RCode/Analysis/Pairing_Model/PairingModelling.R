# Read in the data
pairies<-readxl::read_xlsx("./Analysis_Results/Pairing/Monty_Paired_Validated.xlsx")
# First of all, let's clean up the paired values
table(pairies$paired)
# Remove some of the qualitative columns
pairies%<>%dplyr::select(-description,-`QA score`,-`QA Comments`)
# We see a few errors, let's correct this
pairies%<>%mutate(paired=case_when(paired=="9-" ~ "-9.0", 
                                   paired=="9.0" ~"-9.0",
                                   T ~ paired),
                  confpair=case_when(paired==-9 ~ "Not enough info",
                                     paired==0 ~ "Unpaired - high confidence",
                                     paired==1 ~ "Unpaired - low confidence",
                                     paired==2 ~ "Unknown - no confidence",
                                     paired==3 ~ "Paired - low confidence",
                                     paired==4 ~ "Paired - high confidence"))%>%
  mutate(paired=as.integer(paired))
# Cleanup some of the mess
pairies%<>%filter(targ_hzAb!="costa rica flood november 2010" & dest_hzAb!="costa rica flood november 2010")
# Number of cores
ncores<-30
# Sub-national regions instead of ISOs
cunties<-openxlsx::read.xlsx("./Taxonomies/IsoContinentRegion.xlsx")%>%
  dplyr::select(ISO.Code,UN.Region,World.Bank.Income.Groups.Combined,
                SDG.Region,UN.Sub.Region)%>%
  setNames(c("ISO3C","Region","Income","SDG","Subregion"))%>%
  mutate(Income=case_when(is.na(Income)~"Not Classified", T ~ Income))
# Centroid-based distances
isomat<-openxlsx::read.xlsx("./Taxonomies/Country_DistanceProbability.xlsx"); isomat<-isomat[,2:ncol(isomat)]; isomat<-as.data.frame(apply(isomat,2,as.numeric)); rownames(isomat)<-colnames(isomat);

# Function to modify the paired database into the form we need for predictive modelling
ModPairDF<-function(pairies){
  # Convert dates to days since earliest date, for start and end dates
  mindate<-min(c(pairies$targ_evsdate,pairies$dest_evsdate),na.rm = T)
  pairies%<>%mutate(targ_sday=as.numeric(targ_evsdate-mindate),
                    targ_fday=as.numeric(targ_evfdate-targ_evsdate),
                    dest_sday=as.numeric(dest_evsdate-mindate),
                    dest_fday=as.numeric(dest_evfdate-dest_evsdate))
  # Incase this is the training data
  if(!is.null(pairies$paired)){
    # Make a binary confidence variable for the high-low values
    pairies$confidence<-1; pairies$confidence[pairies$paired!=4 | pairies$paired!=0] <- 0
    # First remove the no-confidence values and group the rest together
    pairies$paired[pairies$paired==0 | pairies$paired==1]<-0
    pairies$paired[pairies$paired==3 | pairies$paired==4]<-1
    pairies%<>%filter(paired<=1 & paired>=0)
    # Convert the paired variable into a character for the binary classification
    pairies$paired%<>%as.character()
    pairies%<>%mutate(paired=case_when(paired=="0" ~ "Unpaired",paired=="1" ~ "Paired"))
    # Get rid of unused covariates
    pairies%<>%filter(!is.na(paired))
  }
  
  #@@@@@@@@@@@@@@@@@@ ESTABLISHING COVARIATES @@@@@@@@@@@@@@@@@@#
  # target variables
  pairies%<>%left_join(cunties%>%setNames(paste0("targ_",colnames(cunties))),
                       join_by("targ_evISOs"=="targ_ISO3C"))
  pairies%<>%left_join(cunties%>%setNames(paste0("dest_",colnames(cunties))),
                       join_by("dest_evISOs"=="dest_ISO3C"))
  # Grouping hazards
  hazzies<-unique(as.vector(str_split(c(pairies$targ_hzAb,pairies$dest_hzAb),dely,simplify = T)))
  hazzies<-hazzies[nchar(hazzies)==2]
  pairies$targ_hzAb%<>%gsub(pattern = "AV", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "AV", replacement = "SL")
  pairies$targ_hzAb%<>%gsub(pattern = "LS", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "LS", replacement = "SL"); 
  pairies$targ_hzAb%<>%gsub(pattern = "MS", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "MS", replacement = "SL"); 
  pairies$targ_hzAb%<>%gsub(pattern = "MM", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "MM", replacement = "SL"); 
  pairies$targ_hzAb%<>%gsub(pattern = "HW", replacement = "ET"); pairies$dest_hzAb%<>%gsub(pattern = "HW", replacement = "ET"); 
  pairies$targ_hzAb%<>%gsub(pattern = "CW", replacement = "ET"); pairies$dest_hzAb%<>%gsub(pattern = "CW", replacement = "ET"); 
  pairies$targ_hzAb%<>%gsub(pattern = "DR", replacement = "ET"); pairies$dest_hzAb%<>%gsub(pattern = "DR", replacement = "ET");  
  pairies$targ_hzAb%<>%gsub(pattern = "HT", replacement = "ET"); pairies$dest_hzAb%<>%gsub(pattern = "HT", replacement = "ET"); 
  pairies$targ_hzAb%<>%gsub(pattern = "FF", replacement = "FL"); pairies$dest_hzAb%<>%gsub(pattern = "FF", replacement = "FL"); 
  pairies$targ_hzAb%<>%gsub(pattern = "SS", replacement = "ST"); pairies$dest_hzAb%<>%gsub(pattern = "SS", replacement = "ST"); 
  pairies$targ_hzAb%<>%gsub(pattern = "TO", replacement = "ST"); pairies$dest_hzAb%<>%gsub(pattern = "TO", replacement = "ST"); 
  pairies$targ_hzAb%<>%gsub(pattern = "VW", replacement = "ST"); pairies$dest_hzAb%<>%gsub(pattern = "VW", replacement = "ST"); 
  pairies$targ_hzAb%<>%gsub(pattern = "FR", replacement = "WF"); pairies$dest_hzAb%<>%gsub(pattern = "FR", replacement = "WF"); 
  pairies$targ_hzAb%<>%gsub(pattern = "WV", replacement = "TS"); pairies$dest_hzAb%<>%gsub(pattern = "WV", replacement = "TS"); 
  pairies$targ_hzAb%<>%gsub(pattern = "WA", replacement = "TS"); pairies$dest_hzAb%<>%gsub(pattern = "WA", replacement = "TS"); 
  pairies$targ_hzAb%<>%gsub(pattern = "EC", replacement = "TC"); pairies$dest_hzAb%<>%gsub(pattern = "EC", replacement = "TC"); 
  # Now make sure to remove any repeated entries in this values, for both target and destination
  pairies$targ_hzAb_m<-sapply(pairies$targ_hzAb,function(x) {
    paste0(sort(unique(as.vector(str_split(x," : ",simplify = T)))),collapse = dely)
  },simplify=T)
  pairies$dest_hzAb_m<-sapply(pairies$dest_hzAb,function(x) {
    paste0(sort(unique(as.vector(str_split(x," : ",simplify = T)))),collapse = dely)
  },simplify=T)
  # Manually adjust the rest
  pairies%<>%mutate(targ_hzAb_m=case_when(targ_hzAb_m=="FL  :  SL"~"FL", 
                                          targ_hzAb_m=="FL  :  SL_HM"~"FL", 
                                          targ_hzAb_m=="FL  :  TS"~"FL", 
                                          targ_hzAb_m=="FL  :  ST"~"ST", 
                                          targ_hzAb_m=="FL  :  TC"~"TC", 
                                          T ~ targ_hzAb_m))
  pairies%<>%mutate(dest_hzAb_m=case_when(dest_hzAb_m=="FL  :  SL"~"FL", 
                                          dest_hzAb_m=="FL  :  SL_HM"~"FL", 
                                          dest_hzAb_m=="FL  :  TS"~"FL", 
                                          dest_hzAb_m=="FL  :  ST"~"ST", 
                                          dest_hzAb_m=="FL  :  TC"~"TC", 
                                          T ~ dest_hzAb_m))
  # Create an index to see if they're the same hazard
  pairies$sameHaz<-pairies$targ_hzAb_m==pairies$dest_hzAb_m
  # Create groups of the hazards: geo, hydromet and 
  pairies%<>%mutate(targ_hazGroup=case_when(targ_hzAb_m%in%c("EQ","VO","TS") ~ "Geophysical",
                                            targ_hzAb_m%in%c("FL","ST","TC","FR","ET","WF") ~ "Climate",
                                            targ_hzAb_m%in%c("SL","MM") ~ "Slide"),
                    dest_hazGroup=case_when(dest_hzAb_m%in%c("EQ","VO","TS") ~ "Geophysical",
                                            dest_hzAb_m%in%c("FL","ST","TC","FR","ET","WF") ~ "Climate",
                                            dest_hzAb_m%in%c("SL") ~ "Slide"))
  # Create the hazard group column
  pairies%<>%mutate(Income=case_when(targ_hazGroup=="Geophysical" | dest_hazGroup=="Geophysical" ~ 1,
                                     targ_hazGroup=="Climate" | dest_hazGroup=="Climate" ~ -1,
                                     targ_hazGroup=="Slide" | dest_hazGroup=="Slide" ~ 0,
                                     T ~ 0))
  # Asymmetry of the two database names means we just need to include the databases as columns
  for(db in unique(pairies$targ_db)) 
    pairies[gsub(" ","",db)]<-grepl(db,pairies$targ_db) | grepl(db,pairies$dest_db)
  # Adding country centroid positions and centroid distances
  if(sum(is.na(pairies$dist_km))!=0) pairies$dist_km[is.na(pairies$dist_km)]<-
    sapply((1:nrow(pairies))[is.na(pairies$dist_km)],function(i) mean(isomat[pairies$targ_evISOs[i], pairies$dest_evISOs[i]],na.rm=T),simplify = T)
  # Create an index of sameISO
  pairies$sameISO<-pairies$targ_evISOs==pairies$dest_evISOs
  pairies$sameRegion<-pairies$targ_Region==pairies$dest_Region
  pairies$sameSubregion<-pairies$targ_Subregion==pairies$dest_Subregion
  pairies$sameSDG<-pairies$targ_SDG==pairies$dest_SDG
  # Check out the NAs
  pairies%<>%mutate(targ_Income=case_when(is.na(targ_Income)~"Not Classified", T~targ_Income),
                    dest_Income=case_when(is.na(dest_Income)~"Not Classified", T~dest_Income))
  # Modify the unclassified income
  incies<-unique(pairies$targ_Income); incies[incies=="Not Classified"]<-"Unknown Income"; pairies$targ_Income[pairies$targ_Income=="Not Classified"]<-"Unknown Income"; pairies$dest_Income[pairies$dest_Income=="Not Classified"]<-"Unknown Income"
  # Create a column for each income group
  pairies%<>%mutate(Income=case_when(targ_Income=="Not Classified" | dest_Income=="Not Classified" ~ 0,
                                     targ_Income=="Low Income" | dest_Income=="Low Income" ~ -1,
                                     targ_Income=="Middle Income" | dest_Income=="Middle Income" ~ 1,
                                     targ_Income=="High Income" | dest_Income=="High Income" ~ 2,
                                     T ~ 0))
  # Now modify the days. NOTE WE KEEP THE FDAYS AS THEY ARE
  pairies%<>%mutate(sday=as.numeric(targ_sday-dest_sday),
                    targ_fday=as.numeric(targ_fday), 
                    dest_fday=as.numeric(dest_fday))
  
  return(pairies)
}

# Go ahead and wrangle the data
pairies%<>%ModPairDF()
# Save a copy of the full pairies dataframe
pairies_save<-pairies
# Save it out for the future
saveRDS(pairies,"./Analysis_Results/Pairing/PairedData.RData")

# Get rid of unwanted columns now
pairies%<>%dplyr::select(-targ_db,-dest_db,
                         -targ_evISOs,-dest_evISOs,
                         -targ_Region,-targ_Subregion,-targ_SDG,-targ_Income,
                         -dest_Region,-dest_Subregion,-dest_SDG,-dest_Income,
                         -targ_hzAb_m,-dest_hzAb_m,-targ_hzAb,-dest_hzAb,
                         -targ_hazGroup,-dest_hazGroup,
                         -targ_lon,-targ_lat,-dest_lon,-dest_lat,
                         -targ_sday,-dest_sday,
                         -confidence,-confpair,
                         -targ_mid,-targ_evID,-targ_URL,-targ_ID,-targ_evsdate,-targ_evfdate,
                         -dest_mid,-dest_evID,-dest_URL,-dest_ID,-dest_evsdate,-dest_evfdate)
# Now remove some of the NA issues to face
pairies%<>%mutate_if(is.character,as.factor)
# Let us know info about the data
paste0("The number of non-NA entries in the dataset are = ",nrow(pairies))
# Make sure we remove only values that require
indy<-apply(pairies,1,function(x) !any(is.na(x)))
# Filter both the modelling data and the full dataset
pairies%<>%filter(indy)
pairies_save%<>%filter(indy)

# How many different databases are we working with?
dbs<-colnames(pairies)[grepl("-",colnames(pairies))]
ndbs<-length(dbs)

#@@@@@@@@@@@@@@@@@@@@@@@@@@ FEATURE IMPORTANCE @@@@@@@@@@@@@@@@@@@@@@@@@@#
# Do some basic bootstrap resampling for a quick analysis of feature importance but ensure uncertainty is represented through bootstrapping
train_control <- caret::trainControl(method="boot",
                                     search = "random",classProbs=T,
                                     summaryFunction=caret::twoClassSummary)
# Run a simple model: glmnet
modie<-caret::train(paired~., data = pairies%>%na.omit(), 
                    method = "glm", metric="ROC", family=binomial(),
                             tuneLength = 12, trControl = train_control,
                             preProcess = c("center","scale"))
# Extract the feature importance
ftimp<-as.data.frame(vip::vi(modie,feature_names=colnames(pairies)[colnames(pairies)!="paired"]))
# Add the directionality
coefy<-data.frame(Variable=names(modie$finalModel$coefficients)[-1],
                  Directionality=-sign(modie$finalModel$coefficients[-1])); row.names(coefy)<-NULL
ftimp%<>%left_join(coefy,by="Variable")%>%
  mutate(Variable=gsub("[`\\\\]|TRUE", "", Variable))
# Which variables don't seem to have much importance?
ftimp$Variable[ftimp$Importance==0]
# and those that do!
ftimp[ftimp$Importance!=0 & !grepl("-",ftimp$Variable),]
# Let's see the model performance
modie$results$ROC
# Remove the columns that weren't found to contribute
pairies%<>%dplyr::select(-sameRegion)

# Clean up to go out to the
# Save this out!
saveRDS(ftimp,"./Analysis_Results/Pairing/FeatureImportance.RData")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@ MACHINE LEARNING @@@@@@@@@@@@@@@@@@@@@@@@@@@#
# How do we want to train the models?
train_control <- caret::trainControl(method="repeatedcv", number=8, repeats=3,
                                     search = "random",classProbs=T,
                                     summaryFunction=caret::twoClassSummary)
# Setup the cross-validation
parallelML_balanced<-function(algo,ncores=4) {
  # Let us know which algorithm we're in
  print(paste0("Currently working with the ",algo," model"))
  # # Parallelise
  cl <- makePSOCKcluster(ncores)  # Create computing clusters
  registerDoParallel(cl)
  getDoParWorkers()
  # CV-split and model the pairing
  out<-do.call(dplyr::bind_rows,lapply(1:ndbs,function(i){
    print(dbs[i])
    datar<-pairies%>%filter(!unname(unlist(as.vector(pairies[gsub(" ","",dbs[i])]))))%>%
      na.omit()
    # Run the model!
    if(algo=="glmnet") {
      modeler<-tryCatch(caret::train(paired~., data = datar, method = algo, 
                            metric="ROC", family="binomial",na.action = na.omit,
                            tuneLength = 12, trControl = train_control),error=function(e) NULL)
    } else modeler<-tryCatch(caret::train(paired~., data = datar, method = algo, 
                                 metric="ROC",na.action = na.omit,
                          tuneLength = 12, trControl = train_control),error=function(e) NULL)
    # Just in case something went wrong only with this dataset
    if(is.null(modeler)) return(data.frame())
    
    return(as.data.frame(t(apply(modeler$results[-1],2,median)))%>%
             mutate(Database=dbs[i],Model=algo))
  }))
  # Remember to close the computing cluster
  stopCluster(cl)
  registerDoSEQ()
  
  return(out)
}

# Run the models here
minimods<-c("svmRadial","naive_bayes","rf",
            "knn","glmnet","nnet")
# Run them! Note, don't bind directly as some may have errored
ressies<-lapply(minimods,function(stst) {
  tryCatch(parallelML_balanced(stst,ncores = ncores),
           error=function(e) NA)
})
saveRDS(ressies,"./Analysis_Results/Pairing/ModelPerformance3_tmp.RData")
# First get rid of the failed models
ressies<-ressies[unlist(lapply(ressies,function(x) !all(is.na(x))))]
# Bind all into one
ressies<-do.call(dplyr::bind_rows,lapply(ressies,function(x) x))

saveRDS(ressies,"./Analysis_Results/Pairing/ModelPerformance3.RData")

#@@@@@@@@@@@@@@@@@@@ MODEL APPLICATION @@@@@@@@@@@@@@@@@@@#

# First we train the best performing model on all of the data:
cl <- makePSOCKcluster(ncores)  # Create computing clusters
registerDoParallel(cl)
getDoParWorkers()
# CV-split and model the pairing
modeler<-caret::train(paired~., data = pairies, method = "svmPoly", 
                      metric="ROC",na.action = na.omit,
                      tuneLength = 12, trControl = train_control)
# Remember to close the computing cluster
stopCluster(cl)
registerDoSEQ()

# Make predictions across all events
rfout<-cbind(predict(modeler,pairies, type = "prob"),pairies_save)
# Save it for the RMarkdown file
saveRDS(rfout,"./Analysis_Results/Pairing/RF_predictions2.RData")

rfroc<-as.data.frame(ROCit::rocit(score = rfout$Paired, class = pairies$paired=="Paired")[c("TPR","FPR")])
# Save it for the RMarkdown file
saveRDS(rfroc,"./Analysis_Results/Pairing/RF_ROC2.RData")
# Quick plot here too
rfroc%>%ggplot()+geom_point(aes(1-FPR,TPR-FPR))+geom_vline(xintercept = 0.95,colour="red")
# Decide on the pairing probability from the model based on the distribution they predict for known GLIDE-paired events
ethresh<-0.99

# Read in all of the data that we can now try to pair
Monty<-readRDS("Analysis_Results/Pairing/Monty_sample_ADAM-WFP_Atlas-USGS_DFO-UniColumbia_DisasterAWARE-PDC_EMDAT-CRED_GDACS-EC-JRC_GIDD-IDMC_GLIDE-ADRC_GO-DREF-IFRC_GO-EA-IFRC_GO-FBA-IFRC_IDU-IDMC_ReliefWeb-UNOCHA_2024-07-29.RData")
# Source the event sampling functions
source("./RCode/Analysis/Pairing_Model/EventSampler.R")

# Which databases are we covering
mondbs<-unique(Monty$database); mondbs<-mondbs[!is.na(mondbs)]
# Skeleton frame
out<-data.frame()  
# Per target database, predict pairing with all other databases, 
# then remove target database from the list
for (targ_db in mondbs){
  print(paste0("Target database = ",targ_db))
  # First make sure we know how many databases are currently present
  ndb<-length(unique(Monty$database))
  if(ndb<=1) next
  outsam<-data.frame()
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
                       submon$haz_Ab%in%PrepCondHazs(unique(submon$haz_Ab),unique(antimon$haz_Ab)) &
                       ev_ISO3s%in%PrepCondISOs(antimon$ev_ISO3s)); if(nrow(submon)==0) next
    antimon%<>%filter(ev_sdate>=dates[1] & ev_sdate<=dates[2] &
                        antimon$haz_Ab%in%PrepCondHazs(unique(antimon$haz_Ab),unique(submon$haz_Ab)) &
                        ev_ISO3s%in%PrepCondISOs(submon$ev_ISO3s)); if(nrow(antimon)==0) next
    # Keep only the variables we need
    antimon%<>%dplyr::select(m_id, event_ID, database, ev_sdate, ev_fdate, imp_lon, imp_lat, 
                            haz_Ab, ev_ISO3s, imp_src_URL, ext_ID)%>%distinct()%>%
      rename(longitude=imp_lon, latitude=imp_lat,URL=imp_src_URL)
    submon%<>%dplyr::select(m_id, event_ID, database, ev_sdate, ev_fdate, imp_lon, imp_lat, 
                            haz_Ab, ev_ISO3s, imp_src_URL, ext_ID)%>%distinct()%>%
      rename(longitude=imp_lon, latitude=imp_lat,URL=imp_src_URL)
    # Sample the destination data
    tmpsam<-tryCatch(PairedSample(submon,antimon),error=function(e) return(NULL))
    # If an error occurred, leave this one
    if(is.null(tmpsam)) break
    # If we no longer have anymore data then try again
    if(nrow(tmpsam)==0) next
    # Add to outsam
    outsam%<>%rbind(tmpsam)
  }
  # Add to the motherbase
  out%<>%rbind(outsam)
  # Remove this database from what will next be sampled
  Monty%<>%filter(database!=targ_db)
}
# Add the distance to the mix
out$dist_km<-geosphere::distHaversine(out[,c("dest_lon","dest_lat")],
                                      out[,c("targ_lon","targ_lat")])/1000
# Sort out the country info
targ_evISOs<-sapply(out$targ_evISOs,function(x){
  if(is.null(x) | length(x)==0) return(NA_character_) else return(unlist(x))
})
dest_evISOs<-sapply(out$dest_evISOs,function(x){
  if(is.null(x) | length(x)==0) return(NA_character_) else return(unlist(x))
}) 
if(sum(nchar(targ_evISOs)!=3,na.rm=T)>0) targ_evISOs[nchar(targ_evISOs)!=3]<-NA_character_
if(sum(nchar(dest_evISOs)!=3,na.rm=T)>0) dest_evISOs[nchar(dest_evISOs)!=3]<-NA_character_
out$targ_evISOs<-targ_evISOs; out$dest_evISOs<-dest_evISOs; 
out%<>%filter(!is.na(targ_evISOs) & !is.na(dest_evISOs))
# Adding country centroid positions and centroid distances
out$dist_km[is.na(out$dist_km)]<-sapply((1:nrow(out))[is.na(out$dist_km)],function(i) mean(isomat[out$targ_evISOs[i], out$dest_evISOs[i]],na.rm=T),simplify = T)
# Go ahead and wrangle the data
out%<>%ModPairDF()
# Add the missing columns, first the databases (thus all are equal to zero)
collies<-grep("-",colnames(pairies)[!colnames(pairies)%in%colnames(out)],value = T)
for (col in collies) out[col]<-F
# Remove NAs for these as there are very few values missing. We could imputate but we're not losing much here and could avoid the additional error
out%<>%filter(!is.na(dist_km) & !is.na(targ_fday) & !is.na(dest_fday) & 
                !is.na(targ_Region) & !is.na(dest_Region))
# Check where any potentially annoying variables are NAs
apply(out%>%dplyr::select(any_of(colnames(pairies))),
      2,function(x) sum(is.na(x)))
# Take out the other variables
redie<-out%>%dplyr::select(-targ_db,-dest_db,
                  -targ_evISOs,-dest_evISOs,
                  -targ_Region,-targ_Subregion,-targ_SDG,-targ_Income,
                  -dest_Region,-dest_Subregion,-dest_SDG,-dest_Income,
                  -targ_hzAb_m,-dest_hzAb_m,-targ_hzAb,-dest_hzAb,
                  -targ_hazGroup,-dest_hazGroup,
                  -targ_lon,-targ_lat,-dest_lon,-dest_lat,
                  -targ_sday,-dest_sday,-targ_mid,-dest_mid,
                  -targ_evID,-targ_URL,-targ_ID,-targ_evsdate,-targ_evfdate,
                  -dest_evID,-dest_URL,-dest_ID,-dest_evsdate,-dest_evfdate)
# Convert to factors
redie%<>%mutate_if(is.character,as.factor)
# Now make the predictions!
rfpred<-predict(modeler,redie, type = "prob")
# Let's look at the predictions
print(paste0("No. records at 3-sigma pairing = ",sum(rfpred$Paired<(1-0.99))))
print(paste0("No. records at 2-sigma pairing = ",sum(rfpred$Paired<(1-0.95))))

redie$paired<-rfpred$Paired
redie$paired_3sig<-rfpred$Paired<(1-0.99)
redie$paired_2sig<-rfpred$Paired<(1-0.95)
# Combine with the other columns
redie%<>%cbind(out%>%dplyr::select(all_of(colnames(out)[!colnames(out)%in%colnames(redie)])))
# Save it out!
saveRDS(redie,"./Analysis_Results/Pairing/PredictedPaired2.RData")

redie%>%arrange(desc(paired))%>%ggplot()+geom_line(aes((1-paired),nrow(redie):1))
# 
# 
# corries<-redie[redie$paired_3sig & (redie$targ_db=="GIDD - IDMC" | 
#         redie$dest_db=="GIDD - IDMC") &
#         (redie$targ_db=="EMDAT - CRED" | 
#         redie$dest_db=="EMDAT - CRED"), ]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# corimp<-do.call(rbind,parallel::mclapply(1:nrow(corries),function(i){
#   # First calculate the index for the EMDAT data
#   if(corries$targ_db[i]=="EMDAT - CRED"){
#     # Extract the relevant 
#     emind<-Monty$m_id==corries$targ_mid[i] & Monty$imp_type=="imptypdeat"
#     giind<-Monty$m_id==corries$dest_mid[i] & Monty$imp_type=="imptypidp"
#     # Checks
#     if(sum(emind)==0 | sum(giind)==0) return(data.frame())
#     # 
#     return(data.frame(EMDAT=Monty$imp_value[emind],
#                GIDD=Monty$imp_value[giind],
#                Hazard=paste0(corries$targ_hzAb_m[i]," - ",corries$dest_hzAb_m[i]),
#                ISO3=paste0(corries$targ_evISOs[i]," - ",corries$dest_evISOs[i]),
#                Region=paste0(corries$dest_Region[i]," - ",corries$targ_Region[i])))
#   } else {
#     # Extract the relevant 
#     emind<-Monty$m_id==corries$dest_mid[i] & Monty$imp_type=="imptypdeat"
#     giind<-Monty$m_id==corries$targ_mid[i] & Monty$imp_type=="imptypidp"
#     # Checks
#     if(sum(emind)==0 | sum(giind)==0) return(data.frame())
#     # 
#     return(data.frame(EMDAT=Monty$imp_value[emind],
#                GIDD=Monty$imp_value[giind],
#                Hazard=paste0(sort(unique(c(corries$targ_hzAb_m[i],corries$dest_hzAb_m[i]))),collapse = ","),
#                ISO3=paste0(sort(unique(c(corries$targ_evISOs[i],corries$dest_evISOs[i]))),collapse = ","),
#                Region=paste0(sort(unique(c(corries$dest_Region[i],corries$targ_Region[i]))),collapse = ",")))
#     
#   }
# },mc.cores = 30))
# 
# corimp%>%ggplot()+geom_point(aes(EMDAT,GIDD,colour=Hazard))+
#   scale_y_log10() + scale_x_log10() +
#   geom_smooth(aes(EMDAT,GIDD,colour=Hazard))+facet_wrap(~Hazard,scales = "free")
# 
# 
# 
# 
# 
# 
# # Extract linked external IDs like GLIDE numbers?
# indy<-sapply(Monty$all_ext_IDs,function(x) sum(!is.na(unlist(x))),simplify = T)!=0
# # Map from ext_IDs to the UUID of that entry so that we can map these later on
# ext_IDs<-do.call(dplyr::bind_rows,lapply(which(indy),function(i){
#   x<-Monty$all_ext_IDs[i]
#   if(class(x)=="list") x<-x[[1]]
#   x$event_ID<-Monty$event_ID[i]
#   x$index<-i
#   x$ext_ID<-as.character(x$ext_ID)
#   return(x)
# }))


# Convert this database to what we need for predictions

# Pair the GLIDE-paired entries















