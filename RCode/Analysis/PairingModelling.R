# Read in the data
pairies<-readxl::read_xlsx("./Analysis_Results/Pairing/Monty_Paired_Validated.xlsx")
# First of all, let's clean up the paired values
table(pairies$paired)
# Remove some of the qualitative columns
pairies%<>%dplyr::select(-description,-`QA score`,-`QA Comments`)
# Convert dates to days since earliest date, for start and end dates
mindate<-min(c(pairies$targ_evsdate,pairies$dest_evsdate),na.rm = T)
# We see a few errors, let's correct this
pairies%<>%mutate(paired=case_when(paired=="9-" ~ "-9.0", 
                                   paired=="9.0" ~"-9.0",
                                   T ~ paired),
                  confpair=case_when(paired==-9 ~ "Not enough info",
                                     paired==0 ~ "Unpaired - high confidence",
                                     paired==1 ~ "Unpaired - low confidence",
                                     paired==2 ~ "Unknown - no confidence",
                                     paired==3 ~ "Paired - low confidence",
                                     paired==4 ~ "Paired - high confidence"),
                  targ_sday=targ_evsdate-mindate,
                  targ_fday=targ_evfdate-targ_evsdate,
                  dest_sday=dest_evsdate-mindate,
                  dest_fday=dest_evfdate-dest_evsdate)%>%
  mutate(paired=as.integer(paired))
# Make a binary confidence variable for the high-low values
pairies$confidence<-1; pairies$confidence[pairies$paired!=4 | pairies$paired!=0] <- 0
# Cleanup some of the mess
pairies%<>%filter(targ_hzAb!="costa rica flood november 2010" & dest_hzAb!="costa rica flood november 2010")
# Save it out for the future
saveRDS(pairies,"./Analysis_Results/Pairing/PairedData.RData")
# Save a copy of the dataframe, just in case
tmp<-pairies
# First remove the no-confidence values and group the rest together
pairies$paired[pairies$paired==0 | pairies$paired==1]<-0
pairies$paired[pairies$paired==3 | pairies$paired==4]<-1
pairies%<>%filter(paired<=1 & paired>=0)
# Convert the paired variable into a character for the binary classification
pairies$paired%<>%as.character()
pairies%<>%mutate(paired=case_when(paired=="0" ~ "Unpaired",paired=="1" ~ "Paired"))
# Number of cores
ncores<-30
# How many different databases are we working with?
dbs<-unique(pairies$targ_db)
ndbs<-length(dbs)
# Get rid of unused covariates
pairies%<>%dplyr::select(-targ_mid,-targ_evID,-targ_URL,-targ_ID,-targ_evsdate,-targ_evfdate,
                         -dest_mid,-dest_evID,-dest_URL,-dest_ID,-dest_evsdate,-dest_evfdate,
                         -confpair)%>%
  filter(!is.na(paired))

#@@@@@@@@@@@@@@@@@@ ESTABLISHING COVARIATES @@@@@@@@@@@@@@@@@@#
# Sub-national regions instead of ISOs
cunties<-openxlsx::read.xlsx("./Taxonomies/IsoContinentRegion.xlsx")%>%
  dplyr::select(ISO.Code,UN.Region,World.Bank.Income.Groups.Combined,
                SDG.Region,UN.Sub.Region)%>%
  setNames(c("ISO3C","Region","Income","SDG","Subregion"))%>%
  mutate(Income=case_when(is.na(Income)~"Not Classified", T ~ Income))
# target variables
pairies%<>%left_join(cunties%>%setNames(paste0("targ_",colnames(cunties))),
                     join_by("targ_evISOs"=="targ_ISO3C"))
pairies%<>%left_join(cunties%>%setNames(paste0("dest_",colnames(cunties))),
                     join_by("dest_evISOs"=="dest_ISO3C"))
# Check out the NAs
pairies%<>%mutate(targ_Income=case_when(is.na(targ_Income)~"Not Classified", T~targ_Income),
                  dest_Income=case_when(is.na(dest_Income)~"Not Classified", T~dest_Income))
# Grouping hazards
hazzies<-unique(as.vector(str_split(c(pairies$targ_hzAb,pairies$dest_hzAb)," : ",simplify = T)))
hazzies<-hazzies[nchar(hazzies)==2]
pairies$targ_hzAb%<>%gsub(pattern = "AV", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "AV", replacement = "SL")
pairies$targ_hzAb%<>%gsub(pattern = "LS", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "LS", replacement = "SL"); 
pairies$targ_hzAb%<>%gsub(pattern = "MS", replacement = "SL"); pairies$dest_hzAb%<>%gsub(pattern = "MS", replacement = "SL"); 
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
# Now make sure to remove any repeated entries in this values, for both target and destination
pairies$targ_hzAb_m<-sapply(pairies$targ_hzAb,function(x) {
  paste0(unique(as.vector(str_split(x," : ",simplify = T))),collapse = " : ")
},simplify=T)
pairies$dest_hzAb_m<-sapply(pairies$dest_hzAb,function(x) {
  paste0(unique(as.vector(str_split(x," : ",simplify = T))),collapse = " : ")
},simplify=T)
# Manually adjust the rest
pairies%<>%mutate(targ_hzAb_m=case_when(targ_hzAb_m=="FL : SL"~"FL", 
                                   targ_hzAb_m=="FL : TS"~"FL", 
                                   T ~ targ_hzAb_m))
pairies%<>%mutate(dest_hzAb_m=case_when(dest_hzAb_m=="FL : ST"~"ST", 
                                        dest_hzAb_m=="FL : TC"~"TC", 
                                        T ~ dest_hzAb_m))
# Create an index to see if they're the same hazard
pairies$sameHaz<-pairies$targ_hzAb_m==pairies$dest_hzAb_m
# Create groups of the hazards: geo, hydromet and 
pairies%<>%mutate(targ_hazGroup=case_when(targ_hzAb_m%in%c("EQ","VO","TS") ~ "Geophysical",
                                          targ_hzAb_m%in%c("ET") ~ "Temperature",
                                          targ_hzAb_m%in%c("FL","ST","TC") ~ "Storm",
                                          targ_hzAb_m%in%c("WF") ~ "Fire",
                                          targ_hzAb_m%in%c("SL") ~ "Slide"),
                  dest_hazGroup=case_when(dest_hzAb_m%in%c("EQ","VO","TS") ~ "Geophysical",
                                          dest_hzAb_m%in%c("ET") ~ "Temperature",
                                          dest_hzAb_m%in%c("FL","ST","TC") ~ "Storm",
                                          dest_hzAb_m%in%c("WF") ~ "Fire",
                                          dest_hzAb_m%in%c("SL") ~ "Slide"))
# Now create a column for each one
for(hzg in unique(c(pairies$targ_hazGroup,pairies$dest_hazGroup))) 
  pairies[gsub(" ","",hzg)]<-grepl(hzg,pairies$targ_hazGroup) | grepl(hzg,pairies$dest_hazGroup)
# Asymmetry of the two database names means we just need to include the databases as columns
for(db in unique(pairies$targ_db)) 
  pairies[gsub(" ","",db)]<-grepl(db,pairies$targ_db) | grepl(db,pairies$dest_db)
# Adding country centroid positions and centroid distances
isomat<-openxlsx::read.xlsx("./Taxonomies/Country_DistanceProbability.xlsx"); isomat<-isomat[,2:ncol(isomat)]; isomat<-as.data.frame(apply(isomat,2,as.numeric)); rownames(isomat)<-colnames(isomat);
pairies$dist_km[is.na(pairies$dist_km)]<-sapply((1:nrow(pairies))[is.na(pairies$dist_km)],function(i) isomat[pairies$targ_evISOs[i], pairies$dest_evISOs[i]])
# Create an index of sameISO
pairies$sameISO<-pairies$targ_evISOs==pairies$dest_evISOs
pairies$sameRegion<-pairies$targ_Region==pairies$dest_Region
pairies$sameSubregion<-pairies$targ_Subregion==pairies$dest_Subregion
pairies$sameSDG<-pairies$targ_SDG==pairies$dest_SDG
# Modify the unclassified income
incies<-unique(pairies$targ_Income); incies[incies=="Not Classified"]<-"Unknown Income"; pairies$targ_Income[pairies$targ_Income=="Not Classified"]<-"Unknown Income"; pairies$dest_Income[pairies$dest_Income=="Not Classified"]<-"Unknown Income"
# Create a column for each income group
for(incy in unique(pairies$targ_Income)) 
  pairies[gsub(" ","",incy)]<-grepl(incy,pairies$targ_Income) | grepl(incy,pairies$dest_Income)
# Now modify the days. NOTE WE KEEP THE FDAYS AS THEY ARE
pairies%<>%mutate(sday=targ_sday-dest_sday)
# Get rid of unwanted columns now
pairies%<>%dplyr::select(-targ_db,-dest_db,
                         -targ_evISOs,-dest_evISOs,
                         -targ_Region,-targ_Subregion,-targ_SDG,-targ_Income,
                         -dest_Region,-dest_Subregion,-dest_SDG,-dest_Income,
                         -targ_hzAb_m,-dest_hzAb_m,-targ_hzAb,-dest_hzAb,
                         -targ_hazGroup,-dest_hazGroup,
                         -targ_lon,-targ_lat,-dest_lon,-dest_lat,
                         -targ_sday,-dest_sday,
                         -confidence)
# Let us know info about the data
paste0("The number of non-NA entries in the dataset are = ",pairies%>%na.omit()%>%nrow())

#@@@@@@@@@@@@@@@@@@@@@@@@@@ FEATURE IMPORTANCE @@@@@@@@@@@@@@@@@@@@@@@@@@#
# Do some basic bootstrap resampling for a quick analysis of feature importance but ensure uncertainty is represented through bootstrapping
train_control <- caret::trainControl(method="boot",
                                     search = "random",classProbs=T,
                                     summaryFunction=caret::twoClassSummary)
# Run a simple model: glmnet
modie<-caret::train(paired~., data = pairies%>%na.omit(), method = "glm", metric="ROC", family=binomial(),
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
  out<-lapply(1:ndbs,function(i){
    datar<-pairies%>%filter(!unname(unlist(as.vector(pairies[gsub(" ","",dbs[i])]))))%>%
      na.omit()%>%mutate_if(is.numeric, scale)
    # Run the model!
    modeler<-caret::train(paired~., data = datar, method = algo, metric="ROC",
                          tuneLength = 12, trControl = train_control)
    
    return(filter(modeler$results[-1],ROC==median(ROC))%>%mutate(Database=dbs[i]))
  })
  # Remember to close the computing cluster
  stopCluster(cl)
  registerDoSEQ()
  
  return(out)
}

# Now remove some of the NA issues to face
pairies%<>%mutate_if(is.character,as.factor)%>%na.omit()
# Run the models here
minimods<-c("svmLinear","svmRadial","svmPoly","naive_bayes","rf","glmnet","ada")
# Scale some of the numerical variables
pairies%<>%mutate(sday=as.numeric(sday), targ_fday=as.numeric(targ_fday), dest_fday=as.numeric(dest_fday))
# Run them!
ressies<-lapply(minimods,function(stst) tryCatch(parallelML_balanced(stst,ncores = ncores),error=function(e) NA))


#@@@@@@@@@@@@@@@@@@@ REMEMBER TO GET RID OF DATE COLUMNS, LEAVING DAY COLUMNS


























