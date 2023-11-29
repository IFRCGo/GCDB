# Function to convert from a string delimited by 'delim' to a character vector
ParseMonty<-function(veccie, listy=T, delim=delim){
  parsy<-unlist(str_split(veccie,delim))
  stop("Include uniqueness somewhere?")
  if(listy) return(list(parsy)) else return(parsy)
}

# Note we don't use dplyr or magrittr here as they are slow AF
ExpMonty<-function(evvies,impies,hazzies,delim=delim){
  # Read in the JSON Schema template
  skelly<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  # First, setup the version info
  skelly$monty_Info$class_version<-Monty_version
  skelly$monty_Info$db_pubdate<-Sys.Date()
  skelly$monty_Info$helpful_resources<-skelly$monty_Info$helpful_resources[NULL,]
  # Create the output, for when there are multiple event_IDs, this will be added to each time
  outMonty<-skelly; outMonty$event_Level<-list(); outMonty$impact_Data<-list(); outMonty$hazard_Data<-list()
  # Temporary objects for each round
  event_Level<-skelly$event_Level; impact_Data<-skelly$impact_Data; hazard_Data<-skelly$hazard_Data
  # Now, for each event in the impies database, generate a JSON file
  for(ev in unique(evvies$event_ID))
    stop("Write code to make sure that no commas or colons are used in any of the strings, to avoid csv parsing")
    subimp<-impies[event_ID==ev,]
    subhaz<-hazzies[event_ID==ev,]
    ########################## Event level information #########################
    # IDs & linkages
    event_Level$ID_linkage$event_ID<-ev
    # If any of the event names are given in english, we use that name, and otherwise default to the first name in the object and it's corresponding language
    # This is done simply to facilitate our job when validating the JSON object and to maximise the readability for the target audience
    stop("This should be done before, defining evvies object")
    event_Level$ID_linkage<-evvies[,c("ev_name","ev_name_lang")]
    # if(any(subimp$ev_name_lang=="lang_eng")) {
    #   event_Level$ID_linkage$ev_name<-paste0(subimp$ev_name[subimp$ev_name_lang=="lang_eng"],collapse = '"," ')
    #   event_Level$ID_linkage$ev_name_lang<-"lang_eng"
    # } else {
    #   event_Level$ID_linkage$ev_name<-subimp$ev_name[1]
    #   event_Level$ID_linkage$ev_name_lang<-subimp$ev_name_lang[1]
    # }
    stop("Should this really be done here and not beforehand?")
    event_Level$ID_linkage$ext_IDs<-evvies$ext_IDs
    event_Level$ID_linkage$ext_IDs<-
      data.frame(ext_ID=ParseMonty(c(subimp$ext_IDs,hazimp[,c("haz_ext_IDs","linkhaz_ext_IDs")]),listy = F),
                 ext_ID_db=ParseMonty(c(subimp$ext_ID_dbs,hazimp[,c("haz_ext_IDdbs","linkhaz_ext_IDdbs")]),listy = F),
                 ext_ID_org=ParseMonty(c(subimp$ext_ID_orgs,hazimp[,c("haz_ext_IDorgs","linkhaz_ext_IDorgs")]),listy = F))%>%
      distinct()
    # Spatial
    stop("ev_ISO3s and regions should have been defined a long time beforehand! Don't define it here")
    event_Level$spatial$ev_ISO3s<-sort(unique(ParseMonty(c(subimp[,c("ev_ISO3s","imp_ISO3s")],
                                              hazimp$haz_ISO3s))))
    event_Level$spatial$gen_location<-paste0(unique(subimp$location,
                                                    hazimp$location),collapse='"," ')
    event_Level$spatial$regions<-sort(unique(ParseMonty(c(subimp$regions,hazimp$regions))))
    stop("This min-max date stuff should absolutely not be done here: prevent inconsistencies between the two databases!")
    stop("Translate between the time-zones to adjust everything to be the same (UTC 0?)")
    # Temporal
    event_Level$temporal<-subimp[j,names(event_Level$temporal)]
      # as.character(min(subimp%>%pull(ev_sdate)%>%as.Date(),
      #                                                      subhaz%>%pull(ev_sdate)%>%as.Date()))
      # as.character(max(subimp%>%pull(ev_fdate)%>%as.Date(),
      #                                                      subhaz%>%pull(ev_fdate)%>%as.Date()))
    # Hazard classifications
    stop("does all_hazs_Ab or the others actually exist in impies? think not")
    event_Level$allhaz_class$all_hazs_Ab<-subimp$all_hazs_Ab
    event_Level$allhaz_class$all_hazs_type<-subimp$all_hazs_type
    event_Level$allhaz_class$all_hazs_cluster<-subimp$all_hazs_cluster
    event_Level$allhaz_class$all_hazs_spec<-subimp$all_hazs_spec
    stop("sort out the names used for the imp_sub_ID: remove commas in ISO3s and haz_spec")
    ########################## Impact level information ########################
    for(j in 1:nrow(subimp)){
      # IDs and linkage information
      impact_Data$ID_linkage$event_ID<-ev
      impact_Data$ID_linkage$imp_sub_ID<-subimp$imp_sub_ID[j]
      stop("Separate out the different haz_sub_IDs from the variable in impies")
      impact_Data$ID_linkage$haz_sub_ID<-ParseMonty(subimp$haz_sub_ID[j])
      # Source information of the impact data
      impact_Data$source<-subimp[j,names(impact_Data$source)]
      # Impact estimates
      impact_Data$impact_detail<-subimp[j,names(impact_Data$impact_detail)]
      # Impact categorisation according to taxonomy
      impact_Data$impact_taxonomy<-subimp[j,names(impact_Data$impact_taxonomy)]
      # Temporal information
      stop("Add time zone information on the temporal data?")
      impact_Data$temporal<-subimp[j,names(impact_Data$temporal)]
      # Spatial data of the impact estimate
      stop("For each value in spatial instance, add the colindex and rowindex")
      stop("Make sure that the ordering of spatial corresponds to that of im")
      indies<-str_split_1(subimp$imp_spat_ID,delim)
      for(spsp in indies){
        impact_Data$spatial[[j]]$ID_linkage<-
        impact_Data$spatial[[j]]$spatial_info<-
        impact_Data$spatial[[j]]$source<-
      }
    }
    stop("Need to modify the haz_sub_ID variable in the impies database to allow for multiple IDs, delimited by ':'")
    ########################## Hazard level information ########################
    for(hz in unique(subhaz$haz_sub_ID)){
      # Separate out each entry
      subsubhaz<-subhaz%>%filter(haz_sub_ID==hz)
      # Quick check, just in case
      if(nrow(subsubhaz)!=1) stop("Error in the subsubhaz element of the hazard data")
      
      
      
      hazard_Data$ID_linkage$event_ID<-ev
      hazard_Data$ID_linkage$haz_sub_ID<-hz
      stop("check the array levels in imp_ext_IDs to ensure no repeated values")
    }
    
    
    
    
    
    
    
    
    
    # Save out into the main object
    outMonty$event_Level%<>%rbind(event_Level)
    outMonty$impact_Data%<>%rbind(impact_Data)
    outMonty$hazard_Data%<>%rbind(hazard_Data)
    # Return this lovely work!
    return(outMonty)
}















