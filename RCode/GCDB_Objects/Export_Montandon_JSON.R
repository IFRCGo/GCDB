ExpMonty<-function(impies,hazzies){
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
  for(ev in unique(impies$event_ID))
    subimp<-impies%>%filter(event_ID==ev)
    subhaz<-hazzies%>%filter(event_ID==ev)
    ########################## Event level information #########################
    # IDs & linkages
    event_Level$ID_linkage$event_ID<-ev
    event_Level$ID_linkage$ev_name<-subimp%>%
      pull(ev_name)%>%paste0(collapse = ",")
    event_Level$ID_linkage$ev_name_lang<-subimp%>%
      pull(ev_name_lang)%>%paste0(collapse = ",")
    stop("Swap for Ext_IDs")
    event_Level$ID_linkage$all_ext_IDs<-list(unique(subimp%>%pull(all_ext_IDs),
                                                      hazimp%>%pull(haz_ext_IDs,linkhaz_ext_IDs)))
    # Spatial
    stop("Create ev_ISO3s & replace ISO3 with imp_ISO3s/haz_ISO3s in impies and hazzies")
    stop(" separate out the ISO3s from collapsed data.frame format into single entities in list")
    stop("same for Regions/Continents")
    stop("for delimiter, use ':' and not ',' for ISO3s and regions")
    event_Level$spatial$ISO3s<-list(unique(subimp%>%pull(ev_ISO3s,imp_ISO3s),
                                                  hazimp%>%pull(haz_ISO3s)))
    event_Level$spatial$gen_location<-paste0(unique(subimp%>%pull(location),
                                                         hazimp%>%pull(location)),collapse=", ")
    event_Level$spatial$Regions<-list(unique(subimp%>%pull(Region),
                                                    hazimp%>%pull(Region)))
    # Temporal
    event_Level$temporal$ev_sdate<-as.character(min(subimp%>%pull(ev_sdate)%>%as.Date(),
                                                           subhaz%>%pull(ev_sdate)%>%as.Date()))
    event_Level$temporal$ev_fdate<-as.character(max(subimp%>%pull(ev_fdate)%>%as.Date(),
                                                           subhaz%>%pull(ev_fdate)%>%as.Date()))
    # Principal hazard classification
    stop("does prpl_haz_Ab or the others actually exist in impies? think not")
    stop("modal won't work if there is nothing in prpl_haz_...")
    event_Level$principal_hazard$prpl_haz_Ab<-subimp%>%pull(prpl_haz_Ab)%>%modal()
    event_Level$principal_hazard$prpl_haz_type<-subimp%>%pull(prpl_haz_type)%>%modal()
    event_Level$principal_hazard$prpl_haz_cluster<-subimp%>%pull(prpl_haz_cluster)%>%modal()
    event_Level$principal_hazard$prpl_haz_spec<-subimp%>%pull(prpl_haz_spec)%>%modal()
    
    stop("sort out the names used for the imp_sub_ID: remove commas in ISO3s and haz_spec")
    ########################## Impact level information ########################
    for(im in unique(subimp$imp_sub_ID)){
      # Separate out each entry
      subsubimp<-subimp%>%filter(imp_sub_ID==im)
      # Quick check, just in case
      if(nrow(subsubimp)!=1) stop("Error in the subsubimp element of the impact data")
      
      impact_Data$ID_linkage$event_ID<-ev
      impact_Data$ID_linkage$imp_sub_ID<-im
      stop("Separate out the different haz_sub_IDs from the variable in impies")
      impact_Data$ID_linkage$haz_sub_ID<-list(subsubimp%>%pull(haz_sub_ID))
      # Source
      impact_Data$source$imp_src_db<-subsubimp%>%pull()
      stop("imp_src_orgatt needs sorting out!")
      stop("imp_src_lic needs sorting out!")
      stop("imp_cats and imp_subcats rename to remove the 's' at the end")
    }
    stop("Need to modify the haz_sub_ID variable in the impies database to allow for multiple IDs, dlimited by ':'")
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















