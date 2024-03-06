taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
imp_class<-taxies%>%filter(list_name=="imp_type")%>%dplyr::select(2:3)
exp_class<-data.frame(
  exp_det_code=taxies%>%filter(list_name=="exp_specs")%>%pull(name),
  exp_det_lab=taxies%>%filter(list_name=="exp_specs")%>%pull(label),
  exp_subcat_code=taxies%>%filter(list_name=="exp_specs")%>%pull(link_group),
  exp_subcat_lab=left_join(taxies[taxies$list_name=="exp_specs",2:4],
                           taxies[taxies$list_name=="exp_subcats",2:4],
                           by=c("link_group"="name"))%>%pull(label.y),
  exp_cat_code=taxies%>%filter(list_name=="exp_specs")%>%pull(link_maingroup),
  exp_cat_lab=left_join(left_join(taxies[taxies$list_name=="exp_specs",2:4],
                                  taxies[taxies$list_name=="exp_subcats",2:4],
                                  by=c("link_group"="name")),
                        taxies[taxies$list_name=="exp_cats",2:3],
                        by=c("link_group.y"="name"))%>%pull(label)
)
haz_class<-data.frame(
  haz_spec_code=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(name),
  haz_spec_lab=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(label),
  haz_cluster_code=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(link_group),
  haz_cluster_lab=left_join(taxies[taxies$list_name=="hazardsubsubtypes",2:4],
                            taxies[taxies$list_name=="hazardsubtypes",2:4],
                            by=c("link_group"="name"))%>%pull(label.y),
  haz_type_code=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(link_maingroup),
  haz_type_lab=left_join(left_join(taxies[taxies$list_name=="hazardsubsubtypes",2:4],
                                   taxies[taxies$list_name=="hazardsubtypes",2:4],
                                   by=c("link_group"="name")),
                         taxies[taxies$list_name=="hazardtypes",2:3],
                         by=c("link_group.y"="name"))%>%pull(label)  
)

GetMonty_ID<-function(DF,haz=NULL) {
  # In case a specific hazard is fed in
  if(!is.null(haz)) DF%<>%mutate(haz_Ab=haz)
  # Generate the names from the dataframe
  namerz<-DF%>%
    dplyr::select(haz_Ab,ev_sdate,ev_ISO3s)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  paste0(namerz,"-GCDB_V1")
}

stripevent_ID<-function(ID){
  apply(str_split(ID,"-",simplify = T)[,1:4],1,paste0,collapse="-")
}

GetGCDB_impID<-function(impies){
  if(!any(colnames(impies)=="imp_spat_ID")) impies$imp_spat_ID<-NA_character_

  tmp<-impies; tmp[is.na(tmp)]<-""
  impies$imp_sub_ID<-tmp%>%dplyr::select(c(event_ID,imp_src_db,exp_spec,imp_type,imp_spat_ID))%>%
    mutate(imp_src_db=stringr::str_remove(stringi::stri_trans_totitle(imp_src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  return(impies)
}

GetGCDB_hazID<-function(impies){
  if(!any(colnames(impies)=="haz_spat_ID")) impies$haz_spat_ID<-NA_character_
  tmp<-impies; tmp[is.na(tmp)]<-""
  impies$haz_sub_ID<-tmp%>%dplyr::select(c(event_ID,haz_src_db,haz_cluster,haz_spec,haz_spat_ID))%>%
    mutate(haz_src_db=stringr::str_remove(stringi::stri_trans_totitle(haz_src_db),pattern = " "),
           haz_src_db=stringr::str_remove(stringi::stri_trans_totitle(haz_src_db),pattern = " "),
    )%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  return(impies)
}

GetGCDB_imp_spatID<-function(impies){
  impies[is.na(impies)]<-""
  impies%>%dplyr::select(imp_spat_srcorg,imp_spat_srcdb,imp_spat_covcode,imp_spat_res,imp_spat_resunits)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
}

GetGCDB_haz_spatID<-function(impies){
  impies[is.na(impies)]<-""
  impies%>%dplyr::select(haz_spat_srcorg,haz_spat_srcdb,haz_spat_covcode,haz_spat_res,haz_spat_resunits)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
}

# A function to go from a series of data into the JSON-accepted DF-list style object
Add_EvIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-do.call(rbind,lapply(unique(dframe$event_ID),function(ID){
    dframe%>%filter(event_ID==ID)%>%
    reframe(event_ID=ID,ev_name=paste0('\"',unique(ev_name),collapse = '\",\"','\"'))%>%
      distinct()
  }))
  # Generate all the elements of the dataset
  output$all_ext_IDs<-sapply(output$event_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$event_ID==ID
    # check for no external IDs
    if(any(is.na(dframe$ext_ID[indy]))) return(list())
    # Highlight the external IDs that share the same Monty IDs 
    list(data.frame(ext_ID=dframe$ext_ID[indy],
               ext_ID_db=dframe$ext_ID_db[indy], 
               ext_ID_org=dframe$ext_ID_org[indy])%>%distinct())
  },simplify = T)
  # Let's keep this neat
  names(output$all_ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}

Add_EvSpat_Monty<-function(dframe){
  # Setup the other entries
  output<-do.call(rbind,lapply(unique(dframe$event_ID),function(ID){
    dframe%>%filter(event_ID==ID)%>%
      reframe(event_ID=ID,gen_location=paste0('\"',unique(gen_location),collapse = '\",\"','\"'))
  }))
  # and the ISO3C codes
  output$ev_ISO3s<-lapply(output$event_ID,function(ID){
    # return the ISO3C codes in a list
    unique(dframe$ev_ISO3s[dframe$event_ID==ID])
  })
  # Gimme gimme gimme
  return(output%>%dplyr::select(-event_ID))
}

Add_EvTemp_Monty<-function(dframe){
  # Take the event start date as the minimum and maximum dates
  do.call(rbind,lapply(unique(dframe$event_ID),function(ID){
    dframe%>%filter(event_ID==ID)%>%
      reframe(ev_sdate=as.character(min(as.Date(ev_sdate))),
            ev_fdate=as.character(max(as.Date(ev_fdate))))
  }))
}

Add_EvHazTax_Monty<-function(dframe){
  # Extract all of the haz_spec codes and output a list
  lapply(unique(dframe$event_ID),function(ev){
    # indices
    indy<-which(dframe$event_ID==ev)
    do.call(rbind,lapply(indy,function(i){
      # Output
      data.frame(
        all_hazs_Ab=dframe$haz_Ab[i],
        all_hazs_spec=c(str_split(dframe$haz_spec[i],delim,simplify = T))
      )%>%distinct()
    }))%>%distinct()
  })
}

# Add_HazTax_Monty<-function(dframe){
#   # Extract all of the haz_spec codes and output a list
#   lapply(unique(dframe$event_ID),function(ev){
#     # indices
#     indy<-which(dframe$event_ID==ev)
#     do.call(rbind,lapply(indy,function(i){
#       # Output
#       data.frame(
#         all_hazs_Ab=dframe$haz_Ab[i],
#         all_hazs_spec=c(str_split(dframe$haz_spec[i],delim,simplify = T)),
#         haz_maxvalue=dframe$haz_maxvalue[i],
#         haz_maxunits=dframe$haz_maxunits[i],
#         haz_est_type=dframe$haz_est_type[i]
#       )%>%distinct()
#     }))
#   })
# }
Add_HazTax_Monty<-function(dframe){
  
  output<-dframe%>%dplyr::select(event_ID,haz_maxvalue,haz_maxunits,haz_est_type,haz_Ab)%>%
    rename(all_hazs_Ab=haz_Ab)
  # Extract all of the haz_spec codes and output a list
  output$all_hazs_spec<-lapply(output$event_ID,function(ev){
    all_hazs_spec=c(gsub(" ", "",str_split(dframe$haz_spec[dframe$event_ID==ev],":",simplify = T)))
  })
  
  return(output%>%dplyr::select(-event_ID))
}

# A function to form the impact IDlinkage field
Add_ImpIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,imp_sub_ID)%>%distinct()
  # Generate all the elements of the dataset
  output$haz_sub_ID<-sapply(output$imp_sub_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$imp_sub_ID==ID
    # check for no haz_sub_imp values
    if(any(is.na(dframe$haz_sub_ID[indy]))) return(list())
    # Highlight the external IDs that share the same Monty IDs 
    list(unique(dframe$haz_sub_ID[indy]))
  },simplify = T)
  # Let's keep this neat
  names(output$haz_sub_ID)<-NULL
  # Now let's add the external IDs
  output$imp_ext_IDs<-sapply(output$imp_sub_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$imp_sub_ID==ID
    # Check for no IDs
    if(all(is.na(dframe$ext_ID[indy]))) return(list())
    # Highlight the external IDs that share the same Monty IDs 
    list(data.frame(ext_ID=dframe$ext_ID[indy],
                    ext_ID_db=dframe$ext_ID_db[indy], 
                    ext_ID_org=dframe$ext_ID_org[indy])%>%distinct())
  },simplify = T)
  # Let's keep this neat
  names(output$imp_ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}

Add_ImpSpatID_Monty<-function(dframe){
  
  output<-sapply(dframe$imp_spat_rowname,function(rnm){
  list(data.frame(
    imp_spat_ID="GO-ADM0-World-shp",
    imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
    imp_spat_colname="iso3",
    imp_spat_rowname=rnm
  )%>%distinct())},simplify = T)
  # Let's keep this neat
  row.names(output)<-NULL
  
  return(output)
  
}

Add_ImpSpatAll_Monty<-function(ID_linkage,spatial_info,source){
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res,imp_spat_fileread
  lapply(unique(ID_linkage$imp_sub_ID),function(ID){
    # Set out only the entries that we need
    indy<-ID_linkage$imp_sub_ID==ID
    # Extract the easier elements
    minout<-ID_linkage%>%filter(indy)%>%
      dplyr::select(imp_spat_ID,imp_spat_fileloc)%>%distinct()
    # Add the column and row specifier elements
    minout$imp_spat_colname<-list(ID_linkage$imp_spat_colname[indy])
    minout$imp_spat_rowname<-list(ID_linkage$imp_spat_rowname[indy])
    # Output
    output<-list()
    output$ID_linkage<-minout
    output$spatial_info<-spatial_info%>%filter(indy)%>%dplyr::select(
                       imp_ISO3s,imp_spat_covcode,imp_spat_res,imp_spat_resunits,
                       imp_spat_fileread,imp_spat_crs)
    output$source<-source%>%filter(indy)
    return(output)
  })
}

Add_hazIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,haz_sub_ID)%>%distinct()
  # Extract the external ID codes
  output$haz_ext_IDs<-sapply(output$haz_sub_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$haz_sub_ID==ID
    # Check for no IDs
    if(all(is.na(dframe$ext_ID[indy]))) return(list())
    # Highlight the external IDs that share the same Monty IDs 
    list(data.frame(ext_ID=dframe$ext_ID[indy],
                    ext_ID_db=dframe$ext_ID_db[indy], 
                    ext_ID_org=dframe$ext_ID_org[indy])%>%distinct())
  },simplify = T)
  # Let's keep this neat
  names(output$haz_ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}


Add_hazSpatAll_Monty<-function(ID_linkage,spatial_info,source){
  # multiple-entry rows: haz_spat_rowname,haz_spat_colname,haz_ISO3s,haz_spat_res,haz_spat_fileread
  lapply(ID_linkage$haz_sub_ID,function(ID){
    # Set out only the entries that we need
    indy<-ID_linkage$haz_sub_ID==ID
    # Extract the easier elements
    minout<-ID_linkage%>%filter(indy)%>%
      dplyr::select(haz_spat_ID,haz_spat_fileloc)%>%distinct()
    # Add the column and row specifier elements
    minout$haz_spat_colname<-list(ID_linkage$haz_spat_colname[indy])
    minout$haz_spat_rowname<-list(ID_linkage$haz_spat_rowname[indy])
    # Output
    output<-list()
    output$ID_linkage<-minout
    output$spatial_info<-spatial_info%>%filter(indy)%>%dplyr::select(
      haz_ISO3s,haz_spat_covcode,haz_spat_res,haz_spat_resunits,
      haz_spat_fileread,haz_spat_crs)
    output$source<-source%>%filter(indy)
    return(output)
  })
}







# Function to left_join the s@#t out of Monty
OverlapMonty<-function(MontyA,MontyB){
  # Check which MontyA events lie generally within the window of MontyB
  iiis<-
    as.Date(MontyA$event_Level$temporal$ev_sdate)>
    min(as.Date(MontyB$event_Level$temporal$ev_sdate))-10 &
    as.Date(MontyA$event_Level$temporal$ev_fdate)>
    max(as.Date(MontyB$event_Level$temporal$ev_fdate))+10
  # First reduce the crossover by hazard type (through the abbreviated hazard)
  allMatch<-mclapply(which(iiis),function(i){
    # First check for any overlapping hazards
    haz_Ab<-unique(MontyA$event_Level$allhaz_class[[i]]$all_hazs_Ab)
    # Find all indices worth pursuing
    indy<-vapply(1:length(MontyB$event_Level$allhaz_class),function(j) {
      any(haz_Ab%in%
            unique(MontyB$event_Level$allhaz_class[[j]]$all_hazs_Ab))
    },FUN.VALUE = logical(1))
    # Now let's check by overlapping dates
    indy<-indy & 
      as.Date(MontyA$event_Level$temporal$ev_sdate[i]) > 
      as.Date(MontyB$event_Level$temporal$ev_sdate)-10 &
      as.Date(MontyA$event_Level$temporal$ev_fdate[i]) <
      as.Date(MontyB$event_Level$temporal$ev_fdate)+10
    # Now lets check for overlapping continents or countries
    indy & 
      vapply(1:length(MontyB$event_Level$spatial$ev_ISO3s),function(j) {
        any(convIso3Continent_alt(MontyA$event_Level$spatial$ev_ISO3s[[i]]) %in% 
        convIso3Continent_alt(MontyB$event_Level$spatial$ev_ISO3s[[j]])) |
        any(MontyA$event_Level$spatial$ev_ISO3s[[i]] %in% 
        MontyB$event_Level$spatial$ev_ISO3s[[j]])
      },FUN.VALUE = logical(1)) %>%
      which()
  },mc.cores = 12)
  
  overlaps<-rep(F,length(iiis))
  overlaps[iiis]<-vapply(1:length(allMatch),function(k) any(allMatch[[k]]),logical(1))
  
}

# Merge multiple Monty objects, all gathered in a list
MergeMonty<-function(lMonty,jsoner=F){
  # Checks: check none of the Monty instances have empty event or impact data
  chk<-sapply(seq_along(lMonty),function(i){
    !is.null(lMonty[[i]]$event_Level) & nrow(lMonty[[i]]$event_Level)>0 &
      !is.null(lMonty[[i]]$impact_Data) & nrow(lMonty[[i]]$impact_Data)>0
  },simplify = T)
  if(any(!chk)) {
    warning(paste0("Issues with one of the Monty objects passed into MergeMonty, removing ",sum(!chk)," objects"))
    # If only one object (or less) remains after the checks, return it
    if(sum(chk)<2) return(lMonty[chk])
    # Reduce the list of objects
    lMonty<-lMonty[chk]
  }
  # Copy the first list element as a template
  Monty<-lMonty[[1]]
  #@@@@@@@@@@@@@@@ Merge the event-level data @@@@@@@@@@@@@@@#
  # Store out the data we need
  ID_linkage<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$event_Level$ID_linkage}))
  temporal<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$event_Level$temporal}))
  spatial<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$event_Level$spatial}))
  allhaz_class<-do.call(c,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$event_Level$allhaz_class}))
  # Replace the event_Level field of Monty instance 
  Monty$event_Level<-data.frame(indy=1:nrow(ID_linkage))
  Monty$event_Level$ID_linkage<-ID_linkage
  Monty$event_Level$temporal<-temporal
  Monty$event_Level$spatial<-spatial
  Monty$event_Level$allhaz_class<-allhaz_class
  Monty$event_Level$indy<-NULL
  #@@@@@@@@@@@@@@@ Merge the impact-level data @@@@@@@@@@@@@@@#
  # Store out the data we need
  ID_linkage<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$impact_Data$ID_linkage}))
  source<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$impact_Data$source}))
  impact_detail<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$impact_Data$impact_detail}))
  temporal<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$impact_Data$temporal}))
  spatial<-do.call(c,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$impact_Data$spatial}))
  # Replace the impact_Data field of Monty instance
  Monty$impact_Data<-data.frame(indy=1:nrow(ID_linkage))
  Monty$impact_Data$ID_linkage<-ID_linkage
  Monty$impact_Data$source<-source
  Monty$impact_Data$impact_detail<-impact_detail
  Monty$impact_Data$temporal<-temporal
  Monty$impact_Data$spatial<-spatial
  Monty$impact_Data$indy<-NULL
  #@@@@@@@@@@@@@@@ Merge the hazard-level data @@@@@@@@@@@@@@@#
  # Create a boolean array to check whether each list element has hazard data
  hasHaz<-sapply(seq_along(lMonty),function(i) length(lMonty[[i]]$hazard_Data)!=0,simplify = T)
  if(any(hasHaz)) {
    if(sum(hasHaz)==1){
      Monty$hazard_Data<-lMonty[[seq_along(lMonty)[hazHaz]]]$hazard_Data
    } else {
      # Only include elements that have hazard_Data
      tMonty<-lMonty[[seq_along(lMonty)[hazHaz]]]
      # Store out the data we need
      ID_linkage<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$ID_linkage}))
      source<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$source}))
      hazard_detail<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$hazard_detail}))
      temporal<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$temporal}))
      spatial<-do.call(c,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$spatial}))
      rm(tMonty)
      # Replace the impact_Data field of Monty instance
      Monty$hazard_Data<-data.frame(indy=1:nrow(ID_linkage))
      Monty$hazard_Data$ID_linkage<-ID_linkage
      Monty$hazard_Data$source<-source
      Monty$hazard_Data$hazard_detail<-hazard_detail
      Monty$hazard_Data$temporal<-temporal
      Monty$hazard_Data$spatial<-spatial
      Monty$hazard_Data$indy<-NULL
    }
  }
  #@@@@@@@@@@@@@@ Merge the response-level data @@@@@@@@@@@@@@#
  if(length(Monty$response_Data)!=0) {
    warning("Response-level data not ready to be merged")
  }
  
  # Source-related taxonomy information
  Monty$taxonomies$src_info<-do.call(rbind,lapply(seq_along(lMonty),function(i){
    lMonty[[i]]$taxonomies$src_info}))
  
  # Return it!
  if(jsoner) return(jsonlite::toJSON(Monty,pretty = T,auto_unbox = T)) else return(Monty)
}

# Split the Monty instance by event_IDs (evs)
SplitMonty<-function(Monty,evs,form=NULL,arranger=T){
  #@@@@@@@@@@@@@@@ Split the event-level data @@@@@@@@@@@@@@@#
  if(arranger) {
    indy<-Monty$event_Level$ID_linkage$event_ID%in%evs
    indy<-arrange(data.frame(id=seq_along(indy[indy]),
                             date=Monty$event_Level$temporal$ev_sdate[indy]),
                  date)%>%pull(id)
  } else indy<-Monty$event_Level$ID_linkage$event_ID%in%evs
  # Store out the data we need
  ID_linkage<-Monty$event_Level$ID_linkage[indy,]
  temporal<-Monty$event_Level$temporal[indy,]
  spatial<-Monty$event_Level$spatial[indy,]
  allhaz_class<-Monty$event_Level$allhaz_class[indy]
  # Replace the event_Level field of Monty instance 
  Monty$event_Level<-data.frame(indy=seq_along(indy))
  Monty$event_Level$ID_linkage<-ID_linkage
  Monty$event_Level$temporal<-temporal
  Monty$event_Level$spatial<-spatial
  Monty$event_Level$allhaz_class<-allhaz_class
  Monty$event_Level$indy<-NULL
  #@@@@@@@@@@@@@@@ Split the impact-level data @@@@@@@@@@@@@@@#
  if(arranger) {
    indy<-Monty$impact_Data$ID_linkage$event_ID%in%evs
    indy<-arrange(data.frame(id=seq_along(indy[indy]),
                             date=Monty$impact_Data$temporal$imp_sdate[indy]),
                  date)%>%pull(id)
  } else indy<-Monty$impact_Data$ID_linkage$event_ID%in%evs
  # Store out the data we need
  ID_linkage<-Monty$impact_Data$ID_linkage[indy,]
  source<-Monty$impact_Data$source[indy,]
  impact_detail<-Monty$impact_Data$impact_detail[indy,]
  temporal<-Monty$impact_Data$temporal[indy,]
  spatial<-Monty$impact_Data$spatial[indy,]
  # Replace the impact_Data field of Monty instance
  Monty$impact_Data<-data.frame(indy=seq_along(indy))
  Monty$impact_Data$ID_linkage<-ID_linkage
  Monty$impact_Data$source<-source
  Monty$impact_Data$impact_detail<-impact_detail
  Monty$impact_Data$temporal<-temporal
  Monty$impact_Data$spatial<-spatial
  Monty$impact_Data$indy<-NULL
  #@@@@@@@@@@@@@@@ Split the hazard-level data @@@@@@@@@@@@@@@#
  if(length(Monty$hazard_Data)!=0) {
    if(arranger) {
      indy<-Monty$hazard_Data$ID_linkage$event_ID%in%evs
      indy<-arrange(data.frame(id=seq_along(indy[indy]),
                               date=Monty$hazard_Data$temporal$haz_sdate[indy]),
                    date)%>%pull(id)
    } else indy<-Monty$hazard_Data$ID_linkage$event_ID%in%evs
    # Store out the data we need
    ID_linkage<-Monty$hazard_Data$ID_linkage[indy,]
    source<-Monty$hazard_Data$source[indy,]
    hazard_detail<-Monty$hazard_Data$hazard_detail[indy,]
    temporal<-Monty$hazard_Data$temporal[indy,]
    spatial<-Monty$hazard_Data$spatial[indy,]
    # Replace the impact_Data field of Monty instance
    Monty$hazard_Data<-data.frame(indy=seq_along(indy))
    Monty$hazard_Data$ID_linkage<-ID_linkage
    Monty$hazard_Data$source<-source
    Monty$hazard_Data$hazard_detail<-hazard_detail
    Monty$hazard_Data$temporal<-temporal
    Monty$hazard_Data$spatial<-spatial
    Monty$hazard_Data$indy<-NULL
  }
  #@@@@@@@@@@@@@@ Split the response-level data @@@@@@@@@@@@@@#
  if(length(Monty$response_Data)!=0) {
    warning("Response-level data not ready to be split")
  }
  # Return it!
  if(is.null(form)) return (Monty) 
  if(form=="json") return(jsonlite::toJSON(Monty,pretty = T,auto_unbox = T))
}

# Filter the Monty instance to find the appropriate eventids
FilterMontyEv<-function(Monty,sdate=NULL,fdate=NULL,ISO3=NULL,haz_Ab=NULL){
  # Setup the initial indices
  indy<-rep(T,length(Monty$event_Level$ID_linkage$event_ID))
  # Start date range NOTE WE TAKE THE LARGER SET OF POSSIBLE MATCHES VIA ev_fdate
  if(!is.null(sdate)) indy <- indy & Monty$event_Level$temporal$ev_fdate >= sdate
  # End date range NOTE WE TAKE THE LARGER SET OF POSSIBLE MATCHES VIA ev_sdate
  if(!is.null(fdate)) indy <- indy & Monty$event_Level$temporal$ev_sdate <= fdate
  # Abbreviated hazard code
  if(!is.null(haz_Ab)) indy <- indy & sapply(1:length(Monty$event_Level$allhaz_class),
                                             function(i){
                                               any(grepl(haz_Ab, Monty$event_Level$allhaz_class[[i]]$all_hazs_Ab, 
                                                         ignore.case = T))
                                             },simplify = T)
  # Country ISO3C code, out of any of the associated ISO3s
  if(!is.null(ISO3)) indy <- indy & sapply(1:length(Monty$event_Level$spatial$ev_ISO3s),
                                           function(i){
                                             any(grepl(ISO3, Monty$event_Level$spatial$ev_ISO3s[[i]], 
                                                       ignore.case = T))
                                           },simplify = T)
  # Return the event_IDs associated to the filter
  return(Monty$event_Level$ID_linkage$event_ID[indy])
}

# Perform checks on the filtering criteria provided to the Plumber API
CheckAPIinput<-function(sdate=NULL,fdate=NULL,ISO3=NULL,haz_Ab=NULL){
  # Check the sdate variable, which must be later than 1900
  if(!is.null(sdate)) if(!grepl("^[1,2][0,9]\\d{2}-[0,1]\\d{1}-[0,1,2,3]\\d{1}$",sdate))
    return(list(valid=F,message="Invalid start date input to the API, must be in YYYY-MM-DD format and later than 1900"))
  # Check the fdate variable, which must be later than 1900
  if(!is.null(fdate)) if(!grepl("^[1,2][0,9]\\d{2}-[0,1]\\d{1}-[0,1,2,3]\\d{1}$",fdate))
    return(list(valid=F,message="Invalid end date input to the API, must be in YYYY-MM-DD format and later than 1900"))
  # Check the ISO3C code
  if(!is.null(ISO3)) if(!grepl("^[A-Z]{3}$", ISO3, ignore.case = T))
    return(list(valid=F,message="Invalid ISO3 input to the API, must be a three-letter (A-Z) code conforming to ISO3C standard country codes"))
  # Check the abbreviated hazard
  if(!is.null(haz_Ab)) if(!grepl("^[A-Z]{2}$", haz_Ab, ignore.case = T))
    return(list(valid=F,message="Invalid abbreviated hazard input to the API, must be a two-letter (A-Z) code"))
  # Return safe message if nothing went wrong
  return(list(valid=T,message="Valid input to API"))
}

# Function: for JSON-based dataframes, with certain variables as nested lists
#           we convert these nested lists to characters and then paste them together
squishLDF<-function(DF){
  if(!is.null(ncol(DF[[1]]))) {
    return(do.call(rbind,lapply(seq_along(DF),function(i){
      apply(DF[[i]],2,function(x) paste0(unique(x),collapse=delim))
    })))
  } else {
    # Create a template for when there are no elements in the list
    return(unlist(lapply(seq_along(DF),function(i){
      if(length(DF[[i]])==0) return(list(""))
      sapply(DF[[i]],function(x) paste0(unique(x),collapse=delim))
    })))
  }
}

# Function: for JSON-based dataframes, with certain variables as nested lists
#           we left join to the given taxonomies and then use squishDF to merge together
JoinDFMonty<-function(DF,taxy,DFjoiner,taxyjoiner=NULL){
  # Conditional treatment based on the input class of DF
  if(class(DF)=="character") {
    # if DF is a character vector, convert to data.frame, left_join and output
    DF%<>%as.data.frame(); colnames(DF)<-taxyjoiner
    DF%>%left_join(taxy,by=taxyjoiner)%>%return()
  } else if (class(DF)=="data.frame"){
    # if DF is a data.frame and there are no nested lists, left_join and output
    if(class(DF[[1]])=="character"){
      colnames(DF)<-taxyjoiner
      DF%>%left_join(taxy,by=taxyjoiner)%>%return()
    } else if(class(DF[[1]])=="list" & length(DF)==1){
      DF<-DF[[1]]
      # if DF[[1]] is now a list of nested vectors, then convert to data.frame, left join, squish and then output
      do.call(rbind,lapply(seq_along(DF),function(i){
        tmp<-data.frame(DF[[i]]); colnames(tmp)<-taxyjoiner
        tmp%>%left_join(taxy,by=taxyjoiner)%>%
          apply(2,function(x) paste0(unique(x),collapse=delim))
      }))%>%as.data.frame()%>%return()
    } else stop("Attempting to join a data.frame of a nested-list with a Monty taxonomy, see JoinDFMonty function")
  } else if(class(DF)=="list"){
    # If DF is a list:
    if(class(DF[[1]])=="data.frame"){
      # if DF[[1]] is a data.frame, then left_join then squish then output
      lapply(seq_along(DF),function(i){
        # Join by the provided name: 'joiner'
        colnames(DF[[i]])[colnames(DF[[i]])==DFjoiner]<-taxyjoiner
        DF[[i]]%>%left_join(taxy,by=taxyjoiner)
      })%>%squishLDF()%>%as.data.frame()%>%return()
    } else if(class(DF[[1]])=="character"){
      # if DF[[1]] is a character vector, then convert to data.frame, left join, squish and then output
      do.call(rbind,lapply(seq_along(DF),function(i){
        tmp<-data.frame(DF[[i]]); colnames(tmp)<-taxyjoiner
        tmp%>%left_join(taxy,by=taxyjoiner)%>%
          apply(2,function(x) paste0(unique(x),collapse=delim))
      }))%>%as.data.frame()%>%return()
    } else stop("Attempting to join a nested-list object of inappropriate nested-class with a Monty taxonomy, see JoinDFMonty function")
  } else stop("Attempting to join an object of inappropriate class with a Monty taxonomy, see JoinDFMonty function")
}

SortConcurHaz<-function(concur){
  warning("SortConcurHar function returns empty column")
  lapply(seq_along(concur),function(i) return(data.frame(haz_concur="")))
}

Monty_Ev2Tab<-function(Monty,red=F){
  #%%%%%%%%%%%%%%%%%%%%%% EVENT LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  # IDs and linkages first
  ev_lv<-Monty$event_Level$ID_linkage[,c("event_ID","ev_name")]
  # Add the external IDs
  ev_lv%<>%cbind(squishLDF(Monty$event_Level$ID_linkage$all_ext_IDs))
  # Add the temporal information and general location variable
  ev_lv%<>%cbind(Monty$event_Level$temporal,
                 Monty$event_Level$spatial%>%dplyr::select(gen_location))
  # Add the ISOs
  ev_lv$ev_ISO3s<-squishLDF(Monty$event_Level$spatial$ev_ISO3s)
  # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
  ev_lv%<>%cbind(JoinDFMonty(Monty$event_Level$spatial%>%dplyr::select(ev_ISO3s), 
                             Monty$taxonomies$ISO_info,
                             "ev_ISO3s","ISO3")%>%dplyr::select(-ISO3))
  # Hazard classifications
  ev_lv%<>%cbind(squishLDF(Monty$event_Level$allhaz_class))
  # Hazard taxonomy from specific hazards
  ev_lv%<>%cbind(JoinDFMonty(Monty$event_Level$allhaz_class, 
                             Monty$taxonomies$haz_class,
                             "all_hazs_spec","haz_spec_code"))%>%
    dplyr::select(-c("all_hazs_Ab","all_hazs_spec"))%>%
    rename(haz_Ab=all_hazs_Ab)
  # If we want to reduce it to the barebones version
  if(red) {
    ev_lv%>%dplyr::select(-c(grep("_code",colnames(ev_lv),value = T)))%>%return()
  }
  
  return(ev_lv)
}

Monty_Imp2Tab<-function(Monty,red=F){
  #%%%%%%%%%%%%%%%%%%%%%% IMPACT LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  # IDs and linkages first
  imp_lv<-Monty$impact_Data$ID_linkage[,c("event_ID","imp_sub_ID")]
  # Hazard IDs
  imp_lv$haz_sub_ID<-squishLDF(Monty$impact_Data$ID_linkage$haz_sub_ID)
  # External IDs
  imp_lv%<>%cbind(squishLDF(Monty$impact_Data$ID_linkage$imp_ext_IDs))
  colnames(imp_lv)[grepl("ext_",colnames(imp_lv))]<-c("imp_ext_IDs","imp_extID_db","imp_extID_org")
  # Source, impact detail and temporal
  imp_lv%<>%cbind(Monty$impact_Data$source%>%dplyr::select(-c(imp_src_db,imp_src_org)),
                  Monty$impact_Data$impact_detail)%>%dplyr::select(-imp_src_URL)
  # Exposure classification
  imp_lv%<>%rename(exp_spec_code=exp_spec)%>%
    left_join(Monty$taxonomies$exp_class,
                      by="exp_spec_code")
  # Impact type
  imp_lv%<>%rename(imp_type_code=imp_type)%>%
    left_join(Monty$taxonomies$imp_class,
              by="imp_type_code")
  # Impact units
  imp_lv%<>%rename(unit_codes=imp_units)%>%
    left_join(Monty$taxonomies$units_info,
              by="unit_codes")%>%
    rename(imp_unit_code=unit_codes,imp_unit_lab=units_lab,
           imp_unitgroup_code=unit_groups_code,
           imp_unitgroup_lab=unit_groups_lab)
  # Estimate type
  imp_lv%<>%cbind(JoinDFMonty(Monty$impact_Data$impact_detail%>%dplyr::select(imp_est_type), 
                              Monty$taxonomies$est_type,
                              "imp_est_type","est_type_code")%>%
                    setNames(c("imp_esttype_code","imp_esttype_lab")))%>%dplyr::select(-imp_est_type)
  # Temporal information
  imp_lv%<>%cbind(Monty$impact_Data$temporal)
  # Organisation name and database
  imp_lv%<>%cbind(JoinDFMonty(Monty$impact_Data$source%>%dplyr::select(imp_src_db,imp_src_org), 
                              Monty$taxonomies$src_info,
                              c("imp_src_db","imp_src_org"),c("src_db_code","src_org_code")))
  # Sort out the column names for the source database information
  colnames(imp_lv)[grepl("src_db_",colnames(imp_lv))]<-
    str_replace_all(colnames(imp_lv)[grepl("src_db_",colnames(imp_lv))],"src_db_","imp_srcdb_")
  # Sort out the column names for the source organisation information 
  colnames(imp_lv)[grepl("src_org_",colnames(imp_lv))]<-
    str_replace_all(colnames(imp_lv)[grepl("src_org_",colnames(imp_lv))],"src_org_","imp_srcorg_")
  # Spatial ID linkages
  imp_lv%<>%cbind(do.call(rbind,lapply(Monty$impact_Data$spatial,function(x) {
    outy<-x$ID_linkage
    outy$imp_spat_colname%<>%squishLDF()
    outy$imp_spat_rowname%<>%squishLDF()
    return(outy)
  })))
  # Spatial object type
  imp_lv%<>%cbind(left_join(
    do.call(rbind,lapply(Monty$impact_Data$spatial,function(x) x$spatial_info)), 
    Monty$taxonomies$spatial_coverage%>%setNames(c("imp_spat_covcode","imp_spat_covlab")),
    by="imp_spat_covcode")%>%
                    dplyr::select("imp_spat_covcode","imp_spat_covlab","imp_spat_res","imp_spat_resunits","imp_spat_crs"))
  # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
  imp_lv%<>%cbind(left_join(
    do.call(rbind,lapply(Monty$impact_Data$spatial,function(x) x$spatial_info))%>%rename(ISO3=imp_ISO3s), 
    Monty$taxonomies$ISO_info,by="ISO3")%>%
                    dplyr::select("ISO3","Country","UNRegion","WorldBankRegions",
                                  "Continent","UNSubRegion","WorldBankIncomeGroups")%>%
                    setNames(c("imp_ISO3","imp_Country","imp_UNRegion","imp_WorldBankRegions",
                             "imp_Continent","imp_UNSubRegion","imp_WorldBankIncomeGroups")))
  # Spatial data source information 
  imp_lv%<>%cbind(left_join(
    do.call(rbind,lapply(Monty$impact_Data$spatial,function(x) x$source))%>%
      setNames(c("imp_spat_srcdbcode","imp_spat_srcdbURL","imp_spat_srcorgcode")), 
    Monty$taxonomies$src_info%>%
      setNames(str_replace_all(str_replace_all(colnames(Monty$taxonomies$src_info),"_",""),"src","imp_spat_src"))%>%
      dplyr::select(-imp_spat_srcdbURL),
      by=c("imp_spat_srcorgcode","imp_spat_srcdbcode")))
  # Re-order this all
  imp_lv%<>%dplyr::select(c("event_ID","imp_sub_ID","haz_sub_ID", #ID and linkages
                            "imp_ext_IDs","imp_extID_db","imp_extID_org",
                            # source
                            "imp_srcdb_code","imp_srcdb_lab",
                            "imp_srcorg_code","imp_srcorg_lab","imp_srcorg_typecode",
                            "imp_srcorg_typelab","imp_srcorg_email","imp_srcdb_attr",
                            "imp_srcdb_lic","imp_srcdb_URL","src_addinfo",
                            # impact detail
                            "exp_spec_code","exp_spec_lab","exp_subcat_code",
                            "exp_subcat_lab","exp_cat_code","exp_cat_lab",
                            "imp_value","imp_type_code","imp_type_lab",
                            "imp_unit_code","imp_unit_lab","imp_unitgroup_code",
                            "imp_unitgroup_lab","imp_esttype_code","imp_esttype_lab",
                            "imp_unitdate",
                            # temporal
                            "imp_sdate","imp_fdate",
                            # spatial
                            "imp_spat_ID","imp_ISO3","imp_Country","imp_UNRegion",
                            "imp_WorldBankRegions","imp_Continent","imp_UNSubRegion",
                            "imp_WorldBankIncomeGroups","imp_spat_fileloc",
                            "imp_spat_colname","imp_spat_rowname","imp_spat_covcode",
                            "imp_spat_covlab","imp_spat_res","imp_spat_resunits",
                            "imp_spat_crs","imp_spat_srcdbcode",
                            "imp_spat_srcdblab","imp_spat_srcdbURL",
                            "imp_spat_srcorgcode","imp_spat_srcorglab",
                            "imp_spat_srcorgtypecode","imp_spat_srcorgtypelab",
                            "imp_spat_srcorgemail","imp_spat_srcdbattr",
                            "imp_spat_srcdblic","imp_spat_srcaddinfo"))
  
  if(red) {
    imp_lv%>%dplyr::select(c("event_ID","imp_sub_ID","haz_sub_ID", #ID and linkages
                             "imp_ext_IDs","imp_extID_db","imp_extID_org",
                             # source
                             "imp_srcdb_lab","imp_srcorg_lab",
                             "imp_srcorg_typelab","imp_srcorg_email","imp_srcdb_attr",
                             "imp_srcdb_lic","imp_srcdb_URL","src_addinfo",
                             # impact detail
                             "exp_spec_lab","exp_subcat_lab","exp_cat_lab",
                             "imp_value","imp_type_lab","imp_unit_lab",
                             "imp_unitgroup_lab","imp_esttype_lab",
                             "imp_unitdate",
                             # temporal
                             "imp_sdate","imp_fdate",
                             # spatial
                             "imp_spat_ID","imp_ISO3","imp_Country","imp_UNRegion",
                             "imp_WorldBankRegions","imp_Continent","imp_UNSubRegion",
                             "imp_WorldBankIncomeGroups","imp_spat_fileloc",
                             "imp_spat_colname","imp_spat_rowname",
                             "imp_spat_covlab","imp_spat_res","imp_spat_resunits",
                             "imp_spat_crs","imp_spat_srcdblab","imp_spat_srcdbURL",
                             "imp_spat_srcorglab",
                             "imp_spat_srcorgtypelab",
                             "imp_spat_srcorgemail","imp_spat_srcdbattr",
                             "imp_spat_srcdblic","imp_spat_srcaddinfo"))%>%return()
  }
  return(imp_lv)
}

Monty_Haz2Tab<-function(Monty,red=F){
  #%%%%%%%%%%%%%%%%%%%%%% HAZARD LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  # IDs and linkages first
  haz_lv<-Monty$hazard_Data$ID_linkage[,c("event_ID","haz_sub_ID")]
  # External IDs
  haz_lv%<>%cbind(squishLDF(Monty$hazard_Data$ID_linkage$haz_ext_IDs))
  colnames(haz_lv)[grepl("ext_",colnames(haz_lv))]<-c("haz_ext_IDs","haz_extID_db","haz_extID_org")
  # Source, impact detail and temporal
  haz_lv%<>%cbind(Monty$hazard_Data$source%>%dplyr::select(-c(haz_src_db,haz_src_org)),
                  Monty$hazard_Data$hazard_detail%>%
                    dplyr::select(-c(all_hazs_spec))%>%
                    rename(haz_Ab=all_hazs_Ab))%>%
    dplyr::select(-haz_src_URL)
  # Hazard classification
  haz_lv%<>%cbind(JoinDFMonty(Monty$hazard_Data$hazard_detail$all_hazs_spec,
                              Monty$taxonomies$haz_class,"haz_spec_code","haz_spec_code"))
  # Concurrent hazards
  warning("We return an empty concurrent hazard data.frame")
  haz_lv%<>%dplyr::select(-concur_haz)
  # Impact units
  haz_lv%<>%rename(unit_codes=haz_maxunits)%>%
    left_join(Monty$taxonomies$units_info,
              by="unit_codes")%>%
    rename(haz_maxunit_code=unit_codes,haz_maxunit_lab=units_lab,
           haz_maxunitgroup_code=unit_groups_code,
           haz_maxunitgroup_lab=unit_groups_lab)
  # Estimate type
  haz_lv%<>%cbind(JoinDFMonty(Monty$hazard_Data$hazard_detail%>%dplyr::select(haz_est_type), 
                              Monty$taxonomies$est_type,
                              "haz_est_type","est_type_code")%>%
                    setNames(c("haz_esttype_code","haz_esttype_lab")))%>%dplyr::select(-haz_est_type)
  # Temporal information
  haz_lv%<>%cbind(Monty$hazard_Data$temporal)
  # Organisation name and database
  haz_lv%<>%cbind(JoinDFMonty(Monty$hazard_Data$source%>%dplyr::select(haz_src_db), 
                              Monty$taxonomies$src_info,
                              "haz_src_db","src_db_code"))
  # Sort out the column names for the source database information
  colnames(haz_lv)[grepl("src_db_",colnames(haz_lv))]<-
    str_replace_all(colnames(haz_lv)[grepl("src_db_",colnames(haz_lv))],"src_db_","haz_srcdb_")
  # Sort out the column names for the source organisation information 
  colnames(haz_lv)[grepl("src_org_",colnames(haz_lv))]<-
    str_replace_all(colnames(haz_lv)[grepl("src_org_",colnames(haz_lv))],"src_org_","haz_srcorg_")
  # Spatial ID linkages
  haz_lv%<>%cbind(do.call(rbind,lapply(Monty$hazard_Data$spatial,function(x) {
    outy<-x$ID_linkage
    outy$haz_spat_colname%<>%replace(is.na(.),"")%>%squishLDF()
    outy$haz_spat_rowname%<>%replace(is.na(.),"")%>%squishLDF()
    return(outy)
  })))
  # Spatial object type
  haz_lv%<>%cbind(left_join(
    do.call(rbind,lapply(Monty$hazard_Data$spatial,function(x) x$spatial_info)),
                              Monty$taxonomies$spatial_coverage%>%setNames(c("haz_spat_covcode","haz_spat_covlab")),
    by="haz_spat_covcode")%>%
      dplyr::select("haz_spat_covcode","haz_spat_covlab","haz_spat_res","haz_spat_resunits","haz_spat_crs"))
  # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
  haz_lv%<>%cbind(left_join(
    do.call(rbind,lapply(Monty$hazard_Data$spatial,function(x) x$spatial_info))%>%rename(ISO3=haz_ISO3s), 
    Monty$taxonomies$ISO_info,by="ISO3")%>%
      dplyr::select("ISO3","Country","UNRegion","WorldBankRegions",
                    "Continent","UNSubRegion","WorldBankIncomeGroups")%>%
      setNames(c("haz_ISO3","haz_Country","haz_UNRegion","haz_WorldBankRegions",
                 "haz_Continent","haz_UNSubRegion","haz_WorldBankIncomeGroups")))
  # Re-order this all
  haz_lv%<>%dplyr::select(c("event_ID","haz_sub_ID", #ID and linkages
                            "haz_ext_IDs","haz_extID_db","haz_extID_org",
                            # source
                            "haz_srcdb_code","haz_srcdb_lab",
                            "haz_srcorg_code","haz_srcorg_lab","haz_srcorg_typecode",
                            "haz_srcorg_typelab","haz_srcorg_email","haz_srcdb_attr",
                            "haz_srcdb_lic","haz_srcdb_URL","src_addinfo",
                            # hazard detail
                            "haz_Ab","haz_spec_code",
                            "haz_spec_lab","haz_cluster_code","haz_cluster_lab",
                            "haz_type_code","haz_type_lab",
                            "haz_maxvalue","haz_maxunit_code","haz_maxunit_lab",
                            "haz_maxunitgroup_code","haz_maxunitgroup_lab",
                            "haz_esttype_code","haz_esttype_lab",
                            # temporal
                            "haz_sdate","haz_fdate",
                            # spatial
                            "haz_spat_ID","haz_ISO3","haz_Country","haz_UNRegion",
                            "haz_WorldBankRegions","haz_Continent","haz_UNSubRegion",
                            "haz_WorldBankIncomeGroups","haz_spat_fileloc",
                            "haz_spat_colname","haz_spat_rowname","haz_spat_covcode",
                            "haz_spat_covlab","haz_spat_res","haz_spat_resunits",
                            "haz_spat_crs"))
  
  if(red) {
    haz_lv%>%dplyr::select(c("event_ID","haz_sub_ID", #ID and linkages
                             "haz_ext_IDs","haz_extID_db","haz_extID_org",
                             # source
                             "haz_srcdb_lab","haz_srcorg_lab",
                             "haz_srcorg_typelab","haz_srcorg_email","haz_srcdb_attr",
                             "haz_srcdb_lic","haz_srcdb_URL","src_addinfo",
                             # hazard detail
                             "haz_Ab","haz_spec_code","haz_spec_lab","haz_cluster_lab",
                             "haz_type_lab","haz_maxvalue","haz_maxunit_lab",
                             "haz_maxunitgroup_lab","haz_esttype_lab",
                             # temporal
                             "haz_sdate","haz_fdate",
                             # spatial
                             "haz_spat_ID","haz_ISO3","haz_Country","haz_UNRegion",
                             "haz_WorldBankRegions","haz_Continent","haz_UNSubRegion",
                             "haz_WorldBankIncomeGroups","haz_spat_fileloc",
                             "haz_spat_colname","haz_spat_rowname",
                             "haz_spat_covlab","haz_spat_res","haz_spat_resunits",
                             "haz_spat_crs"))%>%return()
  }
  return(haz_lv)
}

ConvMonty2Tabs<-function(Monty,red=F){
  #%%%%%%%%%%%%%%%%%%%%%% EVENT LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  ev_lv<-Monty_Ev2Tab(Monty,red=red)
  #%%%%%%%%%%%%%%%%%%%%% IMPACT LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  imp_lv<-Monty_Imp2Tab(Monty,red=red)
  
  # The minimum to be returned:
  outer<-list(event_Level=ev_lv,impact_Level=imp_lv)
  
  #%%%%%%%%%%%%%%%%%%%%% HAZARD LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  if(length(Monty$hazard_Data)>0) outer$hazard_Level<-Monty_Haz2Tab(Monty,red=red)
  
  return(outer)
  
}


# Sort EM-DAT & GIDD data first?
# Match by event id minus the day
# Make tabular data out of this matched Monty object
# Attach the impact admin boundary data (ADM-0 for now)
# For hazard data, extract from those with GDACS data directly







