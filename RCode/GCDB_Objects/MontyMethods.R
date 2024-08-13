taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
imp_class<-taxies%>%filter(list_name=="imp_type")%>%dplyr::select(2:3)%>%
  rename("imp_type_code"="name","imp_type_lab"="label")
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
units_class<-taxies%>%filter(list_name=="measunits")%>%dplyr::select(2:3)%>%
  rename("units_code"="name","units_lab"="label")

GetMonty_ID<-function(DF,haz=NULL) {
  # In case a specific hazard is fed in
  if(!is.null(haz)) DF%<>%mutate(haz_Ab=haz)
  # When more than one iso code is given
  if(is.list(DF$ev_ISO3s)) DF$ev_ISO3s<-unname(unlist(parallel::mclapply(DF$ev_ISO3s,paste0,collapse=":",mc.cores=ncores)))
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
  # For empty spatial IDs
  if(any(colnames(impies)=="imp_spat_ID")) impies$tmp<-unlist(lapply(impies$imp_spat_ID, function(x) paste0(x,collapse=":")))
  # Compile the ID
  impies$imp_sub_ID<-impies%>%dplyr::select(dplyr::any_of(c("event_ID","imp_src_db","exp_spec","imp_type","imp_sdate","imp_fdate","imp_units","tmp","imp_lon","imp_lat")))%>%
    mutate(imp_src_db=stringr::str_remove(stringi::stri_trans_totitle(imp_src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  # Make sure that no duplicates exist...
  impies%>%group_by(imp_sub_ID)%>%
    mutate(imp_sub_ID=paste0(imp_sub_ID,"_",1:length(imp_sub_ID)))%>%
    dplyr::select(-any_of(c("tmp")))%>%as.data.frame()
}

GetGCDB_hazID<-function(hazzies){
  # For empty spatial IDs
  if(any(colnames(hazzies)=="haz_spat_ID")) hazzies$tmp<-unlist(lapply(hazzies$haz_spat_ID, function(x) paste0(x,collapse=":")))
  # Compile the ID
  hazzies$haz_sub_ID<-hazzies%>%dplyr::select(dplyr::any_of(c("event_ID","haz_src_db","haz_cluster","haz_spec","haz_sdate","haz_fdate","haz_units","tmp","haz_lon","haz_lat")))%>%
    mutate(haz_src_db=stringr::str_remove(stringi::stri_trans_totitle(haz_src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  # Make sure that no duplicates exist...
  hazzies%>%group_by(haz_sub_ID)%>%
    mutate(haz_sub_ID=paste0(haz_sub_ID,"_",1:length(haz_sub_ID)))%>%
    dplyr::select(-any_of(c("tmp")))%>%as.data.frame()
}

GetGCDB_imp_spatID<-function(impies){
  impies[is.na(impies)]<-""
  impies%>%dplyr::select(imp_spat_srcorg,imp_spat_srcdb,imp_spat_covcode,imp_spat_res,imp_spat_resunits)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
}

GetGCDB_haz_spatID<-function(impies){
  if(all(is.na(impies$haz_spat_res))) impies$haz_spat_res<-0
  impies[is.na(impies)]<-""
  impies%>%dplyr::select(haz_spat_srcorg,haz_spat_srcdb,haz_spat_covcode,haz_spat_res,haz_spat_resunits)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
}

# A function to go from a series of data into the JSON-accepted DF-list style object
Add_EvIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-do.call(rbind,parallel::mclapply(unique(dframe$event_ID),function(ID){
    dframe%>%filter(event_ID==ID)%>%
    reframe(event_ID=ID,ev_name=ev_name[1])%>%
      distinct()
  },mc.cores=ncores))
  # Generate all the elements of the dataset
  output$all_ext_IDs<-parallel::mclapply(output$event_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$event_ID==ID
    # check for no external IDs
    if(any(is.na(dframe$ext_ID[indy]))) 
      return(data.frame(ext_ID=NA_character_,
                             ext_ID_db=NA_character_, 
                             ext_ID_org=NA_character_))
    # Highlight the external IDs that share the same Monty IDs 
    data.frame(ext_ID=dframe$ext_ID[indy],
               ext_ID_db=dframe$ext_ID_db[indy], 
               ext_ID_org=dframe$ext_ID_org[indy])%>%distinct()
  },mc.cores=ncores)
  # Let's keep this neat
  names(output$all_ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}

Add_EvSpat_Monty<-function(dframe){
  # Setup the other entries
  output<-do.call(rbind,parallel::mclapply(unique(dframe$event_ID),function(ID){
    dframe%>%filter(event_ID==ID)%>%
      reframe(event_ID=ID,gen_location=paste0('\"',unique(gen_location),collapse = '\",\"','\"'))
  },mc.cores=ncores))
  # and the ISO3C codes
  output$ev_ISO3s<-parallel::mclapply(output$event_ID,function(ID){
    # return the ISO3C codes in a list
    unique(dframe$ev_ISO3s[dframe$event_ID==ID])
  },mc.cores=ncores)
  # Gimme gimme gimme
  return(output%>%dplyr::select(-event_ID))
}

Add_EvTemp_Monty<-function(dframe){
  # Take the event start date as the minimum and maximum dates
  do.call(rbind,parallel::mclapply(unique(dframe$event_ID),function(ID){
    tryCatch(dframe%>%filter(event_ID==ID)%>%
      reframe(ev_sdate=as.character(min(as.Date(ev_sdate),na.rm = T)),
            ev_fdate=as.character(max(as.Date(ev_fdate),na.rm = T))),
      error=function(e) {
        tmp<-dframe%>%filter(event_ID==ID)
        warning(paste0("ev_sdate=",paste0(tmp$ev_sdate,collapse = " : "),
                       ".    ev_fdate=",paste0(tmp$ev_fdate,collapse = " : ")))
        dframe%>%dplyr::select(ev_sdate,ev_fdate)%>%slice(1)
      })
  },mc.cores=ncores))
}

Add_EvHazTax_Monty<-function(dframe){
  # Extract all of the haz_spec codes and output a list
  parallel::mclapply(unique(dframe$event_ID),function(ev){
    # indices
    indy<-which(dframe$event_ID==ev)
    
    return(list(all_hazs_Ab=str_replace_all(unique(as.character(unname(unlist(str_split(dframe$haz_Ab[indy],":",simplify = T)))))," ",""),
                all_hazs_spec=str_replace_all(unique(as.character(unname(unlist(str_split(dframe$haz_spec[indy],":",simplify = T)))))," ","")))
  },mc.cores=ncores)
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
  
  output<-dframe%>%dplyr::select(event_ID,haz_maxvalue,haz_maxunits,haz_est_type)%>%distinct()
  # Extract all of the haz_Ab codes and output a list
  output$all_hazs_Ab<-parallel::mclapply(output$event_ID,function(ev){
    as.character(unlist(gsub(" ", "",str_split(dframe$haz_Ab[dframe$event_ID==ev],":",simplify = T))))
  },mc.cores=ncores)
  # Extract all of the haz_spec codes and output a list
  output$all_hazs_spec<-parallel::mclapply(output$event_ID,function(ev){
    as.character(unlist(gsub(" ", "",str_split(dframe$haz_spec[dframe$event_ID==ev],":",simplify = T))))
  },mc.cores=ncores)
  
  return(output%>%dplyr::select(-event_ID))
}

# A function to form the impact IDlinkage field
Add_ImpIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,imp_sub_ID)%>%distinct()
  # Generate all the elements of the dataset
  output$haz_sub_ID<-parallel::mclapply(output$imp_sub_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$imp_sub_ID==ID
    # check for no haz_sub_imp values
    if(any(is.na(dframe$haz_sub_ID[indy]))) return(list())
    # Highlight the external IDs that share the same Monty IDs 
    list(unique(dframe$haz_sub_ID[indy]))
  },mc.cores=ncores)
  # Now let's add the external IDs
  output$imp_ext_IDs<-parallel::mclapply(output$imp_sub_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$imp_sub_ID==ID
    # Check for no IDs
    if(all(is.na(dframe$ext_ID[indy]))) 
      return(list(data.frame(ext_ID=rep(NA_character_,sum(indy)),
                           ext_ID_db=rep(NA_character_,sum(indy)), 
                           ext_ID_org=rep(NA_character_,sum(indy)))%>%distinct()))
    # Highlight the external IDs that share the same Monty IDs 
    list(data.frame(ext_ID=dframe$ext_ID[indy],
                    ext_ID_db=dframe$ext_ID_db[indy], 
                    ext_ID_org=dframe$ext_ID_org[indy])%>%distinct())
  },mc.cores=ncores)
  # Output that bea-u-t
  return(output)
}

Add_ImpSpatID_Monty<-function(dframe){
  stop("This is wrong: Add_ImpSpatID_Monty")
  output<-sapply(dframe$imp_spat_rowname,function(rnm){
  list(data.frame(
    imp_spat_ID="GO-ADM0-World-shp",
    imp_spat_fileloc="https://go-user-library.ifrc.org/maps"
  )%>%distinct())},simplify = T)
  # Let's keep this neat
  row.names(output)<-NULL
  
  return(output)
  
}

Add_ImpSpatAll_Monty<-function(ID_linkage,spatial_info,source){
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res
  parallel::mclapply(unique(ID_linkage$imp_sub_ID),function(ID){
    # Set out only the entries that we need
    indy<-ID_linkage$imp_sub_ID==ID
    # Extract the easier elements
    minout<-ID_linkage%>%filter(indy)%>%
      dplyr::select(imp_spat_ID,imp_spat_fileloc)%>%distinct()
    # Output
    output<-list()
    output$ID_linkage<-minout
    output$spatial_info<-spatial_info%>%filter(indy)%>%dplyr::select(
                       imp_ISO3s,imp_spat_covcode,imp_spat_res,imp_spat_resunits,
                       imp_spat_crs)
    output$source<-source%>%filter(indy)
    return(output)
  },mc.cores=ncores)
}

Add_hazIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,haz_sub_ID)%>%distinct()
  # Extract the external ID codes
  output$haz_ext_IDs<-parallel::mclapply(output$haz_sub_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$haz_sub_ID==ID
    # Check for no IDs
    if(all(is.na(dframe$ext_ID[indy]))) return(NULL)
    # Highlight the external IDs that share the same Monty IDs 
    data.frame(ext_ID=dframe$ext_ID[indy],
                    ext_ID_db=dframe$ext_ID_db[indy], 
                    ext_ID_org=dframe$ext_ID_org[indy])%>%distinct()
  },mc.cores=ncores)
  # Let's keep this neat
  names(output$haz_ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}


Add_hazSpatAll_Monty<-function(ID_linkage,spatial_info,source){
  # multiple-entry rows: haz_spat_rowname,haz_spat_colname,haz_ISO3s,haz_spat_res
  parallel::mclapply(ID_linkage$haz_sub_ID,function(ID){
    # Set out only the entries that we need
    indy<-ID_linkage$haz_sub_ID==ID
    # Extract the easier elements
    minout<-ID_linkage%>%filter(indy)%>%
      dplyr::select(haz_spat_ID,haz_spat_fileloc)%>%distinct()
    # Output
    output<-list()
    output$ID_linkage<-minout
    output$spatial_info<-spatial_info%>%filter(indy)%>%dplyr::select(
      haz_ISO3s,haz_spat_covcode,haz_spat_res,haz_spat_resunits,
      haz_spat_crs)
    output$source<-source%>%filter(indy)
    return(output)
  },mc.cores=ncores)
}

AddEmptyColImp<-function(DF){
  for(i in which(!names(col_tabGCDB)%in%colnames(DF))){
    tmp<-NA
    class(tmp)<-col_tabGCDB[i]
    DF$tmp<-tmp
    colnames(DF)[ncol(DF)]<-names(col_tabGCDB)[i]
  }
  DF[,names(col_tabGCDB)]
}

# Convert from a database's impact taxonomy to ours
ImpLabs<-function(ImpDB,nomDB="Desinventar",dropName=T){
  # Open up the database impact taxonomy conversion file
  imptax<-openxlsx::read.xlsx("./Taxonomies/ConvertFromDatabases/ConvertImpact_Taxonomy.xlsx")%>%
    filter(imp_src_db==nomDB)
  # Find where the Desinventar data impact estimates stop 
  vlim<-which(colnames(ImpDB)%in%imptax$VarName)
  # For all columns that correspond to impact estimates, return the data
  ImpDB%<>%reshape2::melt(measure.vars=colnames(ImpDB)[vlim])%>%
    mutate(VarName=as.character(variable),imp_value=as.numeric(value))%>%
    dplyr::select(-c(variable,value))%>%
    left_join(dplyr::select(imptax,-c("imp_src_orgtype","imp_src_org","imp_src_db")),by="VarName")
  # Spit it out!
  if(dropName) return(ImpDB) else return(dplyr::select(ImpDB,-VarName))
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
  allMatch<-parallel::mclapply(which(iiis),function(i){
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
    lMonty[[i]]$event_Level$temporal%>%mutate(across(where(~ !is.character(.x)),as.character))}))
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
    lMonty[[i]]$impact_Data$temporal%>%mutate(across(where(~ !is.character(.x)),as.character))}))
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
      Monty$hazard_Data<-lMonty[[seq_along(lMonty)[which(hasHaz)]]]$hazard_Data
    } else {
      # Only include elements that have hazard_Data
      tMonty<-lMonty[seq_along(lMonty)[hasHaz]]
      # Store out the data we need
      ID_linkage<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$ID_linkage}))
      source<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$source}))
      hazard_detail<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$hazard_detail}))
      temporal<-do.call(rbind,lapply(seq_along(tMonty),function(i){
        tMonty[[i]]$hazard_Data$temporal%>%mutate(across(where(~ !is.character(.x)),as.character))}))
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
    lMonty[[i]]$taxonomies$src_info}))%>%distinct()
  
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
  spatial<-Monty$impact_Data$spatial[indy]
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
    spatial<-Monty$hazard_Data$spatial[indy]
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

MFilter_Events<-function(Monty,indy,allobjs=T){
  # First of all, save out all of the event IDs to be kept later
  evs<-unique(Monty$event_Level$ID_linkage$event_ID[indy])
  # Store out the data we need
  ID_linkage<-Monty$event_Level$ID_linkage[indy,]
  temporal<-Monty$event_Level$temporal[indy,]
  spatial<-Monty$event_Level$spatial[indy,]
  allhaz_class<-Monty$event_Level$allhaz_class[indy]
  # Replace the event_Level field of Monty instance
  if(!is.logical(indy)) indy<-rep(T,length(indy))
  Monty$event_Level<-data.frame(indy=which(indy))
  Monty$event_Level$ID_linkage<-ID_linkage
  Monty$event_Level$temporal<-temporal
  Monty$event_Level$spatial<-spatial
  Monty$event_Level$allhaz_class<-allhaz_class
  Monty$event_Level$indy<-NULL
  # If we also want to remove the associated eventIDs from all objects (events, hazards, ...)
  if(allobjs){
    if(length(Monty$impact_Data)!=0) Monty%<>%MFilter_Impacts(indy=Monty$impact_Data$ID_linkage$event_ID%in%evs)
    if(length(Monty$hazard_Data)!=0) Monty%<>%MFilter_Hazards(indy=Monty$hazard_Data$ID_linkage$event_ID%in%evs)
    if(length(Monty$response_Data)!=0) Monty%<>%MFilter_Responses(indy=Monty$response_Data$ID_linkage$event_ID%in%evs)
  }
  
  return(Monty)
}

MFilter_Impacts<-function(Monty,indy,allobjs=F){
  # First of all, save out all of the event IDs to be kept later
  evs<-unique(Monty$impact_Data$ID_linkage$event_ID[indy])
  # Store out the data we need
  ID_linkage<-Monty$impact_Data$ID_linkage[indy,]
  source<-Monty$impact_Data$source[indy,]
  impact_detail<-Monty$impact_Data$impact_detail[indy,]
  temporal<-Monty$impact_Data$temporal[indy,]
  spatial<-Monty$impact_Data$spatial[indy,]
  # Replace the impact_Data field of Monty instance
  if(!is.logical(indy)) indy<-rep(T,length(indy))
  Monty$impact_Data<-data.frame(indy=which(indy))
  Monty$impact_Data$ID_linkage<-ID_linkage
  Monty$impact_Data$source<-source
  Monty$impact_Data$impact_detail<-impact_detail
  Monty$impact_Data$temporal<-temporal
  Monty$impact_Data$spatial<-spatial
  Monty$impact_Data$indy<-NULL
  # If we also want to remove the associated eventIDs from all objects (events, hazards, ...)
  if(allobjs){
    Monty%<>%MFilter_Events(indy=Monty$event_Level$ID_linkage$event_ID%in%evs)
    if(length(Monty$hazard_Data)!=0) Monty%<>%MFilter_Hazards(indy=Monty$hazard_Data$ID_linkage$event_ID%in%evs)
    if(length(Monty$response_Data)!=0) Monty%<>%MFilter_Responses(indy=Monty$response_Data$ID_linkage$event_ID%in%evs)
  }
  
  return(Monty)
}

MFilter_Hazards<-function(Monty,indy,allobjs=F){
  # First of all, save out all of the event IDs to be kept later
  evs<-unique(Monty$hazard_Data$ID_linkage$event_ID[indy])
  # Store out the data we need
  ID_linkage<-Monty$hazard_Data$ID_linkage[indy,]
  source<-Monty$hazard_Data$source[indy,]
  hazard_detail<-Monty$hazard_Data$hazard_detail[indy,]
  temporal<-Monty$hazard_Data$temporal[indy,]
  spatial<-Monty$hazard_Data$spatial[indy,]
  # Replace the hazard_Data field of Monty instance
  if(!is.logical(indy)) indy<-rep(T,length(indy))
  Monty$hazard_Data<-data.frame(indy=which(indy))
  Monty$hazard_Data$ID_linkage<-ID_linkage
  Monty$hazard_Data$source<-source
  Monty$hazard_Data$hazard_detail<-hazard_detail
  Monty$hazard_Data$temporal<-temporal
  Monty$hazard_Data$spatial<-spatial
  Monty$hazard_Data$indy<-NULL
  # If we also want to remove the associated eventIDs from all objects (events, hazards, ...)
  if(allobjs){
    Monty%<>%MFilter_Events(indy=Monty$event_Level$ID_linkage$event_ID%in%evs)
    if(length(Monty$impact_Data)!=0) Monty%<>%MFilter_Impacts(indy=Monty$impact_Data$ID_linkage$event_ID%in%evs)
    if(length(Monty$response_Data)!=0) Monty%<>%MFilter_Responses(indy=Monty$response_Data$ID_linkage$event_ID%in%evs)
  }
  
  return(Monty)
}

MFilter_Responses<-function(Monty,indy,allobjs=F){
  # First of all, save out all of the event IDs to be kept later
  evs<-unique(Monty$response_Data$ID_linkage$event_ID[indy])
  # Store out the data we need
  ID_linkage<-Monty$response_Data$ID_linkage[indy,]
  source<-Monty$response_Data$source[indy,]
  response_detail<-Monty$response_Data$response_detail[indy,]
  temporal<-Monty$response_Data$temporal[indy,]
  spatial<-Monty$response_Data$spatial[indy]
  # Replace the response_Data field of Monty instance
  if(!is.logical(indy)) indy<-rep(T,length(indy))
  Monty$response_Data<-data.frame(indy=which(indy))
  Monty$response_Data$ID_linkage<-ID_linkage
  Monty$response_Data$source<-source
  Monty$response_Data$response_detail<-response_detail
  Monty$response_Data$temporal<-temporal
  Monty$response_Data$spatial<-spatial
  Monty$response_Data$indy<-NULL
  # If we also want to remove the associated eventIDs from all objects (events, hazards, ...)
  if(allobjs){
    Monty%<>%MFilter_Events(indy=Monty$event_Level$ID_linkage$event_ID%in%evs)
    if(length(Monty$impact_Data)!=0) Monty%<>%MFilter_Impacts(indy=Monty$impact_Data$ID_linkage$event_ID%in%evs)
    if(length(Monty$hazard_Data)!=0) Monty%<>%MFilter_Hazards(indy=Monty$hazard_Data$ID_linkage$event_ID%in%evs)
  }
  
  return(Monty)
} 

modSpatMonty<-function(ID_linkage,spatial_info,source){
  # First perform a simple check
  if(length(ID_linkage)!=length(spatial_info) | length(ID_linkage)!=length(source)) stop("Error in spatial data dimensionality")
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res
  lapply(1:length(ID_linkage),function(i){
    output<-list()
    output$ID_linkage<-ID_linkage[[i]]
    output$spatial_info<-spatial_info[[i]]
    output$source<-source[[i]]
    return(output)
  })
}

# Function: for JSON-based dataframes, with certain variables as nested lists
#           we convert these nested lists to characters and then paste them together
# squishLDF<-function(DF){
#   if(!is.null(ncol(DF[[1]]))) {
#     return(do.call(rbind,lapply(seq_along(DF),function(i){
#       if(ncol(DF[[i]])==0) return(data.frame(ext_ID=NA_character_,ext_ID_db=NA_character_,ext_ID_org=NA_character_))
#       apply(DF[[i]],2,function(x) {
#         paste0(unique(x),collapse=delim)
#       })
#     })))
#   } else if (class(DF[[1]])=="list" & 
#              all(sapply(seq_along(DF),function(i) length(DF[[i]])%in%0:1))){
#     return(do.call(rbind,lapply(DF,function(x){
#       if(length(x)==0){
#         return(data.frame(ext_ID=NA_character_,ext_ID_db=NA_character_,ext_ID_org=NA_character_))
#       } else if (length(x)==1){
#         return(x[[1]])
#       } else stop("length of array not compatible for squishLDF")
#     })))
#   } else {
#     # Create a template for when there are no elements in the list
#     return(unlist(lapply(seq_along(DF),function(i){
#       if(length(DF[[i]])==0) return(list(""))
#       sapply(DF[[i]],function(x) paste0(unique(x),collapse=delim))
#     })))
#   }
# }
squishLDF<-function(DF){
  if(!is.null(ncol(DF[[1]]))) {
    return(do.call(rbind,lapply(seq_along(DF),function(i){
      if(ncol(DF[[i]])==0) return(data.frame(ext_ID=NA_character_,ext_ID_db=NA_character_,ext_ID_org=NA_character_))
      apply(DF[[i]],2,function(x) {
        paste0(unique(x),collapse=delim)
      })
    })))
  } else if (class(DF[[1]])=="list" & 
             all(sapply(seq_along(DF),function(i) length(DF[[i]])==0))){
    return(rep(NA_character_,length(DF)))
  }  else if (class(DF[[1]])=="list" & 
              all(sapply(seq_along(DF),function(i) length(DF[[i]])==1)) & 
              all(sapply(seq_along(DF),function(i) class(DF[[i]][[1]])=="data.frame"))){
    return(do.call(rbind,lapply(DF,function(x){
      if(length(x)==0 | any(is.na(x))){
        return(data.frame(ext_ID=NA_character_,ext_ID_db=NA_character_,ext_ID_org=NA_character_))
      } else if (length(x)==1){
        return(x[[1]])
      } else stop("length of array not compatible for squishLDF")
    })))
  } else if (class(unlist(DF[[1]]))=="list" &
             all(sapply(seq_along(DF),function(i) length(unlist(DF[[i]]))%in%0:1))){
    return(do.call(rbind,lapply(DF,function(x){
      if(length(x)==0 | any(is.na(x))){
        return(data.frame(ext_ID=NA_character_,ext_ID_db=NA_character_,ext_ID_org=NA_character_))
      } else if (length(x)==1){
        return(x[[1]])
      } else stop("length of array not compatible for squishLDF")
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
      do.call(rbind,parallel::mclapply(seq_along(DF),function(i){
        left_join(as.data.frame(DF[[i]]),taxy,
                  by=join_by(!!sym(DFjoiner)==!!sym(taxyjoiner)))%>%
          apply(2,function(x) paste0(unique(x),collapse=delim))
      },mc.cores=ncores))%>%as.data.frame()%>%return()
    } else stop("Attempting to join a data.frame of a nested-list with a Monty taxonomy, see JoinDFMonty function")
  } else if(class(DF)=="list"){
    # If DF is a list:
    if(class(DF[[1]])=="data.frame"){
      # if DF[[1]] is a data.frame, then left_join then squish then output
      parallel::mclapply(seq_along(DF),function(i){
        # Join by the provided name: 'joiner'
        left_join(DF[[i]],taxy,
                  by=join_by(!!sym(DFjoiner)==!!sym(taxyjoiner)))
      },mc.cores=ncores)%>%squishLDF()%>%as.data.frame()%>%return()
    } else if(class(DF[[1]])=="character"){
      # if DF[[1]] is a character vector, then convert to data.frame, left join, squish and then output
      do.call(rbind,parallel::mclapply(seq_along(DF),function(i){
        # Join by the provided name: 'joiner'
        otot<-left_join(data.frame(DF[[i]])%>%setNames(taxyjoiner),
                  taxy,by=taxyjoiner)
        if(nrow(otot)>1) apply(otot,2,function(x) paste0(unique(x),collapse=delim))
        return(otot)
      },mc.cores=ncores))%>%return()
    } else if(class(DF[[1]])=="list"){
      # if DF[[1]] is a list, then left_join then squish then output, per element
      if(is.null(DF[[1]][DFjoiner])) stop("Attempting to join a nested-list object of inappropriate nested-class with a Monty taxonomy, see JoinDFMonty function")
      # Otherwise go ahead
      parallel::mclapply(seq_along(DF),function(i){
        # Join by the provided name: 'joiner'
        left_join(as.data.frame(DF[[i]][DFjoiner]),taxy,
                            by=join_by(!!sym(DFjoiner)==!!sym(taxyjoiner)))
      },mc.cores=ncores)%>%squishLDF()%>%as.data.frame()%>%return()
    } else stop("Attempting to join a nested-list object of inappropriate nested-class with a Monty taxonomy, see JoinDFMonty function")
  } else stop("Attempting to join an object of inappropriate class with a Monty taxonomy, see JoinDFMonty function")
}

SortConcurHaz<-function(concur){
  warning("SortConcurHar function returns empty column")
  lapply(seq_along(concur),function(i) return(data.frame(haz_concur="")))
}

# Convert the nested event_Level object to 2D tabular format
Monty_Ev2Tab<-function(Monty,red=F){
  #%%%%%%%%%%%%%%%%%%%%%% EVENT LEVEL %%%%%%%%%%%%%%%%%%%%%%#
  # IDs and linkages first
  ev_lv<-Monty$event_Level$ID_linkage[,c("event_ID","ev_name")]
  # Hazard taxonomy from specific hazards
  ev_lv%<>%cbind(JoinDFMonty(Monty$event_Level$allhaz_class, 
                             Monty$taxonomies$haz_class,
                             "all_hazs_spec","haz_spec_code"))%>%
    rename(haz_spec_code=all_hazs_spec)
  # Abbreviated hazard code
  ev_lv$haz_Ab<-unname(unlist(parallel::mclapply(Monty$event_Level$allhaz_class,function(x){
    paste0(unique(x$all_hazs_Ab),collapse=delim)
  },mc.cores=ncores)))
  # Add the temporal information and general location variable
  ev_lv%<>%cbind(Monty$event_Level$temporal,
                 Monty$event_Level$spatial%>%dplyr::select(gen_location))
  # Add the ISOs
  ev_lv$ev_ISO3s<-unname(unlist(parallel::mclapply(1:length(Monty$event_Level$spatial$ev_ISO3s),function(i){
    paste0(unique(Monty$event_Level$spatial$ev_ISO3s[[i]]),collapse=delim)
  },mc.cores=ncores)))
  # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
  ev_lv%<>%cbind(JoinDFMonty(Monty$event_Level$spatial$ev_ISO3s, 
                             Monty$taxonomies$ISO_info,
                             "ev_ISO3s","ISO3"))
  # Add the external IDs
  ev_lv%<>%cbind(squishLDF(Monty$event_Level$ID_linkage$all_ext_IDs)%>%
                   setNames(c("all_ext_ID","all_extID_db","all_extID_org")))
  # If we want to reduce it to the barebones version
  if(red) {
    ev_lv%>%dplyr::select(-c(grep("_code",colnames(ev_lv),value = T)))%>%return()
  }
  
  return(ev_lv)
}

# Convert an impact_Data nested object into 2D tabular format
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
  # If the impact spatial data field is a data frame, convert it to a list
  if(class(Monty$impact_Data$spatial)=="data.frame") {
    stop("Why is the spatial data in Monty object a data.frame and not a list?")
    # # Spatial object type
    # imp_lv%<>%cbind(left_join(
    #   do.call(rbind,lapply(Monty$impact_Data$spatial$spatial_info,function(x) x)), 
    #   Monty$taxonomies$spatial_coverage%>%setNames(c("imp_spat_covcode","imp_spat_covlab")),
    #   by="imp_spat_covcode")%>%
    #     dplyr::select("imp_spat_covcode","imp_spat_covlab","imp_spat_res","imp_spat_resunits","imp_spat_crs"))
    # # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
    # imp_lv%<>%cbind(left_join(
    #   do.call(rbind,lapply(Monty$impact_Data$spatial$spatial_info,function(x) x))%>%rename(ISO3=imp_ISO3s), 
    #   Monty$taxonomies$ISO_info,by="ISO3")%>%
    #     dplyr::select("ISO3","country","unregion","worldbankregion",
    #                   "continent","unsubregion","worldbankincomegroup")%>%
    # setNames(c("imp_ISO3","imp_country","imp_unregion","imp_worldbankregion",
    #            "imp_continent","imp_unsubregion","imp_worldbankincomegroup")))
    # # Spatial data source information 
    # imp_lv%<>%cbind(left_join(
    #   do.call(rbind,lapply(Monty$impact_Data$spatial$source,function(x) x))%>%
    #     setNames(c("imp_spat_srcdbcode","imp_spat_srcdbURL","imp_spat_srcorgcode")), 
    #   Monty$taxonomies$src_info%>%
    #     setNames(str_replace_all(str_replace_all(colnames(Monty$taxonomies$src_info),"_",""),"src","imp_spat_src"))%>%
    #     dplyr::select(-imp_spat_srcdbURL),
    #   by=c("imp_spat_srcorgcode","imp_spat_srcdbcode")))
  } else {
    # Spatial ID linkages
    imp_lv%<>%cbind(do.call(rbind,lapply(Monty$impact_Data$spatial,function(x) {
      outy<-x$ID_linkage
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
        dplyr::select("ISO3","country","unregion","worldbankregion",
                      "continent","unsubregion","worldbankincomegroup")%>%
        setNames(c("imp_ISO3","imp_country","imp_unregion","imp_worldbankregion",
                   "imp_continent","imp_unsubregion","imp_worldbankincomegroup")))
    # Spatial data source information 
    imp_lv%<>%cbind(left_join(
      do.call(rbind,lapply(Monty$impact_Data$spatial,function(x) x$source))%>%
        setNames(c("imp_spat_srcdbcode","imp_spat_srcdburl","imp_spat_srcorgcode")), 
      Monty$taxonomies$src_info%>%
        setNames(str_replace_all(str_replace_all(colnames(Monty$taxonomies$src_info),"_",""),"src","imp_spat_src"))%>%
        dplyr::select(-imp_spat_srcdburl),
      by=c("imp_spat_srcorgcode","imp_spat_srcdbcode")))
  }
  # Select certain columns 
  if(red) {
    impcols<-c("event_ID","imp_sub_ID","haz_sub_ID", #ID and linkages
               "imp_ext_IDs","imp_extID_db","imp_extID_org",
               # source
               "imp_srcdb_lab","imp_srcorg_lab",
               "imp_srcorg_typelab","imp_srcorg_email","imp_srcdb_attr",
               "imp_srcdb_lic","imp_srcdb_url","imp_src_addinfo",
               # impact detail
               "exp_spec_lab","exp_subcat_lab","exp_cat_lab",
               "imp_value","imp_type_lab","imp_unit_lab",
               "imp_unitgroup_lab","imp_esttype_lab",
               "imp_unitdate",
               # temporal
               "imp_sdate","imp_fdate",
               # spatial
               "imp_spat_ID","imp_ISO3","imp_country","imp_unregion",
               "imp_worldbankregion","imp_continent","imp_unsubregion",
               "imp_worldbankincomegroup","imp_spat_fileloc",
               "imp_spat_covlab","imp_spat_res","imp_spat_resunits",
               "imp_spat_crs","imp_spat_srcdblab","imp_spat_srcdburl",
               "imp_spat_srcorglab",
               "imp_spat_srcorgtypelab",
               "imp_spat_srcorgemail","imp_spat_srcdbattr",
               "imp_spat_srcdblic","imp_spat_srcaddinfo")
  } else {
    impcols<-c("event_ID","imp_sub_ID","haz_sub_ID", #ID and linkages
               "imp_ext_IDs","imp_extID_db","imp_extID_org",
               # source
               "imp_srcdb_code","imp_srcdb_lab",
               "imp_srcorg_code","imp_srcorg_lab","imp_srcorg_typecode",
               "imp_srcorg_typelab","imp_srcorg_email","imp_srcdb_attr",
               "imp_srcdb_lic","imp_srcdb_url","imp_src_addinfo",
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
               "imp_spat_ID","imp_ISO3","imp_country","imp_unregion",
               "imp_worldbankregion","imp_continent","imp_unsubregion",
               "imp_worldbankincomegroup","imp_spat_fileloc",
               "imp_spat_covcode",
               "imp_spat_covlab","imp_spat_res","imp_spat_resunits",
               "imp_spat_crs","imp_spat_srcdbcode",
               "imp_spat_srcdblab","imp_spat_srcdburl",
               "imp_spat_srcorgcode","imp_spat_srcorglab",
               "imp_spat_srcorgtypecode","imp_spat_srcorgtypelab",
               "imp_spat_srcorgemail","imp_spat_srcdbattr",
               "imp_spat_srcdblic","imp_spat_srcaddinfo")
  }
  # Fill empty columns with NAs
  imp_lv[,impcols[!impcols%in%colnames(imp_lv)]]<-NA
  # Re-order this all
  imp_lv%<>%dplyr::select(all_of(impcols))
  
  return(imp_lv)
}

# Convert an hazard_Data nested object into 2D tabular format
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
  if(class(Monty$hazard_Data$spatial)=="data.frame"){
    stop("Why is the spatial data in Monty object a data.frame and not a list?")
    # if(!(is.null(Monty$hazard_Data$spatial$ID_linkage[[1]]$haz_spat_colname) &
    #      is.null(Monty$hazard_Data$spatial$ID_linkage[[1]]$haz_spat_rowname))) {
    #   haz_lv%<>%cbind(do.call(rbind,lapply(Monty$hazard_Data$spatial$ID_linkage,function(x) {
    #     x$haz_spat_colname%<>%replace(is.na(.),"")%>%squishLDF()
    #     x$haz_spat_rowname%<>%replace(is.na(.),"")%>%squishLDF()
    #     return(x)
    #   })))
    # } else {
    #   haz_lv%<>%cbind(do.call(rbind,lapply(Monty$hazard_Data$spatial$ID_linkage,function(x) x)))
    # }
    # # Spatial object type
    # haz_lv%<>%cbind(left_join(
    #   do.call(rbind,lapply(Monty$hazard_Data$spatial$spatial_info,function(x) x)),
    #   Monty$taxonomies$spatial_coverage%>%setNames(c("haz_spat_covcode","haz_spat_covlab")),
    #   by="haz_spat_covcode")%>%
    #     select_if(names(.) %in% c("haz_spat_covcode","haz_spat_covlab",
    #                               "haz_spat_res","haz_spat_resunits","haz_spat_crs")))
    # # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
    # haz_lv%<>%cbind(left_join(
    #   do.call(rbind,lapply(Monty$hazard_Data$spatial$spatial_info,function(x) x))%>%rename(ISO3=haz_ISO3s), 
    #   Monty$taxonomies$ISO_info,by="ISO3")%>%
    #     dplyr::select("ISO3","country","unregion","worldbankregion",
    #                   "continent","unsubregion","worldbankincomegroup")%>%
    # setNames(c("haz_ISO3","haz_country","haz_unregion","haz_worldbankregion",
    #            "haz_continent","haz_unsubregion","haz_worldbankincomegroup")))
  } else {
    haz_lv%<>%cbind(do.call(rbind,lapply(Monty$hazard_Data$spatial,function(x) {
      x$ID_linkage})))
    # Spatial object type
    haz_lv%<>%cbind(left_join(
      do.call(rbind,lapply(Monty$hazard_Data$spatial,function(x) x$spatial_info)),
      Monty$taxonomies$spatial_coverage%>%setNames(c("haz_spat_covcode","haz_spat_covlab")),
      by="haz_spat_covcode")%>%
        select_if(names(.) %in% c("haz_spat_covcode","haz_spat_covlab",
                                  "haz_spat_res","haz_spat_resunits","haz_spat_crs")))
    # Country info from ISOs (accounting for when ev_ISOs is a nested list of DFs)
    haz_lv%<>%cbind(left_join(
      do.call(rbind,lapply(Monty$hazard_Data$spatial,function(x) x$spatial_info))%>%rename(ISO3=haz_ISO3s), 
      Monty$taxonomies$ISO_info,by="ISO3")%>%
        dplyr::select("ISO3","country","unregion","worldbankregion",
                      "continent","unsubregion","worldbankincomegroup")%>%
        setNames(c("haz_ISO3","haz_country","haz_unregion","haz_worldbankregion",
                   "haz_continent","haz_unsubregion","haz_worldbankincomegroup")))
  }
  # Select certain columns 
  if(red) {
    hazcols<-c("event_ID","haz_sub_ID", #ID and linkages
               "haz_ext_IDs","haz_extID_db","haz_extID_org",
               # source
               "haz_srcdb_lab","haz_srcorg_lab",
               "haz_srcorg_typelab","haz_srcorg_email","haz_srcdb_attr",
               "haz_srcdb_lic","haz_srcdb_url","haz_src_addinfo",
               # hazard detail
               "haz_Ab","haz_spec_code","haz_spec_lab","haz_cluster_lab",
               "haz_type_lab","haz_maxvalue","haz_maxunit_lab",
               "haz_maxunitgroup_lab","haz_esttype_lab",
               # temporal
               "haz_sdate","haz_fdate",
               # spatial
               "haz_spat_ID","haz_ISO3","haz_Country","haz_unregion",
               "haz_worldbankregion","haz_continent","haz_unsubregion",
               "haz_worldbankincomegroup","haz_spat_fileloc",
               "haz_spat_covlab","haz_spat_res","haz_spat_resunits",
               "haz_spat_crs")
  } else {
    hazcols<-c("event_ID","haz_sub_ID", #ID and linkages
               "haz_ext_IDs","haz_extID_db","haz_extID_org",
               # source
               "haz_srcdb_code","haz_srcdb_lab",
               "haz_srcorg_code","haz_srcorg_lab","haz_srcorg_typecode",
               "haz_srcorg_typelab","haz_srcorg_email","haz_srcdb_attr",
               "haz_srcdb_lic","haz_srcdb_url","haz_src_addinfo",
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
               "haz_spat_ID","haz_ISO3","haz_Country","haz_unregion",
               "haz_worldbankregion","haz_continent","haz_unsubregion",
               "haz_worldbankincomegroup","haz_spat_fileloc",
               "haz_spat_covcode",
               "haz_spat_covlab","haz_spat_res","haz_spat_resunits",
               "haz_spat_crs")
  }
  # Fill empty columns with NAs
  haz_lv[,hazcols[!hazcols%in%colnames(haz_lv)]]<-NA
  # Re-order this all
  haz_lv%<>%dplyr::select(all_of(hazcols))
  
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

# Function to extract all of the variable names directly from the Monty JSON schema
MontyJSONnames<-function(adder=T){
  # JSON schema
  schemy<-jsonlite::fromJSON("./Taxonomies/Montandon_Schema_V1-00.json")
  # First extract the names from the taxonomies section of the schema
  jsy<-names(plyr::llply(schemy, unlist)$properties)
  # clean it up
  taxy<-jsy[grepl("taxonomies",jsy) &!grepl("enum",jsy) & !grepl("description",jsy) &
              !grepl("type",jsy) & !grepl("codelist",jsy) & !grepl("openCodelist",jsy) &
              !grepl("\\$ref",jsy) & !grepl("minItems",jsy) & !grepl("uniqueItems",jsy) & !grepl(".format",jsy)]
  # clean it up again
  taxy<-sort(unname(sapply(taxy,function(x) {
    x<-str_split(x,pattern = ".title",simplify = T)[1,1]
    tmp<-str_split(x,"\\.",simplify = T)
    return(tmp[length(tmp)])
  },simplify=T))[-1])
  # Now extract from the objects inside the schema, using the 'defs' field
  jsy<-names(plyr::llply(schemy, unlist)$`$defs`)
  # clean it up
  defs<-jsy[grepl("title",jsy) & !grepl("enum",jsy) & !grepl("description",jsy) & !grepl("type",jsy) & 
              !grepl("codelist",jsy) & !grepl("openCodelist",jsy) & !grepl("\\$ref",jsy) & 
              !grepl("minItems",jsy) & !grepl("uniqueItems",jsy) & !grepl(".format",jsy) &
              !grepl("pattern",jsy) & !grepl("required",jsy) & !grepl("minLength",jsy)]
  # clean it up again
  defs<-sort(unname(sapply(defs,function(x) {
    x<-str_split(x,pattern = ".title",simplify = T)[1,1]
    tmp<-str_split(x,"\\.",simplify = T)
    return(tmp[length(tmp)])
  },simplify=T))[-1])
  # make sure to keep only the deepest nested variable names, not the umbrella terms
  defs<-unique(defs[!grepl("_obj",defs)])
  # Same again
  defs<-defs[!defs%in%c("spatial","spatial_info","temporal","ID_linkage","allhaz_class","concur_haz","impact_detail","items","source")]
  
  if(!adder) return(c(taxy,defs))
  
  return(sort(unique(c(taxy,defs,"ext_ID","imp_src_orgtype","haz_type","haz_cluster",
           "haz_spec","haz_potlink","hazlink","exp_cat","exp_subcat",
           "imp_type","imp_est_type","haz_est_type"))))
}








