taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
imp_class<-data.frame(
  imp_det_code=taxies%>%filter(list_name=="imp_det")%>%pull(name),
  imp_det_lab=taxies%>%filter(list_name=="imp_det")%>%pull(label),
  imp_subcat_code=taxies%>%filter(list_name=="imp_det")%>%pull(link_group),
  imp_subcat_lab=left_join(taxies[taxies$list_name=="imp_det",2:4],
                           taxies[taxies$list_name=="imp_subcats",2:4],
                           by=c("link_group"="name"))%>%pull(label.y),
  imp_cat_code=taxies%>%filter(list_name=="imp_det")%>%pull(link_maingroup),
  imp_cat_lab=left_join(left_join(taxies[taxies$list_name=="imp_det",2:4],
                                  taxies[taxies$list_name=="imp_subcats",2:4],
                                  by=c("link_group"="name")),
                        taxies[taxies$list_name=="imp_cats",2:3],
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

# ExtrLabel_Monty<-function(impies,HazNOTImp=F){
#   if(HazNOTImp){
#     
#   } else {
#     
#   }
#   
#   return(impies)
# }


# A function to go from a series of data into the JSON-accepted DF-list style object
Add_EvIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%group_by(event_ID)%>%
    reframe(ev_name=paste0(ev_name,collapse = '\",\"'))
  # Generate all the elements of the dataset
  output$ext_IDs<-sapply(unique(dframe$event_ID),function(ID){
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
  names(output$ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}

Add_EvSpat_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%group_by(event_ID)%>%
    reframe(gen_location=paste0(gen_location,collapse = '\",\"'))
  # and the ISO3C codes
  output$ev_ISO3s<-lapply(unique(dframe$event_ID),function(ID){
    # return the ISO3C codes in a list
    dframe$ev_ISO3s[dframe$event_ID==ID]
  })
  # Gimme gimme gimme
  return(output%>%distinct())
}

Add_EvTemp_Monty<-function(dframe){
  # Take the event start date as the minimum and maximum dates
  dframe%>%group_by(event_ID)%>%
    reframe(ev_sdate=as.character(min(as.Date(ev_sdate))),
            ev_fdate=as.character(max(as.Date(ev_fdate))))%>%
    distinct()%>%dplyr::select(-event_ID)
}

Add_HazTax_Monty<-function(dframe){
  # Extract all of the haz_spec codes and output a list
  lapply(unique(dframe$event_ID),function(ev){
    # indices
    indy<-which(dframe$event_ID==ev)
    do.call(rbind,lapply(indy,function(i){
      # Output
      data.frame(
        event_ID=ev,
        all_hazs_Ab=dframe$haz_Ab[i],
        all_hazs_spec=c(str_split(dframe$haz_spec[i],delim,simplify = T))
      )%>%distinct()
    }))
  })
}

# A function to form the impact IDlinkage field
Add_ImpIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,imp_sub_ID)%>%distinct()
  # Generate all the elements of the dataset
  output$haz_sub_ID<-sapply(unique(dframe$imp_sub_ID),function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$imp_sub_ID==ID
    # check for no haz_sub_imp values
    if(any(is.na(dframe$haz_sub_ID[indy]))) return(list())
    # Highlight the external IDs that share the same Monty IDs 
    list(dframe$haz_sub_ID[indy])
  },simplify = T)
  # Let's keep this neat
  names(output$haz_sub_ID)<-NULL
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
  lapply(ID_linkage$imp_sub_ID,function(ID){
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
  output$haz_ext_IDs<-sapply(dframe$haz_sub_ID,function(ID){
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


MergeMonty<-function(MontyA,MontyB){
  Monty<-MontyA
  Monty$event_Level$ID_linkage<-rbind(MontyA$event_Level$ID_linkage,
                                      MontyB$event_Level$ID_linkage)
  
}








