# A function to go from a series of data into the JSON-accepted DF-list style object
Add_EvIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%group_by(event_ID)%>%
    reframe(ev_name=paste0(ev_name,collapse = '\",\"'))
  # Generate all the elements of the dataset
  output$ext_IDs<-sapply(unique(dframe$event_ID),function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$event_ID==ID
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
  return(output)
}

Add_EvTemp_Monty<-function(dframe){
  # Take the event start date as the minimum and maximum dates
  dframe%>%group_by(event_ID)%>%
    reframe(ev_sdate=as.character(min(as.Date(ev_sdate))),
            ev_fdate=as.character(max(as.Date(ev_fdate))))%>%
    dplyr::select(-event_ID)%>%distinct()
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
Add_ImIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,imp_sub_ID)%>%distinct()
  # Generate all the elements of the dataset
  output$haz_sub_ID<-sapply(unique(dframe$imp_sub_ID),function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$imp_sub_ID==ID
    # Highlight the external IDs that share the same Monty IDs 
    list(dframe$haz_sub_ID[indy])
  },simplify = T)
  # Let's keep this neat
  names(output$haz_sub_ID)<-NULL
  # Output that bea-u-t
  return(output)
}

Add_ImSpatID_Monty<-function(dframe){
  
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

Add_ImSpatAll_Monty<-function(ID_linkage,spatial_info,source){
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














