# A function to go from a series of data into the JSON-accepted DF-list style object
Add_EvIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,ev_name,ev_name_lang)%>%distinct()
  # Generate all the elements of the dataset
  output$ext_IDs<-sapply(dframe$event_ID,function(ID){
    # Find the corresponding indices for this entry
    indy<-dframe$event_ID==ID
    # Highlight the external IDs that share the same Monty IDs 
    list(data.frame(ext_ID=dframe$ext_ID[indy],
               ext_ID_db=dframe$ext_ID_db[indy], 
               ext_ID_org=dframe$ext_ID_org[indy]))
  },simplify = T)
  # Let's keep this neat
  names(output$ext_IDs)<-NULL
  # Output that bea-u-t
  return(output)
}

# A function to form the impact IDlinkage field
Add_ImIDlink_Monty<-function(dframe){
  # Setup the other entries
  output<-dframe%>%dplyr::select(event_ID,imp_sub_ID)%>%distinct()
  # Generate all the elements of the dataset
  output$haz_sub_ID<-sapply(dframe$imp_sub_ID,function(ID){
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
  ))},simplify = T)
  # Let's keep this neat
  row.names(output)<-NULL
  
  return(output)
  
}





