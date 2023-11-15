library(methods)
#######################################################################
####################### tabGCDB CLASS DEFINITION ######################
############### (Global Crisis Data Bank - Tabular Object) #############
#######################################################################
# FIELDS:
# Version info
add_tabGCDBinfo<-function(){
  list(obj_version="1.0",
       product="Global Crisis Data Bank",
       org="IFRC",
       org_sec="Information Management - Geneva, CH")
}
# risk data-related fields
               # Basic event-level information
col_tabGCDB<-c("event_ID"="character", # GCDB event ID
               "ext_IDs"="character", # external ID numbers, such as the GLIDE number of impacting-hazard (note: not necessarily the primary hazard)
               "ext_ID_dbs"="character", # Source database of the external ID
               "ext_ID_orgs"="character", # Source organisation of the external ID
               "ev_name"="character", # Name of the event in any language
               "ev_name_lang"="character", # Language that the event name is written in (ISO 639-2 standard code)
               "location"="character", # general description of hazard location
               "ev_ISO3s"="character", # ISO3-codes
               "regions"="character", # Local-continent/region/multi-country grouping
               "ev_sdate"="character", # Start date of the event or the impacting-hazard
               "ev_fdate"="character", # Finish date of the event or the impacting-hazard
               # Add triggering hazard details
               "prim_haz_Ab"="character", # Primary (triggering) hazard 2-letter abbreviation
               "prim_haz_type"="character", # Primary (triggering) hazard  type
               "prim_haz_cluster"="character", # Primary (triggering) hazard cluster 
               "prim_haz_spec"="character", # Primary (triggering) specific hazard 
               
               # Impact information
               "imp_ISO3s"="character", # country ISO3-C codes for the impact data
               "imp_sub_ID"="character", # ID of each impact element in the overall event
               "imp_sdate"="character", # Start date of the impact estimate (in case it aggregates over a range of dates)
               "imp_fdate"="character", # End date of the impact estimate (in case it aggregates over a range of dates)
               "imp_cat"="character", # Impact category
               "imp_subcat"="character", # Impact subcategory
               "imp_det"="character", # Impact subsubcategory
               "imp_value"="numeric", # Impact quantity
               "imp_type"="character", # Impact units
               "imp_units"="character", # Impact type (e.g. excess mortality, displacement stock)
               "imp_unitdate"="character", # date associated to the unit (for currencies almost exclusively)
               "imp_est_type"="character", # Estimate type: primary, secondary, modelled
               "imp_src_db"="character", # Source database name of impact estimate or the curated estimate
               "imp_src_org"="character", # Source organisation of impact estimate or the curated estimate
               "imp_src_orgtype"="character", # Source organisation type
               "imp_src_URL"="character", # URL of the impact estimate
               "imp_src_dbdesc"="character", # General database description
               "imp_src_orgatt"="character", # Attribution of the data to the source organisation (e.g. host, curator, etc)
               "imp_src_addinfo"="character", # Additional information about the impact source
               "imp_src_email"="character", # Email address of the source organisation
               "imp_src_phone"="character", # Phone number of the source organisation
               "imp_src_lic"="character", # licenses of the data from the provided organisation
               
               # Hazard information (can change from impact to impact for the same event)
               "haz_ISO3s"="character", # country ISO3-C codes for the hazard data
               "haz_sub_ID"="character", # ID of each hazard event in the overall event, e.g. aftershocks or flash floods with cyclone
               "haz_sdate"="character", # Start date of the hazard estimate (in case it aggregates over a range of dates)
               "haz_fdate"="character", # End date of the hazard estimate (in case it aggregates over a range of dates)
               "haz_Ab"="character", # Abbreviated, simplified name of the hazard
               "haz_type"="character", # Impacting hazard type
               "haz_cluster"="character", # Impacting hazard cluster
               "haz_spec"="character", # Impacting specific hazard
               "haz_link"="character", # Associated impactful-hazards to the specific hazard
               "haz_potlink"="character", # Potential other impactful-hazards that may be associated to the specific hazard
               "haz_maxvalue"="numeric", # Maximum intensity or magnitude of the hazard, e.g.  
               "haz_units"="character", # Units of the max intensity/magnitude value estimate
               "haz_est_type"="character", # Estimate type: primary, secondary, modelled
               "haz_src_db"="character", # Source database name of impact estimate or the curated estimate
               "haz_src_org"="character", # Source organisation of impact estimate or the curated estimate
               "haz_src_orgtype"="character", # Source organisation type
               "haz_src_URL"="character", # URL of the impact estimate
               "haz_ext_IDs"="character", # external IDs related to the hazard data, such as GLIDE numbers
               "haz_ext_IDdbs"="character", # source database name of the external ID
               "haz_ext_IDorgs"="character", # source organisation of the external ID
               "linkhaz_ext_IDs"="character", # external IDs related to the linked hazard data, such as GLIDE numbers
               "linkhaz_ext_IDdbs"="character", # source database name of the linked external ID
               "linkhaz_ext_IDorgs"="character", # source organisation of the linked external ID
               
               # Spatial info - impact
               "imp_spat_ID"="character", # ID of the spatial object
               "imp_spat_type"="character", # Spatial object type
               "imp_spat_res"="character", # Spatial resolution of impact estimate
               "imp_spat_resunits"="character", # Spatial resolution units of impact estimate (e.g. ADM level, raster grid)
               "imp_spat_srcorg"="character", # organisation of the spatial data
               "imp_spat_srcurl"="character", # URL of the impact estimate
               "imp_spat_colIDs"="character",
               "imp_spat_rowIDs"="character",
               "imp_spat_fileloc"="character",
               
               # Spatial info - hazard
               "haz_spat_ID"="character", # ID of the spatial object
               "haz_spat_type"="character", # Spatial object type
               "haz_spat_res"="character", # Spatial resolution of impact estimate
               "haz_spat_resunits"="character", # Spatial resolution units of impact estimate (e.g. ADM level, raster grid)
               "haz_spat_srcorg"="character", # Source organisation from where the spatial object comes from
               "haz_spat_srcurl"="character", # URL of the impact estimate
               "haz_spat_colIDs"="character",
               "haz_spat_rowIDs"="character",
               "haz_spat_fileloc"="character")
# Required fields
oblig_tabGCDB<-c("event_ID","imp_sub_ID","ev_ISO3s","imp_cat","imp_subcat","imp_det","imp_units",
                 "imp_type","imp_est_type","imp_src_org","imp_src_orgtype","imp_src_URL",
                 "haz_Ab","haz_type","haz_cluster")
# METHODS:
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
  
  impies$imp_sub_ID<-impies%>%dplyr::select(c(event_ID,imp_src_db,imp_det,imp_type,imp_spat_ID))%>%
    mutate(imp_src_db=stringr::str_remove(stringi::stri_trans_totitle(imp_src_db),pattern = " "))%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  return(impies)
}

GetGCDB_hazID<-function(impies){
  if(!any(colnames(impies)=="haz_spat_ID")) impies$haz_spat_ID<-NA_character_
  
  impies$haz_sub_ID<-impies%>%dplyr::select(c(event_ID,haz_src_db,haz_cluster,haz_spec,haz_spat_ID))%>%
    mutate(haz_src_db=stringr::str_remove(stringi::stri_trans_totitle(haz_src_db),pattern = " "),
           haz_src_db=stringr::str_remove(stringi::stri_trans_totitle(haz_src_db),pattern = " "),
           )%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  return(impies)
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

ImpLabs<-function(ImpDB,nomDB="Desinventar",dropName=T){
  # Open up the database impact taxonomy conversion file
  imptax<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/ConvertImpact_Taxonomy.xlsx")%>%
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

# tmp<-googledrive::drive_download("https://docs.google.com/spreadsheets/d/1agqy6DV5VmJuaamVaXZE7jfkDOC5AOhM/edit?usp=sharing&ouid=109118346520870360454&rtpof=true&sd=true",overwrite = T)
# tmp<-openxlsx::read.xlsx(tmp$local_path)

# tabGCDB object skeleton
imp_skel<-function(nr=0){
  skelly<-as.data.frame(matrix(NA,nr,length(col_tabGCDB)))
  colnames(skelly)<-names(col_tabGCDB)
  for(i in 1:ncol(skelly)) class(skelly[,i])<-col_tabGCDB[i]
  
  return(skelly)
}

tabGCDB_ColClass<-function(DF){
  for(i in 1:ncol(DF)){
    j<-which(names(DF)[i]==names(col_tabGCDB))
    # Change the class to make sure this variable is compatible
    class(DF[,i])<-col_tabGCDB[j]
  }
  return(DF)
}

# The data.frame component of the tabGCDB object
tabGCDB_data<-function(DF){
  # Check the column names are what we need and nothing more, nothing less
  if(!all(colnames(DF)%in%names(col_tabGCDB))) stop("Error in impact data input fields to form tabGCDB object")
  # Incase not all columns were provided
  modDF<-imp_skel(nrow(DF)); modDF[,colnames(DF)]<-DF
  # Check the data types of each column
  DF%<>%tabGCDB_ColClass()
  # Re-order the object columns
  DF%<>%dplyr::select(names(col_tabGCDB))
  
  return(DF)
}

# Create the class
tabGCDB<-setClass("tabGCDB", 
         slots = c(obj_info="list",
                   impacts="data.frame"))

# Set the rules of object validity
setValidity("tabGCDB", function(object) {
  if(any(!colnames(object@impacts)==names(col_tabGCDB))) return("Not all or too many columns present in tabGCDB object")
  # Check the data types of each column
  if(!all(sapply(1:ncol(object@impacts),function(i){
    j<-which(names(object@impacts)[i]==names(col_tabGCDB))
    # Check the class
    class(object@impacts[,i])==col_tabGCDB[j]
  },simplify = T))) return("Class mis-match in the impact object of tabGCDB")
  # Obligatory columns to have in tabGCDB object
  if(nrow(na.omit(dplyr::select(object@impacts,oblig_tabGCDB)))!=nrow(object@impacts)) return("NAs found in obligatory columns of tabGCDB column")
  
  TRUE
})

# Initialisation method
setMethod(f="initialize", signature="tabGCDB",
          definition=function(.Object,impacts=NULL) {
            .Object@obj_info<-add_tabGCDBinfo()
            # Skeleton of tabGCDB object
            if(is.null(impacts)) {
              # Assign a skeleton object
              .Object@impacts<-tabGCDB_data(imp_skel())
              
            } else .Object@impacts<-tabGCDB_data(impacts)
            
            validObject(.Object)
            
            return(.Object)
          })

# Simple example:
# impies<-tabGCDB()
