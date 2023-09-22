library(methods)
#######################################################################
####################### tabGCDB CLASS DEFINITION ######################
############### (Global Crisis Data Bank - Tabular Object) #############
#######################################################################
# FIELDS:
#   - obj_info: GCDB-specific info about version, product info, etc
#   - impacts: data frame of impacts and relevant information
# METHODS:
#   - Not much!
#######################################################################

add_tabGCDBinfo<-function(){
  list(obj_version="1.0",
       product="Global Crisis Data Bank",
       org="IFRC",
       org_sec="Information Management - Geneva, CH")
}
 
col_tabGCDB<-c("GCDB_ID"="character", # GCDB event ID
               "GLIDE"="character", # GLIDE number of impacting-hazard (not necessarily the primary hazard)
               
               
               # add the fucking principal event hazard!
               
               
               "impsub_ID"="character", # ID of each impact element in the overall event
               "imphaz_ID"="character", # ID of each hazard event in the overall event, e.g. aftershocks or flash floods with cyclone
               "ev_name_orig"="character", # Name of the event in original language
               "ev_name_en"="character", # Name of the event in english
               "location"="character", # general description of hazard location
               "ISO3"="character", # ISO3-codes
               "Continent"="character", # Local-continent
               "ev_sdate"="POSIXct", # Start date of the event or the impacting-hazard
               "ev_fdate"="POSIXct", # Finish date of the event or the impacting-hazard
               "imp_sdate"="POSIXct", # Start date of the impact estimate (in case it aggregates over a range of dates)
               "imp_fdate"="POSIXct", # End date of the impact estimate (in case it aggregates over a range of dates)
               "haz_sdate"="POSIXct", # Start date of the hazard estimate (in case it aggregates over a range of dates)
               "haz_fdate"="POSIXct", # End date of the hazard estimate (in case it aggregates over a range of dates)
               "impactcats"="character", # Impact category
               "impactsubcats"="character", # Impact subcategory
               "impactdetails"="character", # Impact subsubcategory
               "impvalue"="numeric", # Impact quantity
               "imptype"="character", # Impact units
               "imp_units"="character", # Impact type (e.g. excess mortality, displacement stock)
               "unitdate"="character", # date associated to the unit (for currencies almost exclusively)
               "imp_est_type"="character", # Estimate type: primary, secondary, modelled
               "haz_est_type"="character", # Estimate type: primary, secondary, modelled
               "src_db"="character", # Source database name of impact estimate or the curated estimate
               "src_org"="character", # Source organisation of impact estimate or the curated estimate
               "src_orgtype"="character", # Source organisation type
               "src_URL"="character", # URL of the impact estimate
               "hazAb"="character", # Abbreviated, simplified name of the hazard
               "haztype"="character", # Impacting hazard type
               "hazcluster"="character", # Impacting hazard cluster
               "hazspec"="character", # Impacting specific hazard
               "hazlink"="character", # Associated impactful-hazards to the specific hazard
               "hazpotlink"="character", # Potential other impactful-hazards that may be associated to the specific hazard
               "spat_ID"="character", # ID of the spatial object
               "spat_type"="character", # Spatial object type
               "spat_res"="character", # Spatial resolution of impact estimate
               "spat_srcorg"="character") # Source organisation from where the spatial object comes from

oblig_tabGCDB<-c("GCDB_ID","impsub_ID","ISO3","impcat","impsubcat","imp_units",
                 "imp_type","est_type","src_org","src_orgtype","src_URL",
                 "hazAb","haztype","hazcluster")

GetGCDB_ID<-function(DF,haz=NULL) {
  # In case a specific hazard is fed in
  if(!is.null(haz)) DF%<>%mutate(hazAb=haz)
  # Generate the names from the dataframe
  namerz<-DF%>%
    dplyr::select(hazAb,ev_sdate,ISO3)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  paste0(namerz,"-GCDB")
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
  imptax<-openxlsx::read.xlsx("./RCode/MainlyImpactData/ConvertImpact_Taxonomy.xlsx")%>%
    filter(src_db==nomDB)
  # Find where the Desinventar data impact estimates stop 
  vlim<-which(colnames(ImpDB)%in%imptax$VarName)
  # For all columns that correspond to impact estimates, return the data
  ImpDB%<>%reshape2::melt(measure.vars=colnames(ImpDB)[vlim])%>%
    mutate(VarName=as.character(variable),impvalue=as.numeric(value))%>%
    dplyr::select(-c(variable,value))%>%
    left_join(dplyr::select(imptax,-c("src_orgtype","src_org","src_db")),by="VarName")
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
