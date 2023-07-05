library(methods)
#######################################################################
####################### hazGCDB CLASS DEFINITION ######################
############### (Global Crisis Data Bank - Impact Object) #############
#######################################################################
# FIELDS:
#   - obj_info: GCDB-specific info about version, product info, etc
#   - hazards: data frame of hazards and relevant information
# METHODS:
#   - Not much!
#######################################################################

add_hazGCDBinfo<-function(){
  list(obj_version="1.0",
       product="Global Crisis Data Bank",
       org="IFRC",
       org_sec="Information Management - Geneva, CH")
}
 
col_hazGCDB<-c("GCDB_ID"="character", # GCDB event ID
               "GLIDE"="character", # GLIDE number of hazard (not necessarily the primary hazard)
               "hazsub_ID"="character", # ID of each hazard element in the overall event
               "ev_name_orig"="character", # Name of the event in original language
               "ev_name_en"="character", # Name of the event in english
               "location"="character", # general description of hazard location
               "ISO3"="character", # ISO3-codes
               "Continent"="character", # Local-continent
               "ev_sdate"="POSIXct", # Start date of the event or the hazard
               "ev_fdate"="POSIXct", # Finish date of the event or the hazard
               "haztype"="character", # Impacting hazard type
               "hazcluster"="character", # Impacting hazard cluster
               "hazspec"="character", # Impacting specific hazard
               "hazlink"="character", # Associated hazards to the specific hazard
               "hazpotlink"="character", # Potential other hazards that may be associated to the specific hazard
               "hazvalue"="numeric", # Impact units
               "hazAb"="character", # Abbreviated, simplified name of the hazard
               "haz_sev"="numeric", # Hazard severity
               "haz_sev_add"="numeric", # In case an additional severity measurement is provided (e.g. EQ magnitude AND DEPTH)
               "haz_sev_type"="character", # Type of hazard severity, (intensity or magnitude)
               "haz_unit"="character", # What is the principal hazard unit (e.g. Richter scale)
               "haz_add_unit"="character", # What is the secondary hazard unit (e.g. meters in depth)
               "forecast"="logical", # Is this hazard information a forecast or of the actual occurred hazard?
               "est_type"="character", # Estimate type: primary, secondary, modelled
               "src_db"="character", # Source database name of hazard data
               "src_org"="character", # Source organisation of hazard data
               "src_orgtype"="character", # Source organisation type
               "src_URL"="character", # URL of the hazard data
               "spat_ID"="character", # ID of the spatial object
               "spat_type"="character", # Spatial object type
               "spat_res"="character", # Spatial resolution of hazard data
               "spat_srcorg"="character") # Source organisation from where the spatial object comes from

oblig_hazGCDB<-c("GCDB_ID","hazsub_ID","ISO3",
                 "est_type","src_org","src_orgtype","src_URL",
                 "haz_sev","haz_unit","hazAb","haztype","hazcluster")

GetGCDB_ID<-function(DF,haz="EQ") {
  namerz<-DF%>%
    mutate(haz=haz)%>%
    dplyr::select(haz,ev_sdate,ISO3)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  paste0(namerz,"-GCDB")
}

AddEmptyColImp<-function(DF){
  for(i in which(!names(col_hazGCDB)%in%colnames(DF))){
    tmp<-NA
    class(tmp)<-col_hazGCDB[i]
    DF$tmp<-tmp
    colnames(DF)[ncol(DF)]<-names(col_hazGCDB)[i]
  }
  DF[,names(col_hazGCDB)]
}

# hazGCDB object skeleton
haz_skel<-function(nr=0){
  skelly<-as.data.frame(matrix(NA,nr,length(col_hazGCDB)))
  colnames(skelly)<-names(col_hazGCDB)
  for(i in 1:ncol(skelly)) class(skelly[,i])<-col_hazGCDB[i]
  
  return(skelly)
}

hazGCDB_ColClass<-function(DF){
  for(i in 1:ncol(DF)){
    j<-which(names(DF)[i]==names(col_hazGCDB))
    # Change the class to make sure this variable is compatible
    class(DF[,i])<-col_hazGCDB[j]
  }
  return(DF)
}

# The data.frame component of the hazGCDB object
hazGCDB_data<-function(DF){
  # Check the column names are what we need and nothing more, nothing less
  if(!all(colnames(DF)%in%names(col_hazGCDB))) stop("Error in hazard data input fields to form hazGCDB object")
  # Incase not all columns were provided
  modDF<-haz_skel(nrow(DF)); modDF[,colnames(DF)]<-DF
  # Check the data types of each column
  DF%<>%hazGCDB_ColClass()
  # Re-order the object columns
  DF%<>%dplyr::select(names(col_hazGCDB))
  
  return(DF)
}

# Create the class
hazGCDB<-setClass("hazGCDB", 
         slots = c(obj_info="list",
                   hazards="data.frame"))

# Set the rules of object validity
setValidity("hazGCDB", function(object) {
  if(any(!colnames(object@hazards)==names(col_hazGCDB))) return("Not all or too many columns present in hazGCDB object")
  # Check the data types of each column
  if(!all(sapply(1:ncol(object@hazards),function(i){
    j<-which(names(object@hazards)[i]==names(col_hazGCDB))
    # Check the class
    class(object@hazards[,i])==col_hazGCDB[j]
  },simplify = T))) return("Class mis-match in the hazard object of hazGCDB")
  # Obligatory columns to have in hazGCDB object
  if(nrow(na.omit(dplyr::select(object@hazards,oblig_hazGCDB)))!=nrow(object@hazards)) return("NAs found in obligatory columns of hazGCDB column")
  
  TRUE
})

# Initialisation method
setMethod(f="initialize", signature="hazGCDB",
          definition=function(.Object,hazards=NULL) {
            .Object@obj_info<-add_hazGCDBinfo()
            # Skeleton of hazGCDB object
            if(is.null(hazards)) {
              # Assign a skeleton object
              .Object@hazards<-hazGCDB_data(haz_skel())
              
            } else .Object@hazards<-hazGCDB_data(hazards)
            
            validObject(.Object)
            
            return(.Object)
          })

# Simple example:
# impies<-hazGCDB()
