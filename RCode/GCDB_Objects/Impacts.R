library(methods)
#######################################################################
####################### impGCDB CLASS DEFINITION ######################
############### (Global Crisis Data Bank - Impact Object) #############
#######################################################################
# FIELDS:
#   - obj_info: GCDB-specific info about version, product info, etc
#   - impacts: data frame of impacts and relevant information
# METHODS:
#   - Not much!
#######################################################################

add_impGCDBinfo<-function(){
  list(obj_version="1.0",
       product="Global Crisis Data Bank",
       org="IFRC",
       org_sec="Information Management - Geneva, CH")
}
 
col_impGCDB<-c("GCDB_ID"="character", # GCDB event ID
               "GLIDE"="character", # GLIDE number of impacting-hazard (not necessarily the primary hazard)
               "impsub_ID"="character", # ID of each impact element in the overall event
               "imphaz_ID"="character", # ID of each hazard of each impact element in the overall event
               "ev_name_orig"="character", # Name of the event in original language
               "ev_name_en"="character", # Name of the event in english
               "location"="character", # general description of hazard location
               "ISO3"="character", # ISO3-codes
               "Continent"="character", # Local-continent
               "imp_sdate"="POSIXct", # Start date of the impact estimate (in case it aggregates over a range of dates)
               "imp_fdate"="POSIXct", # End date of the impact estimate (in case it aggregates over a range of dates)
               "ev_sdate"="POSIXct", # Start date of the event or the impacting-hazard
               "ev_fdate"="POSIXct", # Finish date of the event or the impacting-hazard
               "impactcats"="character", # Impact category
               "impactsubcats"="character", # Impact subcategory
               "impactdetails"="character", # Impact subsubcategory
               "impvalue"="numeric", # Impact units
               "imptype"="character", # Impact units
               "measunits"="character", # Impact type (e.g. excess mortality, displacement stock)
               "unitdate"="character", # date associated to the unit (for currencies almost exclusively)
               "est_type"="character", # Estimate type: primary, secondary, modelled
               "src_db"="character", # Source database name of impact estimate or the curated estimate
               "src_org"="character", # Source organisation of impact estimate or the curated estimate
               "src_orgtype"="character", # Source organisation type
               "src_URL"="character", # URL of the impact estimate
               "haztype"="character", # Impacting hazard type
               "hazcluster"="character", # Impacting hazard cluster
               "hazspec"="character", # Impacting specific hazard
               "hazlink"="character", # Associated impactful-hazards to the specific hazard
               "hazpotlink"="character", # Potential other impactful-hazards that may be associated to the specific hazard
               "spat_ID"="character", # ID of the spatial object
               "spat_type"="character", # Spatial object type
               "spat_res"="character", # Spatial resolution of impact estimate
               "spat_srcorg"="character") # Source organisation from where the spatial object comes from

oblig_impGCDB<-c("GCDB_ID","impsub_ID","ISO3","impcat","impsubcat","imp_units",
                 "imp_type","est_type","src_org","src_orgtype","src_URL",
                 "haztype","hazcluster")

GetGCDB_ID<-function(DF,haz="EQ") {
  namerz<-DF%>%
    mutate(haz=haz)%>%
    dplyr::select(haz,ev_sdate,ISO3)%>%
    apply(1,function(x) paste0(x,collapse = "-"))
  
  paste0(namerz,"-GCDB")
}

AddEmptyColImp<-function(DF){
  for(i in which(!names(col_impGCDB)%in%colnames(DF))){
    tmp<-NA
    class(tmp)<-col_impGCDB[i]
    DF$tmp<-tmp
    colnames(DF)[ncol(DF)]<-names(col_impGCDB)[i]
  }
  DF[,names(col_impGCDB)]
}

ImpLabs<-function(ImpDB,nomDB="Desinventar"){
  # Open up the database impact taxonomy conversion file
  imptax<-openxlsx::read.xlsx("./RawData/MostlyImpactData/ConvertImpact_Taxonomy.xlsx")%>%
    filter(src_db==nomDB)
  # Find where the Desinventar data impact estimates stop 
  vlim<-which(colnames(ImpDB)%in%imptax$VarName)
  # For all columns that correspond to impact estimates, return the data
  ImpDB%>%reshape2::melt(measure.vars=colnames(ImpDB)[vlim])%>%
    mutate(VarName=as.character(variable),impvalue=as.numeric(value))%>%
    dplyr::select(-c(variable,value))%>%
    left_join(dplyr::select(imptax,-c("src_orgtype","src_org","src_db")),by="VarName")%>%
    dplyr::select(-VarName)
  
}

# tmp<-googledrive::drive_download("https://docs.google.com/spreadsheets/d/1agqy6DV5VmJuaamVaXZE7jfkDOC5AOhM/edit?usp=sharing&ouid=109118346520870360454&rtpof=true&sd=true",overwrite = T)
# tmp<-openxlsx::read.xlsx(tmp$local_path)

# impGCDB object skeleton
imp_skel<-function(nr=0){
  skelly<-as.data.frame(matrix(NA,nr,length(col_impGCDB)))
  colnames(skelly)<-names(col_impGCDB)
  for(i in 1:ncol(skelly)) class(skelly[,i])<-col_impGCDB[i]
  
  return(skelly)
}

impGCDB_ColClass<-function(DF){
  for(i in 1:ncol(DF)){
    j<-which(names(DF)[i]==names(col_impGCDB))
    # Change the class to make sure this variable is compatible
    class(DF[,i])<-col_impGCDB[j]
  }
  return(DF)
}

# The data.frame component of the impGCDB object
impGCDB_data<-function(DF){
  # Check the column names are what we need and nothing more, nothing less
  if(!all(colnames(DF)%in%names(col_impGCDB))) stop("Error in impact data input fields to form impGCDB object")
  # Incase not all columns were provided
  modDF<-imp_skel(nrow(DF)); modDF[,colnames(DF)]<-DF
  # Check the data types of each column
  DF%<>%impGCDB_ColClass()
  # Re-order the object columns
  DF%<>%dplyr::select(names(col_impGCDB))
  
  return(DF)
}

# Create the class
impGCDB<-setClass("impGCDB", 
         slots = c(obj_info="list",
                   impacts="data.frame"))

# Set the rules of object validity
setValidity("impGCDB", function(object) {
  if(any(!colnames(object@impacts)==names(col_impGCDB))) return("Not all or too many columns present in impGCDB object")
  # Check the data types of each column
  if(!all(sapply(1:ncol(object@impacts),function(i){
    j<-which(names(object@impacts)[i]==names(col_impGCDB))
    # Check the class
    class(object@impacts[,i])==col_impGCDB[j]
  },simplify = T))) return("Class mis-match in the impact object of impGCDB")
  # Obligatory columns to have in impGCDB object
  if(nrow(na.omit(dplyr::select(object@impacts,oblig_impGCDB)))!=nrow(object@impacts)) return("NAs found in obligatory columns of impGCDB column")
  
  TRUE
})

# Initialisation method
setMethod(f="initialize", signature="impGCDB",
          definition=function(.Object,impacts=NULL) {
            .Object@obj_info<-add_impGCDBinfo()
            # Skeleton of impGCDB object
            if(is.null(impacts)) {
              # Assign a skeleton object
              .Object@impacts<-impGCDB_data(imp_skel())
              
            } else .Object@impacts<-impGCDB_data(impacts)
            
            validObject(.Object)
            
            return(.Object)
          })

# Simple example:
# impies<-impGCDB()
