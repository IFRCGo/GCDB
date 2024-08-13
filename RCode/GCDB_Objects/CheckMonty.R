# Check that, upon reading in the Monty JSON file, that the spatial data is in the correct format (list)
checkSpatialMonty<-function(Monty){
  if(!is.null(Monty$impact_Data$spatial) & class(Monty$impact_Data$spatial)=="data.frame"){
    Monty$impact_Data$spatial<-modSpatMonty(Monty$impact_Data$spatial$ID_linkage,
                                            Monty$impact_Data$spatial$spatial_info,
                                            Monty$impact_Data$spatial$source)
  } 
  if(!is.null(Monty$hazard_Data$spatial) & class(Monty$hazard_Data$spatial)=="data.frame"){
    Monty$hazard_Data$spatial<-modSpatMonty(Monty$hazard_Data$spatial$ID_linkage,
                                            Monty$hazard_Data$spatial$spatial_info,
                                            Monty$hazard_Data$spatial$source)
  }
  return(Monty)
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

remSpecChar<-function(vecy){
  if(class(vecy)!="character"){print(vecy); stop("non-character provided to")}
  # Check for special characters, then replace with empty string
  vecy%<>%str_replace_all("['´`]", "")%>%str_replace_all('["]', "")
  # Convert all letters to remove accents
  vecy%>%iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
}
checkCharMonty<-function(Monty){
  # Check the following;
  #   - no special characters
  #   - they are all unique (within their respective groups - events, impacts, hazards)
  Monty$event_Level$ID_linkage$event_ID%<>%remSpecChar()
  Monty$event_Level$ID_linkage$ev_name%<>%remSpecChar()
  Monty$event_Level$spatial$gen_location%<>%remSpecChar()
  Monty$impact_Data$ID_linkage$event_ID%<>%remSpecChar()
  Monty$impact_Data$ID_linkage$imp_sub_ID%<>%remSpecChar()
  Monty$impact_Data$ID_linkage$haz_sub_ID<-
    parallel::mclapply(Monty$impact_Data$ID_linkage$haz_sub_ID,
                       function(x) {
                         if(length(x)==0) return(x)
                         remSpecChar(unlist(x))
                       },mc.cores=ncores)
  Monty$impact_Data$spatial<-
    parallel::mclapply(Monty$impact_Data$spatial, function(x){
      if(length(x$ID_linkage$imp_spat_ID)==0) return(x)
      x$ID_linkage$imp_spat_ID%<>%unlist()%>%remSpecChar()%>%list()
      return(x)
    },mc.cores=ncores)
  if(!is.null(Monty$hazard_Data$spatial)){
    Monty$hazard_Data$spatial<-
      parallel::mclapply(Monty$hazard_Data$spatial, function(x){
        if(length(x$ID_linkage$haz_spat_ID)==0) return(x)
        x$ID_linkage$haz_spat_ID%<>%unlist()%>%remSpecChar()%>%list()
        return(x)
      },mc.cores=ncores)
  }
  
  # Uniqueness of important IDs
  if(any(duplicated(Monty$event_Level$ID_linkage$event_ID))) warning("duplicated event_IDs found in the Monty object")
  if(any(duplicated(Monty$impact_Data$ID_linkage$imp_sub_ID))) warning("duplicated imp_sub_IDs found in the Monty object")
  if(any(duplicated(Monty$hazard_Data$ID_linkage$haz_sub_ID))) warning("duplicated haz_sub_IDs found in the Monty object")
  
  # Check external IDs (only GLIDE numbers for now)
  Monty$event_Level$ID_linkage$all_ext_IDs<-lapply(Monty$event_Level$ID_linkage$all_ext_IDs,function(x){
    if(any(!is.na(x$ext_ID_db) & x$ext_ID_db=="GLIDE")) {
      x%<>%filter(!(ext_ID_db=="GLIDE" & 
                      !(grepl("^[A-Z]{2}-\\d{4}-\\d{6}-[A-Z]{3}$",ext_ID) |
                          grepl("^\\d{4}-\\d{6}-[A-Z]{3}$",ext_ID) |
                          grepl("^\\d{6}-[A-Z]{3}$",ext_ID))))
    } 
    return(x)
  })
  
  return(Monty)
}

# Provided a set of event_IDs, provide a warning that includes the data source info
warnEvsMonty<-function(Monty,evs,texty){
  dbs<-table(Monty$impact_Data$source$imp_src_db[Monty$impact_Data$ID_linkage$event_ID%in%evs])
  if(length(dbs)==0 & length(Monty$hazard_Data)>0) dbs<-unique(Monty$hazard_Data$source$imp_src_db[Monty$hazard_Data$ID_linkage$event_ID%in%evs])
  if(length(dbs)==0 & length(Monty$response_Data)>0) dbs<-unique(Monty$response_Data$source$imp_src_db[Monty$response_Data$ID_linkage$event_ID%in%evs])
  if(length(dbs)==0) {
    warning(paste0("issue with ",length(evs)," events in Monty: NULL ",texty," in unknown database")) 
  } else {
    warning(paste0("issue with ",paste0(unname(dbs),collapse = ", "),
                   " events in Monty: NULL ",texty," in ",
                   paste0(names(dbs),collapse = ", "),
                   " database(s), respectively"))
  }
}

# Check that no entries have NULL haz_spec_code, haz_Ab code, exp_spec_code, imp_type_code, imp_unit_code, event_ID
checkNULLvars<-function(Monty){
  # Specific hazard & abbreviated hazard codes
  indy<-sapply(Monty$event_Level$allhaz_class, function(x) length(x)!=0) &
    sapply(Monty$event_Level$allhaz_class,function(x) (all(is.na(x$haz_Ab)) | all(is.na(x$haz_spec))))
  # Provide a warning about this, including source(s) info
  if(sum(!indy)!=0) {
    warnEvsMonty(Monty,
                 Monty$event_Level$ID_linkage$event_ID[!indy],
                 "specific hazards or abb. hazards")
    # Filter them to keep what we want, filter also impacts, hazards + responses
    Monty%<>%MFilter_Events(indy,allobjs = T)
  }
  
  if(length(Monty$hazard_Data)!=0){
    # Specific hazard
    indy<-!is.na(Monty$hazard_Data$hazard_detail$all_hazs_Ab) |
      !sapply(Monty$hazard_Data$hazard_detail$all_hazs_spec, function(x) {
        class(x)!="character" & length(x)>0
      })
    # Provide a warning about this, including source(s) info
    if(sum(!indy)!=0) {
      warnEvsMonty(Monty,
                   unique(Monty$hazard_Data$ID_linkage$event_ID[!indy]),
                   "specific hazard or abbreviated hazard")
      # Filter them to keep what we want, remove also from impacts, hazards + responses
      Monty%<>%MFilter_Hazards(indy,allobjs = T)
    }
  }
  
  if(length(Monty$impact_Data)!=0){
    # Specific exposure, impact type and impact units
    indy<-apply(dplyr::select(Monty$impact_Data$impact_detail,-"imp_unitdate"),1,function(x) !any(is.na(x)))
    # Provide a warning about this, including source(s) info
    if(sum(!indy)!=0){ 
      warnEvsMonty(Monty,
                   unique(Monty$impact_Data$ID_linkage$event_ID[!indy]),
                   "specific exposure, impact type or impact unit")
      # Filter them to keep what we want, don't remove events or other objects associated to this event_ID
      Monty%<>%MFilter_Impacts(indy)
    }
  }
  
  return(Monty)
}

checkDateMonty<-function(Monty,filties=F){
  # Event start and end dates
  indy<-apply(Monty$event_Level$temporal,1,function(x) !any(is.na(as.Date(x))))
  # Filter them out
  evs<-Monty$event_Level$ID_linkage$event_ID[indy]
  # Provide a warning about this, including source(s) info
  if(sum(!indy)!=0) {
    warnEvsMonty(Monty,
                 Monty$event_Level$ID_linkage$event_ID[!indy],
                 "incorrect event start/end dates")
    # Filter them out, including all the other objects (hazards,...)
    if(filties) Monty%<>%MFilter_Events(indy,allobjs = T)
  }
  
  if(length(Monty$impact_Data)!=0){
    # Event start and end dates
    indy<-apply(Monty$impact_Data$temporal[,1:2],1,function(x) !any(is.na(as.Date(x))))
    # Filter them out
    evs<-unique(Monty$impact_Data$ID_linkage$event_ID[indy])
    # Provide a warning about this, including source(s) info
    if(sum(!indy)!=0) {
      warnEvsMonty(Monty,
                   unique(Monty$impact_Data$ID_linkage$event_ID[!indy]),
                   "incorrect impact start/end dates")
      # Filter them out
      if(filties) Monty%<>%MFilter_Impacts(indy)
    }
  }
  if(length(Monty$hazard_Data)!=0){
    # Event start and end dates
    indy<-apply(Monty$hazard_Data$temporal[,1:2],1,function(x) !any(is.na(as.Date(x))))
    # Filter them out
    evs<-unique(Monty$hazard_Data$ID_linkage$event_ID[indy])
    # Provide a warning about this, including source(s) info
    if(sum(!indy)!=0) {
      warnEvsMonty(Monty,
                   unique(Monty$hazard_Data$ID_linkage$event_ID[!indy]),
                   "incorrect hazard start/end dates")
      # Filter them out
      if(filties) Monty%<>%MFilter_Hazards(indy)
    }
  }
  if(length(Monty$response_Data)!=0){
    # Event start and end dates
    indy<-apply(Monty$response_Data$temporal[,1:2],1,function(x) !any(is.na(as.Date(x))))
    # Filter them out
    evs<-unique(Monty$response_Data$ID_linkage$event_ID[indy])
    # Provide a warning about this, including source(s) info
    if(sum(!indy)!=0) {
      warnEvsMonty(Monty,
                   unique(Monty$response_Data$ID_linkage$event_ID[!indy]),
                   "incorrect response start/end dates")
      # Filter them out
      if(filties) Monty%<>%MFilter_Responses(indy)
    }
  }
  
  return(Monty)
}

checkAwkEvsMonty<-function(Monty){
  # External IDs to each database (such as GLIDE)
  if (class(Monty$event_Level$ID_linkage$all_ext_IDs)!="list") stop("Something went wrong with the external ID variable in the event object")
  if(any(sapply(Monty$event_Level$ID_linkage$all_ext_IDs,function(x) class(x))!="data.frame")) {
    if(any(sapply(Monty$event_Level$ID_linkage$all_ext_IDs,function(x) class(x[[1]]))=="data.frame")) {
      Monty$event_Level$ID_linkage$all_ext_IDs<-lapply(Monty$event_Level$ID_linkage$all_ext_IDs, function(x) {
        if(class(x)=="list" & length(x)==1 & class(x[[1]])=="data.frame") return(x[[1]])
        stop("Something went wrong with the external ID variable in the event object")
      })
    } else stop("Something went wrong with the external ID variable in the event object")
  }
  # Hazard classifications of the events
  if (class(Monty$event_Level$allhaz_class)=="data.frame" &
      all(colnames(Monty$event_Level$allhaz_class)%in%c("all_hazs_Ab","all_hazs_spec"))) {
    Monty$event_Level$allhaz_class<-lapply(1:nrow(Monty$event_Level$allhaz_class),function(i) Monty$event_Level$allhaz_class[i,])
  } else if (class(Monty$event_Level$allhaz_class)!="list") stop("Something went wrong with the allhaz_class variable in the event object")
  if(any(sapply(Monty$event_Level$allhaz_class,function(x) is.null(x$all_hazs_Ab)))) {
    if(any(sapply(Monty$event_Level$allhaz_class,function(x) class(x[[1]]))!="character")) {
      Monty$event_Level$allhaz_class<-lapply(Monty$event_Level$allhaz_class, function(x) x[[1]])
    } else stop("Something went wrong with the allhaz_class variable in the event object")
  }
  return(Monty)
}

checkAwkImpsMonty<-function(Monty){
  # haz_sub_ID
  if (class(Monty$impact_Data$ID_linkage$haz_sub_ID)!="list") stop("Something went wrong with the haz_sub_ID variable in the impact object")
  if(any(sapply(Monty$impact_Data$ID_linkage$haz_sub_ID,function(x){
    if(class(x)=="list" & length(x)==0) return(F) 
    else if (class(x[[1]])=="character") return(F)
    else return(T)
  }))) stop("Something went wrong with the haz_sub_ID variable in the impact object")
  # imp_ext_IDs
  if (class(Monty$impact_Data$ID_linkage$imp_ext_IDs)!="list") stop("Something went wrong with the external ID variable in the impact object")
  if(any(sapply(Monty$impact_Data$ID_linkage$imp_ext_IDs,function(x) class(x))!="data.frame")) {
    if(any(sapply(Monty$impact_Data$ID_linkage$imp_ext_IDs,function(x) class(x[[1]]))=="data.frame")) {
      Monty$impact_Data$ID_linkage$imp_ext_IDs<-lapply(Monty$impact_Data$ID_linkage$imp_ext_IDs, function(x) {
        if(class(x)=="list" & length(x)==1 & class(x[[1]])=="data.frame") return(x[[1]])
        stop("Something went wrong with the external ID variable in the impact object")
      })
    } else stop("Something went wrong with the external ID variable in the impact object")
  }
  
  return(Monty)
}

checkAwkHazsMonty<-function(Monty){
  # haz_ext_IDs
  if (class(Monty$hazard_Data$ID_linkage$haz_ext_IDs)!="list") stop("Something went wrong with the external ID variable in the hazard object")
  if(any(sapply(Monty$hazard_Data$ID_linkage$haz_ext_IDs,function(x) class(x))!="data.frame")) {
    if(any(sapply(Monty$hazard_Data$ID_linkage$haz_ext_IDs,function(x) class(x[[1]]))=="data.frame")) {
      Monty$hazard_Data$ID_linkage$haz_ext_IDs<-lapply(Monty$hazard_Data$ID_linkage$haz_ext_IDs, function(x) {
        if(class(x)=="list" & length(x)==1 & class(x[[1]])=="data.frame") return(x[[1]])
        stop("Something went wrong with the external ID variable in the hazard object")
      })
    } else stop("Something went wrong with the external ID variable in the hazard object")
  }
  
  
  
  
  
  
  
  # all_hazs_spec
  
  
  
  
  
  
  # Check if its a dataframe or not
  if (class(Monty$hazard_Data$hazard_detail$all_hazs_spec)=="data.frame" &
      all(colnames(Monty$hazard_Data$hazard_detail$all_hazs_spec)%in%c("all_hazs_Ab","all_hazs_spec"))) {
    Monty$hazard_Data$hazard_detail$all_hazs_spec<-lapply(1:nrow(Monty$hazard_Data$hazard_detail$all_hazs_spec),function(i) Monty$hazard_Data$hazard_detail$all_hazs_spec[i,])
  } else if (class(Monty$hazard_Data$hazard_detail$all_hazs_spec)!="list") stop("Something went wrong with the all_hazs_spec variable in the hazard object")
  if(any(sapply(Monty$hazard_Data$hazard_detail$all_hazs_spec,function(x) class(x))!="character")){
    if(all(sapply(Monty$hazard_Data$hazard_detail$all_hazs_spec,function(x) class(x))=="list") &
       all(sapply(Monty$hazard_Data$hazard_detail$all_hazs_spec,function(x) class(x[[1]]))=="character")){
      Monty$hazard_Data$hazard_detail$all_hazs_spec<-lapply(Monty$hazard_Data$hazard_detail$all_hazs_spec,function(x) unlist(x))
    } else stop("Something went wrong with the all_hazs_spec variable in the hazard object")
  }
  
  return(Monty)
}

checkAwkListMonty<-function(Monty){
  # Ensure that the spatial data in the Monty object is in the correct format
  Monty%<>%checkSpatialMonty()
  # allhaz_class in the event object
  Monty%<>%checkAwkEvsMonty()
  # haz_sub_ID + imp_ext_IDs in the impact object
  if(length(Monty$impact_Data)!=0) Monty%<>%checkAwkImpsMonty()
  # haz_ext_IDs + all_hazs_spec + concur_haz in hazard object 
  if(length(Monty$hazard_Data)!=0) Monty%<>%checkAwkHazsMonty()
  
  return(Monty)
}

# Time-order the Monty database (by default, this is in database-time order)
ArrangeMonty<-function(Monty){
  # Save this for later
  indy<-1:nrow(Monty$event_Level)
  # EVENTS
  indy<-arrange(data.frame(id=indy,
                           date=Monty$event_Level$temporal$ev_sdate),
                date)%>%pull(id)
  # 'Filter' the data such that the order is defined by the indy index (not boolean)
  Monty%<>%MFilter_Events(indy)
  # IMPACTS
  if(length(Monty$impact_Data)!=0){
    indy<-1:nrow(Monty$impact_Data)
    indy<-arrange(data.frame(id=indy,
                             date=Monty$impact_Data$temporal$imp_sdate),
                  date)%>%pull(id)
    # 'Filter' the data such that the order is defined by the indy index (not boolean)
    Monty%<>%MFilter_Impacts(indy)
  }
  # HAZARDS
  if(length(Monty$hazard_Data)!=0){
    indy<-1:nrow(Monty$hazard_Data)
    indy<-arrange(data.frame(id=indy,
                             date=Monty$hazard_Data$temporal$haz_sdate),
                  date)%>%pull(id)
    # 'Filter' the data such that the order is defined by the indy index (not boolean)
    Monty%<>%MFilter_Hazards(indy)
  }
  # RESPONSE
  if(length(Monty$response_Data)!=0){
    indy<-1:nrow(Monty$response_Data)
    indy<-arrange(data.frame(id=indy,
                             date=Monty$response_Data$temporal$res_sdate),
                  date)%>%pull(id)
    # 'Filter' the data such that the order is defined by the indy index (not boolean)
    Monty%<>%MFilter_Response(indy)
  }
  
  return(Monty)
}


checkHazSpecs<-function(Monty){
  # EVENTS
  Monty$event_Level$allhaz_class%<>%parallel::mclapply(function(x){
    x$all_hazs_spec<-as.character(trimws(unname(unlist(str_split(eval(parse(text=x$all_hazs_spec)),pattern = ":",simplify = T)))))
    x$all_hazs_Ab<-as.character(trimws(unname(unlist(str_split(eval(parse(text=x$all_hazs_Ab)),pattern = ":",simplify = T)))))
    return(x)
  },mc.cores=ncores)
  # HAZARDS
  if(length(Monty$hazard_Data)!=0){
    Monty$hazard_Data$hazard_detail$all_hazs_spec%<>%parallel::mclapply(function(x){
      as.character(trimws(unname(unlist(str_split(x,pattern = ":",simplify = T)))))
    },mc.cores=ncores)
  }
  
  return(Monty)
}

checkMonty<-function(Monty){
  # Check all the awkward lists: all_ext_IDs + allhaz_class in the event object, haz_sub_ID + imp_ext_IDs in the impact object, haz_ext_IDs + all_hazs_spec + concur_haz in hazard object and the spatial elements of both
  Monty%<>%checkAwkListMonty()
  # Check that no entries have NULL haz_spec_code, haz_Ab code, exp_spec_code, imp_type_code, imp_unit_code, event_ID
  Monty%<>%checkNULLvars()
  # Check for special characters in certain character vectors (such as ev_name)
  Monty%<>%checkCharMonty()
  # Check ev_sdate, ev_fdate, imp_sdate, imp_fdate
  Monty%<>%checkDateMonty()
  # Check the haz_spec codes
  Monty%<>%checkHazSpecs()
  
  # Sources checked that they all exist in the src_info taxonomy
  
  # taxonomy checks for impacts, exposure and hazards
  #   exp_spec, imp_type, imp_units, est_type, 
  
  # Check country ISO3C codes, also in the spatial objects of hazards and impacts
  
  
  
  # Add hazpotlink hazards, such as flash floods to tropical cyclones
  
  
  
  # what to do if it fails? warn some, fix some, stop others
  
  # Return the data in time-order
  return(ArrangeMonty(Monty))
}