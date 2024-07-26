GetDessieISOs<-function(){
  DesIsos<-data.frame(isos=
                        c("ago", "alb", "arg", "arm", "atg", "bdi", "bfa", "blr", "blz", "bol",
                          "brb", "btn", "chl", "col", "com", "cpv", "cri", "dji", "dma", "dom",
                          "ecu", "egy", "esp", "eth", "etm", "gha", "gin", "gmb", "gnb", "gnq", 
                          "grd", "gtm", "guy", "hnd", "idn", "irn", "irq", "jam", "jor", "ken", 
                          "khm", "kna", "lao", "lbn", "lbr", "lca", "lka", "mal", "mar", "mdg", 
                          "mdv", "mex", "mli", "mmr", "mne", "mng", "moz", "mus", "mwi", "nam",
                          "ner", "nga", "nic", "npl", "pac", "pak", "pan", "per", "prt", "pry", 
                          "pse", "rwa", "sdn", "sen", "sle", "slv", "som", "srb", "swz", "sy11",
                          "syc", "syr", "tgo", "tls", "tto", "tun", "tur", "tza", "uga", "ury", 
                          "vct", "ven", "vnm", "xkx", "yem", "zmb", "znz", "019", "033", "005"))
  # try to automatically extract as many names as possible
  DesIsos%<>%mutate(country=convIso3Country(isos),
                    actualiso=str_to_upper(isos))
  # We know the annoying ISOs:
  issiso<-c("ETM", "MAL", "PAC", "SY11", "XKX", "ZNZ", "019", "033", "005")
  # Check which ones are NAs and replace them manually
  if(all(DesIsos$actualiso[is.na(DesIsos$country)]%in%issiso) & 
     length(DesIsos$actualiso[is.na(DesIsos$country)])==length(issiso)){
    DesIsos$actualiso[is.na(DesIsos$country)]<-c("TLS","MDV","PAC","SYR","XKX","TZA","IND","IND","IND")
    DesIsos$country[is.na(DesIsos$country)]<-c("Timor-Leste","Maldives",
                                               "Secretary of Pacific Community (23 countries)",
                                               "Syrian Arab Republic","Kosovo","Tanzania",
                                               "India","India","India")
  } else stop("GetDessieISOs: an extra unknown country name was found in the list")
    
  return(DesIsos)
}

GetDessie<-function(iso3,forcer=F){
  iso3%<>%str_to_lower()
  print(iso3)
  # Don't waste time if the file already exists
  if(file.exists(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")) & !forcer) return(T)
  # Temporary location to store the zip file
  temp<-paste0("./RawData/tmp/tmp_",iso3,".zip")
  # Set the maximum timeout limit
  options(timeout = 60)#  options(timeout = max(10, getOption("timeout")))
  # Download the raster file to the location 'temp'
  download.file(paste0("https://www.desinventar.net/DesInventar/download/DI_export_",iso3,".zip"),temp)
  # Output location: one folder per country, to house everything
  outloc<-paste0("./RawData/MostlyImpactData/Desinventar/",iso3)
  # Check the end location exists
  if(!dir.exists(outloc)) dir.create(outloc,showWarnings = F)
  # Unpack the files in the zip document
  unzip(paste0(temp),exdir = outloc)
  
  return(T)
}

DesCols<-c('serial'='ext_ID',
           'muertos'= 'deaths',
           'hay_muertos'='flag_deaths',
           'heridos'= 'injured',
           'hay_heridos'='flag_injured',
           'desaparece'= 'missing',
           'hay_deasparece'='flag_missing',
           'vivdest'= 'houses_destroyed',
           'hay_vivdest'='flag_houses_destroyed',
           'vivafec'= 'houses_damaged',
           'hay_vivafec'='flag_houses_damaged',
           'damnificados'= 'directly_affected',
           'hay_damnificados'='flag_directly_affected',
           'afectados'= 'indirectly_affected',
           'hay_afectados'='flag_indirectly_affected',
           'reubicados'= 'relocated',
           'hay_reubicados'='flag_relocated',
           'evacuados'= 'evacuated',
           'hay_evacuados'='flag_evacuated',
           'valorus'= 'losses_in_dollar',
           'valorloc'= 'losses_local_currency',
           'nescuelas'= 'education_centers',
           'nhospitales'= 'hospitals',
           'nhectareas'= 'damages_in_crops_ha',
           'cabezas'= 'lost_cattle',
           'kmvias'= 'damages_in_roads_mts',
           'level0'= 'level0',
           'level1'= 'level1',
           'level2'= 'level2',
           'name0'= 'name0',
           'name1'= 'name1',
           'name2'= 'name2',
           'latitude' = 'latitude',
           'longitude' = 'longitude',
           'evento'= 'event',
           'glide'='GLIDE',
           'lugar'= 'location',
           'magnitud2'='haz_maxvalue',
           'duracion'='duration',
           'fechano'= 'year',
           'fechames'= 'month',
           'fechadia'= 'day')

RegCols<-c("codregion"="ADMcode",
           "nivel"="ADMlevel",
           "nombre"="regnamloc",
           "nombre_en"="regnamen",
           "x"="centLon",
           "y"="centLat",
           "xmin_"="mnlo",
           "ymin"="mnla",
           "xmax_"="mxlo",
           "ymax"="mxla")

ExtImpDev<-function(xmlly){
  # Extract all the impact estimate tabular information
  impacts<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$fichas),function(i){
    return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$fichas[[i]])))))
  })) %>% distinct(); rownames(impacts)<-NULL
  # Select and rename the columns
  impacts%<>%dplyr::select(any_of(names(DesCols)))
  impacts%<>%setNames(unname(DesCols[colnames(impacts)]))
  # Create one single data column, as a character
  impacts$date<-sapply(1:nrow(impacts),function(i) 
    as.character(as.Date(ISOdate(year = impacts$year[i],
                    month = impacts$month[i],
                    day = impacts$day[i]))))
  # Remove unnecessary columns to save space
  impacts%<>%dplyr::select(-c(year,month,day))
  # convert to integer:
  inties<-c("deaths", "injured", "missing", "houses_destroyed", 
            "houses_damaged", "directly_affected", 
            "indirectly_affected", "relocated", "evacuated", 
            "education_centers", "hospitals", "lost_cattle")
  # convert to numeric:
  nummies<-c("losses_in_dollar", "losses_local_currency", 
             "damages_in_crops_ha", "damages_in_roads_mts",
             "latitude","longitude","duration")
  # Convert all integer and numeric columns
  impacts %<>% mutate_at(inties, as.integer)
  impacts %<>% mutate_at(nummies, as.numeric)
  # by default, set minimal duration to be 1 day
  impacts$duration[is.na(impacts$duration)]<-1 
  # Ensure that un-entered impacts (where the flag = -1) are set to NA
  flaggies<-str_split(grep("flag_",colnames(impacts),value = T),"flag_",simplify = T)[,2]
  # I know it's bad practice to use for loops, but I can't be fucked, quite frankly
  for(fl in flaggies) impacts[is.na(impacts[,paste0("flag_",fl)]) | 
                                impacts[,paste0("flag_",fl)]!=-1,fl]<-NA
  # For the impacts that do not have a flag, set them to NA if they are equal to zero, just in case
  for(im in c('losses_in_dollar','losses_local_currency','education_centers',
    'hospitals','damages_in_crops_ha','lost_cattle','damages_in_roads_mts')) {
    if(im%in%colnames(impacts)) impacts[is.na(impacts[,im]) | impacts[,im]<=1e-5,im]<-NA
  }
  
  return(impacts)
}

FindCol<-function(coln="ADMcode",reggie,ADM){
  # Which column are we trying to match from the regions dataframe?
  reggie$xx<-reggie[[coln]]
  # Find the number of unique, non-NA values for each admin region
  lennies<-unname(apply(ADM@data,2,function(x) length(x[!(duplicated(x) | is.na(x))])))
  # Check the lengths
  checker<-reggie%>%group_by(ADMlevel)%>%
    summarise(innie=sum(!(is.na(xx) | duplicated(xx)))%in%lennies,
              .groups="drop_last"); bodger<-F
  # Check that something was found
  if(sum(checker$innie)==0) {
    # print("No perfect matches")
    # First find the correct admin level
    checker<-reggie%>%group_by(ADMlevel)%>%
      summarise(innie=min(abs(sum(!(is.na(xx) | duplicated(xx)))-lennies)),
                .groups="drop_last")%>%mutate(innie=innie==min(innie))
  }
  # Set the appropriate data frame & remove duplciates and NAs
  minireg<-reggie%>%filter(ADMlevel==checker$ADMlevel[which(checker$innie)])%>%
    distinct()
  # Now find the appropriate data in the admin boundary file
  codin<-apply(ADM@data,2, 
               function(x) {
                 sum(unique(minireg$xx)%in%unique(x[!(duplicated(x) | is.na(x))]))==
                   length(unique(minireg$xx))
               })
  if(sum(codin)>1) {
    print("multiple columns containing matching code values, taking first value")
    codin[which(codin)[2:sum(codin)]]<-F
  }
  # Check that some codes were found
  if(all(!codin)) {
    # print("Patching over the ADM codes: not great!")
    # Recalculate codin from the minimum
    codin<-apply(ADM@data,2, 
                 function(x) {
                   abs(sum(unique(minireg$xx)%in%unique(x[!(duplicated(x) | is.na(x))]))-
                         length(unique(minireg$xx)))
                 })
    codin<-codin==min(codin)
    # Signal that this is a bodge-job
    bodger<-T
  }
  
  return(list(codin=codin,bodger=bodger,reggie=dplyr::select(minireg,-xx)))
}

# This function tries to map the columns between the spatial data to the formatted regions dataframe
# It is ugly... such is life!
# Returns the combined ADMout spatial file (combined in the sense all ADM levels are in one)
LoveExceptions_Mod<-function(ADMout,regions){
  
  prematched<-c()
  for(j in 1:length(ADMout)){
    # Make sure already-matched admin levels don't match again
    if(length(prematched)>0){
      tmp<-filter(regions,!ADMlevel%in%prematched)
    } else tmp<-regions
    # First find the correct admin level
    codecol<-FindCol("ADMcode",tmp,ADMout[[j]])
    # and for the names
    tmp%<>%filter(ADMlevel==unique(codecol$reggie$ADMlevel))
    nomnom<-ifelse(sum(!is.na(tmp$regnamloc))>sum(!is.na(tmp$regnamen)),"regnamloc","regnamen")
    namecol<-FindCol(nomnom,tmp,ADMout[[j]])
    # Change to character to make sure it complies with overall spatial dataframe
    ADMout[[j]]@data%<>%mutate_all(as.character)
    # Which columns are we choosing to keep?
    collies<-c(colnames(ADMout[[j]]@data)[codecol$codin | namecol$codin])
    # Warn for bodgings
    if(codecol$bodger){
      print("Dataframe comparison:")
      print(head(left_join(ADMout[[j]]@data%>%dplyr::select(all_of(collies)),
                codecol$reggie[,1:4],
                by=join_by(!!sym(names(codecol$codin[codecol$codin]))=="ADMcode"))))
    }
    # select only what has been matched
    ADMout[[j]]@data<-ADMout[[j]]@data%>%dplyr::select(all_of(collies))
    # left_join to match variables to the ADM data
    ADMout[[j]]@data<-left_join(ADMout[[j]]@data%>%dplyr::select(all_of(collies)),
              codecol$reggie,
              by=join_by(!!sym(names(codecol$codin[codecol$codin]))=="ADMcode"))%>%
      rename("ADMcode"=!!sym(names(codecol$codin[codecol$codin])))
    # Remove any duplicated or NA values
    ADMout[[j]]<-ADMout[[j]][!(is.na(ADMout[[j]]@data$ADMcode) | duplicated(ADMout[[j]]@data$ADMcode)),]
    # Transfer any names over from english to original
    ADMout[[j]]@data$regnamloc[is.na(ADMout[[j]]@data$regnamloc)]<-
      ADMout[[j]]@data$regnamen[is.na(ADMout[[j]]@data$regnamloc)]
    # Any errors in admin levels is returned as minus 999
    ADMout[[j]]@data$ADMlevel[is.na(ADMout[[j]]@data$ADMlevel)]<- -999
    # Extract bounding box
    for(rr in which(is.na(ADMout[[j]]@data$centLon))){
      # Extract the bounding box of each admin boundary
      bbox<-do.call(rbind,lapply(1:length(ADMout[[j]]@polygons[[rr]]@Polygons), function(pp){
             c(apply(ADMout[[j]]@polygons[[rr]]@Polygons[[pp]]@coords,2,
                     function(x) c(min(x),max(x))))[c(1,3,2,4)]}))
      # Across all polygons of each admin boundary
      bbox<-c(min(bbox[,1]),min(bbox[,2]),max(bbox[,3]),max(bbox[,4]))
      # Now replace all the NA values
      ADMout[[j]]@data[rr,c("centLon","centLat","mnlo","mnla","mxlo","mxla")]<-
        as.character(ADMout[[j]]@polygons[[rr]]@labpt,bbox)
    }
    # Add to list of don't touch:
    prematched%<>%c(unique(codecol$reggie$ADMlevel))
  }
  
  if(length(ADMout)>1) {
    outy<-ADMout[[1]]
    for(i in 2:length(ADMout)) outy%<>%bind(ADMout[[i]])
  } else outy<-ADMout[[1]]
  
  outy@data%>%dplyr::select(all_of(colnames(regions)))
  
  return(outy)
}

LoveExceptions<-function(ADMout,regions){
  
  chch<-sapply(seq_along(ADMout), function(j){
    # Find the number of unique, non-NA values for each admin region
    lennies<-unname(apply(ADMout[[j]]@data,2,function(x) length(x[!(duplicated(x) | is.na(x))])))
    # First find the correct admin level
    checker<-regions%>%group_by(ADMlevel)%>%
      summarise(innie=sum(!(is.na(ADMcode) | duplicated(ADMcode)))%in%lennies,
                .groups="drop_last")
    # Check that something was found
    return(any(checker$innie))
  })
  
  inds<-seq_along(ADMout); goanyway<-F
  if(sum(chch)==(length(chch)-1)) {
    if(!chch[length(chch)]) {
      # nchies<-sort(unique(nchar(ADMout[[3]]$WID)))
      # if(length(nchies>1)) stop("length of the level2 impact ID character string has varying lengths! (WID)")
      #   
      # length(unique(impacts$level2[!grepl("x",impacts$level2) & 
      #                                nchar(impacts$level2)==nchies &
      #                                 !is.na(impacts$level2)]))
      #
      inds<-seq_along(ADMout)[-length(ADMout)]
      # goanyway<-T
    } else stop()
  } else if (sum(chch)<length(chch)) stop()
             
  prematched<-c()
  for(j in inds){
    # Find the number of unique, non-NA values for each admin region
    lennies<-unname(apply(ADMout[[j]]@data,2,function(x) length(x[!(duplicated(x) | is.na(x))])))
    # Make sure already-matched admin levels don't match again
    if(length(prematched)>0){
      tmp<-filter(regions,!ADMlevel%in%prematched)
    } else tmp<-regions
    # First find the correct admin level
    checker<-tmp%>%group_by(ADMlevel)%>%
      summarise(innie=sum(!(is.na(ADMcode) | duplicated(ADMcode)))%in%lennies,
                .groups="drop_last")
    # Check that something was found
    if(goanyway){
      minireg<-regions%>%filter(ADMlevel==2)
    } else if(sum(checker$innie)==0) {
      stop()
    } else {
      # Set the appropriate data frame
      minireg<-regions%>%filter(ADMlevel==checker$ADMlevel[which(checker$innie)])
    }
    # Remove duplciates and NAs
    minireg%<>%na.omit()%>%distinct()
    # Now find the appropriate data in the admin boundary file
    codin<-apply(ADMout[[j]]@data,2, 
                 function(x) {
                   sum(unique(minireg$ADMcode)%in%unique(x[!(duplicated(x) | is.na(x))]))==length(unique(minireg$ADMcode))
                 })
    # Check that some codes were found
    if(all(!codin)) stop()
    if(sum(codin)>1) warning("multiple columns containing matching code values")
    # Else: crack on!
    ADMout[[j]]@data<-ADMout[[j]]@data%>%dplyr::select(colnames(ADMout[[j]]@data)[codin])
    colnames(ADMout[[j]]@data)<-"ADMcode"
    # Change to character to make sure it complies with overall spatial dataframe
    ADMout[[j]]@data$ADMcode%<>%as.character()
    # Remove any duplicated or NA values
    ADMout[[j]]<-ADMout[[j]][!(is.na(ADMout[[j]]@data$ADMcode) | duplicated(ADMout[[j]]@data$ADMcode)),]
    # Replace the modified names to the standardised ones
    ADMout[[j]]@data%<>%left_join(minireg,by="ADMcode")
    # Add to list of don't touch:
    prematched%<>%c(checker$ADMlevel[which(checker$innie)])
  }
  
  if(length(inds)>1) {
    outy<-ADMout[[1]]
    for(i in inds[-1]) outy%<>%bind(ADMout[[i]])
  } else outy<-ADMout[[1]]
  
  return(outy)
  
}

CleanADM<-function(ADM){
  # Copy the object
  tmp<-ADM
  # Create an identifier column for original positions
  tmp@data$numid<-1:nrow(tmp@data)
  # Order the dataframe by this parameter
  tmp@data%<>%arrange(desc(numid))
  # Remove all NAs and duplicated entries 
  indies<-apply(dplyr::select(tmp@data,-numid),1,function(x) all(is.na(x)))
  indies<-!(indies | tmp@data%>%dplyr::select(-numid)%>%duplicated())
  # Return the reduced spatialpolygondataframe
  return(ADM[sort(tmp@data$numid[indies]),])
}

DesADMmodVartyp<-function(ADMout){
  # Integer variables
  inties<-c("ADMlevel")
  # Numeric variables
  nummies<-c("centLon",
             "centLat",
             "mnlo",
             "mnla",
             "mxlo",
             "mxla")
  # Character variables (doesn't need to be done as variables are characters by default)
  charies<-c("ADMcode",
             "regnamloc",
             "regnamen")
  ADMout@data %<>% mutate_at(inties, as.integer)
  ADMout@data %<>% mutate_at(nummies, as.numeric)
  
  return(ADMout)
}

ExtADMDev<-function(xmlly,iso3){
  iso3%<>%str_to_lower()
  # Extract the regions used for the mapping by the database
  regions<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$regiones),function(i){
    return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$regiones[[i]])))))
  })) %>% distinct(); rownames(regions)<-NULL
  
  regions<-do.call(rbind,lapply(seq_along(xmlly$DESINVENTAR$regiones),
                                function(i) {
                                  tmp<-t(as.data.frame(unlist(xmlly$DESINVENTAR$regiones[[i]][names(RegCols)])))
                                  rownames(tmp)<-NULL
                                  missies<-names(RegCols)[!names(RegCols)%in%colnames(tmp)]
                                  if(!is.null(missies)) {
                                    filler<-as.data.frame(matrix(NA,1,length(missies)))
                                    colnames(filler)<-missies
                                    tmp%<>%cbind(filler)
                                  }
                                  tmp%<>%dplyr::select(names(RegCols))
                                  colnames(tmp)<-unname(RegCols)
                                  return(tmp)
                                }))
  # Now let's prepare for the maps
  maps<-as.data.frame(do.call(rbind,lapply(seq_along(xmlly$DESINVENTAR$level_maps),
                                           function(i) {
                                             tmp<-t(as.data.frame(unlist(xmlly$DESINVENTAR$level_maps[[i]][1:2])))
                                             rownames(tmp)<-NULL
                                             missies<-c("map_level","filename")[!c("map_level","filename")%in%colnames(tmp)]
                                             if(is.null(missies) | length(missies)==0) return(tmp)
                                             return(NULL)
                                           })))
  ADMout<-c()
  # Extract the spatial data
  for(i in 1:nrow(maps)){
    # Extract the file name of the shapefile for the admin boundaries
    loccy<-str_split(maps$filename[i],"/",simplify = T); loccy<-loccy[length(loccy)]
    # Adminboundary read-in
    ADM<-sf::st_read(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/",loccy),quiet=T)
    # 
    if(class(ADM$geometry)[1]=="sfc_POINT"){
      next
    }
    # Convert to the spatialpolygonsdataframe
    ADM%<>%as("Spatial")
    # Ensure the projection is consistent
    projection(ADM)<-"+proj=longlat +datum=WGS84 +no_defs"
    # Clean out any NA values or errors
    ADM%<>%CleanADM()
    # Add to list
    ADMout%<>%c(list(ADM))
  }
  # Handle exceptions with the data
  ADMout%<>%LoveExceptions_Mod(regions)
  # Change the variable types
  ADMout%<>%DesADMmodVartyp()
    
  return(ADMout)
}

ReadDessie<-function(iso3, forcer=F){
  iso3%<>%str_to_lower()
  # Temporary save out location
  savout<-paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,"_xml.RData")
  # Extract the aggregated data
  if(file.exists(savout) & !forcer) {
    # Keep only the important columns
    impacts<-readRDS(savout)
  } else {
    xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
    saveRDS(xmlly,savout)
    # Keep only the important columns
    impacts<-ExtImpDev(xmlly)
    # Store it for later
    saveRDS(impacts,savout)
  }
  # Create a folder for the results
  dir.create(paste0("./CleanedData/MostlyImpactData/Desinventar/",iso3),showWarnings = F,recursive = T)
  # Save out to be read in later on
  openxlsx::write.xlsx(impacts,paste0("./CleanedData/MostlyImpactData/Desinventar/",iso3,"/",iso3,".xlsx"))
  # If needed, extract the admin boundary data
  if(!file.exists(paste0("./CleanedData/SocioPoliticalData/Desinventar/",iso3,"/ADM_",iso3,".geojson"))){
    ADMout<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))%>%
      ExtADMDev(iso3)
    # Create a folder for the results
    dir.create(paste0("./CleanedData/SocioPoliticalData/Desinventar/",iso3),showWarnings = F,recursive = T)
    # Save out to be read in later on
    rgdal::writeOGR(ADMout,
                    dsn=paste0("./CleanedData/SocioPoliticalData/Desinventar/",iso3,"/ADM_",iso3,".geojson"),
                    layer = paste0("/ADM_",iso3),
                    driver = "GeoJSON",overwrite_layer = T)
  }
  # Output the safeword... TRUE!
  return(T)
}

WrangleDessie<-function(iso3,forcer=T){
  iso3%<>%str_to_lower()
  
  chk<-tryCatch(GetDessie(iso3,forcer = forcer),error=function(e) NA)
  if(is.na(chk)) return(F)
  chk<-tryCatch(ReadDessie(iso3,forcer = forcer),error=function(e) NA)
  if(is.na(chk)) return(F)
  
  return(T)
}

DesHazards<-function(Dessie){
  # Extract the list of translated Desinventar hazards
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/Desinventar_HIP.xlsx")
  # Make sure to avoid missing out!
  colConv$event%<>%str_to_lower()
  # Also check for duplicates
  colConv%<>%dplyr::select(-ISO3)%>%distinct()
  # Extract the names of the disasters
  haznams<-colConv$event[!is.na(colConv$haz_Ab)]
  # Now remove all non-relevant hazards
  Dessie%<>%mutate(event=str_to_lower(event))%>%filter(event%in%haznams)
  # Reduce the translated vector and merge
  Dessie%<>%left_join(colConv%>%dplyr::select(-c(event_en))%>%distinct(),
                      by = "event",relationship="many-to-one")
  # Remove all irrelevant hazards
  Dessie%<>%filter(!is.na(haz_type))
  
  return(Dessie)
}

# Function to produce the Excel spreadsheet that can be used to translate the hazards
SpitDesTrans<-function(Dessie){
  out<-Dessie%>%group_by(ev_ISO3s)%>%reframe(event=unique(event))
  out$event%<>%str_to_lower()
  out%<>%filter(!duplicated(out$event))
  # Try to automatically translate them using DeepL
  # Find out which languages are available
  deep_langs<-deeplr::available_languages2(auth_key = deepl_token)
  # Translate it!
  colConv<-do.call(rbind,lapply(1:nrow(out),function(i){
    trtr<-tryCatch(deeplr::translate2(out$event[i],auth_key = deepl_token,get_detect = T),error=function(e) NULL)
    if(is.null(trtr)) return(data.frame(ev_ISO3s=out$ev_ISO3s[i],event=out$event[i],event_en=NA,src_lang=NA))
    # Output the expected language from DeepL
    src_lang<-deep_langs$name[deep_langs$language==trtr$source_lang]
    # Output it
    data.frame(ev_ISO3s=out$ev_ISO3s[i],event=out$event[i],event_en=trtr$translation,src_lang=src_lang)
  }))
  # Save it out
  openxlsx::write.xlsx(colConv,"./Taxonomies/MostlyImpactData/Desinventar_HIP.xlsx")
  
  return(colConv)
}

PostModTransies<-function(colConv){
  # General Hazard Definitions 
  colConv$haz_Ab[grepl("earthquake",colConv$event_en,ignore.case = T)]<-"EQ"
  colConv$haz_Ab[grepl("flood",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz_Ab[grepl("inundation",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz_Ab[grepl("tsunami",colConv$event_en,ignore.case = T)]<-"TS"
  colConv$haz_Ab[grepl("tidal wave",colConv$event_en,ignore.case = T)]<-"TS"
  colConv$haz_Ab[grepl("rain",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("storm",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("wind",colConv$event_en,ignore.case = T)]<-"VW"
  colConv$haz_Ab[grepl("lightning",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("surge",colConv$event_en,ignore.case = T)]<-"SS"
  colConv$haz_Ab[grepl("torrent",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz_Ab[grepl("cyclone",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("hurricane",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("tornado",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("typhoon",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("heat",colConv$event_en,ignore.case = T)]<-"HW"
  colConv$haz_Ab[grepl("cold",colConv$event_en,ignore.case = T)]<-"CW"
  colConv$haz_Ab[grepl("frost",colConv$event_en,ignore.case = T)]<-"ET"
  colConv$haz_Ab[grepl("ice ",colConv$event_en,ignore.case = T)]<-"ET"
  colConv$haz_Ab[grepl("fire",colConv$event_en,ignore.case = T)]<-"WF"
  colConv$haz_Ab[grepl("eruption",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz_Ab[grepl("volcan",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz_Ab[grepl("lava",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz_Ab[grepl("landslide",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("liquefaction",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("mudflow",colConv$event_en,ignore.case = T)]<-"MS"
  colConv$haz_Ab[grepl("mud flow",colConv$event_en,ignore.case = T)]<-"MS"
  colConv$haz_Ab[grepl("land slide",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("debris flow",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("rock",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("avalanche",colConv$event_en,ignore.case = T)]<-"AV"
  colConv$haz_Ab[grepl("drought",colConv$event_en,ignore.case = T)]<-"DR"
  colConv$haz_Ab[grepl("hail",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("snow",colConv$event_en,ignore.case = T)]<-"SN"
  colConv$haz_Ab[grepl("epidemic",colConv$event_en,ignore.case = T)]<-"EP"
  colConv$haz_Ab[grepl("biolog",colConv$event_en,ignore.case = T)]<-"EP"
  colConv$haz_Ab[grepl("cyclone & flood",colConv$event_en,ignore.case = T)]<-"TC:FL"
  
  # hazard Types
  colConv$haz_type[colConv$haz_Ab%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
  colConv$haz_type[colConv$haz_Ab%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
  colConv$haz_type[colConv$haz_Ab=="WF"]<-"haztypeenviron"
  colConv$haz_type[colConv$haz_Ab=="EP"]<-"haztypebio"
  colConv$haz_type[grepl("cyclone & flood",colConv$event_en,ignore.case = T)]<-"haztypehydromet"
  
  # Hazard clusters
  colConv$haz_cluster[colConv$haz_Ab=="DR"]<-"hazhmprecip:hazhmtemp"
  colConv$haz_cluster[colConv$haz_Ab=="FL"]<-"hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="ST"]<-"hazhmconv:hazhmwind:hazhmpress:hazhmflood"
  colConv$haz_cluster[grepl("rain",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("wind",colConv$event_en,ignore.case = T)]<-"hazhmwind,hazhmpress"
  colConv$haz_cluster[grepl("lightning",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[colConv$haz_Ab=="ET"]<-"hazhmtemp"
  colConv$haz_cluster[colConv$haz_Ab=="TC"]<-"hazhmwind:hazhmpress:hazhmconv:hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="TS"]<-"hazgeoother:hazhmmarine:hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="EQ"]<-"hazgeoseis"
  colConv$haz_cluster[colConv$haz_Ab=="VO"]<-"hazgeovolc"
  colConv$haz_cluster[colConv$haz_Ab=="WF"]<-"hazenvenvdeg"
  colConv$haz_cluster[grepl("hail",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[colConv$haz_Ab=="LS"]<-"hazgeoseis:hazenvenvdeg:hazgeovolc:hazgeoother"
  colConv$haz_cluster[grepl("rock",colConv$event_en,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("mud",colConv$event_en,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("liquefaction",colConv$event_en,ignore.case = T)]<-"hazgeoseis:hazgeoother"
  colConv$haz_cluster[colConv$haz_Ab=="AV"]<-"hazhmterr"
  colConv$haz_cluster[grepl("surge",colConv$event_en,ignore.case = T)]<-"hazhmmarine:hazhmflood:hazhmwind"
  colConv$haz_cluster[grepl("tidal",colConv$event_en,ignore.case = T)]<-"hazhmmarine:hazhmflood"
  colConv$haz_cluster[grepl("coastal flood",colConv$event_en,ignore.case = T)]<-"hazhmflood:hazhmmarine"
  colConv$haz_cluster[grepl("wave",colConv$event_en,ignore.case = T)]<-"hazhmmarine:hazhmflood"
  colConv$haz_cluster[grepl("hail",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("tropical storm",colConv$event_en,ignore.case = T)]<-"hazhmwind"
  colConv$haz_cluster[grepl("convective storm",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("electric",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("cold wave",colConv$event_en,ignore.case = T)]<-"hazhmtemp"
  
  # Specific Hazards
  # Avalanche
  colConv$haz_spec[colConv$haz_Ab=="AV"]<-"MH0050"
  # Cold wave
  colConv$haz_spec[colConv$haz_Ab=="CW"]<-"MH0040"
  # Drought
  colConv$haz_spec[colConv$haz_Ab=="DR"]<-"MH0035"
  # Dzud
  colConv$haz_spec[colConv$haz_Ab=="DZ"]<-"MH0041"
  # Extreme precipitation
  colConv$haz_spec[colConv$haz_Ab=="EP"]<-"MH0006:MH0007:TL0046"
  # Earthquakes
  colConv$haz_spec[colConv$haz_Ab=="EQ"]<-"GH0001:GH0002"
  # Extreme temperatures
  colConv$haz_spec[colConv$haz_Ab=="ET"]<-"MH0047:MH0040:MH0042:MH0043:MH0037:MH0038:MH0039"
  # Floods
  colConv$haz_spec[colConv$haz_Ab=="FL"]<-"MH0004:MH0005:MH0006:MH0007:MH0008:MH0012"
  # Heatwave
  colConv$haz_spec[colConv$haz_Ab=="HW"]<-"MH0047"
  # Landslide
  colConv$haz_spec[colConv$haz_Ab=="LS"]<-"GH0007:GH0005:MH0051:MH0052"
  # Mudslide
  colConv$haz_spec[colConv$haz_Ab=="MS"]<-"MH0051"
  # Snow
  colConv$haz_spec[colConv$haz_Ab=="SN"]<-"MH0038"
  # Storm surge
  colConv$haz_spec[colConv$haz_Ab=="SS"]<-"MH0027"
  # Storm
  colConv$haz_spec[colConv$haz_Ab=="ST"]<-"MH0059:MH0001:MH0002:MH0003:MH0027:MH0054:MH0060"
  # Tropical cyclone
  colConv$haz_spec[colConv$haz_Ab=="TC"]<-"MH0057:MH0059"
  # Tsunami
  colConv$haz_spec[colConv$haz_Ab=="TS"]<-"GH0006"
  # Volcanic activity
  colConv$haz_spec[colConv$haz_Ab=="VO"]<-"GH0012:GH0013:GH0009:GH0010"
  # Violent winds
  colConv$haz_spec[colConv$haz_Ab=="VW"]<-"MH0054:MH0060"
  # Wildfire
  colConv$haz_spec[colConv$haz_Ab=="WF"]<-"EN0013"
  # Tropical cyclone and flooding
  colConv$haz_spec[colConv$haz_Ab=="TC:FL"]<-"MH0057:MH0059:MH0004:MH0005:MH0006:MH0007:MH0008:MH0012"
  
  # Save it out
  openxlsx::write.xlsx(colConv,"./Taxonomies/MostlyImpactData/Desinventar_HIP.xlsx")
  
  return(colConv)
}

Des2tabGCDB<-function(Dessie){
  # Modify date names
  Dessie$imp_sdate<-Dessie$ev_sdate<-Dessie$imp_unitdate<-Dessie$date
  # Ensure that the event duration is used to specify the date range
  Dessie$imp_fdate<-Dessie$ev_fdate<-as.Date(Dessie$date)+Dessie$duration
  # Any event without a date are automatically removed
  Dessie%<>%filter(!is.na(imp_sdate) | !is.na(imp_fdate))
  # Make sure we can properly match them both
  Dessie$event%<>%str_to_lower()
  # Extract only the relevant hazards
  Dessie%<>%DesHazards()
  # Rename some of the variables
  Dessie%<>%rename("ev_name"="event",
                   "imp_lon"="longitude",
                   "imp_lat"="latitude",
                   "gen_location"="location")
  # Add some of the extra details that are Desinventar-specific
  Dessie%<>%mutate(imp_est_type="esttype_prim",
                   imp_src_URL=paste0("https://www.desinventar.net/DesInventar/download/DI_export_",imp_ISO3s,".zip"),
                   imp_spat_fileloc=imp_src_URL,
                   imp_spat_URL=imp_src_URL,
                   imp_src_db="Desinventar",
                   imp_src_org="UNDRR",
                   imp_spat_srcorg="UNDRR",
                   imp_spat_srcdb="GovDes",
                   imp_spat_URL="https://www.desinventar.net/download.html",
                   imp_src_orgtype="orgtypeacad",
                   imp_spat_covcode="spat_polygon",
                   imp_spat_resunits="adminlevel",
                   imp_spat_crs="EPSG:4326")
  # Admin level resolution
  Dessie$imp_spat_res<-0
  Dessie$imp_spat_res[!is.na(Dessie$level1)]<-1
  Dessie$imp_spat_res[!is.na(Dessie$level2)]<-2
  # Form the spatial ID
  Dessie$imp_spat_ID<-parallel::mclapply(1:nrow(Dessie),function(i){
    if(Dessie$imp_spat_res[i]==0) return(paste0("UNDRR-GovDes-ADM0-",Dessie$imp_ISO3s[i]))
    if(Dessie$imp_spat_res[i]==1) return(paste0("UNDRR-GovDes-ADM1-",Dessie$imp_ISO3s[i],
                                             "-",Dessie$level1[i]))
    if(Dessie$imp_spat_res[i]==2) return(paste0("UNDRR-GovDes-ADM2-",Dessie$imp_ISO3s[i],
                                             "-",Dessie$level2[i]))
    return(NA_character_)
  },mc.cores = ncores)  
  # Just in case the ID numbers were not included in the data
  if(is.null(Dessie$ext_ID)) Dessie$ext_ID<-Dessie$imp_ISO3s else 
    Dessie$ext_ID<-paste0(Dessie$imp_ISO3s,"-",Dessie$ext_ID)
  # Sort out the IDs and external IDs (GLIDE numbers)
  Dessie$all_ext_IDs<-parallel::mclapply(1:nrow(Dessie), function(i){
    # First extract EM-DAT event ID
    out<-data.frame(ext_ID=Dessie$ext_ID[i],
                    ext_ID_db="Desinventar",
                    ext_ID_org="UNDRR")
    # If no other external IDs are provided, return only the Em-DAT ID
    if(is.na(Dessie$GLIDE[i])) return(out) else 
      return(rbind(out,data.frame(ext_ID=Dessie$GLIDE[i],
                                  ext_ID_db="GLIDE",
                                  ext_ID_org="ADRC")))
  },mc.cores = ncores)
  # Generate GCDB event ID
  Dessie$event_ID<-GetMonty_ID(Dessie)
  # Correct the labels of the impacts, melting by impact detail
  Dessie%<>%ImpLabs(nomDB = "Desinventar",dropName = T)
  # Create an impact-specific ID
  Dessie%<>%GetGCDB_impID()
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  Dessie%>%dplyr::select(any_of(MontyJSONnames()))
}

GetDesinventar_ind<-function(ISO3,forcer=F){
  # Desinventar is in lower case ISO3C code
  ISO3%<>%str_to_lower()
  # Find the impact files
  filer<-paste0("./CleanedData/MostlyImpactData/Desinventar/",ISO3,"/",ISO3,".xlsx")
  # Check if the file exists
  if(!file.exists(filer)) {
    # If not, try to extract it from the database
    if(!WrangleDessie(isos[i],forcer = T)) {
      print(paste0("Desinventar data not possible for ", ISO3))
      return(data.frame())
    }
  }
  # Load the data
  out<-openxlsx::read.xlsx(paste0("./CleanedData/MostlyImpactData/Desinventar/",filez[i]))
  # Add the country ISO3 code to the data
  out$imp_ISO3s<-out$ev_ISO3s<-DesIsos$actualiso[DesIsos$isos==isos[i]]
  # Get in tabGCDB format
  impies<-Des2tabGCDB(Dessie)
  # The Desinventar database has many entries per single event, so we take the most recent estimate
  impies<-impies[nrow(impies):1,]#%>%filter(imp_value>0)
  # Find the duplicated elements
  inds<-impies%>%dplyr::select(imp_sub_ID)%>%duplicated()
  
  return(impies[!inds,])
}

GetDesinventar<-function(ISO3s=NULL,forcer=F){
  # Find the impact files
  filez<-list.files("./CleanedData/MostlyImpactData/Desinventar/",
                    pattern = ".xlsx",
                    include.dirs = T,all.files = T,recursive = T,ignore.case = T)
  # Extract data for all countries which have spatial data 
  isos<-stringr::str_split(filez,"/",simplify = T)[,1]
  # Which have admin boundary data?
  # tISOS<-isos%in%stringr::str_split(spatf,"/",simplify = T)[,1]
  # Further filter it down
  tISOS<-!is.na(convIso3Country(isos))
  # User-defined country selection
  if(!is.null(ISO3s)) tISOS <- tISOS & str_to_upper(isos)%in%str_to_upper(ISO3s)
  # Get the translated names of the Desinventar countries
  DesIsos<-GetDessieISOs()
  # Get all countries data
  Dessie<-do.call(dplyr::bind_rows,parallel::mclapply(which(tISOS),function(i){
    # Extract impact data
    out<-openxlsx::read.xlsx(paste0("./CleanedData/MostlyImpactData/Desinventar/",filez[i]))%>%
      mutate(date=as.character(date))
    # if data isn't there, extract it
    if(nrow(out)==0) return(data.frame())
    # if(nrow(out)==0) {
    #   if(!WrangleDessie(isos[i],forcer = T)) return(data.frame())
    #   out<-openxlsx::read.xlsx(paste0("./CleanedData/MostlyImpactData/Desinventar/",filez[i]))
    # }
    # Add the country
    out$imp_ISO3s<-out$ev_ISO3s<-DesIsos$actualiso[DesIsos$isos==isos[i]]
    
    return(out)
  },mc.cores = ncores))
  # Get in tabGCDB format
  impies<-Des2tabGCDB(Dessie)
  # The Desinventar database has many entries per single event, so we take the most recent estimate
  impies<-impies[nrow(impies):1,]#%>%filter(imp_value>0)
  # Find the duplicated elements
  inds<-impies%>%dplyr::select(imp_sub_ID)%>%duplicated()
  
  return(impies[!inds,])
}


convDessie_Monty<-function(forcer=T, ISO3s=NULL, taby=F){
  # Only certain countries have Desinventar databases
  if(is.null(ISO3s)) ISO3s<-GetDessieISOs()$isos
  # Download the most recent data from Desinventar
  if(forcer) {
    wran<-unname(unlist(parallel::mclapply(ISO3s,function(is) WrangleDessie(is,forcer = forcer),mc.cores=min(10,ncores))))
    if(any(!wran)) print("countries that didn't work = ",ISO3s[!wran],collapse(" ,"))
    ISO3s<-ISO3s[wran]
  }
  # Extract the Monty JSON schema template
  dMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  # Create the path for the output
  dir.create("./CleanedData/MostlyHazardData/UNDRR",showWarnings = F)
  
  if(taby) funcy<-function(x) do.call(dplyr::bind_rows,x) else funcy<-MergeMonty
  
  fulldes<-funcy(lapply(ISO3s,function(iso){
    # Extract raw Dessie data
    Dessie<-GetDesinventar_ind(iso)
    # Get rid of repeated entries
    Dessie%<>%distinct()%>%arrange(ev_sdate)%>%
      filter(!is.na(haz_spec) & !is.na(imp_value) & imp_value>0)
    
    if(taby) return(Dessie)
    
    #@@@@@ Event-level data @@@@@#
    # IDs
    ID_linkage<-Add_EvIDlink_Monty(
      do.call(rbind,parallel::mclapply(1:nrow(Dessie),function(i) {
        Dessie$all_ext_IDs[[i]]%>%mutate(event_ID=Dessie$event_ID[i],
                                         ev_name=Dessie$ev_name[i])
      },mc.cores=ncores))
    )
    # Spatial
    spatial<-Add_EvSpat_Monty(
      Dessie%>%dplyr::select(event_ID, ev_ISO3s, gen_location)
    )
    # temporal
    temporal<-Add_EvTemp_Monty(
      Dessie%>%dplyr::select(event_ID,ev_sdate,ev_fdate)
    )
    # Hazards
    allhaz_class<-Add_EvHazTax_Monty(
      Dessie%>%dplyr::select(event_ID, haz_Ab, haz_spec)
    )
    # Gather it all and store it in the template!
    dMonty$event_Level<-data.frame(ev=ID_linkage$event_ID)
    dMonty$event_Level$ID_linkage<-ID_linkage
    dMonty$event_Level$temporal<-temporal
    dMonty$event_Level$spatial<-spatial
    dMonty$event_Level$allhaz_class<-allhaz_class
    dMonty$event_Level$ev<-NULL
    
    #@@@@@ Hazard-level data @@@@@#
    # Nothing to put here as we haven't linked any hazard data yet
    dMonty$hazard_Data<-list()
    
    #@@@@@ Impact-level data @@@@@#
    # First need to ensure that any impacts with zero impacts estimated are removed to prevent bias
    Dessie%<>%filter(!is.na(haz_spec) & !is.na(imp_value) & imp_value>0)%>%distinct()
    # IDs
    ID_linkage<-Add_ImpIDlink_Monty(
      do.call(rbind,parallel::mclapply(1:nrow(Dessie),function(i) {
        Dessie$all_ext_IDs[[i]]%>%mutate(event_ID=Dessie$event_ID[i],
                                         imp_sub_ID=Dessie$imp_sub_ID[i],
                                         haz_sub_ID=NA_character_)
      },mc.cores=ncores))
    )
    # Sources for impact data
    srcy<-do.call(rbind,parallel::mclapply(unique(Dessie$imp_sub_ID),function(ID){
      return(Dessie[Dessie$imp_sub_ID==ID,]%>%
               dplyr::select(imp_src_db,imp_src_URL,imp_src_org)%>%
               slice(1))
    },mc.cores=ncores))
    # impact estimates
    impact_detail<-Dessie%>%distinct(imp_sub_ID,.keep_all = T)%>%
      dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
    # Add temporal information
    temporal<-Dessie%>%distinct(imp_sub_ID,.keep_all = T)%>%
      dplyr::select(imp_sdate,imp_fdate,imp_credate,imp_moddate)
    # Spatial data relevant to the impact estimates
    # multiple-entry rows: imp_ISO3s,imp_spat_res
    spatial<-Add_ImpSpatAll_Monty(
      ID_linkage=Dessie%>%dplyr::select(imp_sub_ID,imp_spat_ID,imp_spat_fileloc),
      spatial_info=Dessie%>%dplyr::select(
        imp_ISO3s,
        imp_lon,
        imp_lat,
        imp_spat_covcode,
        imp_spat_res,
        imp_spat_resunits,
        imp_spat_crs
      ),
      source=Dessie%>%dplyr::select(
        imp_spat_srcdb,
        imp_spat_URL,
        imp_spat_srcorg
      )
    )
    
    # Gather it all and store it in the template!
    # (I know this is hideous, but I don't understand how JSON files can have lists that are also S3 data.frames)
    dMonty$impact_Data<-data.frame(imp_sub_ID=unique(Dessie$imp_sub_ID))
    dMonty$impact_Data$ID_linkage=ID_linkage
    dMonty$impact_Data$source=srcy
    dMonty$impact_Data$impact_detail=impact_detail
    dMonty$impact_Data$temporal=temporal
    dMonty$impact_Data$spatial=spatial
    dMonty$impact_Data$imp_sub_ID<-NULL
    
    #@@@@@ Response-level data @@@@@#
    # Nothing to put here as we haven't linked any response data yet
    dMonty$response_Data<-list()
    #@@@@@ Source Data In Taxonomy Field @@@@@#
    dMonty$taxonomies$src_info<-readxl::read_xlsx("./Taxonomies/Monty_DataSources.xlsx")%>%distinct()
    
    # Write it out just for keep-sake
    write(jsonlite::toJSON(dMonty,pretty = T,auto_unbox=T,na = 'null'),
          paste0("./CleanedData/MostlyHazardData/UNDRR/Desinventar_",iso,"_",Sys.Date(),".json"))
    
    return(dMonty)
  }))
  
  # Write it out just for keep-sake
  write(jsonlite::toJSON(dMonty,pretty = T,auto_unbox=T,na = 'null'),
        paste0("./CleanedData/MostlyHazardData/UNDRR/Desinventar_",Sys.Date(),".json"))
  
  #@@@@@ Checks and validation @@@@@#
  dMonty%<>%checkMonty()
  
  return(dMonty)
}




# 
# DesIsos<-list.dirs("./RawData/MostlyImpactData/Desinventar/",recursive = F,full.names = F)
# 
# impies<-parallel::mclapply(mc.cores = 10, DesIsos, function(iso3) {
# 
#   chk<-GetDessie(iso3,T)
# 
#   xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
# 
#   impacts<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$fichas),function(i){
#     return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$fichas[[i]])))))
#   })) %>% distinct(); rownames(impacts)<-NULL
# 
#   return(impacts)
# })



# DesIsos<-list.dirs("./RawData/MostlyImpactData/Desinventar/",recursive = F,full.names = F)
# 
# desADM<-parallel::mclapply(mc.cores = 10, Des, function(iso3) {
# 
#   chk<-GetDessie(iso3,T)
# 
#   xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
# 
#   regions<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$regiones),function(i){
#     return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$regiones[[i]])))))
#   })) %>% distinct(); rownames(regions)<-NULL
# 
#   maps<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$level_maps),function(i){
#     return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$level_maps[[i]])))))
#   })) %>% distinct(); rownames(maps)<-NULL
#   # Remove any NAs
#   maps%<>%filter(!is.na(filename))
#   # Extract the spatial data
#   mapsout<-lapply(1:nrow(maps),function(j){
#     # Extract the file name of the shapefile for the admin boundaries
#     loccy<-str_split(maps$filename[j],"/",simplify = T); loccy<-loccy[length(loccy)]
#     # Adminboundary read-in
#     sf::st_read(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/",loccy),quiet=T)%>%as.data.frame()%>%dplyr::select(-geometry)
#   })
#   tmp<-tryCatch(do.call(dplyr::bind_rows,mapsout),error=function(e) NA)
#   if(!is.na(tmp)) {mapsout<-tmp; rm(tmp)}
# 
#   saveRDS(list(regions=regions,mapsout=mapsout),paste0("./RawData/MostlyImpactData/Desinventar/Maps/",iso3,"_regionmaps.RData"))
# 
#   return(T)
# })


# tmp<-Dessie[nrow(Dessie):1,]%>%filter(imp_value>0)
# inds<-tmp%>%dplyr::select(imp_sub_ID)%>%duplicated()
# Dessie<-tmp[!inds,]
# saveRDS(Dessie,"./CleanedData/MostlyImpactData/Desinventar/subDessie.RData")


# chk<-sapply(DesIsos, function(is) tryCatch(GetDessie(is),error=function(e) NA),simplify = T)
# 
# chk<-sapply(DesIsos, function(is) {
#   print(paste0("Trying for country: ",is))
#   out<-tryCatch(ReadDessie(is),error=function(e) NA)
#   if(is.na(out)) {print("FAIL")} else print("SUCCESS")
#   return(out)
# },simplify = T)

# chk<-sapply(DesIsos, function(is) WrangleDessie(is),simplify = T)
# chk<-mclapply(DesIsos, function(is) WrangleDessie(str_to_lower(is)),mc.cores = 10)
# chk<-unlist(mclapply(unique(na.omit(impies$imp_ISO3s)), 
#               function(is) tryCatch(GetDessie(is,forcer=T),
#                                     error=function(e) F),
#               mc.cores = 10))
# 
# chk<-mclapply(unique(na.omit(impies$imp_ISO3s))[chk], 
#               function(is) tryCatch(ReadDessie(is,forcer=T),
#                                     error=function(e) F),
#               mc.cores = 10)

# 

# fully<-data.frame(CountryName=DesCountries[unlist(sapply(list.files("./CleanedData/SocioPoliticalData/Desinventar/"), function(st) which(DesIsos==st),simplify = T))],
#                   ISO3C=list.files("./CleanedData/SocioPoliticalData/Desinventar/"),
#                   Status="Complete", RawData="Downloaded", 
#                   ImpactData="Wrangled", ADMboundaries="Wrangled")
# # write_csv2(fully,"./CleanedData/MostlyImpactData/Desinventar/CountryStatus_Full.csv")
# fully<-read_csv2("./CleanedData/MostlyImpactData/Desinventar/CountryStatus_Full.csv")
# 
# Incomplete<-list.files("./CleanedData/MostlyImpactData/Desinventar/"); Incomplete<-Incomplete[!grepl(".csv",Incomplete)]
# Incomplete<-Incomplete[!Incomplete%in%fully$ISO3C]
#   
# Incomplete<-data.frame(CountryName=DesCountries[unlist(sapply(Incomplete, function(st) which(DesIsos==st),simplify = T))],
#                   ISO3C=Incomplete, Status="Incomplete",
#                   RawData="Downloaded", ImpactData="Wrangled",
#                   ADMboundaries="Incomplete")
# 
# # Partially complete countries
# itmp<-Incomplete$ISO3C%in%list.files("./CleanedData/SocioPoliticalData/Desinventar/") &
#   !Incomplete$ISO3C%in%fully$ISO3C
# # Extract them
# ParComp<-Incomplete[itmp,]
# Incomplete<-Incomplete[!itmp,]
# 
# # Modify the partially completed countries
# ParComp$Status<-"Complete"
# ParComp$ADMboundaries<-"Wrangled but with ADM level 2 bodging"
# 
# CurStat<-rbind(fully, ParComp, Incomplete)
# 
# write_csv2(CurStat,"./CleanedData/MostlyImpactData/Desinventar/CountryStatus_all.csv")
# 
# # 
# chk<-sapply(Incomplete$ISO3C, function(is) {
#   print(paste0("Trying for country: ",is))
#   out<-tryCatch(ReadDessie(is),error=function(e) NA)
#   if(is.na(out)) {print("FAIL")} else print("SUCCESS")
#   return(out)
# },simplify = T)













# javascript:window.location='http://www.desinventar.net/DesInventar/stats_excel.jsp?bookmark=1&countrycode=alb&maxhits=100&lang=EN&logic=AND&sortby=0&frompage=/definestats.jsp&bSum=Y&_stat=fichas.fechano,,&nlevels=1&_variables=1,fichas.muertos,fichas.heridos,fichas.desaparece,fichas.vivdest,fichas.vivafec,fichas.damnificados,fichas.afectados,fichas.reubicados,fichas.evacuados,fichas.valorus,fichas.valorloc,fichas.nescuelas,fichas.nhospitales,fichas.nhectareas,fichas.cabezas,fichas.kmvias&rndp=13180'
# 
# "https://www.desinventar.net/DesInventar/stats_spreadsheet.jsp?bookmark=1&countrycode=alb&maxhits=100&lang=EN&logic=AND&sortby=0&frompage=/definestats.jsp&bSum=Y&_stat=fichas.fechano,,&nlevels=1&_variables=1,fichas.muertos,fichas.heridos,fichas.desaparece,fichas.vivdest,fichas.vivafec,fichas.damnificados,fichas.afectados,fichas.reubicados,fichas.evacuados,fichas.valorus,fichas.valorloc,fichas.nescuelas,fichas.nhospitales,fichas.nhectareas,fichas.cabezas,fichas.kmvias&_eventos="


