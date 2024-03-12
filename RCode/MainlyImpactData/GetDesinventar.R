
DesIsos<-c("com", "dji", "eth", "brb", "gmb", "gin", "ken", "mdg", "mli", "mus",
           "moz", "mar", "nam", "ner", "sen", "sle",
           "syc", "tgo", "tun", "uga", "znz", "arg", "blz", "bol", "chl",
           "col", "cri", "ecu", "slv", "gtm", "guy",
           "hnd", "mex", "nic", "pan", "pry", "per", "ury", "ven", "019",
           "033", "005", "irn", "jor", "lao", "lbn",
           "mal", "npl", "pak", "pse", "lka", "sy11", "etm", "vnm", "yem",
           "alb", "esp", "srb", "tur", "atg",
           "dma", "dom", "jam", "grd", "lca", "kna", "vct", "tto", "pac")

DesCountries = c("Comoros", "Djibouti", "Ethiopia", "Barbados", "Gambia", "Guinea", "Kenya",
                "Madagascar", "Mali", "Mauritius",
                "Mozambique", "Morocco", "Namibia", "Niger", "Senegal",
                "Sierra Leone", "Seychelles", "Togo", "Tunisia",
                "Uganda", "Zanzibar (United Rep. of Tanzania)", "Argentina",
                "Belize", "Bolivia", "Chile", "Colombia",
                "Costa Rica", "Ecuador", "El Salvador", "Guatemala", "Guyana",
                "Honduras", "Mexico", "Nicaragua",
                "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela",
                "India Orissa", "India Tamil Nadu",
                "India Uttarakhand", "I. R. Iran", "Jordan", "Laos", "Lebanon",
                "Maldives", "Nepal", "Pakistan",
                "Palestine", "Sri Lanka", "Syrian Arab Republic", "Timor Leste",
                "Viet Nam", "Yemen", "Albania",
                "Spain", "Serbia", "Turkey", "Antigua and Barbuda", "Dominica",
                "Dominican Republic", "Jamaica",
                "Grenada", "Saint Lucia", "Saint Kitts and Nevis",
                "Saint Vincent and the Grenadines",
                "Trinidad and Tobago",
                "Secretary of Pacific Community (23 countries)")

# Link from Desinventar to extract the data
desbaseurl<-"https://www.desinventar.net/DesInventar/download/DI_export_"

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
  download.file(paste0(desbaseurl,iso3,".zip"),temp)
  # Output location: one folder per country, to house everything
  outloc<-paste0("./RawData/MostlyImpactData/Desinventar/",iso3)
  # Check the end location exists
  if(!dir.exists(outloc)) dir.create(outloc)
  # Unpack the files in the zip document
  unzip(paste0(temp),exdir = outloc)
  
  return(T)
}

DesCols<-c('muertos'= 'deaths',
           'heridos'= 'injured',
           'desaparece'= 'missing',
           'vivdest'= 'houses_destroyed',
           'vivafec'= 'houses_damaged',
           'damnificados'= 'directly_affected',
           'afectados'= 'indirectly_affected',
           'reubicados'= 'relocated',
           'evacuados'= 'evacuated',
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
           "evento"= "event",
           "lugar"= "location",
           "fechano"= "year",
           "fechames"= "month",
           "fechadia"= "day")

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

# convert to integer:
inties<-c("deaths", "injured", "missing", "houses_destroyed", 
          "houses_damaged", "directly_affected", 
          "indirectly_affected", "relocated", "evacuated", 
          "education_centers", "hospitals", "lost_cattle")

nummies<-c("losses_in_dollar", "losses_local_currency", 
           "damages_in_crops_ha", "damages_in_roads_mts")

ExtImpDev<-function(xmlly){
  # Extract all the impact estimate tabular information
  impacts<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$fichas),function(i){
    return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$fichas[[i]])))))
  })) %>% distinct(); rownames(impacts)<-NULL
  
  stop("ExtImpDev - need to rename the columns from the country xml file")
  
  # Create one single data column, as a character
  impacts$date<-sapply(1:nrow(impacts),function(i) 
    as.character(as.Date(ISOdate(year = impacts$year[i],
                    month = impacts$month[i],
                    day = impacts$day[i]))))
  # Remove unnecessary columns to save space
  impacts%<>%dplyr::select(-c(year,month,day))
  # Convert all integer and numeric columns
  impacts %<>% mutate_at(inties, as.integer)
  impacts %<>% mutate_at(nummies, as.numeric)
  
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
    print("No perfect matches")
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
    print("Patching over the ADM codes: not great!")
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
    # Warn for bodgings
    if(codecol$bodger){
      print("Dataframe comparison:")
      print(head(codecol$reggie[,1:4]))
      print(head(ADMout[[j]]@data))
      print("---")
      print("---")
      print("---")
      print("ADMcode:")
      print(head(sort(ADMout[[j]]@data[,codecol$codin])))
      print(head(sort(codecol$reggie$ADMcode)))
      print("---")
      print("---")
      print("---")
      print("ADMname:")
      print(head(sort(ADMout[[j]]@data[,namecol$codin])))
      print(head(sort(codecol$reggie$regnamen)))
      print(" ")
    }
    # Which columns are we choosing to keep?
    collies<-c(colnames(ADMout[[j]]@data)[codecol$codin | namecol$codin])
    # select only what has been matched
    ADMout[[j]]@data<-ADMout[[j]]@data%>%dplyr::select(all_of(collies))
    # Make sure the names correspond
    colnames(ADMout[[j]]@data)[colnames(ADMout[[j]]@data)==names(codecol$codin[codecol$codin])]<-"ADMcode"
    colnames(ADMout[[j]]@data)[colnames(ADMout[[j]]@data)==names(namecol$codin[namecol$codin])]<-"regnamen"
    # Change to character to make sure it complies with overall spatial dataframe
    ADMout[[j]]@data%<>%mutate_all(as.character)
    # Remove any duplicated or NA values
    ADMout[[j]]<-ADMout[[j]][!(is.na(ADMout[[j]]@data$ADMcode) | duplicated(ADMout[[j]]@data$ADMcode)),]
    # Replace the modified names to the standardised ones
    ADMout[[j]]@data%<>%left_join(codecol$reggie,
                                  by=c("ADMcode","regnamen"))
    # Transfer any names over from english to original
    ADMout[[j]]@data$regnamloc[is.na(ADMout[[j]]@data$regnamloc)]<-
      ADMout[[j]]@data$regnamen[is.na(ADMout[[j]]@data$regnamloc)]
    # Any errors in admin levels is returned as minus 999
    ADMout[[j]]@data$ADMlevel[is.na(ADMout[[j]]@data$ADMlevel)]<- -999
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

ChangeVarType<-function(ADMout){
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
  
  stop("ExtADMDev not ready to extract all of the variables from the spatial data")
  
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
  ADMout%<>%ChangeVarType()
    
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
  Dessie%<>%left_join(colConv%>%dplyr::select(-c(event_en)),by = "event")
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
  colConv$haz[grepl("earthquake",colConv$event_en,ignore.case = T)]<-"EQ"
  colConv$haz[grepl("flood",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz[grepl("inundation",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz[grepl("tsunami",colConv$event_en,ignore.case = T)]<-"TS"
  colConv$haz[grepl("tidal wave",colConv$event_en,ignore.case = T)]<-"TS"
  colConv$haz[grepl("rain",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz[grepl("storm",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz[grepl("wind",colConv$event_en,ignore.case = T)]<-"VW"
  colConv$haz[grepl("lightning",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz[grepl("surge",colConv$event_en,ignore.case = T)]<-"SS"
  colConv$haz[grepl("torrent",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz[grepl("cyclone",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz[grepl("hurricane",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz[grepl("tornado",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz[grepl("typhoon",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz[grepl("heat",colConv$event_en,ignore.case = T)]<-"HW"
  colConv$haz[grepl("cold",colConv$event_en,ignore.case = T)]<-"CW"
  colConv$haz[grepl("frost",colConv$event_en,ignore.case = T)]<-"ET"
  colConv$haz[grepl("ice ",colConv$event_en,ignore.case = T)]<-"ET"
  colConv$haz[grepl("fire",colConv$event_en,ignore.case = T)]<-"WF"
  colConv$haz[grepl("eruption",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz[grepl("volcan",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz[grepl("lava",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz[grepl("landslide",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz[grepl("liquefaction",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz[grepl("mudflow",colConv$event_en,ignore.case = T)]<-"MS"
  colConv$haz[grepl("mud flow",colConv$event_en,ignore.case = T)]<-"MS"
  colConv$haz[grepl("land slide",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz[grepl("debris flow",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz[grepl("rock",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz[grepl("avalanche",colConv$event_en,ignore.case = T)]<-"AV"
  colConv$haz[grepl("drought",colConv$event_en,ignore.case = T)]<-"DR"
  colConv$haz[grepl("hail",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz[grepl("snow",colConv$event_en,ignore.case = T)]<-"SN"
  colConv$haz[grepl("epidemic",colConv$event_en,ignore.case = T)]<-"EP"
  colConv$haz[grepl("biolog",colConv$event_en,ignore.case = T)]<-"EP"
  colConv$haz[grepl("cyclone & flood",colConv$event_en,ignore.case = T)]<-"TC,FL"
  
  # hazard Types
  colConv$haz_type[colConv$haz%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
  colConv$haz_type[colConv$haz%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
  colConv$haz_type[colConv$haz=="WF"]<-"haztypeenviron"
  colConv$haz_type[colConv$haz=="EP"]<-"haztypebio"
  colConv$haz_type[grepl("cyclone & flood",colConv$event_en,ignore.case = T)]<-"haztypehydromet"
  
  # Hazard clusters
  colConv$haz_cluster[colConv$haz=="DR"]<-"hazhmprecip,hazhmtemp"
  colConv$haz_cluster[colConv$haz=="FL"]<-"hazhmflood"
  colConv$haz_cluster[colConv$haz=="ST"]<-"hazhmconv,hazhmwind,hazhmpress,hazhmflood"
  colConv$haz_cluster[grepl("rain",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("wind",colConv$event_en,ignore.case = T)]<-"hazhmwind,hazhmpress"
  colConv$haz_cluster[grepl("lightning",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[colConv$haz=="ET"]<-"hazhmtemp"
  colConv$haz_cluster[colConv$haz=="TC"]<-"hazhmwind,hazhmpress,hazhmconv,hazhmflood"
  colConv$haz_cluster[colConv$haz=="TS"]<-"hazgeoother,hazhmmarine,hazhmflood"
  colConv$haz_cluster[colConv$haz=="EQ"]<-"hazgeoseis"
  colConv$haz_cluster[colConv$haz=="VO"]<-"hazgeovolc"
  colConv$haz_cluster[colConv$haz=="WF"]<-"hazenvenvdeg"
  colConv$haz_cluster[grepl("hail",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[colConv$haz=="LS"]<-"hazgeoseis,hazenvenvdeg,hazgeovolc,hazgeoother"
  colConv$haz_cluster[grepl("rock",colConv$event_en,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("mud",colConv$event_en,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("liquefaction",colConv$event_en,ignore.case = T)]<-"hazgeoseis,hazgeoother"
  colConv$haz_cluster[colConv$haz=="AV"]<-"hazhmterr"
  colConv$haz_cluster[grepl("surge",colConv$event_en,ignore.case = T)]<-"hazhmmarine,hazhmflood,hazhmwind"
  colConv$haz_cluster[grepl("tidal",colConv$event_en,ignore.case = T)]<-"hazhmmarine,hazhmflood"
  colConv$haz_cluster[grepl("coastal flood",colConv$event_en,ignore.case = T)]<-"hazhmflood,hazhmmarine"
  colConv$haz_cluster[grepl("wave",colConv$event_en,ignore.case = T)]<-"hazhmmarine,hazhmflood"
  colConv$haz_cluster[grepl("hail",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("tropical storm",colConv$event_en,ignore.case = T)]<-"hazhmwind"
  colConv$haz_cluster[grepl("convective storm",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("electric",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("cold wave",colConv$event_en,ignore.case = T)]<-"hazhmtemp"
  
  # Specific Hazards
  colConv$haz_spec[colConv$haz=="EQ"]<-"GH0001,GH0002"
  colConv$haz_potlink[colConv$haz=="EQ"]<-paste0(c("GH0003","GH0004","GH0005","GH0006","GH0007"),collapse = ",")
  
  # Save it out
  openxlsx::write.xlsx(colConv,"./Taxonomies/MostlyImpactData/Desinventar_HIP.xlsx")
  
  return(colConv)
}

Des2tabGCDB<-function(Dessie){
  # Modify date names
  Dessie$imp_sdate<-Dessie$imp_fdate<-Dessie$ev_sdate<-Dessie$ev_fdate<-Dessie$imp_unitdate<-Dessie$date
  # Any event without a date are automatically removed
  Dessie%<>%filter(!is.na(imp_sdate))
  # Make sure we can properly match them both
  Dessie$event%<>%str_to_lower()
  # Extract only the relevant hazards
  Dessie%<>%DesHazards()
  # Rename the event name
  colnames(Dessie)[colnames(Dessie)=="event"]<-"ev_name"
  Dessie$ev_name_lang<-"lang_xxx"
  # Add the continent, then remove the unnecesary layers
  Dessie%<>%mutate(region=convIso3Continent(imp_ISO3s))%>%
    dplyr::select(-c(date,level0,name0))%>%filter(!is.na(region))
  # Generate GCDB event ID
  Dessie$event_ID<-GetMonty_ID(Dessie)
  # Add some of the extra details that are Desinventar-specific
  Dessie%<>%mutate(
    imp_est_type="esttype_prim",
    imp_src_URL=paste0(desbaseurl,imp_ISO3s,".zip"),
    imp_src_org="Local Government Estimate",
    imp_src_db="Desinventar",
    imp_src_orgtype="orgtypegov",
    imp_spat_covcode="spat_polygon",
    imp_spat_ID=apply(Dessie[,c("level1","level2")],1,function(x) {
      if(all(is.na(x))) return(NA_character_)
      if(any(is.na(x))) return(x[!is.na(x)])
      paste0(c(ifelse(is.na(x[1]),"",x[1]),
               ifelse(is.na(x[2]),"",x[2])),
             collapse = ",")
    },simplify = T),
    imp_spat_srcorg="Local Government Estimate",
    imp_spat_srcdb="Desinventar",
    imp_spat_URL=paste0(desbaseurl,imp_ISO3s,".zip"),
    imp_spat_covcode="spat_polygon",
    imp_spat_res=0,
    imp_spat_resunits="adminlevel",
    imp_spat_fileread="spatfstanshp",
    imp_spat_crs="EPSG:4326")
  # Admin level resolution
  Dessie$imp_spat_res[!is.na(Dessie$level1)]<-1; Dessie$imp_spat_res[!is.na(Dessie$level2)]<-2
  # Clean up!
  Dessie%<>%dplyr::select(-c(level1,level2,name1,name2))
  # Correct the labels of the impacts, melting by impact detail
  Dessie%<>%ImpLabs(nomDB = "Desinventar")
  # Create an impact-specific ID
  Dessie%<>%GetGCDB_impID()
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  Dessie%>%AddEmptyColImp()
}

GetDesinventar<-function(){
  # Find the impact files
  filez<-list.files("./CleanedData/MostlyImpactData/Desinventar/",
                    pattern = ".xlsx",
                    include.dirs = T,all.files = T,recursive = T,ignore.case = T)
  # Find the Desinventar spatial files
  spatf<-list.files("./CleanedData/SocioPoliticalData/Desinventar/",
                    pattern = "ADM_",
                    include.dirs = T,all.files = T,recursive = T,ignore.case = T)
  # Extract data for all countries which have spatial data 
  isos<-stringr::str_split(filez,"/",simplify = T)[,1]
  # Which have data?
  tISOS<-isos%in%stringr::str_split(spatf,"/",simplify = T)[,1]
  # Get all countries data
  Dessie<-do.call(rbind,lapply(which(tISOS),function(i){
    # Extract impact data
    out<-openxlsx::read.xlsx(paste0("./CleanedData/MostlyImpactData/Desinventar/",filez[i]))
    # Add the country
    out$imp_ISO3s<-stringr::str_to_upper(isos[i])
    
    return(out)
  }))
  # Get in tabGCDB format
  impies<-Des2tabGCDB(Dessie)
  # The Desinventar database has many entries per single event, so we take the most recent estimate
  impies<-impies[nrow(impies):1,]#%>%filter(imp_value>0)
  # Find the duplicated elements
  inds<-impies%>%dplyr::select(imp_sub_ID)%>%duplicated()
  
  return(impies[!inds,])
  # Form a GCDB impacts object from EMDAT data (if there is a problem, return an empty tabGCDB object)
  # tryCatch(new("tabGCDB",impies),error=function(e) new("tabGCDB"))
}


convDessie_Monty<-function(){
  
  stop("Ensure Dessie has record-level ID numbers that include the country code")
  
  # Extract raw Dessie data
  Dessie<-GetDesinventar()
  # Get rid of repeated entries
  Dessie%<>%distinct(imp_sub_ID,.keep_all = TRUE)%>%
    arrange(ev_sdate)
  # Extract the Monty JSON schema template
  dMonty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
  #@@@@@ Impact-level data @@@@@#
  # IDs
  ID_linkage<-Add_ImpIDlink_Monty(
    rbind(Dessie%>%mutate(ext_ID_db="Desinventar",ext_ID_org="UNDRR")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, Dessie_ID,
                          ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=Dessie_ID)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org),
          Dessie%>%filter(!is.na(ext_IDs))%>%mutate(ext_ID_db="Desinventar",ext_ID_org="UNDRR")%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, ext_IDs, ext_ID_dbs, ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)%>%
            dplyr::select(event_ID, imp_sub_ID, haz_sub_ID, 
                          ext_ID, ext_ID_db, ext_ID_org)
    )
  )
  # Sources for impact data
  source<-Dessie%>%dplyr::select(imp_src_db,imp_src_URL,imp_src_org)
  # impact estimates
  impact_detail<-Dessie%>%
    dplyr::select(exp_spec,imp_value,imp_type,imp_units,imp_est_type,imp_unitdate)
  # Add temporal information
  temporal<-Dessie%>%dplyr::select(imp_sdate,imp_fdate)
  # Spatial data relevant to the impact estimates
  # multiple-entry rows: imp_spat_rowname,imp_spat_colname,imp_ISO3s,imp_spat_res,imp_spat_fileread
  spatial<-Add_ImpSpatAll_Monty(
    ID_linkage=data.frame(
      imp_sub_ID=Dessie$imp_sub_ID,
      imp_spat_ID="GO-ADM0-World-shp",
      imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
      imp_spat_colname="iso3",
      imp_spat_rowname=Dessie$imp_ISO3s
    ),
    spatial_info=Dessie%>%dplyr::select(
      imp_ISO3s,
      imp_spat_covcode,
      imp_spat_res,
      imp_spat_resunits,
      imp_spat_fileread,
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
  dMonty$impact_Data$source=source
  dMonty$impact_Data$impact_detail=impact_detail
  dMonty$impact_Data$temporal=temporal
  dMonty$impact_Data$spatial=spatial
  dMonty$impact_Data$imp_sub_ID<-NULL
  
  #@@@@@ Event-level data @@@@@#
  # IDs
  ID_linkage<-Add_EvIDlink_Monty(
    # By default, only Desinventar eventIDs are used
    rbind(Dessie%>%mutate(ext_ID_db="Dessie",ext_ID_org="UNDRR")%>%
            dplyr::select(event_ID, ev_name, Dessie_ID,ext_ID_db,ext_ID_org)%>%
            rename(ext_ID=Dessie_ID),
          Dessie%>%filter(!is.na(ext_IDs))%>%
            dplyr::select(event_ID, ev_name, ext_IDs,ext_ID_dbs,ext_ID_orgs)%>%
            rename(ext_ID=ext_IDs,ext_ID_db=ext_ID_dbs,ext_ID_org=ext_ID_orgs)
    )
  )
  # Spatial
  spatial<-Add_EvSpat_Monty(
    Dessie%>%dplyr::select(event_ID,imp_ISO3s,location)%>%
      rename(ev_ISO3s=imp_ISO3s,gen_location=location)
  )
  # temporal
  temporal<-Add_EvTemp_Monty(
    Dessie%>%dplyr::select(event_ID,imp_sdate,imp_fdate,ev_sdate,ev_fdate)
  )
  # Hazards
  hazs<-Dessie%>%dplyr::select(event_ID, haz_Ab, haz_spec)
  allhaz_class<-Add_EvHazTax_Monty(
    do.call(rbind,lapply(1:nrow(hazs),function(i){
      specs<-c(str_split(hazs$haz_spec[i],":",simplify = T))
      outsy<-hazs[rep(i,length(specs)),]
      outsy$haz_spec<-specs
      return(outsy)
    }))
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
  
  #@@@@@ Response-level data @@@@@#
  # Nothing to put here as we haven't linked any response data yet
  dMonty$response_Data<-list()
  
  #@@@@@ Source Data In Taxonomy Field @@@@@#
  dMonty$taxonomies$src_info<-data.frame(
    src_org_code="UNDRR",
    src_org_lab="United Nations Disaster Risk Reduction (UNDRR)",
    src_org_typecode="orgtypeun",
    src_org_typelab="UN & International Organisations",
    src_org_email="undrr-bonn@un.org",
    src_db_code="Desinventar",
    src_db_lab="Disaster Inventory System (Desinventar)",
    src_db_attr="mediator",
    src_db_lic="unknown",
    src_db_URL="www.gdacs.org",
    src_addinfo=""
  )
  # Create the path for the output
  dir.create("./CleanedData/MostlyHazardData/Desinventar",showWarnings = F)
  # Write it out just for keep-sake
  write(jsonlite::toJSON(dMonty,pretty = T,auto_unbox=T),
        paste0("./CleanedData/MostlyHazardData/Desinventar/Desinventar_",Sys.Date(),".json"))
  
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


