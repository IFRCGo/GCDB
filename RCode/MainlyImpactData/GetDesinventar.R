

DesIsos<-c("com", "dji", "eth", "gmb", "gin", "ken", "mdg", "mli", "mus",
           "moz", "mar", "nam", "ner", "sen", "sle",
           "syc", "tgo", "tun", "uga", "znz", "arg", "blz", "bol", "chl",
           "col", "cri", "ecu", "slv", "gtm", "guy",
           "hnd", "mex", "nic", "pan", "pry", "per", "ury", "ven", "019",
           "033", "005", "irn", "jor", "lao", "lbn",
           "mal", "npl", "pak", "pse", "lka", "sy11", "etm", "vnm", "yem",
           "alb", "esp", "srb", "tur", "atg",
           "dma", "dom", "jam", "grd", "lca", "kna", "vct", "tto", "pac")

DesCountries = c("Comoros", "Djibouti", "Ethiopia", "Gambia", "Guinea", "Kenya",
                "Madagascar", "Mali", "Mauritius",
                "Mozambique", "Morocco", "Namibia", "Niger", "Senegal",
                "Sierra Leone", "Seychelles", "Togo", "Tunisia",
                "Uganda", "Zanzibar (United Rep. of Tanzania)", "Argentina",
                "Belize", "Bolivia", "Chile", "Colombia",
                "Costa Rica", "Ecuador", "El Salvador", "Guatemla", "Guyana",
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
                "Secretary of Pacific Community (23 counries)")

GetDessie<-function(iso3){
  # Link from Desinventar to extract the data
  baseurl<-"https://www.desinventar.net/DesInventar/download/DI_export_"
  # Temporary location to store the zip file
  temp<-"./RawData/tmp/tmp.zip"
  # Download the raster file to the location 'temp'
  download.file(paste0(baseurl,iso3,".zip"),temp)
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
           "nombre"="regnamloc",
           "nombre_en"="regnamen",
           "x"="centLon",
           "y"="centLat",
           "xmin_"="mnlo",
           "ymin"="mnla",
           "xmax_"="mxlo",
           "ymax"="mxla",
           "nivel"="ADMlevel")

# convert to integer:
inties<-c("deaths", "injured", "missing", "houses_destroyed", 
          "houses_damaged", "directly_affected", 
          "indirectly_affected", "relocated", "evacuated", 
          "education_centers", "hospitals", "lost_cattle")

nummies<-c("losses_in_dollar", "losses_local_currency", 
           "damages_in_crops_ha", "damages_in_roads_mts")

ExtImpDev<-function(xmlly){
  impacts<-do.call(rbind,lapply(seq_along(xmlly$DESINVENTAR$fichas),
                       function(i) {
                         tmp<-t(as.data.frame(unlist(xmlly$DESINVENTAR$fichas[[i]][names(DesCols)])))
                         rownames(tmp)<-NULL
                         missies<-names(DesCols)[!names(DesCols)%in%colnames(tmp)]
                         if(!is.null(missies)) {
                           filler<-as.data.frame(matrix(NA,1,length(missies)))
                           colnames(filler)<-missies
                           tmp%<>%cbind(filler)
                         }
                         tmp%<>%dplyr::select(names(DesCols))
                         colnames(tmp)<-unname(DesCols)
                         return(tmp)
                       })) %>% distinct()
  # Create one single data column, as a character
  impacts$date<-sapply(1:nrow(impacts),function(i) 
    as.character(as.Date(ISOdate(year = impacts$year[i],
                    month = impacts$month[i],
                    day = impacts$day[i]))))
  # Remove unnecessary columns to save space
  impacts%<>%dplyr::select(-c(year,month,day))
  # Convert all integer and numeric columns
  for(x in inties) impacts[,x]%<>%as.integer()
  for(x in nummies) impacts[,x]%<>%as.numeric()
  
  return(impacts)
}

LoveExceptions<-function(ADMout,regions){
  
  if(length(ADMout)>1) {ADMout<-do.call(bind,ADMout)} else ADMout<-ADMout[[1]]
  # Change for the merge
  colnames(ADMout@data)<-c("ADMcode","regnamloc")
  # Add all the rest of the polygon data into the polygon data frame
  ADMout@data%<>%merge(regions)
  
}

ExtADMDev<-function(xmlly,iso3){
  # Extract the regions used for the mapping by the database
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
  # Extract the spatial data
  ADMout<-lapply(1:nrow(maps),function(i) {
    # Extract the file name of the shapefile for the admin boundaries
    loccy<-str_split(maps$filename[i],"/",simplify = T); loccy<-loccy[length(loccy)]
    # Adminboundary read-in
    ADM<-as(sf::st_read(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/",loccy)),"Spatial")
    # Ensure the projection is consistent
    projection(ADM)<-"+proj=longlat +datum=WGS84 +no_defs"
    
    return(ADM)
  })
  # Handle exceptions with the data
  ADMout%<>%LoveExceptions(regions)
  
  return(ADMout)
}

ReadDessie<-function(iso3){
  # Extract the aggregated data
  xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
  # Keep only the important columns
  impacts<-ExtImpDev(xmlly)
  # Extract the admin boundary data
  ADMout<-ExtADMDev(xmlly,iso3)
  # Create a folder for the results
  dir.create(paste0("./CleanedData/SocioPoliticalData/Desinventar/",iso3),showWarnings = F,recursive = T)
  # Save out to be read in later on
  rgdal::writeOGR(ADMout,
                  dsn=paste0("./CleanedData/SocioPoliticalData/Desinventar/",iso3,"/ADM_",iso3,".geojson"),
                  layer = paste0("/ADM_",iso3),
                  driver = "GeoJSON",overwrite_layer = T)
  # Create a folder for the results
  dir.create(paste0("./CleanedData/MostlyImpactData/Desinventar/",iso3),showWarnings = F,recursive = T)
  # Save out to be read in later on
  write_csv2(impacts,paste0("./CleanedData/MostlyImpactData/Desinventar/",iso3,"/",iso3,".csv"))
  # Output the safeword... TRUE!
  return(T)
  
}

WrangleDessie<-function(iso3){
  GetDessie(iso3)
  ReadDessie(iso3)
}

WrangleDessie("eth")

# 










javascript:window.location='http://www.desinventar.net/DesInventar/stats_excel.jsp?bookmark=1&countrycode=alb&maxhits=100&lang=EN&logic=AND&sortby=0&frompage=/definestats.jsp&bSum=Y&_stat=fichas.fechano,,&nlevels=1&_variables=1,fichas.muertos,fichas.heridos,fichas.desaparece,fichas.vivdest,fichas.vivafec,fichas.damnificados,fichas.afectados,fichas.reubicados,fichas.evacuados,fichas.valorus,fichas.valorloc,fichas.nescuelas,fichas.nhospitales,fichas.nhectareas,fichas.cabezas,fichas.kmvias&rndp=13180'

"https://www.desinventar.net/DesInventar/stats_spreadsheet.jsp?bookmark=1&countrycode=alb&maxhits=100&lang=EN&logic=AND&sortby=0&frompage=/definestats.jsp&bSum=Y&_stat=fichas.fechano,,&nlevels=1&_variables=1,fichas.muertos,fichas.heridos,fichas.desaparece,fichas.vivdest,fichas.vivafec,fichas.damnificados,fichas.afectados,fichas.reubicados,fichas.evacuados,fichas.valorus,fichas.valorloc,fichas.nescuelas,fichas.nhospitales,fichas.nhectareas,fichas.cabezas,fichas.kmvias&_eventos="


