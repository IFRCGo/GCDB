library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, host = "localhost", port=5432,
                               dbname = "risk", user = "postgres",password=" ")

tables<-dbGetQuery(conn, "SELECT
  tablename
FROM
  pg_tables
WHERE schemaname NOT IN ('pg_catalog', 'information_schema');")$tablename

tables<-do.call(rbind,lapply(tables,function(x){
  data.frame(table=x,
             size=unname(unlist(dbGetQuery(conn, paste0("SELECT pg_size_pretty( pg_total_relation_size('",x,"') );")))))
}))

tables$total_size<-extractnumbers(tables$size)
tables$total_size[grepl(" kB",tables$size)]<-tables$total_size[grepl(" kB",tables$size)]*1000
tables$total_size[grepl(" MB",tables$size)]<-tables$total_size[grepl(" MB",tables$size)]*1000000
tables$size<-NULL

dbGetColnames<-function(conn,table_name){
  query <- sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", table_name)
  # Execute the query and retrieve column names
  dbGetQuery(conn, query)$column_name
}

tables$colnames<-unname(sapply(tables$table,function(x){
  paste0(dbGetColnames(conn,x),collapse = " , ")
}))

View(tables)

pdc<-sf::st_read(conn,"imminent_pdc")
pdcdis<-sf::st_read(conn,"imminent_pdcdisplacement")
adam<-sf::st_read(conn,"imminent_adam")
gwis<-sf::st_read(conn,"imminent_gwis")


length(unique(pdc$hazard_id))
length(unique(pdc$id))
table(pdc$hazard_type)

tables$rowcount<-unname(sapply(tables$table,function(x){
  dbGetQuery(conn,paste0("SELECT COUNT(*) FROM ",x,";"))$count
},simplify = T))
write_csv(tables%>%dplyr::select(-colnames),"./CleanedData/Monty_Forecast_Data_Info.csv")

# Only the active earthquake events are displayed... no long term storage access
adam_eq<-jsonlite::fromJSON("https://x8qclqysv7.execute-api.eu-west-1.amazonaws.com/dev/events/earthquakes")
adam_fl<-jsonlite::fromJSON("https://x8qclqysv7.execute-api.eu-west-1.amazonaws.com/dev/events/floods")
adam_tc<-jsonlite::fromJSON("https://x8qclqysv7.execute-api.eu-west-1.amazonaws.com/dev/events/cyclones")

# Api request url
url = "https://sentry.pdc.org/hp_srv/services/hazards/t/json/get_active_hazards"
headers <- httr::add_headers(Authorization = paste0("Bearer ", PDC_ACCESS_TOKEN))
response <- httr::GET(url, headers)
out<-jsonlite::fromJSON(httr::content(response, "text"))
stop("sort issues with dates from PDC data")
# # Create the JSON body as a list
# bod <- jsonlite::toJSON(list(
#   username = "hamish.patten@ifrc.org",
#   password = psswd
# ), auto_unbox = T)
# # Make the POST request
# response <- httr::GET(
#   "https://goadmin.ifrc.org/get_auth_token",
#   httr::add_headers(`Content-Type` = "application/json"),
#   # body = bod,
#   httr::timeout(10000)
# )

CleanADAM<-function(adam){
  # Minor changes that make things work later on
  adam%<>%rename("ev_name" = "title",
               "haz_Ab" = "hazard_type",
               "ext_ID" = "event_id",
               "imp_moddate" = "publish_date")
  # Get rid of the few entries that have barely any information
  adam%<>%filter(!is.na(event_details))
  # Difficult... the event details information
  adam%<>%cbind(do.call(dplyr::bind_rows,parallel::mclapply(adam$event_details, function(x){
    if(all(is.na(x))) return(data.frame(ev=NA))
    x<-jsonlite::fromJSON(x)
    x$url=paste0(unname(unlist(x$url)), collapse=" ; ")
    if(!is.null(x$event_id)) x$event_id<-as.character(x$event_id)
    return(as.data.frame(Filter(Negate(is.null),x)))
  },mc.cores=ncores)))%>%dplyr::select(-c(event_details,geojson))
  # Add the geospatial tropical cyclone track data
  # adam$storm_position_geojson[!is.na(adam$storm_position_geojson)]<-
  #   unname(unlist(parallel::mclapply(adam$storm_position_geojson[!is.na(adam$storm_position_geojson)],
  #                        function(x) {
  #                          # Convert from json
  #                          x<-jsonlite::fromJSON(x)
  #                          # Find the polygon that covers the largest area: green alerts 
  #                          ind<-which(x$features$properties$alert_level=="Green" &
  #                            !is.na(x$features$properties$alert_level_label) &
  #                            x$features$geometry$type=="MultiPolygon")
  #                          # Reduce the features to only this polygon
  #                          x$features<-x$features[ind[length(ind)],]
  #                          # Get rid of the useless variables
  #                          x$features$properties<-x$features$properties[,c("alert_level","alert_level_label")]
  #                          # return as json
  #                          return(as.character(jsonlite::toJSON(x)))
  #                        },mc.cores=ncores)))
  # Get rid of it all for now
  adam%<>%dplyr::select(-storm_position_geojson)
  # Sort out the population exposure, due to the different adam data when importing storms or tropical cyclones
  adam$population_impact[is.na(adam$population_impact)]<-adam$population_exposure[is.na(adam$population_impact)]
  adam$population_impact[is.na(adam$population_impact)]<-adam$population[is.na(adam$population_impact)]
  # Equally, sort out the hazard magnitudes
  # Add wind speed for TC first
  adam$haz_maxvalue<-adam$wind_speed
  adam$haz_maxunits<-"unitskm_per_hr"
  # Now EQs
  adam$haz_maxvalue[is.na(adam$wind_speed)]<-adam$mag[is.na(adam$wind_speed)]
  adam$haz_maxunits[is.na(adam$wind_speed)]<-"unitsrichter"
  # Now FLs
  ind<-is.na(adam$haz_maxvalue)
  adam$haz_maxvalue[ind]<-adam$flood_area[ind]
  adam$haz_maxunits[ind]<-"unitshectare"; rm(ind)
  # Cleaning again from the messy adam data
  adam$countries[is.na(adam$countries)]<-adam$country[is.na(adam$countries)]
  # Extract ISO3Cs when there are multiple countries involved
  adam$haz_ISO3s<-adam$imp_ISO3s<-adam$ev_ISO3s<-unname(unlist(parallel::mclapply(1:nrow(adam), function(i){
    # Get the country names
    x<-adam$countries[i]
    # Check if there is nothing there to convert
    if(is.null(x) | is.na(x) | stringi::stri_trim_both(x)=="") return(NA_character_)
    # Extract countries then convert to ISO3C, then paste together
    x<-convCountryIso3(stringi::stri_trim_both(c(str_split(x,",",simplify = T))))
    # Get rid of NAs
    x<-x[!is.na(x)]
    # Check to see if the ISO3 originally associated to the event has been included
    x2<-adam$iso3[i]
    # If not, add it!
    if(!x2%in%x & !is.na(x2) & !is.null(x2) & stringi::stri_trim_both(x2)!="") x<-c(x,x2)
    # Return a single character rather than a vector
    paste0(sort(x),collapse=" ; ")
  }, mc.cores = ncores)))
  # rearrange by date and slice the latest date, leaving the others out for the time being
  adam%<>%dplyr::select(-c(country_id, population_exposure, population, name, 
                          title, alert_sent, episode_id, analysis_output,
                          wind_speed, mag, depth, mmi, flood_area, mag_type,
                          sitrep, adm1_name, source, eventid, fl_croplnd, iso3,
                          date_processed, effective_date, country, countries,
                          max_storm_surge, dashboard_url))%>%
    rename(
    "haz_sub_ID"="id",
    "haz_spat_fileloc"="url",
    "haz_sdate"="from_date",
    "haz_fdate"="to_date",
    "haz_lat"="latitude",
    "haz_lon"="longitude",
    "gen_location"="place"
  )
  # Check through dates
  adam$imp_moddate<-as.character(as.Date(adam$imp_moddate))
  adam$haz_sdate[is.na(adam$haz_sdate)]<-adam$imp_moddate[is.na(adam$haz_sdate)]
  adam$haz_fdate[is.na(adam$haz_fdate)]<-adam$imp_moddate[is.na(adam$haz_fdate)]
  # Add HIPs taxonomy & tidy
  adam%<>%mutate(
    ev_sdate=haz_sdate,
    ev_fdate=haz_fdate,
    imp_sdate=haz_sdate,
    imp_fdate=haz_fdate,
    haz_moddate=imp_moddate,
    haz_spec=case_when(
      haz_Ab=="EQ"~"GH0001:GH0002",
      haz_Ab=="FL"~"MH0004:MH0005:MH0006:MH0007:MH0008:MH0012",
      storm_status=="Tropical depression"~"MH0057:MH0030:MH0006",
      storm_status=="Tropical storm"~"MH0057:MH0058:MH0006",
      TRUE ~ "MH0057:MH0006"
    ),
    all_hazs_Ab=haz_Ab,
    all_ext_IDs=ext_ID,
    all_hazs_spec=haz_spec,
    imp_src_org="WFP",
    imp_src_db="ADAM",
    haz_src_org="WFP",
    haz_src_db="ADAM",
    imp_src_orgtype="orgtypeun",
    imp_spat_srcorg="IFRC",
    imp_spat_srcdb="GO",
    imp_spat_URL="https://go-user-library.ifrc.org/maps",
    imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
    imp_spat_res=0,
    imp_spat_resunits="adminlevel",
    imp_spat_crs="EPSG:4326",
    imp_spat_covcode="spat_polygon",
    imp_spat_ID=NA_character_,
    haz_spat_covcode="spat_polygon",
    haz_spat_res=NA_real_,
    haz_spat_resunits="spatresother",
    haz_spat_crs="EPSG:4326",
    haz_spat_srcorg="WFP",
    haz_spat_srcdb="ADAM",
    haz_spat_URL=haz_spat_fileloc,
    haz_est_type="esttype_second",
    haz_spat_ID=NA_character_,
    imp_lat=haz_lat,
    imp_lon=haz_lon,
    imp_src_URL=haz_spat_fileloc,
    imp_unitdate=NA_character_,
    imp_credate=NA_character_,
    haz_credate=NA_character_,
    all_hazs_Ab=haz_Ab,
    all_ext_IDs=ext_ID
  )
  # Generate GCDB event ID
  adam$event_ID<-GetMonty_ID(adam)
  # Correct the labels of the impacts, melting by impact detail
  adam%<>%ImpLabs(nomDB = "ADAM")
  # Create an impact-specific ID
  adam%<>%GetGCDB_impID()
  
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  adam%>%dplyr::select(any_of(MontyJSONnames())) 
}


dfs<-readRDS("~/Downloads/DFS_Forecast_noPDCdis.Rdata")
pdc<-dfs$pdc
adam<-dfs$adam
rm(dfs)


PDChazards<-function(pdc){
  colConv<-openxlsx::read.xlsx("./Taxonomies/MostlyImpactData/PDC-HIP.xlsx")
  # Reduce the translated vector and merge
  pdc%<>%left_join(colConv,by = c("haz_Ab"),
                     relationship="many-to-one")
}

CleanPDC<-function(pdc){
  # Rename and add variables
  pdc%<>%rename("imp_credate"="created_at",
                "ext_ID"="uuid",
                "imp_sub_ID"="id",
                "ev_name"="hazard_name",
                "haz_Ab"="hazard_type",
                "haz_lat"="latitude",
                "haz_lon"="longitude",
                "ev_sdate"="start_date",
                "ev_fdate"="end_date",
                "imp_moddate"="pdc_updated_at")%>%
    mutate(haz_credate=imp_credate,
           haz_moddate=imp_moddate,
           imp_sdate=ev_sdate,
           imp_fdate=imp_moddate,
           haz_sdate=ev_sdate,
           haz_fdate=ev_fdate,
           imp_lat=haz_lat,
           imp_lon=haz_lon,
           all_ext_IDs=ext_ID,
           imp_src_URL=paste0("https://sentry.pdc.org/hp_srv/services/hazard/",ext_ID,"/exposure/latest/"),
           haz_spat_URL=paste0("https://sentry.pdc.org/hp_srv/services/hazard/",ext_ID,"/exposure/latest/"),
           all_hazs_Ab=haz_Ab,
           imp_src_org="PDC",
           imp_src_db="DisasterAWARE",
           haz_src_org="PDC",
           haz_src_db="DisasterAWARE",
           imp_src_orgtype="orgtypeacad",
           imp_spat_srcorg="IFRC",
           imp_spat_srcdb="GO",
           imp_spat_URL="https://go-user-library.ifrc.org/maps",
           imp_spat_fileloc="https://go-user-library.ifrc.org/maps",
           imp_spat_res=0,
           imp_spat_resunits="adminlevel",
           imp_spat_crs="EPSG:4326",
           imp_spat_covcode="spat_polygon",
           imp_spat_ID=NA_character_,
           haz_spat_covcode="spat_polygon",
           haz_spat_res=NA_real_,
           haz_spat_resunits="spatresother",
           haz_spat_crs="EPSG:4326",
           haz_spat_srcorg="PDC",
           haz_spat_srcdb="DisasterAWARE",
           haz_est_type="esttype_second",
           haz_spat_ID=NA_character_,
           imp_unitdate=NA_character_)
  # Now sort out the specific hazards
  pdc%<>%PDChazards()%>%mutate(all_hazs_spec=haz_spec)
  # Generate GCDB event ID
  pdc$event_ID<-GetMonty_ID(pdc)
  
  
  
  
  # left_join with immminent_pdcdisplacement for impact forecast
  
  
  
  
  
  # Correct the labels of the impacts, melting by impact detail
  pdc%<>%ImpLabs(nomDB = "PDC")
  # Create an impact-specific ID
  pdc%<>%GetGCDB_impID()
  
  # Add missing columns & reorder the dataframe to fit imp_GCDB object
  pdc%>%dplyr::select(any_of(MontyJSONnames())) 
}




















