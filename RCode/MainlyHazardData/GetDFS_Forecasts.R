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


CleanADAM<-function(adam){
  adam<-rename("ev_name" = "title",
               "haz_Ab" = "hazard_type",
               "ext_ID" = "event_id",
               "publish_date" = "imp_moddate",
               "population_exposure")
  
  
  
  stop("extract the population_impact variable from the storm_position_geojson geojson variable")
  stop("country_id needs sorting")
  stop("make sure to modify the id to ensure the publish_date is the imp_moddate")
}






























