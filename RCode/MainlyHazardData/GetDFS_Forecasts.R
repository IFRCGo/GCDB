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









