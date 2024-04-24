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

pdc<-st_read(conn,"imminent_pdc")
