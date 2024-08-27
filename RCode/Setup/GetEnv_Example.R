# Which version of the Montandon database are we using?
Monty_version<-"Montandon V1.00"
# Which version of the Montandon JSON schema are we using?
Monty_JSONschema<-"Monty Schema V1.00"
# Do you want only the reduced packages or all? Choose via packred
packred<-F
# Google Earth Engine User Details
gee_user<-''
# Also, where is the GEE cloud runfile stored?
gee_bin<-"./"
# GO token
go_token<-""
# DEEPL token
deepl_token<-""
# How does the Monty delimit some of the data variables, such as ISO3C codes?
delim<-"  :  "
# EM-DAT Token
emdat_token <-""
# IDMC token
idmc_token<-""
# Monty token
monty_token<-""
# Number of cores to use
ncores<-parallel::detectCores()
# ssh -i githubkeys ifrctgc0root@98.71.131.43
# Use this to see the keys for the PostgreSQL and Blobstorages
# ifrctgc0root@ifrctgc001vm:~/devops$ head *
