# This one seems the best so far:
# https://blog.revolutionanalytics.com/2018/12/azurestor.html
# pswd to toDB: gc122cd82ea407643F8_27.b6dbd317da8785 which is gc and then 
# the echo variable after having logged in to the VM and hit "head *" 
# from within the folder "devops"

install.packages("AzureRMR")
library(AzureRMR)
library(AzureStor)

endpoint<-"https://ifrctgc001st.blob.core.windows.net"
akey<-""

az <- create_azure_login(az$tenant)

store <- az$get_storage_account(endpoint) 

blob_endp <- blob_endpoint(
  endpoint,
  key=akey)

cont <- blob_container(endpoint,key=akey, token = az$token)
list_blobs(cont)


newcont <- create_blob_container(blob_endp, "Monty")#,
                                 # public_access="blob")






# The AzureR packages can save your authentication credentials in the directory:
#   ~/.local/share/AzureR

bl_endp_key <- storage_endpoint(endpoint, key=akey, service="blob")
list_storage_containers(bl_endp_key)
newcont <- create_storage_container(bl_endp_key, "Monty")

storage_multiupload(newcont, src="./CleanedData/MostlyHazardData/GDACS/GDACS_2023-11-28.json")


json <- readLines("./CleanedData/MostlyHazardData/GDACS/GDACS_2023-11-28.json")
con <- textConnection(json)
storage_upload(newcont, src=con, dest="Monty.json")
