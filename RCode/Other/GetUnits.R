units<-xml2::as_list(xml2::read_xml("https://docs.unidata.ucar.edu/thredds/udunits2/current/udunits2_combined.xml"))$`udunits-2`$`unit-system`

temp<-"./RawData/tmp/tmp.tar.gz"
# Set the maximum timeout limit
options(timeout = max(500, getOption("timeout")))
# Download the raster file to the location 'temp'
download.file("https://cran.r-project.org/src/contrib/NISTunits_1.0.1.tar.gz",temp)
# Unpack the files in the zip document
untar(paste0(temp),exdir = "./CleanedData/Other/")
# Load into the environment
load("./CleanedData/Other/NISTunits/data/tableNISTfactors.rda")
# Add the final column name (unnecesary, but keep it for another time)
colnames(tableNISTfactors)[4]<-"ScientificNotation"
# Reduce it to only the required units
tableNISTfactors%<>%dplyr::select(convert.to)%>%distinct()
# Add the unit-codes
tableNISTfactors$code<-str_split(str_split(tableNISTfactors$convert.to," \\(",simplify = T)[,2],"\\)",simplify = T)[,1]
# When the notation is given first in square brackets before curved ones
ind<-grepl(tableNISTfactors$convert.to,pattern = "\\[")
tableNISTfactors$code[ind]<-str_split(str_split(tableNISTfactors$convert.to[ind]," \\[",simplify = T)[,2],"\\]",simplify = T)[,1]
# Strip the blank spaces
tableNISTfactors$code%<>%str_remove_all(" ")
# tableNISTfactors%<>%rbind(data.frame(convert.from=unique(tableNISTfactors$convert.to),
#                                      convert.to=unique(tableNISTfactors$convert.to),
#                                      multiply.by=1,ScientificNotation="E+01"))
# Save out

units<-read.csv("./RawData/Other/MeasurementUnit_Codes2.csv")
units%<>%reshape2::melt(measure.vars=colnames(units))%>%filter(value!="")%>%
  mutate(list_name="impimp_units")%>%dplyr::select(list_name,value,variable)
colnames(units)[2:3]<-c("codename","group")

units$group%<>%str_to_lower()%>%str_remove_all(" ")
units$group<-paste0("units",units$group)
units$name<-units$codename%>%str_remove_all(" ")
units$codename<-paste0("units",units$codename%>%str_remove_all(" "))
units%>%dplyr::select(list_name,codename,name,group)%>%
write_csv("CleanedData/Other/measurementunits.csv")

units<-read.csv("./RawData/Other/MeasurementUnit_Codes2.csv")
units%<>%reshape2::melt(measure.vars=colnames(units))%>%filter(value!="")%>%
  mutate(list_name="impimp_units")%>%dplyr::select(list_name,value,variable)
colnames(units)[2:3]<-c("codename","group")
units$codegroup<-units$group%>%str_to_lower()%>%str_remove_all(" ")
units$codegroup<-paste0("units",units$codegroup)
units%>%dplyr::select(group,codegroup)%>%distinct()%>%
write_csv("CleanedData/Other/measurementunitgroups.csv")













