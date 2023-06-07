#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@ GET THE DATA @@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Download the dataset from https://datahub.io/core/currency-codes
url<-"https://datahub.io/core/currency-codes/r/codes-all.csv"
# Establish the file
temp<-"./RawData/Other/ISO4217_CurrencyCodes.csv"
# Download the raster file to the location 'temp'
download.file(url,temp)
# Now read it in and modify it 
curr<-read.csv(temp)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@ CLEAN THE DATA @@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Ensure that the withdrawal dates are in the correct date format %Y-%m-%d
Withd<-Stard<-rep(NA_Date_,nrow(curr))
# Sort out the values that are in the format '%Y-%m' (without the day)
ind<-nchar(curr$WithdrawalDate)==7 & !is.na(curr$WithdrawalDate)
# Make sure that no values that error when creating a date are used
Withd[ind]<-as.Date(paste0(curr$WithdrawalDate[ind],"-01"))
# Sort out the values that are in the format '%Y to %Y' (without the month or day)
ind<-nchar(curr$WithdrawalDate)==12 & !is.na(curr$WithdrawalDate)
tmp<-t(apply(str_split(curr$WithdrawalDate[ind]," to ",simplify = T),1,function(x) paste0(x,"-01-01")))
Withd[ind]<-tmp[,2]; Stard[ind]<-tmp[,1]
# Sort out the values that are in the format '%Y-%Y' (without the month or day) 
ind<-nchar(curr$WithdrawalDate)==9 & !is.na(curr$WithdrawalDate)
tmp<-t(apply(str_split(curr$WithdrawalDate[ind],"-",simplify = T),1,function(x) paste0(x,"-01-01")))
Withd[ind]<-tmp[,2]; Stard[ind]<-tmp[,1]
# Sort out the values that are in the format '%Y to %Y' (without the day)
ind<-nchar(curr$WithdrawalDate)==18 & !is.na(curr$WithdrawalDate)
tmp<-t(apply(str_split(curr$WithdrawalDate[ind]," to ",simplify = T),1,function(x) paste0(x,"-01")))
Withd[ind]<-tmp[,2]; Stard[ind]<-tmp[,1]
# Replace values, translate from country to ISO3C and shuffle the order around
curr%<>%transmute(ISO3=convCountryIso3(Entity),Country=Entity,
                  Currency=Currency,ISO4217C=AlphabeticCode,ISO4217N=NumericCode,
                  WithdrawalDate=as.character(Withd),StartDate=as.character(Stard),
                  MinorUnit=as.numeric(MinorUnit))
curr$ISO3[curr$Country=="GERMAN DEMOCRATIC REPUBLIC"]<-"GDR"
curr$ISO3[curr$Country=="CZECHOSLOVAKIA"]<-"CSK"
curr$ISO3[curr$Country=="NETHERLANDS ANTILLES"]<-"ANT"
curr$ISO3[curr$Country=="SAINT MARTIN"]<-"MAF"
curr$ISO3[curr$Country=="YEMEN, DEMOCRATIC"]<-"YMD"
curr$ISO3[curr$Country=="YUGOSLAVIA"]<-"YUG"
curr$ISO3[curr$Country=="EUROPEAN UNION"]<-"EUR"
curr$ISO3[curr$Country=="EUROPEAN MONETARY CO-OPERATION FUND (EMCF)"]<-"EUR"
curr$ISO3[curr$Country=="MEMBER COUNTRIES OF THE AFRICAN DEVELOPMENT BANK GROUP"]<-"XUA"
curr$ISO3[curr$Country=="INTERNATIONAL MONETARY FUND (IMF)"]<-"XDR"
# We take the local currencies for Serbia and Montenegro
curr%<>%rbind(data.frame(ISO3=c("SRB","MNE"),Country=c("SERBIA","MONTENEGRO"),
              Currency=c("Serbian Dinar","Euro"),
              ISO4217C=c("CSD","EUR"),ISO4217N=c(891,978),
              WithdrawalDate=c(NA_character_,NA_character_),
              StartDate=c(NA_character_,"2002-01-01"),MinorUnit=c(NA,NA)))
curr%<>%filter(Country!="SERBIA AND MONTENEGRO")
# Alerts
print("Countries or entities with no ISO3 code:")
print(curr$Country[is.na(curr$ISO3)])
# Remove all rows with NAs as currency characher and number codes
curr%<>%filter(!is.na(ISO4217C) & !is.na(ISO4217N))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@ IIPS FORMAT @@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
curr%<>%mutate(list_name="currencytype",ISO4217C=paste0("ISO4217C-",ISO4217C))%>%
  dplyr::select(list_name,ISO4217C,Currency,ISO3,Country,
                StartDate,WithdrawalDate,MinorUnit)
# Save it out
write_csv(curr,"./CleanedData/Other/ISO4217_CurrencyCodes.csv")
