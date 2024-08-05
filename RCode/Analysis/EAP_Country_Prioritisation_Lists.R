# Extract the required data - dashboard data from the Monty dashboards
source("./RCode/Analysis/Monty_Dashboard_Data.R")

freqy<-read.csv("./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")%>%
  dplyr::select(Hazard,Hazard_Code,Impact_Type,Database,ISO3,No_Impacts,Once_in_5_Year)
freqy$ISO3[freqy$ISO3=="ZAR"]<-"COD"
# Make the summary from the return period data
lDREF<-freqy%>%filter(Impact_Type=="People Deaths [count]" &
                        !Hazard_Code%in%c("TO","TS","SS","VO","VW","EQ","WF","ET"))%>%
  group_by(Hazard,Impact_Type,ISO3)%>%
  reframe(Once_in_5_Year=max(Once_in_5_Year,na.rm = T))
# Where NAs were present in the Once_in_5_Year column
lDREF$Once_in_5_Year[is.infinite(lDREF$Once_in_5_Year)]<-0
# Pair with population, GDP and GDP per capita data
wbdata<-wbstats::wb_data(indicator = c("SP.POP.TOTL","NY.GDP.MKTP.CD","NY.GDP.PCAP.PP.CD"))%>%
  rename("Population"="SP.POP.TOTL",
         "GDP_PPP"="NY.GDP.MKTP.CD",
         "GDP_PPP_pc"="NY.GDP.PCAP.PP.CD",
         "ISO3"="iso3c",
         "Country_Territory"="country",
         "year"="date")%>%
  dplyr::select(ISO3,Country_Territory,Population,GDP_PPP,GDP_PPP_pc,year)%>%
  group_by(ISO3,Country_Territory)%>%
  reframe(Population=Population[which.max(year)],
          GDP_PPP=GDP_PPP[which.max(year)],
          GDP_PPP_pc=GDP_PPP_pc[which.max(year)])
# Merge them
lDREF%<>%left_join(wbdata,by="ISO3")
# Check for countries that have no population data
unique(lDREF$ISO3[is.na(lDREF$Population)])[is.na(convIso3Country(unique(lDREF$ISO3[is.na(lDREF$Population)])))]
# Where NAs were present in the Once_in_5_Year column
lDREF$Once_in_5_Year[is.infinite(lDREF$Once_in_5_Year)]<-0






# Pair with INFORM data & HDI





# Create the per-capita deaths variable
stop("case_when is economic or not to be divided by population or GDP-PPP per capita")
lDREF%<>%mutate(Once_in_5_Year_pCap=1e6*Once_in_5_Year/Population)

wb<-openxlsx::createWorkbook()
# Create all the worksheets
wb%>%openxlsx::addWorksheet("Deaths")
wb%>%openxlsx::addWorksheet("Deaths Per Capita")
# Reduce to the top-20 worst, wrt deaths per capita
outDREF<-lDREF%>%
  arrange(Hazard,desc(Once_in_5_Year_pCap)) %>%
  group_by(Hazard, Impact_Type) %>%
  slice_head(n = 20) %>%
  arrange(Hazard,desc(Once_in_5_Year_pCap)) %>%
  mutate(Ranking=1:20)%>%
  ungroup()%>%
  dplyr::select(Hazard,Impact_Type,ISO3,Country_Territory,Population,Once_in_5_Year_pCap,Ranking)%>%
  setNames(c("Hazard","Impact Type","ISO3 Code","Country/Territory","Population","One-in-Five Year Impact, Per Capita [Per Million]","Ranking"))
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths Per Capita",
                    outDREF, 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Reduce to the top-20 worst, wrt deaths
outDREF<-lDREF%>%
  arrange(Hazard,desc(Once_in_5_Year)) %>%
  group_by(Hazard, Impact_Type) %>%
  slice_head(n = 20) %>%
  arrange(Hazard,desc(Once_in_5_Year)) %>%
  mutate(Ranking=1:20)%>%
  ungroup()%>%
  dplyr::select(Hazard,Impact_Type,ISO3,Country_Territory,Population,Once_in_5_Year,Ranking)%>%
  setNames(c("Hazard","Impact Type","ISO3 Code","Country/Territory","Population","One-in-Five Year Impact","Ranking"))
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths",
                    outDREF, 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Save it out!
openxlsx::saveWorkbook(wb,"./CleanedData/MostlyImpactData/EAP-Country-Prioritisation.xlsx",overwrite = T)


