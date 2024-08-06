source("./RCode/Setup/GetPackages.R")
# Extract the required data - dashboard data from the Monty dashboards
source("./RCode/Analysis/Monty_Dashboard_Data.R")

freqy<-read.csv("./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")%>%
  dplyr::select(Hazard_Type,Hazard_Code,Impact_Type,Database,ISO3,No_Impacts,Once_in_5_Year)
freqy$ISO3[freqy$ISO3=="ZAR"]<-"COD"
# Make the summary from the return period data
<<<<<<< HEAD
lDREF<-freqy%>%filter(Impact_Type=="People Deaths [count]")%>%
  group_by(Hazard_Type,Hazard_Code,Impact_Type,ISO3)%>%
  reframe(Once_in_5_Year=max(Once_in_5_Year),
          No_Impacts=No_Impacts[which.max(Once_in_5_Year)],
          Database=Database[which.max(Once_in_5_Year)])
=======
lDREF<-freqy%>%filter(Impact_Type%in%c("People Deaths [count]",
                                       "People Internally Displaced Persons (IDPs) [count]",
                                       "People Homeless [count]",
                                       "People Displaced Persons (Internal & External) [count]",
                                       "People Directly Affected [count]",
                                       "People Total Affected [count]",
                                       "People In Need [count]",
                                       "People Indirectly Affected [count]",
                                       "People Targeted [count]",
                                       "Total Cost (Unspec. Inf-Adj) Loss [US Dollar]",
                                       "Total Direct Costs Inf-Adj Loss [US Dollar]",
                                       "Total Direct Costs Non-Inf-Adj Loss [US Dollar]") &
                        !Hazard_Code%in%c("TO","TS","SS","VO","VW","EQ","WF","ET"))%>%
  mutate(Impact_Type=case_when(grepl("Loss \\[US Dollar\\]",Impact_Type) ~ "Total Cost [USD]",
                               grepl("Affected \\[count\\]",Impact_Type) ~ "People Total Affected [count]",
                               Impact_Type=="People In Need [count]" ~ "People Total Affected [count]",
                               Impact_Type=="People Targeted [count]" ~ "People Total Affected [count]",
                               grepl("Displaced Persons", Impact_Type)  ~ "People Total Displaced [count]",
                               Impact_Type=="People Homeless [count]" ~ "People Total Displaced [count]",
                               TRUE ~ Impact_Type))%>%
  group_by(Hazard_Type,Impact_Type,ISO3)%>%
  reframe(Once_in_5_Year=max(Once_in_5_Year,na.rm = T))
>>>>>>> e1169bf6446475ae02de9aab0e32db8a3765e0f2
# Where NAs were present in the Once_in_5_Year column
lDREF$Once_in_5_Year[is.infinite(lDREF$Once_in_5_Year)]<-0
# Create a function to remove the NAs and take the most recent value
rec_notNA <- function(x, y) {
  return(y[!is.na(y)][which.max(x[!is.na(y)])])
}
# Pair with population, GDP and GDP per capita data
wbdata<-wbstats::wb_data(indicator = c("SP.POP.TOTL","NY.GDP.MKTP.CD","NY.GDP.PCAP.PP.CD"),mrnev=1)%>%
  rename("Population"="SP.POP.TOTL",
         "GDP_PPP"="NY.GDP.MKTP.CD",
         "GDP_PPP_pc"="NY.GDP.PCAP.PP.CD",
         "ISO3"="iso3c",
         "Country_Territory"="country",
         "year"="date")%>%
  dplyr::select(ISO3,Country_Territory,Population,GDP_PPP,GDP_PPP_pc,year)%>%
  filter(!is.na(Population) | !is.na(GDP_PPP))%>%
  group_by(ISO3, Country_Territory)%>%
  reframe(Population = rec_notNA(year,Population),
    GDP_PPP = rec_notNA(year, GDP_PPP),
    GDP_PPP_pc = rec_notNA(year, GDP_PPP_pc))%>%as.data.frame()
# Merge them
lDREF%<>%left_join(wbdata,by="ISO3")
# Check for countries that have no population data
unique(lDREF$ISO3[is.na(lDREF$Population)])[is.na(convIso3Country(unique(lDREF$ISO3[is.na(lDREF$Population)])))]
# Where NAs were present in the Once_in_5_Year column
lDREF$Once_in_5_Year[is.infinite(lDREF$Once_in_5_Year)]<-0
# Create the per-capita deaths variable
lDREF%<>%mutate(Once_in_5_Year_pCap=case_when(Impact_Type=="Total Cost [USD]" ~ Once_in_5_Year/GDP_PPP,
                                              TRUE ~ 1e6*Once_in_5_Year/Population))

<<<<<<< HEAD
iftop20<-T
# Also filter to the required hazards only
if(iftop20) lDREF%<>%filter(!Hazard_Code%in%c("TO","TS","SS","VO","VW","EQ","WF","ET"))

wb<-openxlsx::createWorkbook()
# Create all the worksheets
wb%>%openxlsx::addWorksheet("Deaths")
wb%>%openxlsx::addWorksheet("Deaths Per Capita")
# Reduce to the top-20 worst, wrt deaths per capita
outDREF<-lDREF%>%
  arrange(Hazard_Type,desc(Once_in_5_Year_pCap)) %>%
  group_by(Hazard_Type, Impact_Type) %>%
  arrange(Hazard_Type,desc(Once_in_5_Year_pCap)) %>%
  mutate(Ranking=1:n())%>%
  ungroup()%>%
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Database,No_Impacts,Once_in_5_Year_pCap,Ranking)%>%
  setNames(c("Hazard_Type","Impact Type","ISO3 Code","Country/Territory","Population","Database","No. Records","One-in-Five Year Impact, Per Capita [Per Million]","Ranking"))
# If we're only concentrating on the top 20
if(iftop20) outDREF%<>%filter(Ranking<=20)
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths Per Capita",
                    outDREF, 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Reduce to the top-20 worst, wrt deaths
outDREF<-lDREF%>%
=======
# Create the workbook to saveout
wb<-openxlsx::createWorkbook()

###################### Deaths, Affected & Displaced  ######################
# Reduce to the top-20 worst, wrt deaths
outDREF<-lDREF%>%
  filter(Impact_Type!="Total Cost [USD]")%>%
>>>>>>> e1169bf6446475ae02de9aab0e32db8a3765e0f2
  arrange(Hazard_Type,desc(Once_in_5_Year)) %>%
  group_by(Hazard_Type, Impact_Type) %>%
  arrange(Hazard_Type,desc(Once_in_5_Year)) %>%
  mutate(Ranking=1:n())%>%
  ungroup()%>%
<<<<<<< HEAD
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Database,No_Impacts,Once_in_5_Year,Ranking)%>%
  setNames(c("Hazard_Type","Impact Type","ISO3 Code","Country/Territory","Population","Database","No. Records","One-in-Five Year Impact","Ranking"))
# If we're only concentrating on the top 20
if(iftop20) outDREF%<>%filter(Ranking<=20)
=======
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Once_in_5_Year,Ranking)%>%
  setNames(c("Hazard Type","Impact Type","ISO3 Code","Country/Territory","Population","One-in-Five Year Impact","Ranking"))

# Deaths
wb%>%openxlsx::addWorksheet("Deaths")
>>>>>>> e1169bf6446475ae02de9aab0e32db8a3765e0f2
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths",
                    outDREF%>%filter(`Impact Type`=="People Deaths [count]"), 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Affected
wb%>%openxlsx::addWorksheet("Affected")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Affected",
                    outDREF%>%filter(`Impact Type`=="People Total Affected [count]"), 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Affected
wb%>%openxlsx::addWorksheet("Displaced")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Displaced",
                    outDREF%>%filter(`Impact Type`=="People Total Displaced [count]"), 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)

###################### Deaths, Affected & Displaced PER CAPITA  ######################
# Reduce to the top-20 worst, wrt deaths per capita
outDREF<-lDREF%>%
  filter(Impact_Type!="Total Cost [USD]")%>%
  arrange(Hazard_Type,desc(Once_in_5_Year_pCap)) %>%
  group_by(Hazard_Type, Impact_Type) %>%
  arrange(Hazard_Type,desc(Once_in_5_Year_pCap)) %>%
  mutate(Ranking=1:n())%>%
  ungroup()%>%
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Once_in_5_Year_pCap,Ranking)%>%
  setNames(c("Hazard Type","Impact Type","ISO3 Code","Country/Territory","Population","One-in-Five Year Impact, Per Capita [Per Million]","Ranking"))
# Deaths
wb%>%openxlsx::addWorksheet("Deaths Per Capita")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths Per Capita",
                    outDREF%>%filter(`Impact Type`=="People Deaths [count]"), 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Affected
wb%>%openxlsx::addWorksheet("Affected Per Capita")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Affected Per Capita",
                    outDREF%>%filter(`Impact Type`=="People Total Affected [count]"), 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Affected
wb%>%openxlsx::addWorksheet("Displaced Per Capita")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Displaced Per Capita",
                    outDREF%>%filter(`Impact Type`=="People Total Displaced [count]"), 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)




stop("Economic loss stuff here!")





# Save it out!
openxlsx::saveWorkbook(wb,"./CleanedData/MostlyImpactData/EAP-Country-Prioritisation.xlsx",overwrite = T)
# openxlsx::saveWorkbook(wb,"./CleanedData/MostlyImpactData/full_Country-Prioritisation_5yrRP-Deaths.xlsx",overwrite = T)


















#@@@@@@@@@@@@@@@@@@@@@ FULL LIST NOT TOP 20 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#


