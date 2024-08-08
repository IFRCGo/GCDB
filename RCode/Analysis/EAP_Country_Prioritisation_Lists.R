# Import (and install, if necessary) all packages
source("./RCode/Setup/GetPackages.R")

# Extract the required data - dashboard data from the Monty dashboards
if(!file.exists("./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")) 
  source("./RCode/Analysis/Monty_Dashboard_Data.R")

# For the DREF-team to specifically focus on certain hazards
iftop20<-T

# Import underlying data
freqy<-read.csv("./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")%>%
  dplyr::select(Hazard_Type,Hazard_Code,Impact_Type,Database,ISO3,No_Impacts,Once_in_5_Year)
freqy$ISO3[freqy$ISO3=="ZAR"]<-"COD"
# Make the summary from the return period data
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
                                       "Total Direct Costs Non-Inf-Adj Loss [US Dollar]"))%>%
  mutate(Impact_Type=case_when(grepl("Loss \\[US Dollar\\]",Impact_Type) ~ "Total Cost [USD]",
                               grepl("Affected \\[count\\]",Impact_Type) ~ "People Total Affected [count]",
                               Impact_Type=="People In Need [count]" ~ "People Total Affected [count]",
                               Impact_Type=="People Targeted [count]" ~ "People Total Affected [count]",
                               grepl("Displaced Persons", Impact_Type)  ~ "People Total Displaced [count]",
                               Impact_Type=="People Homeless [count]" ~ "People Total Displaced [count]",
                               TRUE ~ Impact_Type))%>%
  group_by(Hazard_Type,Hazard_Code,Impact_Type,Database,ISO3)%>%
  reframe(Once_in_5_Year=max(Once_in_5_Year,na.rm = T),
          No_Impacts=No_Impacts[which.max(Once_in_5_Year)])
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
lDREF$Once_in_5_Year_pCap<-1e6*lDREF$Once_in_5_Year/lDREF$Population
lDREF$Once_in_5_Year_pCap[lDREF$Impact_Type=="Total Cost [USD]"]<-
  100*lDREF$Once_in_5_Year[lDREF$Impact_Type=="Total Cost [USD]"]/
  lDREF$GDP_PPP[lDREF$Impact_Type=="Total Cost [USD]"]

# Filter certain hazards and extract ODA country data
if(iftop20) {
  # Filter to the required hazards only
  lDREF%<>%filter(!Hazard_Code%in%c("TO","TS","SS","VO","VW","EQ","WF","ET"))
  # To extract the list of ODA countries
  odas<-tryCatch(wbstats::wb_data(indicator = "DT.ODA.ALLD.CD",mrnev = 1)%>%
                   dplyr::select(iso3c)%>%setNames("ISO3")%>%distinct(),
                 error=function(e) NULL)
  # Check if that worked
  if(is.null(odas)) {
    # Try downloading direct from their website
    rety<-tryCatch(download.file("https://api.worldbank.org/v2/en/indicator/DT.ODA.ALLD.CD?downloadformat=excel&_gl=1*g8mrts*_gcl_au*MjA4MzQ4ODk5MS4xNzE1NTg5MjIy",
                                 "./CleanedData/SocioPoliticalData/ODA.xlsx"),error=function(e) NULL)
    # Check if that worked
    if(is.null(rety)) stop("Issues downloading the list of ODA countries from World Bank")
    # if it worked, load the data
    odas<-xlsx::read.xlsx("./CleanedData/SocioPoliticalData/ODA.xlsx",
                          sheetIndex = 1,startRow = 3)
    # Which years do we want to include?
    yrs<-as.integer(AsYear(Sys.Date())); yrs<-paste0("X",c(yrs-3,yrs-2,yrs-1))
    # Select certain columns
    odas<-odas[,c("Country.Code",yrs)] 
    # Check which countries are in the list of ODA recipients
    odas$isODA<-apply(odas[,2:4],1,function(x) !all(is.na(x)))
    # Filter the other countries out
    odas%<>%filter(isODA)%>%dplyr::select(Country.Code)%>%setNames("ISO3")%>%distinct()
  }
  # Now remove all countries that aren't in that list
  lDREF%<>%filter(ISO3%in%odas$ISO3)
}

# Create the workbook to saveout
wb<-openxlsx::createWorkbook()

###################### Deaths, Affected & Displaced  ######################
# Reduce to the top-20 worst, wrt deaths
outDREF<-lDREF%>%
  filter(Impact_Type!="Total Cost [USD]")%>%
  arrange(Hazard_Type,desc(Once_in_5_Year)) %>%
  group_by(Hazard_Type, Impact_Type) %>%
  arrange(Hazard_Type,desc(Once_in_5_Year)) %>%
  mutate(Ranking=1:n())%>%
  ungroup()%>%
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Database,No_Impacts,Once_in_5_Year,Ranking)%>%
  setNames(c("Hazard Type","Impact Type","ISO3 Code","Country/Territory","Population","Database","No. Records","One-in-Five Year Impact","Ranking"))
# If we're only concentrating on the top 20
if(iftop20) outDREF%<>%filter(Ranking<=20)

# Deaths
wb%>%openxlsx::addWorksheet("Deaths")
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

#@@@@@@@@@@@@@@@ Economic Losses @@@@@@@@@@@@@@@#
# Reduce to the top-20 worst, wrt economic losses
outDREF<-lDREF%>%
  filter(Impact_Type=="Total Cost [USD]")%>%
  arrange(Hazard_Type,desc(Once_in_5_Year)) %>%
  group_by(Hazard_Type, Impact_Type) %>%
  arrange(Hazard_Type,desc(Once_in_5_Year)) %>%
  mutate(Ranking=1:n())%>%
  ungroup()%>%
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Database,No_Impacts,Once_in_5_Year,Ranking)%>%
  setNames(c("Hazard Type","Impact Type","ISO3 Code","Country/Territory","Population","Database","No. Records","One-in-Five Year Impact","Ranking"))
# If we're only concentrating on the top 20
if(iftop20) outDREF%<>%filter(Ranking<=20)

# Economic losses
wb%>%openxlsx::addWorksheet("Economic Losses")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Economic Losses",
                    outDREF, 
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
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Database,No_Impacts,Once_in_5_Year_pCap,Ranking)%>%
  setNames(c("Hazard Type","Impact Type","ISO3 Code","Country/Territory","Population","Database","No. Records","One-in-Five Year Impact, Per Million","Ranking"))
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

#@@@@@@@@@@@@@@@ Economic Losses PER % GDP-PPP @@@@@@@@@@@@@@@#
# Reduce to the top-20 worst, wrt economic losses per GDP
outDREF<-lDREF%>%
  filter(Impact_Type=="Total Cost [USD]")%>%
  arrange(Hazard_Type,desc(Once_in_5_Year_pCap)) %>%
  group_by(Hazard_Type, Impact_Type) %>%
  arrange(Hazard_Type,desc(Once_in_5_Year_pCap)) %>%
  mutate(Ranking=1:n())%>%
  ungroup()%>%
  dplyr::select(Hazard_Type,Impact_Type,ISO3,Country_Territory,Population,Database,No_Impacts,Once_in_5_Year_pCap,Ranking)%>%
  setNames(c("Hazard Type","Impact Type","ISO3 Code","Country/Territory","Population","Database","No. Records","One-in-Five Year Impact, per % GDP-PPP","Ranking"))
# If we're only concentrating on the top 20
if(iftop20) outDREF%<>%filter(Ranking<=20)

# Economic losses
wb%>%openxlsx::addWorksheet("Economic Losses Per GDP-PPP")
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Economic Losses Per GDP-PPP",
                    outDREF, 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)

# Save it out!
if(iftop20) {openxlsx::saveWorkbook(wb,"./CleanedData/MostlyImpactData/EAP-Country-Prioritisation.xlsx",overwrite = T)
} else openxlsx::saveWorkbook(wb,"./CleanedData/MostlyImpactData/full_Country-Prioritisation_5yrRP-Deaths.xlsx",overwrite = T)

