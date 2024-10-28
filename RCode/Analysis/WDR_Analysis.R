source("./RCode/Setup/GetPackages.R")

wdrfile<-"./Analysis_Results/Kirsten/WDR_country_data-2023_HP_full.xlsx"
# "./Analysis_Results/Kirsten/WDR_country_data-2023_HP_full.xlsx"

WDR<-openxlsx::read.xlsx(wdrfile,sheet = 9,startRow = 5)%>%
  dplyr::select(1:5)
skeleton<-WDR%>%filter(Year==2010)%>%dplyr::select(1:4)
WDR%<>%rbind(skeleton%>%mutate(Year=AsYear(Sys.Date())))

ISOS<-unique(WDR$ISO3)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF","CW","HW")

emmie<-convEMDAT_Monty(taby=T)%>%mutate(ext_ID=as.character(ext_ID))
iddie<-convIDU_Monty(taby=T)%>%mutate(ext_ID=as.character(ext_ID))
iddie$imp_spat_ID<-lapply(iddie$imp_spat_ID,function(x) x)

impies<-dplyr::bind_rows(emmie,iddie)
# impies<-rbind(CleanEMDAT(openxlsx::read.xlsx("./Analysis_Results/Kirsten/public_emdat_custom_request_2023-11-03_7c5bf33d-052f-4795-a290-bb4554a476b6.xlsx",startRow = 1)),
#               GetGIDD())
# impies<-readRDS("./Analysis_Results/Kirsten/impies_20230910.RData")

sort(unique(impies$haz_Ab))

impies$haz_Ab[impies$haz_Ab%in%c("CW","HW")]<-"ET"
impies$haz_Ab[impies$haz_Ab%in%c("AV","MM")]<-"LS"

impies$haz_Ab[impies$haz_Ab=="LS" & impies$haz_type=="haztypehydromet"]<-"LS-HM"
impies$haz_Ab[impies$haz_Ab=="LS" & impies$haz_type=="haztypegeohaz"]<-"LS-G"

table(impies$haz_Ab)

# Create a variable to separate what is and isn't RC data 
# impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",imp_src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)
# impies%<>%filter(!is.na(Year))

impies$ISO3<-impies$ev_ISO3s
isoEQ<-unique(impies$ISO3[impies$haz_Ab=="EQ"])

taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")

# impies%<>%filter(Year>=2010)

sum(unique(impies$ISO3)%in%WDR$ISO3)

convIso3Country(unique(WDR$ISO3[!WDR$ISO3%in%impies$ISO3]))

climvars<-c("Storm","Flood","Drought","Wildfire","ExtrTemp","LandslideH")
geovars<-c("Earthquake","Volcano","LandslideG")

# Extract EM-DAT deaths and IDMC displacements
# Make an average of the number of events per country to produce the incidence, per year
#   but this only uses IDMC from 2018 onwards
# Also produce the EM-DAT inf-corrected costs and affected people, but in a different xlsx file
# 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% INCIDENCE %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
EMFull<-impies%>%filter(Year>=2010 & imp_src_db=="EMDAT")%>%filter(!duplicated(event_ID))%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haz_type=="haztypehydromet"),
            Storm=sum(haz_Ab%in%c("ST","TC")),
            Flood=sum(haz_Ab=="FL"),
            LandslideH=sum(haz_Ab=="LS-HM" & haz_type=="haztypehydromet"),
            Drought=sum(haz_Ab=="DR"),
            Wildfire=sum(haz_Ab=="WF"),
            ExtrTemp=sum(haz_Ab%in%c("ET","CW","HW")),
            ALL_GEO=sum(haz_type=="haztypegeohaz"),
            Earthquake=sum(haz_Ab=="EQ"),
            Volcano=sum(haz_Ab=="VO"),
            LandslideG=sum(haz_Ab=="LS-G" & haz_type=="haztypegeohaz"),
            ALL=sum(haz_type%in%c("haztypehydromet","haztypegeohaz")),
            .groups="drop")
EMFull<-WDR%>%dplyr::select(1:5)%>%
  left_join(EMFull,by=join_by(ISO3,Year),relationship="one-to-one")

EMFull[is.na(EMFull)]<-0

EMFull$ALL_CLIM<-rowSums(EMFull[,climvars])
EMFull$ALL_GEO<-rowSums(EMFull[,geovars])
EMFull$ALL<-EMFull$ALL_CLIM+EMFull$ALL_GEO

HEFull<-impies%>%filter(Year>=2018 & imp_src_db=="IDU")%>%filter(!duplicated(event_ID))%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haz_type=="haztypehydromet"),
            Storm=sum(haz_Ab%in%c("ST","TC")),
            Flood=sum(haz_Ab=="FL"),
            LandslideH=sum(haz_Ab=="LS-HM" & haz_type=="haztypehydromet"),
            Drought=sum(haz_Ab=="DR"),
            Wildfire=sum(haz_Ab=="WF"),
            ExtrTemp=sum(haz_Ab%in%c("ET","CW","HW")),
            ALL_GEO=sum(haz_type=="haztypegeohaz"),
            Earthquake=sum(haz_Ab=="EQ"),
            Volcano=sum(haz_Ab=="VO"),
            LandslideG=sum(haz_Ab=="LS-G" & haz_type=="haztypegeohaz"),
            ALL=sum(haz_type%in%c("haztypehydromet","haztypegeohaz")),
            .groups="drop")
HEFull<-WDR%>%dplyr::select(1:5)%>%
  left_join(HEFull,by=join_by(ISO3,Year),relationship="one-to-one")

HEFull[is.na(HEFull)]<-0

HEFull$ALL_CLIM<-rowSums(HEFull[,climvars])
HEFull$ALL_GEO<-rowSums(HEFull[,geovars])
HEFull$ALL<-HEFull$ALL_CLIM+HEFull$ALL_GEO

IncFull<-EMFull
# Only do the averaging for when IDMC started recording lots of events
inds<-IncFull$Year>=2018 & IncFull$Year<=AsYear(Sys.Date())
  
IncFull[inds,6:ncol(IncFull)]<-do.call(cbind,lapply(6:ncol(IncFull),function(i) {
  ceiling(apply(cbind(EMFull[inds,i],HEFull[inds,i]),1,max,na.rm = T))
}))

RegIncFull<-IncFull%>%group_by(REGION_IFRC,Year)%>%
  summarise(ALL_CLIM=sum(ALL_CLIM),
            Storm=sum(Storm),
            Flood=sum(Flood),
            LandslideH=sum(LandslideH),
            Drought=sum(Drought),
            Wildfire=sum(Wildfire),
            ExtrTemp=sum(ExtrTemp),
            ALL_GEO=sum(ALL_GEO),
            Earthquake=sum(Earthquake),
            Volcano=sum(Volcano),
            LandslideG=sum(LandslideG),
            ALL=sum(ALL),
            .groups="drop")

global<-RegIncFull%>%group_by(Year)%>%
  summarise(ALL_CLIM=sum(ALL_CLIM),
            Storm=sum(Storm),
            Flood=sum(Flood),
            LandslideH=sum(LandslideH),
            Drought=sum(Drought),
            Wildfire=sum(Wildfire),
            ExtrTemp=sum(ExtrTemp),
            ALL_GEO=sum(ALL_GEO),
            Earthquake=sum(Earthquake),
            Volcano=sum(Volcano),
            LandslideG=sum(LandslideG),
            ALL=sum(ALL),
            .groups="drop")%>%
  mutate(REGION_IFRC="Global")

RegIncFull%<>%rbind(global)%>%arrange(Year)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% FATALITIES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

FatFull<-impies%>%filter(Year>=2010 & imp_src_db%in%c("EMDAT","IDU") &
                         exp_spec=="expspec_allpeop" & imp_type=="imptypdeat")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS-HM" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VO"]),
            LandslideG=sum(imp_value[haz_Ab=="LS-G" & haz_type=="haztypegeohaz"]),
            ALL=sum(imp_value[haz_type%in%c("haztypehydromet","haztypegeohaz")]),
            .groups="drop")
FatFull<-WDR%>%dplyr::select(1:5)%>%left_join(FatFull)

FatFull[is.na(FatFull)]<-0

FatFull$ALL_CLIM<-rowSums(FatFull[,climvars])
FatFull$ALL_GEO<-rowSums(FatFull[,geovars])
FatFull$ALL<-FatFull$ALL_CLIM+FatFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IDPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

IDPFull<-impies%>%filter(Year>=2018 & imp_src_db=="IDU" &
                           exp_spec=="expspec_allpeop" & imp_type=="imptypidp")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS-HM" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VO"]),
            LandslideG=sum(imp_value[haz_Ab=="LS-G" & haz_type=="haztypegeohaz"]),
            ALL=sum(imp_value[haz_type%in%c("haztypehydromet","haztypegeohaz")]),
            .groups="drop")
IDPFull<-WDR%>%dplyr::select(1:5)%>%left_join(IDPFull)

IDPFull[IDPFull$Year<2018,6:ncol(IDPFull)]<- -999

IDPFull[is.na(IDPFull)]<-0
IDPFull[IDPFull== -999]<-NA

IDPFull$ALL_CLIM<-rowSums(IDPFull[,climvars])
IDPFull$ALL_GEO<-rowSums(IDPFull[,geovars])
IDPFull$ALL<-IDPFull$ALL_CLIM+IDPFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%% AFFECTED %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

AFFFull<-impies%>%filter(Year>=2010 & imp_src_db=="EMDAT" &
                           exp_spec=="expspec_allpeop" & imp_type=="imptypaffe")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS-HM" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VO"]),
            LandslideG=sum(imp_value[haz_Ab=="LS-G" & haz_type=="haztypegeohaz"]),
            ALL=sum(imp_value[haz_type%in%c("haztypehydromet","haztypegeohaz")]),
            .groups="drop")
AFFFull<-WDR%>%dplyr::select(1:5)%>%left_join(AFFFull)

AFFFull[is.na(AFFFull)]<-0

AFFFull$ALL_CLIM<-rowSums(AFFFull[,climvars])
AFFFull$ALL_GEO<-rowSums(AFFFull[,geovars])
AFFFull$ALL<-AFFFull$ALL_CLIM+AFFFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

COSFull<-impies%>%filter(Year>=2010 & imp_src_db=="EMDAT" &
                           exp_spec=="expspec_ecodirtotinf" & imp_type=="imptypcost")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS-HM" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VO"]),
            LandslideG=sum(imp_value[haz_Ab=="LS-G" & haz_type=="haztypegeohaz"]),
            ALL=sum(imp_value[haz_type%in%c("haztypehydromet","haztypegeohaz")]),
            .groups="drop")
COSFull<-WDR%>%dplyr::select(1:5)%>%left_join(COSFull)

COSFull[is.na(COSFull)]<-0

COSFull$ALL_CLIM<-rowSums(COSFull[,climvars])
COSFull$ALL_GEO<-rowSums(COSFull[,geovars])
COSFull$ALL<-COSFull$ALL_CLIM+COSFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%% CLIMATE PROGRAMMING %%%%%%%%%%%%%%%%%%%%%#
# Country names copied directly out of https://www.ifrc.org/sites/default/files/2022-11/2022-IFRC-Global-climate-resilience-platform-Brochure.pdf
climprog<-data.frame(CP=TRUE,Country=
                       c("Angola", "Benin", "Burkina Faso", "Burundi", "Central African Republic", "Chad", "Comoros", "Congo ", 
                       "Côte d’Ivoire", "Democratic Republic of the Congo", "Ethiopia ", " Equatorial Guinea ", " Ghana ", "Guinea-Bissau ", " Kenya ", " Lesotho ", " Liberia ", " Madagascar ", " Malawi ", " Mali ", " Mauritania ", 
                       "Mozambique ", " Namibia ", " Niger ", " Nigeria ", " Sao Tome & Principe ", " Sierra Leone ", " Somalia ", 
                       "South Africa", " South Sudan ", " Sudan ", " Tanzania ", " Togo", " Tongo ", " Uganda ", " Zimbabwe ", " Zambia",
                     "Antigua and Barbuda ", " Argentina ", " Belize ", " Bolivia ", " Colombia ", " Costa Rica ", " Cuba ", 
                     " Dominican Republic ", " Ecuador ", " El Salvador ", " Grenada ", " Guatemala ", " Guyana ", " Haiti ", " Honduras ", " Jamaica ", 
                     "Nicaragua ", " Panama ", " Paraguay ", " Peru ", " The Bahamas ", " Trinidad and Tobago ", " Uruguay",
                     "Afghanistan ", " Bangladesh ", " Cook Islands ", " Federated State of Micronesia ", " India ", " Indonesia ", 
                     "Maldives ", " Mongolia ", " Myanmar ", " Nepal ", " Pakistan ", " Palau ", " Philippines ", 
                     " Republic of the Marshall Islands ", " Samoa ", " Solomon Islands ", " Sri Lanka ", 
                     " The Democratic People’s Republic of Korea ", " Timor-Leste ", " Tuvalu ", " Vanuatu ", " Viet Nam",
                     "Albania ", " Bosnia and Herzegovina ", " Kazakhstan ", " Kyrgystan ", " Serbia ", " Tajikistan ", "Turkmenistan ", " Uzbekistan",
                     "Algeria ", " Egypt ", " Iran ", " Iraq ", " Jordan ", " Lebanon ", " Libya ", " Palestine ", " Syria ", " Yemen"))
# Tidy this up a little
climprog$Country%<>%stringi::stri_trim_both()
# Add the ISO codes
climprog$ISO3<-convCountryIso3(climprog$Country)
# Clean up manually any codes that didn't seem to go through (Tongo and Kyrgyzstan - they misspelt the latter)
climprog$Country[is.na(climprog$ISO3)]
# [1] "Tongo"     "Kyrgystan"
# So clean them up!
climprog$ISO3[is.na(climprog$ISO3)]<-c("TON","KGZ")
# Join to the full ISO code list for the WDR
climprog<-left_join(data.frame(ISO3=unique(openxlsx::read.xlsx(wdrfile,sheet = 6,startRow = 6)%>%
                                             dplyr::select(2)%>%pull(ISO))),climprog)
# Set the values
climprog$CP<-factor(is.na(climprog$CP),labels = c("YES","NO"))

#%%%%%%%%%%%%%%%%%%%% INFORM+CLIMATE INDICES %%%%%%%%%%%%%%%%%%%%%#
# https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/2025/INFORM2024_TREND_2015_2024_v69_ALL.xlsx
informy<-readxl::read_xlsx("./Analysis_Results/Kirsten/INFORM2024_TREND_2015_2024_v69_ALL.xlsx")%>%
  filter(IndicatorId%in%c("CC.INS.DRR",# Disaster Risk Reduction
                          "GDPpp",#GDP-PPP per person
                          "VU",#Vulnerability index
                          "MPI",#Multidimensional poverty index
                          "INFORM"))
informy%<>%filter(INFORMYear==max(INFORMYear))%>%
  dplyr::select(Iso3,IndicatorName,IndicatorScore)%>%
  pivot_wider(names_from = IndicatorName,values_from = IndicatorScore)%>%
  setNames(c("ISO3","DRR","GDPcap","VulI","INFORM","MPI"))%>%
  mutate_at(c("DRR","VulI","INFORM","MPI"),function(x) x*10)%>%
  mutate_at(c("MPI"),function(x) round(x*10,1))

IWDR<-openxlsx::read.xlsx(wdrfile,sheet = 6,startRow = 6)%>%
  dplyr::select(2,6:12)
colnames(IWDR)[1]<-"ISO3"
# Make sure all countries are present
informy<-IWDR%>%dplyr::select(ISO3)%>%left_join(informy,by="ISO3")

# Download the compressed file
download.file("https://gain.nd.edu/assets/581929/nd_gain_countryindex_2024.zip",
              "./RawData/SocioPoliticalData/GAIN.zip")
# Decompress the file
R.utils::gunzip("./RawData/SocioPoliticalData/GAIN.zip",
                "./Analysis_Results/Kirsten/GAIN.csv",overwrite=T)
# Extract the data of the GAIN indices
NDGAIN<-read_csv("./Analysis_Results/Kirsten/GAIN.csv")%>%
  dplyr::select(ISO3,`2022`)%>%setNames(c("ISO3","GAIN"))%>%
  mutate(GAIN=round(GAIN*100,1))

# Merge into inform and other indices
informy%<>%left_join(NDGAIN,by="ISO3")

# MVI
MVI<-openxlsx::read.xlsx("https://www.un.org/ohrlls/sites/www.un.org.ohrlls/files/files/mvi_results.xlsx",
                    sheet = 1,startRow = 2)%>%dplyr::select(2:3)%>%
  setNames(c("ISO3","MVI"))%>%mutate(MVI=round(MVI,1))

# Merge into inform and other indices
informy%<>%left_join(MVI,by="ISO3")

# Overall vulnerability index
isnas<-apply(informy[,c(4,5,8)],1,function(x) all(is.na(x)))
informy$OVI[!isnas]=round(apply(informy[!isnas,c(4,5,8)],1,sum,na.rm=T)/3,1)

# Fragile states index
# https://fragilestatesindex.org/global-data/
FSI<-openxlsx::read.xlsx("https://fragilestatesindex.org/wp-content/uploads/2023/06/FSI-2023-DOWNLOAD.xlsx",
                    sheet = 1,startRow = 1)%>%dplyr::select(c(1,4))%>%
  setNames(c("Country","FSI"))%>%
  mutate(FSI=round(10*FSI/12,1), ISO3=convCountryIso3(Country))%>%dplyr::select(c(3,2))
  
# Merge into inform and other indices
informy%<>%left_join(FSI,by="ISO3")

# Create the classification system
for(i in c(2,4:ncol(informy))){
  collie<-colnames(informy)[i]
  informy$value<-informy[,i]
  informy%<>%mutate(tmp=case_when(value<20 ~ "Very Low",
                                  value<40 ~ "Low",
                                  value<60 ~ "Medium",
                                  value<80 ~ "High",
                                  value<100 ~ "Very High",
                                  T~NA_character_))%>%
    dplyr::select(-value)
  colnames(informy)[ncol(informy)]<-paste0(collie,"_class")
}

informy%<>%arrange(ISO3)

#%%%%%%%%%%%%%%%%%%%%%%%%% WRITE-OUT %%%%%%%%%%%%%%%%%%%%%%%%%%#

saveRDS(impies,"./Analysis_Results/Kirsten/impies_2024-10-23.RData")
openxlsx::write.xlsx(IncFull,"./Analysis_Results/Kirsten/Indicence.xlsx")
openxlsx::write.xlsx(RegIncFull,"./Analysis_Results/Kirsten/RegionalIndicence.xlsx")
openxlsx::write.xlsx(FatFull,"./Analysis_Results/Kirsten/Fatalities.xlsx")
openxlsx::write.xlsx(IDPFull,"./Analysis_Results/Kirsten/IDPs.xlsx")
openxlsx::write.xlsx(AFFFull,"./Analysis_Results/Kirsten/Affected.xlsx")
openxlsx::write.xlsx(COSFull,"./Analysis_Results/Kirsten/Cost.xlsx")
openxlsx::write.xlsx(climprog,"./Analysis_Results/Kirsten/ClimateProgram.xlsx")
openxlsx::write.xlsx(informy,"./Analysis_Results/Kirsten/Indices.xlsx")
openxlsx::write.xlsx(informy%>%filter(ISO3%in%unique(WDR$ISO3)),"./Analysis_Results/Kirsten/RedIndices.xlsx")

#%%%%%%%%%%%%%%%%%%%%%%%%% EXTRACT ALL DATA %%%%%%%%%%%%%%%%%%%%%%%%%%#

Monty<-convGOApp_Monty(taby=T)
# EM-DAT
Monty%<>%dplyr::bind_rows(convEMDAT_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# IDMC GIDD
Monty%<>%dplyr::bind_rows(convGIDD_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# IDMC IDU
Monty%<>%dplyr::bind_rows(convIDU_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# GLIDE
Monty%<>%dplyr::bind_rows(convGLIDE_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# GDACS
GDACS<-convGDACS_Monty(taby=T)
# The ISOs are vectors of characters
GDACS$ev_ISO3s<-GDACS$imp_ISO3s<-GDACS$haz_ISO3s<-unname(unlist(parallel::mclapply(GDACS$imp_ISO3s,function(x){
  paste0(unique(x),collapse=":")
},mc.cores=ncores)))
# Now let's bind it all
Monty%<>%dplyr::bind_rows(GDACS%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))

Monty%<>%filter(imp_value!=0 & !is.na(haz_spec) & !is.na(imp_value))

Monty$Year<-AsYear(Monty$imp_sdate)
Monty%<>%filter(!is.na(year))
Monty$ISO3<-Monty$ev_ISO3s
Monty$imp_src_db[is.na(Monty$imp_src_db)]<-Monty$haz_src_db[is.na(Monty$imp_src_db)]
Monty$imp_src_org[is.na(Monty$imp_src_db)]<-Monty$haz_src_org[is.na(Monty$imp_src_db)]

Monty$haz_Ab<-Monty$haz_Ab; 
# Monty$haz_Ab[Monty$haz_Ab%in%c("HW","CW","HT")]<-"ET"
Monty$haz_Ab[Monty$haz_Ab%in%c("HT")]<-"HW"
Monty$haz_Ab[Monty$haz_Ab%in%c("FR")]<-"WF"
Monty$haz_Ab[Monty$haz_Ab%in%c("WV","WA")]<-"SS"
Monty$haz_Ab[Monty$haz_Ab%in%c("MM","MS","SL","AV","ER")]<-"LS"
Monty$haz_Ab[Monty$haz_Ab%in%c("TC,FL","EC")]<-"TC"
Monty%<>%filter(!haz_Ab%in%c("EP","IN","SN"))
Monty$haz_Ab[Monty$haz_Ab=="LS" & Monty$haz_type=="haztypehydromet"]<-"LS-HM"
Monty$haz_Ab[Monty$haz_Ab=="LS" & Monty$haz_type=="haztypegeohaz"]<-"LS-G"

table(Monty$haz_Ab)
table(Monty$imp_src_db[Monty$haz_Ab=="LS"])
table(Monty$haz_type[Monty$haz_Ab=="LS"])

isoEQ<-unique(Monty$ISO3[Monty$haz_Ab=="EQ"])

saveRDS(Monty,"./CleanedData/Monty_2024-10-24_tab.RData")
Monty<-readRDS("./CleanedData/Monty_2024-10-24_tab.RData")
#%%%%%%%%%%%%%%%%%%%%%% REPORT FIGURES %%%%%%%%%%%%%%%%%%%%%%%%#

# Top-5 impacts, per impact type and hazard
top5<-Monty%>%filter(Year==AsYear(Sys.Date()) & 
                     haz_type=="haztypehydromet" &
                     imp_type!="imptypalert")%>%
  dplyr::select(ev_sdate,ev_fdate,ISO3,haz_Ab,exp_spec,imp_type,
                imp_value,imp_units,imp_src_db,imp_src_org,ev_name)%>%
  arrange(desc(imp_value))%>%distinct()%>%
  group_by(exp_spec,imp_type)%>%
  slice(1:5)%>%
  left_join(taxies%>%filter(list_name=="exp_specs")%>%dplyr::select(2:3)%>%
              setNames(c("exp_spec","exposure")),by="exp_spec")%>%
  left_join(taxies%>%filter(list_name=="imp_type")%>%dplyr::select(2:3)%>%
              setNames(c("imp_type","impact_type")),by="imp_type")%>%
  ungroup()%>%
  dplyr::select(ev_sdate,ev_fdate,ISO3,haz_Ab,exposure,impact_type,
                imp_value,imp_units,imp_src_db,imp_src_org,ev_name)

View(top5)
openxlsx::write.xlsx(top5,"./Analysis_Results/Kirsten/Top5_2024.xlsx")


ifrc<-Monty%>%filter(imp_src_org=="IFRC" & Year==2024)

print(paste0("No. hydromet hazards with DREF = ",
             ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" & imp_type=="imptypcost")%>%nrow(),
             ". As a percentage of total DREF = ",
             round(100*ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" & imp_type=="imptypcost")%>%nrow()/ifrc%>%filter(imp_src_db=="GO-DREF" & imp_type=="imptypcost")%>%nrow(),1),"%"))
print(paste0("Total allocation to hydromet DREFs = ",
             ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" & imp_type=="imptypcost")%>%pull(imp_value)%>%sum," [CHF]. As a percentage of total DREF = ",
             round(100*ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" & imp_type=="imptypcost")%>%pull(imp_value)%>%sum/ifrc%>%filter(imp_src_db=="GO-DREF" & imp_type=="imptypcost")%>%pull(imp_value)%>%sum,1),"%"))
print(paste0("Number of EAPs triggered = ",
             ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" &
                             imp_type=="imptypcost" & grepl("EAP",ev_name,ignore.case = F))%>%nrow(),
             ". As a percentage of total EAPs = ",
             round(100*ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" &
                                       imp_type=="imptypcost" & grepl("EAP",ev_name,ignore.case = F))%>%nrow()/ifrc%>%filter(imp_src_db=="GO-DREF" & imp_type=="imptypcost" & grepl("EAP",ev_name,ignore.case = F))%>%nrow(),1),"%"))
print(paste0("Total allocation to hydromet EAPs = ",
             ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" &
                             imp_type=="imptypcost" & grepl("EAP",ev_name,ignore.case = F))%>%pull(imp_value)%>%sum," [CHF]. As a percentage of total DREF = ",
             round(100*ifrc%>%filter(imp_src_db=="GO-DREF" & haz_type=="haztypehydromet" &
                                       imp_type=="imptypcost" & grepl("EAP",ev_name,ignore.case = F))%>%pull(imp_value)%>%sum/ifrc%>%filter(imp_src_db=="GO-DREF" & imp_type=="imptypcost" & grepl("EAP",ev_name,ignore.case = F))%>%pull(imp_value)%>%sum,1),"%"))


# Breakdown per fragility level:
frag<-left_join(ifrc%>%filter(imp_src_db=="GO-DREF" &
                                haz_type=="haztypehydromet" &
                                imp_type=="imptypcost")%>%
                  dplyr::select(ev_sdate,ev_fdate,ISO3,haz_Ab,exp_spec,imp_type,
                                imp_value,imp_units,imp_src_db,imp_src_org,ev_name),informy,by="ISO3")
p<-frag%>%filter(!is.na(FSI_class))%>%
  mutate(FSI_class = factor(FSI_class, levels = c("Very High", "High", "Medium", "Low")))%>%
  ggplot()+geom_bar(aes(FSI_class,fill=FSI_class)) +
  xlab("Fragile States Index Classification")+ylab("Number of DREF Allocations")+
  labs(fill="FSI Class")+
  ggtitle("No. DREF Allocations per Fragility Class")+theme(plot.title = element_text(hjust = 0.5));p
ggsave("./Analysis_Results/Kirsten/FSI_HM-2024.png",p,width=8,height = 5)

RegIncFull%>%filter(Year==2024)


#%%%%%%%%%%%%%%%%%%%%%% MAKE FIGURES %%%%%%%%%%%%%%%%%%%%%%%%#

p<-Monty%>%filter(Year>1990 & Year<=AsYear(Sys.Date()) & #ISO3%in%isoEQ & 
                     !(imp_src_db=="GIDD" & Year<2015) &
                  # exp_spec=="expspec_allpeop" & imp_type=="imptypdeat" &
                  !imp_src_db%in%c("GO-FR","GO-EA","GO-FBA","GDACS"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/(sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=imp_src_db))+geom_point(aes(Year,Percentage,colour=imp_src_db),alpha=0.2)+
  geom_smooth(aes(Year,Percentage,colour=imp_src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = FALSE)+
  ylab("Proportion")+labs(colour="Database")+ylim(c(0.75,0.97))+
  ggtitle("Proportion of Climate & Weather Events")+theme(plot.title = element_text(hjust = 0.5));p
  # facet_wrap(~imp_src_db,scales = "fixed");p
ggsave("./Analysis_Results/Kirsten/Percentage_HM-G_Year.png",p,width=8,height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% STATISTICAL SIGNIFICANCE %%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%% FOR CLIMATE CHANGE %%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

propy<-Monty%>%filter(Year>2000 & Year<=AsYear(Sys.Date()) & ISO3%in%isoEQ &
                         !(imp_src_db=="GIDD" & Year<2015) &
                         !duplicated(imp_sub_ID) &
                         !imp_src_db%in%c("GO-FR","GDACS"))%>%
  group_by(imp_src_db,Year)%>%
  reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/
            (sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)),
          Counts=length(haz_type))%>%ungroup()%>%group_by(imp_src_db)%>%
  mutate(Weights=Counts/max(Counts),Date=as.Date(Date))

burnin<-0.3

propy<-Monty%>%filter(Year>1990 & ISO3%in%isoEQ &
                        !(imp_src_db=="GIDD" & Year<2015) &
                        !duplicated(imp_sub_ID) &
                        !imp_src_db%in%c("GO-FR","GDACS","GO-DREF","GO-EA","GO-FBA"))%>%
  arrange(Year)%>%
  group_by(imp_src_db)%>%
  reframe(Percentage=(cumsum(haz_type=="haztypehydromet")/
            (cumsum(haz_type=="haztypegeohaz" | haz_type=="haztypehydromet")))[round(burnin*n()):n()],
          Date=ev_sdate[round(burnin*n()):n()], 
          Day=(as.numeric(as.Date(Date)-min(as.Date(Date)))/365)[round(burnin*n()):n()])%>%
  ungroup()%>%distinct()%>%group_by(imp_src_db)%>%mutate(Day=Day/max(Day),Date=as.Date(Date))
View(propy)

propy%>%ggplot()+geom_point(aes(Day,Percentage))
propy%>%ggplot()+geom_point(aes(as.Date(Date),Percentage,colour=imp_src_db))

# propy$LogWeights<-log10(propy$Counts)/log10(max(propy$Counts))

# Unweighted
# fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy)%>%summary()
# Weighted by log-count values
# fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy, weights = propy$LogWeights)%>%summary()
# Weighted by database-normalised count values (not log)
# fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy, weights = propy$Weights)%>%summary()

# Check the plot to see the count numbers
propy%>%ggplot()+geom_point(aes(Year,Counts,colour=imp_src_db,size=Weights)) + scale_y_log10()
# Also plot the proportions
propy%>%ggplot()+geom_point(aes(Year,Percentage,colour=imp_src_db,size=Weights))

# Now let's predict some values with the analysis
modelly<-fixest::feglm(Percentage ~ Date | imp_src_db,family = "binomial",data = propy)%>%summary()
  # fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy, weights = propy$Weights)
# Prediction for 2022
predict(modelly,data.frame(imp_src_db="GIDD",Date=Sys.Date()))
# Prediction for 1990
predict(modelly,data.frame(imp_src_db="GIDD",Date=as.Date("2010-01-01")))
# How about for the appeals dataset?
predict(modelly,data.frame(imp_src_db="GO-DREF",Date=Sys.Date()))
predict(modelly,data.frame(imp_src_db="GO-DREF",Date=as.Date("2010-01-01")))

predict(modelly,data.frame(imp_src_db="EMDAT",Date=Sys.Date()))
predict(modelly,data.frame(imp_src_db="EMDAT",Date=as.Date("2010-01-01")))
# 












#%%%% PERCENTAGE OF FUNDING ALLOCATED VS REQUESTED %%%%%#
appeal<-CleanGO_app(ExtractGOdata(db = "GO-App", token = token))

percies<-appeal%>%filter(Year>1990 & Year<=AsYear(Sys.Date()) & 
                  imp_src_db=="GO-App")%>%
  reframe(Year=Year[imp_type=="imptypaidreqifrc"],Percentage=(imp_value[imp_type=="imptypaidallifrc"]/
            (imp_value[imp_type=="imptypaidallifrc"]+imp_value[imp_type=="imptypaidreqifrc"]))  )
View(percies)

percies%>%#filter(Percentage!=0.5)%>%
  group_by(Year)%>%
  reframe(N=length(Percentage),avg=mean(Percentage),sd=sd(Percentage))%>%
  ggplot()+geom_point(aes(Year,avg,size=N))+geom_ribbon(aes(x=Year,ymin=avg-sd,ymax=avg+sd),alpha=0.2)


#%%%%% FUNDING ALLOCATED TO CLIMATE & WEATHER %%%%%#
# Appeals
appeal%>%filter(Year>2000 & Year<=AsYear(Sys.Date()) & 
                  imp_src_db=="GO-App" & !is.na(haz_type))%>%
  group_by(Year,haz_type)%>%
  reframe(ALL=sum(imp_value[imp_type=="imptypaidallifrc"]), REQ=sum(imp_value[imp_type=="imptypaidreqifrc"]),
          TAR=sum(imp_value[imp_type=="imptyptarg"]))%>%
  reshape2::melt(measure.vars=c("ALL","REQ","TAR"))%>%
  ggplot()+geom_point(aes(Year,value,colour=haz_type)) +
  geom_smooth(aes(Year,value,colour=haz_type),method="lm",alpha=0.1,se = FALSE) +
  scale_y_log10() + facet_wrap(~variable)

# As a percentage of the other hazards
appeal%>%mutate(Year=AsYear(ev_sdate))%>%
  filter(Year>2000 & imp_src_db=="GO-App" & !is.na(haz_type))%>%
  mutate(Climate=!is.na(haz_type) & haz_type=="haztypehydromet")%>%
  group_by(Year,event_ID,Climate,.add=T)%>%
  reframe(ALL=sum(imp_value[imp_type=="imptypaidallifrc"],na.rm = T), 
          REQ=sum(imp_value[imp_type=="imptypaidreqifrc"],na.rm = T),
          TAR=sum(imp_value[imp_type=="imptyptarg"],na.rm = T))%>%ungroup()%>%
  group_by(Year)%>%
  reframe(ALL=sum(ALL[Climate])/sum(ALL),
          REQ=sum(REQ[Climate])/sum(REQ), 
          TAR=sum(TAR[Climate])/sum(TAR))%>%ungroup()%>%
  reshape2::melt(measure.vars=c("ALL","REQ","TAR"))%>%
  filter(Year>1990)%>%
  ggplot()+geom_point(aes(Year,value,colour=variable))+
  # geom_point(aes(Year,value,fill=Climate))+
  geom_smooth(aes(Year,value,colour=variable),method="lm",alpha=0.1,se = FALSE)+
  scale_y_log10()+facet_wrap(~variable)
  
# As a percentage, but with allocated only
percies<-appeal%>%mutate(Year=AsYear(ev_sdate))%>%
  filter(Year>2000 & imp_src_db=="GO-App" & !is.na(haz_type))%>%
  mutate(Climate=!is.na(haz_type) & haz_type=="haztypehydromet")%>%
  group_by(Year,event_ID,Climate,.add=T)%>%
  reframe(ALL=sum(imp_value[imp_type=="imptypaidallifrc"],na.rm = T), 
          REQ=sum(imp_value[imp_type=="imptypaidreqifrc"],na.rm = T),
          TAR=sum(imp_value[imp_type=="imptyptarg"],na.rm = T))%>%ungroup()%>%
  group_by(Year)%>%
  reframe(ALL=sum(ALL[Climate])/sum(ALL),
          REQ=sum(REQ[Climate])/sum(REQ), 
          TAR=sum(TAR[Climate])/sum(TAR))%>%ungroup()
  p<-percies%>%ggplot()+geom_point(aes(Year,ALL),colour="red")+
  # geom_point(aes(Year,value,fill=Climate))+
  geom_smooth(aes(Year,ALL),colour="red",method="lm",alpha=0.1,se = FALSE)+
  ylab("Percentage")+ggtitle("% Alloc. Funds to Clim & Weather Evs")+theme(plot.title = element_text(hjust = 0.5))

percies%>%glm(formula=ALL~Year,family = binomial(link="logit"))


cumFunding<-appeal%>%filter(Year>2000 & !is.na(haz_type))%>%
  arrange(as.Date(ev_sdate))%>%
  mutate(Climate=!is.na(haz_type) & haz_type=="haztypehydromet")%>%
  group_by(Climate,imp_type, ev_sdate)%>%
  reframe(cumFunding=sum(imp_value),
         Year=AsYear(ev_sdate),
         Date=as.Date(ev_sdate))%>%
  group_by(Climate,imp_type)%>%
  mutate(cumFunding=cumsum(cumFunding))

p<-cumFunding%>%filter(Year<=AsYear(Sys.Date()) & Year>2005 & imp_type!="imptyptarg")%>%
  ggplot()+geom_line(aes(Date,cumFunding,colour=Climate))+
  xlab("Year")+ylab("Cumulative Funding Allocated")+labs(colour="Climate-Related")+
  facet_wrap(~imp_type);p
ggsave("Cumulative_AllocFund_ClimWeath-vs-All_imp_type.png",p,path="./Plots/",width = 8, height=5)

cumFunding$Days<-as.numeric(cumFunding$Date-min(cumFunding$Date))

# ALLOCATED FUNDING
AcFunds<-cumFunding%>%filter(imp_type=="imptypaidallifrc" & Climate & Year>2005 & Year<=AsYear(Sys.Date()))
tmp<-cumFunding%>%filter(imp_type=="imptypaidallifrc" & !Climate)

AcFunds$cumFundingNonClim<-interp1(tmp$Days,tmp$cumFunding,AcFunds$Days,method="linear")

AcFunds%>%ggplot()+
  geom_point(aes(Days,cumFunding),colour="red")+
  geom_point(aes(Days,cumFundingNonClim),colour="blue")

AcFunds%>%ggplot()+
  geom_point(aes(Date,cumFunding),colour="red")+
  geom_point(aes(Date,cumFundingNonClim),colour="blue")

p<-AcFunds%>%mutate(Percentage=100*cumFunding/(cumFunding+cumFundingNonClim))%>%
  ggplot()+geom_line(aes(Date,Percentage))+ #ylim(c(0,100))+
  ggtitle("% Allocated Funds to Climate & Weather")+theme(plot.title = element_text(hjust = 0.5));p
ggsave("Perc_AllocFund_ClimWeath-vs-All.png",p,path="./Plots/",width = 7, height=5)

# REQUESTED FUNDING
RcFunds<-cumFunding%>%filter(imp_type=="imptypaidreqifrc" & Climate & Year>2005 & Year<=AsYear(Sys.Date()))
tmp<-cumFunding%>%filter(imp_type=="imptypaidreqifrc" & !Climate)

RcFunds$cumFundingNonClim<-interp1(tmp$Days,tmp$cumFunding,RcFunds$Days,method="linear")

q<-RcFunds%>%mutate(Percentage=100*cumFunding/(cumFunding+cumFundingNonClim))%>%
  ggplot()+geom_line(aes(Date,Percentage))+ #ylim(c(0,100))+
  ggtitle("% Requested Funds to Climate & Weather")+theme(plot.title = element_text(hjust = 0.5));q
ggsave("Perc_AllocFund_ClimWeath-vs-All.png",q,path="./Plots/",width = 7, height=5)

# BOTH TOGETHER
cFunds<-rbind(RcFunds,AcFunds)%>%mutate(Percentage=100*cumFunding/(cumFunding+cumFundingNonClim))
cFunds$Funding<-factor(cFunds$imp_type,labels=c("Requested","Allocated"))
p<-cFunds%>%ggplot()+geom_line(aes(Date,Percentage,colour=Funding))+
  ggtitle("% Emergency Appeal Funds to Climate & Weather")+theme(plot.title = element_text(hjust = 0.5));p
ggsave("Perc_Alloc-vs-Req_Fund_ClimWeath-vs-All.png",p,path="./Plots/",width = 7, height=5)

# Save out
outer<-cFunds%>%mutate(Percentage=100*cumFunding/(cumFunding+cumFundingNonClim))%>%
  as.data.frame()%>%
  dplyr::select(c(Date,cumFunding,cumFundingNonClim,Percentage))
colnames(outer)[2:4]<-c("Climate and Weather-Related Allocated Funding - Emergency Appeals",
                        "All Other Funding - Emergency Appeals",
                        "Percentage of Funding to Climate and Weather-Related Emergency Appeals")
outer%>%openxlsx::write.xlsx("./Analysis_Results/Kirsten/Perc_AllocFund_ClimWeath-vs-All.xlsx")




daterange <- Monty%>%mutate(ev_sdate=as.Date(ev_sdate))%>%
  filter(imp_src_db!="GDACS")%>%
  group_by(imp_src_db)%>%
  reframe(ev_sdate=seq(min(ev_sdate,na.rm = T),max(ev_sdate,na.rm = T), by = "1 day"))

tmp <- Monty %>% mutate(ev_sdate=as.Date(ev_sdate,format="%Y-%m-%d"), Year=AsYear(ev_sdate)) %>% arrange(ev_sdate)%>%
  filter(Year>1990 & Year<=AsYear(Sys.Date()) & 
           !(imp_src_db=="GIDD" & Year<2015) &
           !imp_src_db%in%c("GO-FR","GDACS") &
           exp_spec%in%c("expspec_allpeop","expspec_aidunkinf") & 
           imp_type%in%c("imptypdeat","imptypcost","imptypdama","imptypdest",
                         "imptypdiraffe","imptypidp","imptypaffe","imptyptarg")) %>%
  mutate(impact=paste0(exp_spec,"-",imp_type))%>%
  group_by(imp_src_db,impact,ev_sdate)%>%
  reframe(ClimCounts=sum(haz_type=="haztypehydromet",na.rm = T),
          NonClimCounts=sum(haz_type!="haztypehydromet",na.rm = T))%>%
  right_join(daterange) %>%
  mutate(ClimCounts = cumsum(if_else(is.na(ClimCounts), 0, ClimCounts)),
         NonClimCounts = cumsum(if_else(is.na(NonClimCounts), 0, NonClimCounts)),
         Percentage=100*ClimCounts/(ClimCounts+NonClimCounts))

tmp%>%
  ggplot()+geom_point(aes(ev_sdate,ClimCounts,colour=impact)) +
  geom_point(aes(ev_sdate,NonClimCounts,colour=impact)) +
  scale_y_log10()+
  facet_wrap(~imp_src_db)



propy<-appeal%>%mutate(ev_sdate=as.Date(ev_sdate), Year=AsYear(ev_sdate))%>%
  filter(Year>1990 & Year<=AsYear(Sys.Date()) & 
                         # !(imp_src_db=="GIDD" & AsYear(ev_sdate)<2015) &
                         !duplicated(imp_sub_ID) &
                         !imp_src_db%in%c("GO-FR","GDACS"))%>%
  group_by(imp_src_db,Year)%>%
  reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/
            (sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)),
          Counts=length(haz_type))%>%ungroup()%>%group_by(imp_src_db)%>%
  mutate(Weights=Counts/max(Counts))

p<-propy%>%ggplot()+
  geom_point(aes(Year,Counts,colour=imp_src_db))+
  geom_smooth(aes(Year,Counts,colour=imp_src_db), se=F)+ylim(c(0,NA))+
  ylab("No. Recorded Events")+xlab("Year")+labs(colour="Database")+
  facet_wrap(~imp_src_db,scales = "free_y")
ggsave("./Plots/Counts_db_w-LOESS.png",p,width=10,height=6)






Monty%<>%mutate(Region=left_join(Monty,readxl::read_xlsx(filer)%>%
                                    transmute(ISO3=`ISO Code`,continent=`UN Region`),
                                  by="ISO3")$continent,
                 Subregion=left_join(Monty,readxl::read_xlsx(filer)%>%
                                       transmute(ISO3=`ISO Code`,continent=`World Bank Regions`),
                                     by="ISO3")$continent)
# So that the months are plotted in english
Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")

month_labs<-c("January","February","March","April",
              "May","June","July","August",
              "September","October","November","December")
# 
p<-Monty%>%filter(!is.na(Subregion) & Subregion!="Not Classified" & 
                     (haz_type=="haztypehydromet" | haz_Ab=="WF") & imp_src_db=="EMDAT")%>%
  distinct(GCDB_ID,.keep_all = T)%>%group_by(Subregion)%>%
  mutate(difftime=as.numeric((as.Date(ev_sdate)-as.Date(paste0(AsYear(ev_sdate),"-01-01")))/365))%>%
  ggplot()+geom_density(aes(difftime,y=..scaled..,fill=Subregion),alpha=0.5)+
  scale_x_continuous(breaks=0:11/12,labels=month_labs)+xlab("Month")+ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Subregion,nrow=3)+
  ggtitle("Proportion of Climate- & Weather-Related Events");p
ggsave("./Plots/PercEvents_Month_Subregion.png",p,height = 8,width=12) 
# 
p<-Monty%>%filter(!is.na(Subregion) & Subregion!="Not Classified" & 
                     imp_src_db=="EMDAT" & haz_Ab%in%c("FL"))%>%
  distinct(GCDB_ID,.keep_all = T)%>%group_by(Subregion)%>%
  mutate(difftime=as.numeric((as.Date(ev_sdate)-as.Date(paste0(AsYear(ev_sdate),"-01-01")))/365))%>%
  ggplot()+geom_density(aes(difftime,y=..scaled..,fill=Subregion),alpha=0.5)+
  scale_x_continuous(breaks=0:11/12,labels=month_labs)+xlab("Month")+ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Subregion)+
  ggtitle("Proportion of Flood Events");p
ggsave("./Plots/PercFloods_Month_Subregion.png",p,height = 8,width=12) 
# 
p<-Monty%>%filter(!is.na(Subregion) & Subregion!="Not Classified" & 
                     (haz_type=="haztypehydromet" | haz_Ab=="WF") & 
                     exp_spec=="expspec_allpeop" & imp_type=="imptypdeat")%>%
  distinct(GCDB_ID,.keep_all = T)%>%group_by(Subregion)%>%
  mutate(difftime=as.numeric((as.Date(ev_sdate)-as.Date(paste0(AsYear(ev_sdate),"-01-01")))/365),
         Subregion=as.factor(Subregion))%>%filter(imp_src_db=="EMDAT")%>%
  ggplot()+geom_density(aes(difftime,y=..scaled..,weights=imp_value/sum(imp_value),fill=Subregion),alpha=0.5)+
  scale_x_continuous(breaks=0:11/12,labels=month_labs)+xlab("Month")+ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Subregion,nrow=3)+
  ggtitle("Proportion of Climate- & Weather-Related Deaths");p
ggsave("./Plots/PercDeaths_Month_Subregion.png",p,height = 8,width=12) 
# 
p<-Monty%>%filter(!is.na(Subregion) & Subregion!="Not Classified" & 
                     (haz_type=="haztypehydromet" | haz_Ab=="WF") & 
                     exp_spec=="expspec_allpeop" & imp_type=="imptypdeat")%>%
  distinct(GCDB_ID,.keep_all = T)%>%group_by(Subregion)%>%
  mutate(difftime=as.numeric((as.Date(ev_sdate)-as.Date(paste0(AsYear(ev_sdate),"-01-01")))/365))%>%
  filter(imp_src_db=="EMDAT")%>%
  ggplot()+geom_density(aes(difftime,y=..scaled..,weights=imp_value/sum(imp_value),fill=Subregion),alpha=0.5)+
  scale_x_continuous(breaks=0:11/12,labels=month_labs)+xlab("Month")+ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Subregion)+
  ggtitle("Proportion of Flood Deaths");p
ggsave("./Plots/PercFloodDeaths_Month_Subregion.png",p,height = 8,width=12) 


p<-Monty%>%filter(!is.na(Subregion) & Subregion!="Not Classified" & 
                     (haz_type=="haztypehydromet" | haz_Ab=="WF") & 
                     exp_spec%in%c("expspec_ecodirtotinf","expspec_ecodirtotnoninf") & imp_type=="imptypcost")%>%
  distinct(GCDB_ID,.keep_all = T)%>%group_by(Subregion)%>%
  mutate(difftime=as.numeric((as.Date(ev_sdate)-as.Date(paste0(AsYear(ev_sdate),"-01-01")))/365))%>%
  # filter(imp_src_db=="EMDAT")%>%
  ggplot()+geom_density(aes(difftime,y=..scaled..,weights=imp_value/sum(imp_value),
                            fill=Subregion,linetype=imp_src_db,alpha=imp_src_db))+
  scale_x_continuous(breaks=0:11/12,labels=month_labs)+xlab("Month")+ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        plot.title = element_text(hjust = 0.5))+
  scale_alpha_discrete(range = c(0, 0.5))+
  facet_wrap(~Subregion)+
  ggtitle("Proportion of Cost");p
ggsave("./Plots/PercCost_Month_Subregion_perDB.png",p,height = 8,width=12) 

p<-Monty%>%filter(!is.na(Subregion) & Subregion!="Not Classified" & 
                     (haz_type=="haztypehydromet" | haz_Ab=="WF") & 
                     exp_spec=="expspec_ecodirtotinf" & imp_type=="imptypcost")%>%
  distinct(GCDB_ID,.keep_all = T)%>%group_by(Subregion)%>%
  mutate(difftime=as.numeric((as.Date(ev_sdate)-as.Date(paste0(AsYear(ev_sdate),"-01-01")))/365))%>%
  # filter(imp_src_db=="EMDAT")%>%
  ggplot()+geom_density(aes(difftime,y=..scaled..,weights=imp_value/sum(imp_value),
                            fill=Subregion),alpha=0.5)+
  scale_x_continuous(breaks=0:11/12,labels=month_labs)+xlab("Month")+ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        plot.title = element_text(hjust = 0.5))+
  scale_alpha_discrete(range = c(0, 0.5))+
  facet_wrap(~Subregion)+
  ggtitle("Proportion of Cost (EM-DAT)");p
ggsave("./Plots/PercCost_Month_Subregion_EMDAT.png",p,height = 8,width=12) 


tmp<-Monty%>%mutate(Year=AsYear(ev_sdate), Date=as.Date(ev_sdate))%>%
  filter(haz_Ab=="EQ" & imp_src_db=="EMDAT" &
         exp_spec=="expspec_allpeop" & imp_type=="imptypdeat" &
         Year<2005)%>%distinct(GCDB_ID,.keep_all = T)%>%
  arrange(Date)
tmp%>%group_by(Date)%>%
  mutate(Count=sum(tmp$Date>unique(Date)-365/2 & tmp$Date<unique(Date)+365/2))%>%
  ggplot()+geom_point(aes(Date,Count))

tmp




tmp$dateDiff2<-c(0,as.numeric(tmp$Date[2:nrow(tmp)]-tmp$Date[1:(nrow(tmp)-1)]))

tmp%>%mutate(Gradient=zoo::rollmean(1/dateDiff2,k=30, fill=NA, align='right'))%>%
  filter(Gradient<1 & Year>1970)%>%
  ggplot()+geom_point(aes(Date,Gradient))








