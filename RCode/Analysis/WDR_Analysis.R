source("./RCode/Setup/GetPackages.R")

WDR<-openxlsx::read.xlsx("~/Downloads/WDR_country_data-2022update.xlsx",sheet = 9,startRow = 5)%>%
  dplyr::select(1:5)
skeleton<-WDR%>%filter(Year==2010)%>%dplyr::select(1:4)
WDR%<>%rbind(skeleton%>%mutate(Year=2023))

ISOS<-unique(WDR$ISO3)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF","CW","HW")

impies<-readRDS("./CleanedData/MostlyImpactData/AllHaz_impies.RData")
# impies<-rbind(GetEMDAT(),GetGIDD())
impies%<>%filter(hazAb%in%lhaz)
# Create a variable to separate what is and isn't RC data 
# impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)

taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")

impies%<>%filter(Year>=2010)

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

EMFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haztype=="haztypehydromet"),
            Storm=sum(hazAb%in%c("ST","TC")),
            Flood=sum(hazAb=="FL"),
            LandslideH=sum(hazAb=="LS" & haztype=="haztypehydromet"),
            Drought=sum(hazAb=="DR"),
            Wildfire=sum(hazAb=="WF"),
            ExtrTemp=sum(hazAb%in%c("ET","CW","HW")),
            ALL_GEO=sum(haztype=="haztypegeo"),
            Earthquake=sum(hazAb=="EQ"),
            Volcano=sum(hazAb=="VC"),
            LandslideG=sum(hazAb=="LS" & haztype=="haztypegeohaz"),
            ALL=sum(haztype%in%c("haztypehydromet","haztypegeo")),
            .groups="drop")
EMFull<-WDR%>%dplyr::select(1:5)%>%left_join(EMFull)

EMFull[is.na(EMFull)]<-0

EMFull$ALL_CLIM<-rowSums(EMFull[,climvars])
EMFull$ALL_GEO<-rowSums(EMFull[,geovars])
EMFull$ALL<-EMFull$ALL_CLIM+EMFull$ALL_GEO

HEFull<-impies%>%filter(Year>=2018 & src_db=="HELIX")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haztype=="haztypehydromet"),
            Storm=sum(hazAb%in%c("ST","TC")),
            Flood=sum(hazAb=="FL"),
            LandslideH=sum(hazAb=="LS" & haztype=="haztypehydromet"),
            Drought=sum(hazAb=="DR"),
            Wildfire=sum(hazAb=="WF"),
            ExtrTemp=sum(hazAb%in%c("ET","CW","HW")),
            ALL_GEO=sum(haztype=="haztypegeo"),
            Earthquake=sum(hazAb=="EQ"),
            Volcano=sum(hazAb=="VC"),
            LandslideG=sum(hazAb=="LS" & haztype=="haztypegeo"),
            ALL=sum(haztype%in%c("haztypehydromet","haztypegeo")),
            .groups="drop")
HEFull<-WDR%>%dplyr::select(1:5)%>%left_join(HEFull)

HEFull[is.na(HEFull)]<-0

HEFull$ALL_CLIM<-rowSums(HEFull[,climvars])
HEFull$ALL_GEO<-rowSums(HEFull[,geovars])
HEFull$ALL<-HEFull$ALL_CLIM+HEFull$ALL_GEO

IncFull<-EMFull
# Only do the averaging for when IDMC started recording lots of events
inds<-IncFull$Year>=2018 & IncFull$Year<2023
  
IncFull[inds,6:ncol(IncFull)]<-do.call(cbind,lapply(6:ncol(IncFull),function(i) {
  ceiling(apply(cbind(EMFull[inds,i],HEFull[inds,i]),1,max,na.rm = T))
}))

RegIncFull<-IncFull%>%group_by(Year,REGION_IFRC)%>%
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

FatFull<-impies%>%filter(Year>=2010 & src_db%in%c("EM-DAT","HELIX") &
                         impactdetails=="impdetallpeop" & imptype=="imptypdeat")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            LandslideH=sum(impvalue[hazAb=="LS" & haztype=="haztypehydromet"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeo"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
FatFull<-WDR%>%dplyr::select(1:5)%>%left_join(FatFull)

FatFull[is.na(FatFull)]<-0

FatFull$ALL_CLIM<-rowSums(FatFull[,climvars])
FatFull$ALL_GEO<-rowSums(FatFull[,geovars])
FatFull$ALL<-FatFull$ALL_CLIM+FatFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IDPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

IDPFull<-impies%>%filter(Year>=2018 & src_db=="HELIX" &
                           impactdetails=="impdetallpeop" & imptype=="imptypidp")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            LandslideH=sum(impvalue[hazAb=="LS" & haztype=="haztypehydromet"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeo"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
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

AFFFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT" &
                           impactdetails=="impdetallpeop" & imptype=="imptypaffe")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            LandslideH=sum(impvalue[hazAb=="LS" & haztype=="haztypehydromet"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeo"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
AFFFull<-WDR%>%dplyr::select(1:5)%>%left_join(AFFFull)

AFFFull[is.na(AFFFull)]<-0

AFFFull$ALL_CLIM<-rowSums(AFFFull[,climvars])
AFFFull$ALL_GEO<-rowSums(AFFFull[,geovars])
AFFFull$ALL<-AFFFull$ALL_CLIM+AFFFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

COSFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT" &
                           impactdetails=="impdetinfloccur" & imptype=="imptypcost")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            LandslideH=sum(impvalue[hazAb=="LS" & haztype=="haztypehydromet"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeo"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
COSFull<-WDR%>%dplyr::select(1:5)%>%left_join(COSFull)

COSFull[is.na(COSFull)]<-0

COSFull$ALL_CLIM<-rowSums(COSFull[,climvars])
COSFull$ALL_GEO<-rowSums(COSFull[,geovars])
COSFull$ALL<-COSFull$ALL_CLIM+COSFull$ALL_GEO

#%%%%%%%%%%%%%%%%%%%%%%%%% WRITE-OUT %%%%%%%%%%%%%%%%%%%%%%%%%%#

openxlsx::write.xlsx(IncFull,"./Analysis_Results/Kirsten/Indicence.xlsx")
openxlsx::write.xlsx(RegIncFull,"./Analysis_Results/Kirsten/RegionalIndicence.xlsx")
openxlsx::write.xlsx(FatFull,"./Analysis_Results/Kirsten/Fatalities.xlsx")
openxlsx::write.xlsx(IDPFull,"./Analysis_Results/Kirsten/IDPs.xlsx")
openxlsx::write.xlsx(AFFFull,"./Analysis_Results/Kirsten/Affected.xlsx")
openxlsx::write.xlsx(COSFull,"./Analysis_Results/Kirsten/Cost.xlsx")

