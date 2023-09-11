source("./RCode/Setup/GetPackages.R")

WDR<-openxlsx::read.xlsx("~/Downloads/WDR_country_data-2022update.xlsx",sheet = 9,startRow = 5)

ISOS<-unique(WDR$ISO3)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF")

impies<-readRDS("./CleanedData/MostlyImpactData/AllHaz_impies.RData")
impies%<>%filter(hazAb%in%lhaz)
# Create a variable to separate what is and isn't RC data 
impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)

taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")

impies%<>%filter(Year>=2010)

sum(unique(impies$ISO3)%in%WDR$ISO3)
sum(WDR$ISO3%in%unique(impies$ISO3))

convIso3Country(unique(WDR$ISO3[!WDR$ISO3%in%impies$ISO3]))

# Extract EM-DAT deaths and IDMC displacements
# Make an average of the number of events per country to produce the incidence, per year
#   but this only uses IDMC from 2018 onwards
# Also produce the EM-DAT inf-corrected costs and affected people, but in a different xlsx file
# 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% INCIDENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

EMFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haztype=="haztypehydromet"),
            Storm=sum(hazAb%in%c("ST","TC")),
            Flood=sum(hazAb=="FL"),
            Drought=sum(hazAb=="DR"),
            Wildfire=sum(hazAb=="WF"),
            ExtrTemp=sum(hazAb%in%c("ET","CW","HW")),
            ALL_GEO=sum(haztype=="haztypegeo"),
            Earthquake=sum(hazAb=="EQ"),
            Volcano=sum(hazAb=="VC"),
            Landslide=sum(hazAb=="LS"),
            ALL=sum(haztype%in%c("haztypehydromet","haztypegeo")),
            .groups="drop")
EMFull<-WDR%>%dplyr::select(1:5)%>%left_join(EMFull)

EMFull[is.na(EMFull)]<-0

HEFull<-impies%>%filter(Year>=2018 & src_db=="HELIX")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haztype=="haztypehydromet"),
            Storm=sum(hazAb%in%c("ST","TC")),
            Flood=sum(hazAb=="FL"),
            Drought=sum(hazAb=="DR"),
            Wildfire=sum(hazAb=="WF"),
            ExtrTemp=sum(hazAb%in%c("ET","CW","HW")),
            ALL_GEO=sum(haztype=="haztypegeo"),
            Earthquake=sum(hazAb=="EQ"),
            Volcano=sum(hazAb=="VC"),
            Landslide=sum(hazAb=="LS"),
            ALL=sum(haztype%in%c("haztypehydromet","haztypegeo")),
            .groups="drop")
HEFull<-WDR%>%dplyr::select(1:5)%>%left_join(HEFull)

HEFull[is.na(HEFull)]<-0

IncFull<-EMFull
# Only do the averaging for when IDMC started recording lots of events
inds<-IncFull$Year>2018
  
IncFull[inds,6:ncol(IncFull)]<-do.call(cbind,lapply(6:ncol(IncFull),function(i) {
  ceiling(rowMeans(cbind(EMFull[inds,i],HEFull[inds,i]),na.rm = T))
}))

View(IncFull)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% FATALITIES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

FatFull<-impies%>%filter(Year>=2010 & src_db%in%c("EM-DAT","HELIX") &
                         impactdetails=="impdetallpeop" & imptype=="imptypdeat")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            Landslide=sum(impvalue[hazAb=="LS"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
FatFull<-WDR%>%dplyr::select(1:5)%>%left_join(FatFull)
View(FatFull)

FatFull[is.na(FatFull)]<-0

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IDPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

IDPFull<-impies%>%filter(Year>=2018 & src_db=="HELIX" &
                           impactdetails=="impdetallpeop" & imptype=="imptypidp")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            Landslide=sum(impvalue[hazAb=="LS"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
IDPFull<-WDR%>%dplyr::select(1:5)%>%left_join(IDPFull)

IDPFull[IDPFull$Year<2018,6:ncol(IDPFull)]<- -999

IDPFull[is.na(IDPFull)]<-0
IDPFull[IDPFull== -999]<-NA
View(IDPFull)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%% AFFECTED %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

AFFFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT" &
                           impactdetails=="impdetallpeop" & imptype=="imptypaffe")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            Landslide=sum(impvalue[hazAb=="LS"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
AFFFull<-WDR%>%dplyr::select(1:5)%>%left_join(AFFFull)

AFFFull[is.na(AFFFull)]<-0

View(AFFFull)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

COSFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT" &
                           impactdetails=="impdetinfloccur" & imptype=="imptypcost")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(impvalue[haztype=="haztypehydromet"]),
            Storm=sum(impvalue[hazAb%in%c("ST","TC")]),
            Flood=sum(impvalue[hazAb=="FL"]),
            Drought=sum(impvalue[hazAb=="DR"]),
            Wildfire=sum(impvalue[hazAb=="WF"]),
            ExtrTemp=sum(impvalue[hazAb%in%c("ET","CW","HW")]),
            ALL_GEO=sum(impvalue[haztype=="haztypegeo"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            Landslide=sum(impvalue[hazAb=="LS"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeo")]),
            .groups="drop")
COSFull<-WDR%>%dplyr::select(1:5)%>%left_join(COSFull)

COSFull[is.na(COSFull)]<-0

View(COSFull)

