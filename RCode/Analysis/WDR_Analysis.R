source("./RCode/Setup/GetPackages.R")

WDR<-openxlsx::read.xlsx("./Analysis_Results/Kirsten/WDR_country_data-2023_HP_full.xlsx",sheet = 9,startRow = 5)%>%
  dplyr::select(1:5)
skeleton<-WDR%>%filter(Year==2010)%>%dplyr::select(1:4)
WDR%<>%rbind(skeleton%>%mutate(Year=2023))

ISOS<-unique(WDR$ISO3)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF","CW","HW")

impies<-rbind(CleanEMDAT(openxlsx::read.xlsx("./Analysis_Results/Kirsten/emdat_public_2023_09_14_query_uid-xKzgpi.xlsx",startRow = 7)),
              GetGIDD())

# impies<-readRDS("./Analysis_Results/Kirsten/impies_20230910.RData")

impies%<>%filter(hazAb%in%lhaz)
# Create a variable to separate what is and isn't RC data 
# impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)
impies%<>%filter(!is.na(Year))

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

EMFull<-impies%>%filter(Year>=2010 & src_db=="EM-DAT")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haztype=="haztypehydromet"),
            Storm=sum(hazAb%in%c("ST","TC")),
            Flood=sum(hazAb=="FL"),
            LandslideH=sum(hazAb=="LS" & haztype=="haztypehydromet"),
            Drought=sum(hazAb=="DR"),
            Wildfire=sum(hazAb=="WF"),
            ExtrTemp=sum(hazAb%in%c("ET","CW","HW")),
            ALL_GEO=sum(haztype=="haztypegeohaz"),
            Earthquake=sum(hazAb=="EQ"),
            Volcano=sum(hazAb=="VC"),
            LandslideG=sum(hazAb=="LS" & haztype=="haztypegeohaz"),
            ALL=sum(haztype%in%c("haztypehydromet","haztypegeohaz")),
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
            ALL_GEO=sum(haztype=="haztypegeohaz"),
            Earthquake=sum(hazAb=="EQ"),
            Volcano=sum(hazAb=="VC"),
            LandslideG=sum(hazAb=="LS" & haztype=="haztypegeohaz"),
            ALL=sum(haztype%in%c("haztypehydromet","haztypegeohaz")),
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
            ALL_GEO=sum(impvalue[haztype=="haztypegeohaz"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeohaz"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeohaz")]),
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
            ALL_GEO=sum(impvalue[haztype=="haztypegeohaz"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeohaz"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeohaz")]),
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
            ALL_GEO=sum(impvalue[haztype=="haztypegeohaz"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeohaz"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeohaz")]),
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
            ALL_GEO=sum(impvalue[haztype=="haztypegeohaz"]),
            Earthquake=sum(impvalue[hazAb=="EQ"]),
            Volcano=sum(impvalue[hazAb=="VC"]),
            LandslideG=sum(impvalue[hazAb=="LS" & haztype=="haztypegeohaz"]),
            ALL=sum(impvalue[haztype%in%c("haztypehydromet","haztypegeohaz")]),
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

#%%%%%%%%%%%%%%%%%%%%%% REPORT FIGURES %%%%%%%%%%%%%%%%%%%%%%%%#

impies%>%filter(Year>=2010 & src_db=="EM-DAT")%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haztype=="haztypehydromet")/(sum(haztype=="haztypegeohaz")+sum(haztype=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))

impies%>%filter(Year>=2010 & src_db=="EM-DAT" & 
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                  AsMonth(ev_sdate)<10 & AsDay(ev_sdate)<11)%>%
  group_by(Year,haztype)%>%
  reframe(Count=sum(impvalue))%>%
  ggplot()+geom_point(aes(Year,Count,colour=haztype))

brks<-seq.int(2010,2023,by=2)
# brks[length(brks)]<-2023

impies%>%filter(Year>2010 & src_db=="EM-DAT" &
                impactdetails=="impdetallpeop" & imptype%in%c("imptypidp","imptypdeat"))%>%
  mutate(YearGroup = cut(Year,breaks = brks,
                               include.lowest = T,right=F))%>%
  filter(!is.na(YearGroup))%>%
  group_by(YearGroup)%>%
  reframe(Percentage=100*sum(haztype=="haztypehydromet")/(sum(haztype=="haztypegeohaz")+sum(haztype=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(YearGroup,Percentage))+

impies%>%filter(Year>2010 & src_db=="EM-DAT" &
                  impactdetails=="impdetallpeop" & imptype%in%c("imptypidp","imptypdeat"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haztype=="haztypehydromet")/(sum(haztype=="haztypegeohaz")+sum(haztype=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))


isoEQ<-unique(impies$ISO3[impies$hazAb=="EQ"])

impies%>%filter(Year>2010 & src_db=="EM-DAT" & ISO3%in%isoEQ &
                  impactdetails=="impdetallpeop" & imptype%in%c("imptypidp","imptypdeat"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haztype=="haztypehydromet")/(sum(haztype=="haztypegeohaz")+sum(haztype=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))


impies%>%filter(Year>=2000 & ISO3%in%isoEQ &
                  impactdetails=="impdetallpeop" & imptype%in%c("imptypidp","imptypdeat"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haztype=="haztypehydromet")/(sum(haztype=="haztypegeohaz")+sum(haztype=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))



impies%>%filter(Year>2010 & ISO3%in%isoEQ &
                  impactdetails=="impdetallpeop" & imptype=="imptypidp")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(impvalue[haztype=="haztypehydromet"])/(sum(impvalue[haztype=="haztypegeohaz"])+sum(impvalue[haztype=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(as.factor(Year),Percentage))

impies%>%filter(Year>=2000 & ISO3%in%isoEQ &
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(impvalue[haztype=="haztypehydromet"])/(sum(impvalue[haztype=="haztypegeohaz"])+sum(impvalue[haztype=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(as.factor(Year),Percentage))

impies%>%filter(Year>=2000 & ISO3%in%isoEQ &
                  impactdetails=="impdetallpeop" & imptype=="imptypaffe")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(impvalue[haztype=="haztypehydromet"])/(sum(impvalue[haztype=="haztypegeohaz"])+sum(impvalue[haztype=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(as.factor(Year),Percentage))

brks<-seq.int(2000,2023,by=5)

impies%>%filter(ISO3%in%isoEQ &
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat")%>%
  mutate(YearGroup = cut(Year,breaks = brks,
                               include.lowest = T,right=F))%>%
  filter(!is.na(YearGroup))%>%
  group_by(YearGroup)%>%
  reframe(Percentage=100*sum(impvalue[haztype=="haztypehydromet"])/(sum(impvalue[haztype=="haztypegeohaz"])+sum(impvalue[haztype=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(YearGroup,Percentage))






p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                  # impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                  src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(src_db,Year)%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  reframe(Percentage=sum(haztype=="haztypehydromet",na.rm = T)/(sum(haztype=="haztypegeohaz",na.rm = T)+sum(haztype=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=src_db))+geom_point(aes(Year,Percentage,colour=src_db))+
  geom_smooth(aes(Year,Percentage,colour=src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = FALSE)+
  ylab("Proportion")+ylim(c(0.75,0.97))+labs(colour="Database")+
  ggtitle("Proportion of Hydro-Met Hazard Events")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~src_db,scales = "fixed");p
ggsave("./Analysis_Results/Kirsten/Percentage_HM-G_Year.png",p)



p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                     impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                     src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(src_db,Year)%>%
  reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  # reframe(Percentage=sum(haztype=="haztypehydromet",na.rm = T)/(sum(haztype=="haztypegeohaz",na.rm = T)+sum(haztype=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=src_db))+geom_point(aes(Year,Percentage,colour=src_db))+
  geom_smooth(aes(Year,Percentage,colour=src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = FALSE)+
  ylab("Proportion")+ylim(c(0.75,0.97))+labs(colour="Database")+
  ggtitle("Proportion Hydro-Met to Geological Hazards")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~src_db,scales = "fixed");p



p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                     impactdetails=="impdetallpeop" & imptype%in%c("imptypaffe","imptypdiraffe") &
                     src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(src_db,Year)%>%
  reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  # reframe(Percentage=sum(haztype=="haztypehydromet",na.rm = T)/(sum(haztype=="haztypegeohaz",na.rm = T)+sum(haztype=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=src_db))+geom_point(aes(Year,Percentage,colour=src_db))+
  # geom_smooth(aes(Year,Percentage,colour=src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = T)+
  ylab("Proportion")+labs(colour="Database")+
  ggtitle("Proportion Hydro-Met to Geological Hazards")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~src_db,scales = "fixed");p


p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                     impactdetails=="impdetallpeop" & imptype%in%c("imptypaffe","imptypindaffe") &
                     src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(src_db,Year)%>%
  reframe(Percentage=sum(haztype=="haztypegeohaz",na.rm = T))%>%
  # reframe(Percentage=sum(haztype=="haztypehydromet",na.rm = T)/(sum(haztype=="haztypegeohaz",na.rm = T)+sum(haztype=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=src_db))+geom_point(aes(Year,Percentage,colour=src_db))+
  geom_smooth(aes(Year,Percentage,colour=src_db),alpha=0.1,method = "lm",se = FALSE)+
  ylab("Proportion")+labs(colour="Database")+
  ggtitle("Proportion Hydro-Met to Geological Hazards")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~src_db,scales = "fixed");p



tmp<-impies
tmp$hazAb[tmp$hazAb%in%c("CW","HW")]<-"ET"

tmp$hazAb[tmp$hazAb=="LS" & tmp$haztype=="haztypehydromet"]<-"LS-HM"
tmp$hazAb[tmp$hazAb=="LS" & tmp$haztype=="haztypegeohaz"]<-"LS-G"

p<-tmp%>%filter(Year>1990 & Year<2023 & !is.na(hazAb) & hazAb!="GL" & #ISO3%in%isoEQ & 
                     # impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                     src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(hazAb,src_db,Year)%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=length(haztype))%>%
  ggplot()+geom_point(aes(Year,Count,colour=src_db))+
  geom_smooth(aes(Year,Count,colour=src_db),alpha=0.1,se = FALSE)+
  ylab("Proportion")+labs(colour="Hazard")+scale_y_log10()+
  ggtitle("Proportion of Hydro-Met Hazard Events")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~hazAb,scales = "free_y");p


p<-tmp%>%filter(Year>1990 & Year<2023 & !is.na(hazAb) & hazAb!="GL" & #ISO3%in%isoEQ & 
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                  src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(hazAb,src_db,Year)%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=sum(impvalue,na.rm = T))%>%
  ggplot()+geom_point(aes(Year,Count,colour=src_db))+
  geom_smooth(aes(Year,Count,colour=src_db),alpha=0.1,se = FALSE)+
  ylab("Number of Deaths")+labs(colour="Database")+scale_y_log10()+
  ggtitle("Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~hazAb,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/Log-No-Fatalities_haz_db.png",p)


p<-tmp%>%filter(Year>1990 & Year<2023 & !is.na(hazAb) & hazAb!="GL" & #ISO3%in%isoEQ & 
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                  src_db=="EM-DAT")%>%
  # mutate(YearGroup = cut(Year,breaks = seq.int(1990,2020,10),
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(hazAb,Year)%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=sum(impvalue,na.rm = T))%>%
  ggplot()+geom_point(aes(Year,Count,colour=hazAb))+
  geom_smooth(aes(Year,Count,colour=hazAb),alpha=0.1,se = FALSE)+
  ylab("Number of Deaths")+labs(colour="Hazard")+
  ggtitle("EM-DAT - Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~hazAb,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/No-Fatalities_haz_db.png",p)


p<-tmp%>%filter(Year>1990 & Year<2023 & !is.na(hazAb) & hazAb!="GL" & #ISO3%in%isoEQ & 
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                  src_db!="GO-FR")%>%
  mutate(YearGroup = cut(Year,breaks = seq.int(1990,2020,3),
                         include.lowest = T,right=F))%>%
  filter(!is.na(YearGroup))%>%
  group_by(src_db,hazAb,YearGroup)%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=sum(impvalue,na.rm = T))%>%
  ggplot(group=src_db)+geom_point(aes(YearGroup,Count,colour=src_db))+
  # geom_line(aes(YearGroup,Count,colour=src_db))+
  # geom_smooth(aes(YearGroup,Count,colour=src_db),alpha=0.1,se = FALSE)+
  ylab("Number of Deaths")+labs(colour="Hazard")+scale_y_log10()+
  ggtitle("Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~hazAb,scales = "free_y");p

p<-tmp%>%filter(Year>1990 & Year<2023 & !is.na(hazAb) & hazAb!="GL" & #ISO3%in%isoEQ & 
                  src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = seq.int(1990,2020,3),
  #                        include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(src_db,hazAb,Year)%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=length(impvalue))%>%
  ggplot(group=src_db)+geom_point(aes(Year,Count,colour=src_db))+
  # geom_line(aes(Year,Count,colour=src_db))+
  geom_smooth(aes(Year,Count,colour=src_db),alpha=0.1,se = FALSE)+
  ylab("Number of Events")+labs(colour="Hazard")+scale_y_log10()+
  ggtitle("Recorded Events per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~hazAb,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/Log_No-Events_haz_db.png",p)





p<-tmp%>%filter(Year>1990 & Year<2023 & !is.na(hazAb) & hazAb!="GL" & #ISO3%in%isoEQ & 
                  impactdetails=="impdetallpeop" & imptype=="imptypdeat" &
                  src_db!="GO-FR")%>%
  # reframe(Percentage=sum(impvalue[haztype=="haztypehydromet"],na.rm = T)/(sum(impvalue[haztype=="haztypegeohaz"],na.rm = T)+sum(impvalue[haztype=="haztypehydromet"],na.rm = T)))%>%
  ggplot()+geom_point(aes(as.Date(ev_sdate),impvalue,colour=src_db),alpha=0.2)+
  geom_smooth(aes(as.Date(ev_sdate),impvalue,colour=src_db),alpha=0.1,se = FALSE,size=2)+
  ylab("Number of Deaths")+labs(colour="Database")+scale_y_log10()+
  ggtitle("Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~hazAb,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/Log-ObsFatalities_haz_db.png",p)

















































