source("./RCode/Setup/GetPackages.R")

WDR<-openxlsx::read.xlsx("./Analysis_Results/Kirsten/WDR_country_data-2023_HP_full.xlsx",sheet = 9,startRow = 5)%>%
  dplyr::select(1:5)
# skeleton<-WDR%>%filter(Year==2010)%>%dplyr::select(1:4)
# WDR%<>%rbind(skeleton%>%mutate(Year=2023))

ISOS<-unique(WDR$ISO3)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF","CW","HW")

impies<-rbind(CleanEMDAT(openxlsx::read.xlsx("./Analysis_Results/Kirsten/emdat_public_2023_09_14_query_uid-xKzgpi.xlsx",startRow = 7)),
              GetGIDD())
# impies<-readRDS("./Analysis_Results/Kirsten/impies_20230910.RData")

impies$haz_Ab[impies$haz_Ab%in%c("CW","HW")]<-"ET"

impies$haz_Ab[impies$haz_Ab=="LS" & impies$haz_type=="haztypehydromet"]<-"LS-HM"
impies$haz_Ab[impies$haz_Ab=="LS" & impies$haz_type=="haztypegeohaz"]<-"LS-G"
# Create a variable to separate what is and isn't RC data 
# impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",imp_src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)
# impies%<>%filter(!is.na(Year))

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
EMFull<-impies%>%filter(Year>=2010 & imp_src_db=="EM-DAT" & !duplicated(imp_sub_ID))%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haz_type=="haztypehydromet"),
            Storm=sum(haz_Ab%in%c("ST","TC")),
            Flood=sum(haz_Ab=="FL"),
            LandslideH=sum(haz_Ab=="LS" & haz_type=="haztypehydromet"),
            Drought=sum(haz_Ab=="DR"),
            Wildfire=sum(haz_Ab=="WF"),
            ExtrTemp=sum(haz_Ab%in%c("ET","CW","HW")),
            ALL_GEO=sum(haz_type=="haztypegeohaz"),
            Earthquake=sum(haz_Ab=="EQ"),
            Volcano=sum(haz_Ab=="VC"),
            LandslideG=sum(haz_Ab=="LS" & haz_type=="haztypegeohaz"),
            ALL=sum(haz_type%in%c("haztypehydromet","haztypegeohaz")),
            .groups="drop")
EMFull<-WDR%>%dplyr::select(1:5)%>%
  left_join(EMFull,by=join_by(ISO3,Year),relationship="one-to-one")

EMFull[is.na(EMFull)]<-0

EMFull$ALL_CLIM<-rowSums(EMFull[,climvars])
EMFull$ALL_GEO<-rowSums(EMFull[,geovars])
EMFull$ALL<-EMFull$ALL_CLIM+EMFull$ALL_GEO

HEFull<-impies%>%filter(Year>=2018 & imp_src_db=="GIDD" & !duplicated(imp_sub_ID))%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(haz_type=="haztypehydromet"),
            Storm=sum(haz_Ab%in%c("ST","TC")),
            Flood=sum(haz_Ab=="FL"),
            LandslideH=sum(haz_Ab=="LS" & haz_type=="haztypehydromet"),
            Drought=sum(haz_Ab=="DR"),
            Wildfire=sum(haz_Ab=="WF"),
            ExtrTemp=sum(haz_Ab%in%c("ET","CW","HW")),
            ALL_GEO=sum(haz_type=="haztypegeohaz"),
            Earthquake=sum(haz_Ab=="EQ"),
            Volcano=sum(haz_Ab=="VC"),
            LandslideG=sum(haz_Ab=="LS" & haz_type=="haztypegeohaz"),
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

FatFull<-impies%>%filter(Year>=2010 & imp_src_db%in%c("EM-DAT","GIDD") &
                         imp_det=="impdetallpeop" & imp_type=="imptypdeat")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VC"]),
            LandslideG=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypegeohaz"]),
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

IDPFull<-impies%>%filter(Year>=2018 & imp_src_db=="GIDD" &
                           imp_det=="impdetallpeop" & imp_type=="imptypidp")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VC"]),
            LandslideG=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypegeohaz"]),
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

AFFFull<-impies%>%filter(Year>=2010 & imp_src_db=="EM-DAT" &
                           imp_det=="impdetallpeop" & imp_type=="imptypaffe")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VC"]),
            LandslideG=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypegeohaz"]),
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

COSFull<-impies%>%filter(Year>=2010 & imp_src_db=="EM-DAT" &
                           imp_det=="impdetinfloccur" & imp_type=="imptypcost")%>%
  group_by(ISO3,Year)%>%
  summarise(ALL_CLIM=sum(imp_value[haz_type=="haztypehydromet"]),
            Storm=sum(imp_value[haz_Ab%in%c("ST","TC")]),
            Flood=sum(imp_value[haz_Ab=="FL"]),
            LandslideH=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypehydromet"]),
            Drought=sum(imp_value[haz_Ab=="DR"]),
            Wildfire=sum(imp_value[haz_Ab=="WF"]),
            ExtrTemp=sum(imp_value[haz_Ab%in%c("ET","CW","HW")]),
            ALL_GEO=sum(imp_value[haz_type=="haztypegeohaz"]),
            Earthquake=sum(imp_value[haz_Ab=="EQ"]),
            Volcano=sum(imp_value[haz_Ab=="VC"]),
            LandslideG=sum(imp_value[haz_Ab=="LS" & haz_type=="haztypegeohaz"]),
            ALL=sum(imp_value[haz_type%in%c("haztypehydromet","haztypegeohaz")]),
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

impies%>%filter(Year>=2010 & imp_src_db=="EM-DAT")%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haz_type=="haztypehydromet")/(sum(haz_type=="haztypegeohaz")+sum(haz_type=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))

impies%>%filter(Year>=2010 & imp_src_db=="EM-DAT" & 
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  AsMonth(ev_sdate)<10 & AsDay(ev_sdate)<11)%>%
  group_by(Year,haz_type)%>%
  reframe(Count=sum(imp_value))%>%
  ggplot()+geom_point(aes(Year,Count,colour=haz_type))

brks<-seq.int(2010,2023,by=2)
# brks[length(brks)]<-2023

impies%>%filter(Year>2010 & imp_src_db=="EM-DAT" &
                imp_det=="impdetallpeop" & imp_type%in%c("imptypidp","imptypdeat"))%>%
  mutate(YearGroup = cut(Year,breaks = brks,
                               include.lowest = T,right=F))%>%
  filter(!is.na(YearGroup))%>%
  group_by(YearGroup)%>%
  reframe(Percentage=100*sum(haz_type=="haztypehydromet")/(sum(haz_type=="haztypegeohaz")+sum(haz_type=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(YearGroup,Percentage))+

impies%>%filter(Year>2010 & imp_src_db=="EM-DAT" &
                  imp_det=="impdetallpeop" & imp_type%in%c("imptypidp","imptypdeat"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haz_type=="haztypehydromet")/(sum(haz_type=="haztypegeohaz")+sum(haz_type=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))

impies%>%filter(Year>2010 & imp_src_db=="EM-DAT" & ISO3%in%isoEQ &
                  imp_det=="impdetallpeop" & imp_type%in%c("imptypidp","imptypdeat"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haz_type=="haztypehydromet")/(sum(haz_type=="haztypegeohaz")+sum(haz_type=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))


impies%>%filter(Year>=2000 & ISO3%in%isoEQ &
                  imp_det=="impdetallpeop" & imp_type%in%c("imptypidp","imptypdeat"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(haz_type=="haztypehydromet")/(sum(haz_type=="haztypegeohaz")+sum(haz_type=="haztypehydromet")))%>%
  ggplot()+geom_point(aes(Year,Percentage))



impies%>%filter(Year>2010 & ISO3%in%isoEQ &
                  imp_det=="impdetallpeop" & imp_type=="imptypidp")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(imp_value[haz_type=="haztypehydromet"])/(sum(imp_value[haz_type=="haztypegeohaz"])+sum(imp_value[haz_type=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(as.factor(Year),Percentage))

impies%>%filter(Year>=2000 & ISO3%in%isoEQ &
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(imp_value[haz_type=="haztypehydromet"])/(sum(imp_value[haz_type=="haztypegeohaz"])+sum(imp_value[haz_type=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(as.factor(Year),Percentage))

impies%>%filter(Year>=2000 & ISO3%in%isoEQ &
                  imp_det=="impdetallpeop" & imp_type=="imptypaffe")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(Year)%>%
  reframe(Percentage=100*sum(imp_value[haz_type=="haztypehydromet"])/(sum(imp_value[haz_type=="haztypegeohaz"])+sum(imp_value[haz_type=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(as.factor(Year),Percentage))

brks<-seq.int(2000,2023,by=5)

impies%>%filter(ISO3%in%isoEQ &
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat")%>%
  mutate(YearGroup = cut(Year,breaks = brks,
                               include.lowest = T,right=F))%>%
  filter(!is.na(YearGroup))%>%
  group_by(YearGroup)%>%
  reframe(Percentage=100*sum(imp_value[haz_type=="haztypehydromet"])/(sum(imp_value[haz_type=="haztypegeohaz"])+sum(imp_value[haz_type=="haztypehydromet"])))%>%
  ggplot()+geom_point(aes(YearGroup,Percentage))



p<-impies%>%filter(Year>1990 & Year<2023 & ISO3%in%isoEQ & 
                     !(imp_src_db=="GIDD" & Year<2015) &
                  # imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  !imp_src_db%in%c("GO-FR","GO-App"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/(sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=imp_src_db))+geom_point(aes(Year,Percentage,colour=imp_src_db),alpha=0.5)+
  geom_smooth(aes(Year,Percentage,colour=imp_src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = FALSE)+
  ylab("Proportion")+ylim(c(0.75,0.97))+labs(colour="Database")+
  ggtitle("Proportion of Climate & Weather Events")+theme(plot.title = element_text(hjust = 0.5));p
  # facet_wrap(~imp_src_db,scales = "fixed");p
ggsave("./Analysis_Results/Kirsten/Percentage_HM-G_Year.png",p,width=8,height = 5)

p<-impies%>%filter(Year>1990 & Year<2023 & ISO3%in%isoEQ & 
                     # imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                     !imp_src_db%in%c("GIDD","GO-FR","GO-App"))%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/(sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=imp_src_db))+geom_point(aes(Year,Percentage,colour=imp_src_db),alpha=0.5)+
  geom_smooth(aes(Year,Percentage,colour=imp_src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = FALSE)+
  ylab("Proportion")+ylim(c(0.75,0.97))+labs(colour="Database")+
  ggtitle("Proportion of Climate & Weather Events")+theme(plot.title = element_text(hjust = 0.5));p
# facet_wrap(~imp_src_db,scales = "fixed");p
ggsave("./Analysis_Results/Kirsten/Percentage_HM-G_Year_noGIDD.png",p,width=8,height = 5)




p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                     imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                     imp_src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,Year)%>%
  reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  # reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/(sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=imp_src_db))+geom_point(aes(Year,Percentage,colour=imp_src_db))+
  geom_smooth(aes(Year,Percentage,colour=imp_src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = FALSE)+
  ylab("Proportion")+ylim(c(0.75,0.97))+labs(colour="Database")+
  ggtitle("Proportion Hydro-Met to Geological Hazards")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~imp_src_db,scales = "fixed");p



p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                     imp_det=="impdetallpeop" & imp_type%in%c("imptypaffe","imptypdiraffe") &
                     imp_src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,Year)%>%
  reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  # reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/(sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=imp_src_db))+geom_point(aes(Year,Percentage,colour=imp_src_db))+
  # geom_smooth(aes(Year,Percentage,colour=imp_src_db),alpha=0.1,method = "glm", method.args = list(family = "binomial"),se = T)+
  ylab("Proportion")+labs(colour="Database")+
  ggtitle("Proportion Hydro-Met to Geological Hazards")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~imp_src_db,scales = "fixed");p


p<-impies%>%filter(Year>1990 & Year<2023 & #ISO3%in%isoEQ & 
                     imp_det=="impdetallpeop" & imp_type%in%c("imptypaffe","imptypindaffe") &
                     imp_src_db!="GO-FR")%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,Year)%>%
  reframe(Percentage=sum(haz_type=="haztypegeohaz",na.rm = T))%>%
  # reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/(sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)))%>%
  ggplot(aes(group=imp_src_db))+geom_point(aes(Year,Percentage,colour=imp_src_db))+
  geom_smooth(aes(Year,Percentage,colour=imp_src_db),alpha=0.1,method = "lm",se = FALSE)+
  ylab("Proportion")+labs(colour="Database")+
  ggtitle("Proportion Hydro-Met to Geological Hazards")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~imp_src_db,scales = "fixed");p

p<-impies%>%filter(Year>1990 & Year<2023 & !is.na(haz_Ab) & haz_Ab!="GL" & #ISO3%in%isoEQ & 
                     # imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                     imp_src_db!="GO-FR" & haz_Ab%in%lhaz)%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(haz_Ab,imp_src_db,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=length(haz_type))%>%
  ggplot()+geom_point(aes(Year,Count,colour=imp_src_db))+
  geom_smooth(aes(Year,Count,colour=imp_src_db),alpha=0.1,se = FALSE)+
  ylab("No. Events")+labs(colour="Hazard")+scale_y_log10()+
  ggtitle("Number Hazard Events")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~haz_Ab,scales = "free_y");p


p<-impies%>%filter(Year>1990 & Year<2023 & !is.na(haz_Ab) & haz_Ab!="GL" & #ISO3%in%isoEQ & 
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  imp_src_db!="GO-FR" & haz_Ab%in%lhaz)%>%
  # mutate(YearGroup = cut(Year,breaks = brks,
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(haz_Ab,imp_src_db,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=sum(imp_value,na.rm = T))%>%
  ggplot()+geom_point(aes(Year,Count,colour=imp_src_db))+
  geom_smooth(aes(Year,Count,colour=imp_src_db),alpha=0.1,se = FALSE)+
  ylab("Number of Deaths")+labs(colour="Database")+scale_y_log10()+
  ggtitle("Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~haz_Ab,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/Log-No-Fatalities_haz_db.png",p)


p<-impies%>%filter(Year>1990 & Year<2023 & !is.na(haz_Ab) & haz_Ab!="GL" & #ISO3%in%isoEQ & 
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  imp_src_db=="EM-DAT")%>%
  # mutate(YearGroup = cut(Year,breaks = seq.int(1990,2020,10),
  #                              include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(haz_Ab,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=sum(imp_value,na.rm = T))%>%
  ggplot()+geom_point(aes(Year,Count,colour=haz_Ab))+
  geom_smooth(aes(Year,Count,colour=haz_Ab),alpha=0.1,se = FALSE)+
  ylab("Number of Deaths")+labs(colour="Hazard")+
  ggtitle("EM-DAT - Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~haz_Ab,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/No-Fatalities_haz_db.png",p)


p<-impies%>%filter(Year>1990 & Year<2023 & !is.na(haz_Ab) & haz_Ab!="GL" & #ISO3%in%isoEQ & 
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  imp_src_db!="GO-FR")%>%
  mutate(YearGroup = cut(Year,breaks = seq.int(1990,2020,3),
                         include.lowest = T,right=F))%>%
  filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,haz_Ab,YearGroup)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=sum(imp_value,na.rm = T))%>%
  ggplot(group=imp_src_db)+geom_point(aes(YearGroup,Count,colour=imp_src_db))+
  # geom_line(aes(YearGroup,Count,colour=imp_src_db))+
  # geom_smooth(aes(YearGroup,Count,colour=imp_src_db),alpha=0.1,se = FALSE)+
  ylab("Number of Deaths")+labs(colour="Hazard")+scale_y_log10()+
  ggtitle("Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~haz_Ab,scales = "free_y");p

p<-impies%>%filter(Year>1990 & Year<2023 & !is.na(haz_Ab) & haz_Ab!="GL" & #ISO3%in%isoEQ & 
                  imp_src_db!="GO-FR" & haz_Ab%in%lhaz)%>%
  # mutate(YearGroup = cut(Year,breaks = seq.int(1990,2020,3),
  #                        include.lowest = T,right=F))%>%
  # filter(!is.na(YearGroup))%>%
  group_by(imp_src_db,haz_Ab,Year)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  reframe(Count=length(imp_value))%>%
  ggplot(group=imp_src_db)+geom_point(aes(Year,Count,colour=imp_src_db))+
  # geom_line(aes(Year,Count,colour=imp_src_db))+
  geom_smooth(aes(Year,Count,colour=imp_src_db),alpha=0.1,se = FALSE)+
  ylab("Number of Events")+labs(colour="Hazard")+scale_y_log10()+
  ggtitle("Recorded Events per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~haz_Ab,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/Log_No-Events_haz_db.png",p)





p<-impies%>%filter(Year>1990 & Year<2023 & !is.na(haz_Ab) & haz_Ab!="GL" & #ISO3%in%isoEQ & 
                  imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  imp_src_db!="GO-FR" & haz_Ab%in%lhaz)%>%
  # reframe(Percentage=sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)/(sum(imp_value[haz_type=="haztypegeohaz"],na.rm = T)+sum(imp_value[haz_type=="haztypehydromet"],na.rm = T)))%>%
  ggplot()+geom_point(aes(as.Date(ev_sdate),imp_value,colour=imp_src_db),alpha=0.2)+
  geom_smooth(aes(as.Date(ev_sdate),imp_value,colour=imp_src_db),alpha=0.1,se = FALSE,size=2)+
  ylab("Number of Deaths")+labs(colour="Database")+scale_y_log10()+
  ggtitle("Fatalities per Hazard")+theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~haz_Ab,scales = "free_y");p
ggsave("./Analysis_Results/Kirsten/Log-ObsFatalities_haz_db.png",p)






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% STATISTICAL SIGNIFICANCE %%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%% FOR CLIMATE CHANGE %%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

propy<-impies%>%filter(Year>1990 & Year<2023 & ISO3%in%isoEQ &
                         !(imp_src_db=="GIDD" & Year<2015) &
                         !duplicated(imp_sub_ID) &
                         !imp_src_db%in%c("GO-FR","GO-App","GDACS"))%>%
  group_by(imp_src_db,Year)%>%
  reframe(Percentage=sum(haz_type=="haztypehydromet",na.rm = T)/
            (sum(haz_type=="haztypegeohaz",na.rm = T)+sum(haz_type=="haztypehydromet",na.rm = T)),
          Counts=length(haz_type))%>%ungroup()%>%group_by(imp_src_db)%>%
  mutate(Weights=Counts/max(Counts))

propy$LogWeights<-log10(propy$Counts)/log10(max(propy$Counts))

# Unweighted
fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy)%>%summary()
# Weighted by log-count values
fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy, weights = propy$LogWeights)%>%summary()
# Weighted by database-normalised count values (not log)
fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy, weights = propy$Weights)%>%summary()

# Check the plot to see the count numbers
propy%>%ggplot()+geom_point(aes(Year,Counts,colour=imp_src_db,size=Weights)) + scale_y_log10()
# Also plot the proportions
propy%>%ggplot()+geom_point(aes(Year,Percentage,colour=imp_src_db,size=Weights))

# Now let's predict some values with the analysis
modelly<-fixest::feglm(Percentage ~ Year | imp_src_db,family = "binomial",data = propy, weights = propy$Weights)
# Prediction for 2022
predict(modelly,data.frame(imp_src_db="GIDD",Year=2022))
# Prediction for 1990
predict(modelly,data.frame(imp_src_db="GIDD",Year=1990))


































