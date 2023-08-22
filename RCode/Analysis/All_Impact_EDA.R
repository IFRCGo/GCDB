source("./RCode/Analysis/SpatialAnalysis.R")
options(scipen = 999)

# impies<-GatherAllImps(lhaz)
# saveRDS(impies,"./CleanedData/MostlyImpactData/AllHaz_impies.RData")
impies<-readRDS("./CleanedData/MostlyImpactData/AllHaz_impies.RData")
impies%<>%filter(hazAb%in%lhaz)
# Create a variable to separate what is and isn't RC data 
impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)
# Purely EM-DAT data for non-multiple entry visualisations
EMDAT<-impies%>%filter(src_db=="EM-DAT" & ev_sdate>as.Date("2000-01-01"))
# Taxonomies
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Admin boundaries
ADM<-ImpactAggADM0(impies)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF")

pal <- c(
  "EQ" = "magenta",
  "FL" = "blue",
  "TC" = "darkviolet",
  "VO" = "grey23",
  "DR" = "chocolate4",
  "ET" = "darkorange",
  "LS" = "peachpuff4",
  "ST" = "darkturquoise",
  "WF" = "red"
)

p<-impies%>%group_by(hazAb)%>%reframe(Count=length(unique(GCDB_ID)))%>%arrange(desc(Count))%>%
  ggplot()+geom_bar(aes(x=hazAb,y=Count,fill=hazAb),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Number of Impact Records")+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_bar.png",p,path="./Plots/",width = 10,height = 8)  

p<-impies%>%group_by(src_db,spat_res)%>%reframe(Count=length(unique(GCDB_ID)))%>%
  ggplot()+geom_bar(aes(x=src_db,y=Count,fill=spat_res),colour="black",stat="identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+
  labs(fill="Spatial Resolution");p
ggsave("AllHazards_src_db_ADM_bar.png",p,path="./Plots/",width = 9,height = 8)  


p<-impies%>%filter(impactdetails=="impdetallpeop" & imptype=="imptypdeat")%>%
  ggplot()+geom_bar(aes(x=hazAb,fill=spat_res),colour="black")+
  xlab("Impact Database")+ylab("Number of Impact Records")+
  labs(fill="Spatial Resolution");p
ggsave("AllHazards_src_db_bar.png",p,path="./Plots/",width = 9,height = 8)  

p<-impies%>%filter(impactdetails=="impdetallpeop" & imptype=="imptypdeat" & 
                     src_db!="GO-FR" &  ev_sdate>as.Date("2000-01-01"))%>%
  group_by(hazAb)%>%reframe(Deaths=sum(impvalue))%>%arrange(desc(Deaths))%>%
  ggplot()+geom_bar(aes(x=hazAb,y=Deaths,fill=hazAb),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Total Deaths")+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_deaths_bar.png",p,path="./Plots/",width = 10,height = 8)  


minitax<-taxies%>%filter(list_name=="impactcats")%>%
  transmute(impactcats=name,label=label)

p<-left_join(impies,minitax,by="impactcats")%>%
  filter(!is.na(label))%>%
  ggplot()+geom_bar(aes(label,fill=label),colour="black")+
  xlab("Impact Category")+labs(fill="Impact Category")+
  ylab("Number of Impact Recordings")+
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.position="none");p
ggsave("AllHazards_impcats_bar.png",p,path="./Plots/")  


minitax1<-taxies%>%filter(list_name=="impactsubcats")%>%
  transmute(impactsubcats=name,label=label)
minitax2<-taxies%>%filter(list_name=="impacttypes")%>%
  transmute(imptype=name,label=label)
tmp<-left_join(left_join(impies,minitax1,by="impactsubcats"),minitax2,by="imptype")
tmp$label.y[tmp$label.y=="Internally Displaced Persons (IDPs)"]<-"Internally Displaced (IDPs)"
tmp%<>%mutate(label=paste0(str_split(str_split(label.x," \\(",simplify = T)[,1]," – ",simplify = T)[,1],
                           " ",label.y))%>%
  filter(!grepl("NA",label))

# Group to show only the top-10 impacts
toppies<-names(sort(table(tmp$label),decreasing = T)[1:10])
tmp$label[!tmp$label%in%toppies]<-"Other"

tmp$label%<>%factor(levels=c(toppies,"Other"))

p<-tmp%>%ggplot()+geom_bar(aes(label,fill=label),colour="black")+
  xlab("Impact")+labs(fill="Impact")+
  ylab("Number of Impact Recordings")+
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.position="none");p
ggsave("AllHazards_impacts_bar.png",p,path="./Plots/")  

p<-PlotImpAgg(ADM,"N",F)+
  scale_fill_gradient(name = "No. Events",guide="legend", trans = "log",
                      high="chartreuse",low="chartreuse4",
                      breaks=rev(c(10,100, 1000, 10000, 100000, 1000000, 10000000)),
                      labels=rev(c("10^1","10^2","10^3","10^4","10^5","10^6","10^7")));p
ggsave("AllHazards_spatial.png",p,path="./Plots/",width = 13,height = 7)  


impies$Year<-AsYear(impies$ev_sdate)

p<-impies%>%filter(Year>1950 & Year<2024 & hazAb!="LS")%>%
  group_by(Year,hazAb)%>%reframe(Count=length(unique(GCDB_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=hazAb))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  scale_colour_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_temporal.png",p,path="./Plots/",width = 13,height = 7)  

impies$Climate<-!impies$hazAb%in%c("EQ","VO")
impies$Climate[impies$hazAb=="LS"]<-NA

temp<-impies%>%filter(Year>1900 & Year<2024 & hazAb!="LS")%>%
  group_by(Year,Climate)%>%
  reframe(Count=length(unique(GCDB_ID)))

p<-data.frame(Year=unique(temp$Year),Ratio=sapply(unique(temp$Year),function(Y){
  clim<-temp$Climate & temp$Year==Y
  noclim<-!temp$Climate & temp$Year==Y
  if(sum(clim)==0 & sum(noclim)==0) return(NA)
  if(sum(noclim)==0) return(Inf)
  if(sum(clim)==0) return(0)
  temp$Count[clim]/temp$Count[noclim]
}))%>%
  ggplot()+geom_point(aes(Year,Ratio))+geom_hline(yintercept = 1,colour="red");p
ggsave("Clim-Noclim_temporal.png",p,path="./Plots/",width = 13,height = 7)  


FL<-impies%>%filter(hazAb=="FL")%>%ImpactAggADM0()
# ST<-impies%>%filter(hazAb=="ST")%>%ImpactAggADM0()
# TC<-impies%>%filter(hazAb=="TC")%>%ImpactAggADM0()
EQ<-impies%>%filter(hazAb=="EQ")%>%ImpactAggADM0()

p<-PlotImpAgg(FL,loggie = F)+
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log",
                      # low="moccasin",high="green4",
                      breaks=c(10,100, 1000, 10000, 100000, 1000000, 10000000),
                      labels=c("10^1","10^2","10^3","10^4","10^5","10^6","10^7"))+
  guides(fill = guide_legend(reverse=TRUE));p
ggsave("FL_spatial_deaths.png",p,path="./Plots/",width = 13,height = 7)

p<-PlotImpAgg(EQ,loggie = F)+
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log",
                      low="moccasin",high="magenta",
                      breaks=c(10,100, 1000, 10000, 100000, 1000000, 10000000),
                      labels=c("10^1","10^2","10^3","10^4","10^5","10^6","10^7"));p
ggsave("EQ_spatial_deaths.png",p,path="./Plots/",width = 13,height = 7)

p<-impies%>%filter(Year>1950 & Year<2024 & hazAb!="LS")%>%
  group_by(Year,src_db)%>%reframe(Count=length(unique(GCDB_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=src_db))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  labs(colour="Database");p
ggsave("Alldatabases_temporal.png",p,path="./Plots/",width = 13,height = 7)  












#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GCDB WORKSHOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

p<-EMDAT%>%filter(impactdetails=="impdetallpeop" & imptype=="imptypdeat" & 
                  ev_sdate>as.Date("2000-01-01"))%>%
  group_by(hazAb)%>%reframe(Deaths=sum(impvalue))%>%arrange(desc(Deaths))%>%
  ggplot()+geom_bar(aes(x=hazAb,y=Deaths,fill=hazAb),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Total Deaths")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma)+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_deaths_bar.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 8)  

p<-EMDAT%>%filter(impactdetails=="impdetallpeop" & imptype=="imptypdeat" & 
                    ev_sdate>as.Date("2000-01-01"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(loggie = F) +
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);p
                      # breaks=c(10000000,1000000,100000,10000,1000,100,10),
                      # labels=c("10^7","10^6","10^5","10^4","10^3","10^2","10^1"));p
ggsave("allhaz_spatial_deaths.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(ev_sdate>as.Date("2000-01-01"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact = "N",loggie = F) +
  scale_fill_gradient(name = "No. Impact Records",guide="legend", trans = "log10",
                      high="chartreuse",low="chartreuse4");p
                      # breaks=c(10000,1000,100,10,1),
                      # labels=c("10^4","10^3","10^2","10^1","10^0"));p
ggsave("allhaz_spatial_records.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(ev_sdate>as.Date("2000-01-01") & src_db=="GO-App")%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact = "N",loggie = F) +
  scale_fill_gradient(name = "No. Impact Records",guide="legend", trans = "log10",
                      high="orange4",low="orange");p
ggsave("allhaz_spatial_records_GOEmApps.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(hazAb%in%names(pal))%>%
  group_by(hazAb)%>%reframe(Records=length(impvalue))%>%arrange(desc(Records))%>%
  ggplot()+geom_bar(aes(x=hazAb,y=Records,fill=hazAb),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Total No. Impact Records")+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_records_bar.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 8)  

p<-impies%>%filter(Year>1950 & Year<2024 & hazAb!="LS" & hazAb%in%names(pal))%>%
  group_by(Year,hazAb)%>%reframe(Count=length(unique(GCDB_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=hazAb))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  scale_colour_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_temporal.png",p,path="./Plots/GCDB_Workshop/",width = 8,height = 5)  

p<-impies%>%filter(impactsubcats%in%c("impecodirtot","impecoaid") &
                     imptype%in%c("imptypaidreqifrc","imptypcost") &
                     ev_sdate>as.Date("2000-01-01") &
                     !is.na(hazAb) & hazAb%in%lhaz &
                     src_db!="GO-FR")%>%
  group_by(hazAb,RCnot)%>%reframe(Cost=sum(impvalue))%>%
  ggplot()+geom_bar(aes(x=hazAb,y = Cost,fill=RCnot),colour="black",stat = "identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+#scale_y_log10()+
  labs(fill="Data Source")+facet_wrap(~RCnot,scales = "free");p
ggsave("AllHazards_src_db_bar.png",p,path="./Plots/",width = 9,height = 8)  

p<-impies%>%filter(impactsubcats=="impecodirtot" &
                     imptype=="imptypcost" &
                     ev_sdate>as.Date("2000-01-01") &
                     !src_db%in%c("GO-FR","GO-App"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact="impecodirtot-imptypcost",loggie = F) +
  scale_fill_gradient(name = "Total Cost",guide="legend", trans = "log",
                      low="moccasin",high="magenta",
                      breaks=c(10000000,1000000,100000,10000,1000,100,10),
                      labels=c("10^7","10^6","10^5","10^4","10^3","10^2","10^1"));p
ggsave("NoGO_spatial_costs.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(impactsubcats=="impecoaid" &
                   imptype=="imptypaidreqifrc" &
                     ev_sdate>as.Date("2000-01-01") &
                     src_db%in%c("GO-App"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact = "impecoaid-imptypaidreqifrc",loggie = F) +
  scale_fill_gradient(name = "Total Appeal Requested",guide="legend", trans = "log",
                      low="moccasin",high="magenta",
                      breaks=c(10000000,1000000,100000,10000,1000,100,10),
                      labels=c("10^7","10^6","10^5","10^4","10^3","10^2","10^1"));p
ggsave("GO_spatial_costs.png",p,path="./Plots/GCDB_Workshop/",width = 10)


tmp<-impies%>%filter(imptype%in%c("imptypaidallifrc","imptypcost") &
                       ev_sdate>as.Date("2000-01-01") &
                       src_db=="GO-App")

p<-impies%>%filter(ev_sdate>as.Date("2000-01-01") &
                     !is.na(hazAb) & hazAb%in%lhaz &
                     src_db!="GO-FR")%>%
  group_by(hazAb,RCnot)%>%reframe(Counts=length(impvalue))%>%
  ggplot()+geom_bar(aes(x=hazAb,y = Counts,fill=RCnot),colour="black",stat = "identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+#scale_y_log10()+
  labs(fill="Data Source")+facet_wrap(~RCnot,scales = "free");p
ggsave("AllHazards_Records_bar_Gocomp.png",p,path="./Plots/GCDB_Workshop/",width = 9,height = 4)  


p<-impies%>%filter(Year>1950 & Year<2024 & src_db!="GO-FR")%>%
  group_by(Year,RCnot)%>%reframe(Count=length(unique(GCDB_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=RCnot))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  labs(colour="Data Source");p
ggsave("RCnot_temporal.png",p,path="./Plots/GCDB_Workshop/",width = 8,height = 5)  

centrams<-c("COL","CRI","PAN","HND")
  
subbie<-impies%>%filter(ISO3%in%centrams)

p<-subbie%>%filter(!is.na(hazAb) & hazAb%in%lhaz &
                     ev_sdate>as.Date("2000-01-01") &
                     src_db!="GO-FR")%>%
  group_by(RCnot,hazAb,ISO3)%>%reframe(Counts=length(src_db))%>%
  ggplot(aes(x=ISO3,y=Counts,group=hazAb))+geom_bar(aes(x=ISO3,y=Counts,fill=hazAb),colour="black",stat = "identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+#scale_y_log10()+
  # scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
  labs(fill="Data Source")+facet_wrap(~RCnot,scales = "free");p
ggsave("NS_allhazards_records_notRC.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 5)  


p<-subbie%>%filter(!is.na(hazAb) & hazAb%in%lhaz &
                     impactdetails=="impdetallpeop" & imptype=="imptypdeat" & 
                     ev_sdate>as.Date("2000-01-01") &
                     src_db!="GO-FR")%>%
  group_by(hazAb,ISO3)%>%reframe(Deaths=sum(impvalue))%>%
  ggplot()+geom_bar(aes(x=ISO3,y=Deaths,fill=hazAb),colour="black",stat="identity")+
  xlab("Impact Database")+ylab("Total Deaths")+#scale_y_log10()+
  # scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
  labs(fill="Data Source"); p #+facet_wrap(~RCnot,scales = "free");p
ggsave("NS_allhazards_Deaths.png",p,path="./Plots/GCDB_Workshop/",width = 9,height = 8)  


p<-subbie%>%filter(!is.na(hazAb) & hazAb%in%lhaz &
                     impactsubcats=="impecodirtot" & imptype=="imptypcost" & 
                     ev_sdate>as.Date("2000-01-01") &
                     src_db!="GO-FR")%>%
  group_by(hazAb,ISO3)%>%reframe(Cost=sum(impvalue))%>%
  ggplot()+geom_bar(aes(x=ISO3,y=Cost,fill=hazAb),colour="black",stat="identity")+
  xlab("Impact Database")+ylab("Total Economic Cost")+#scale_y_log10()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma)+
  # scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
  labs(fill="Data Source"); p #+facet_wrap(~RCnot,scales = "free");p
ggsave("NS_allhazards_Cost.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 7)  

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% DESINVENTAR SUB-NATIONAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

sapply(seq_along(centrams),function(i){
  
  iso3<-centrams[i]
  
  cntimps<-impies%>%filter(src_db=="Desinventar" & ISO3==iso3)
  
  filer<-paste0("./CleanedData/SocioPoliticalData/Desinventar/",
                stringr::str_to_lower(iso3),
                "/ADM_",stringr::str_to_lower(iso3),
                ".geojson")
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  ADM$Allrecords<-sapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,cntimps$spat_ID,ignore.case = T))
  })
  
  q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  
  sapply(lhaz,function(hazzie){
    
    ADM$records<-sapply(ADM$ADMcode,function(codie){
      sum(grepl(codie,cntimps$spat_ID[cntimps$hazAb==hazzie],ignore.case = T))
    })
    
    q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+
      scale_fill_gradient(paste0("No. Records - ",hazzie), high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    return(T)},simplify = T)
  
  return(T)},simplify = T)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%% EM-DAT SUB-NATIONAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

sapply(seq_along(centrams),function(i){
  
  iso3<-centrams[i]
  
  cntimps<-impies%>%filter(src_db=="EM-DAT" & ISO3==iso3)
  
  filer<-paste0("./CleanedData/SocioPoliticalData/EMDAT/",
                iso3,"/ADM_",iso3,
                ".geojson")
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  ADM1<-aggregate(ADM, by = "ADM1_CODE")
  ADM2<-aggregate(ADM, by = "ADM2_CODE")
  
  ADM2$Allrecords<-sapply(ADM2$ADM2_CODE,function(codie){
    sum(grepl(codie,cntimps$spat_ID,ignore.case = T))
  })
  
  q<-ADM2%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM2_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  ADM1$Allrecords<-sapply(ADM1$ADM1_CODE,function(codie){
    sum(grepl(codie,cntimps$spat_ID,ignore.case = T))
  })
  
  q<-ADM1%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM1_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  
  sapply(lhaz,function(hazzie){
    
    ADM2$records<-sapply(ADM2$ADM2_CODE,function(codie){
      sum(grepl(codie,cntimps$spat_ID[cntimps$hazAb==hazzie],ignore.case = T))
    })
    
    q<-ADM2%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM2_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    ADM1$records<-sapply(ADM1$ADM1_CODE,function(codie){
      sum(grepl(codie,cntimps$spat_ID[cntimps$hazAb==hazzie],ignore.case = T))
    })
    
    q<-ADM1%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM1_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    return(T)},simplify = T)
  
  return(T)},simplify = T)















