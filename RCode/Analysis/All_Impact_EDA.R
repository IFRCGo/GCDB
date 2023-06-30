impies<-readRDS("./CleanedData/MostlyImpactData/AllHaz_impies.RData")
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
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

p<-impies%>%filter(impactdetails=="impdetallpeop" & imptype=="imptypdeat" & src_db!="GO-FR")%>%
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






