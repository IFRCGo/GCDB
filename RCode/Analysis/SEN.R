options(scipen = 999)

iso3<-"SEN"

impies<-readRDS("./CleanedData/MostlyImpactData/AllHaz_impies_20231019_nozeros.RData")
# Load taxonomies
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Load the SEN data specifically
SEN<-impies%>%filter(ISO3=="SEN" & Year>2000 & !is.na(haz_Ab))
# left join imp_det variable to the dataframe
SEN%<>%left_join(data.frame(
  imp_det=taxies%>%filter(list_name=="imp_det")%>%pull(name),
  imp_det_lab=taxies%>%filter(list_name=="imp_det")%>%pull(label)
))
# left join imp_type variable to the dataframe
SEN%<>%left_join(data.frame(
  imp_type=taxies%>%filter(list_name=="imp_type")%>%pull(name),
  imp_type_lab=taxies%>%filter(list_name=="imp_type")%>%pull(label)
))
# Merge the two so that we can see the full impact descriptions
SEN%<>%mutate(Impact=paste0(imp_det_lab," ",imp_type_lab))

SEN$Impact[SEN$Impact=="General Aid Contributions NA"]<-"IFRC Funding Allocated (DREF & EA)"
# Plot the yearly counts by data base
SEN%>%group_by(Year,imp_src_db)%>%reframe(Count=n())%>%
  arrange(Year)%>%group_by(imp_src_db)%>%mutate(Cum=cumsum(Count))%>%
  ggplot()+geom_point(aes(Year,Cum,colour=imp_src_db))#+scale_y_log10()
table(SEN$imp_cats)
SEN%>%filter(imp_cats=="impcatphyinf")%>%
  ggplot()+geom_bar(aes(factor(Impact),fill=imp_src_db))+facet_wrap(~imp_src_db)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

unique(impies$imp_spat_ID[impies$imp_src_db=="Desinventar"])

table(SEN$imp_src_db)

q<-SEN%>%group_by(Year,imp_src_db,haz_Ab)%>%reframe(Count=n())%>%
  arrange(Year)%>%group_by(imp_src_db,haz_Ab)%>%mutate(Cum=cumsum(Count))%>%
  ggplot()+geom_point(aes(Year,Cum,colour=haz_Ab))+
  xlab("Year")+ylab("Cumulative Number")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=18))+
  ggtitle("Cumulative Number of Hazard Events")+labs(colour="Hazard")+
  facet_wrap(~imp_src_db);q
ggsave("./Plots/SEN/SEN_NoFL_Des_perYear.png",q,width=12,height=8)

q<-SEN%>%filter(Impact%in%c("Buildings Damaged","People (All Demographics) Deaths",
                            "People (All Demographics) Total Affected",
                            "IFRC Funding Allocated (DREF & EA)"))%>%
  mutate(Date=as.Date(ev_sdate))%>%
  ggplot()+geom_point(aes(Date,imp_value,colour=haz_Ab))+
  facet_wrap(~Impact,scales = "free_y")+labs(colour="Hazard")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=18))+
  ggtitle("Estimated Impact")+labs(colour="Hazard")+
  ylab("Impact");q
ggsave("./Plots/SEN/SEN_HazTimeline_perImpact.png",q,width=12,height=4)

#@@@@@@@@@@ ADM 1 @@@@@@@@@@#
# Desinventar
i<-1
maps<-readRDS("./tmp_maps_SEN.RData")
loccy<-str_split(maps$filename[i+1],"/",simplify = T); loccy<-loccy[length(loccy)]
dADM<-sf::st_read(paste0("./RawData/MostlyImpactData/Desinventar/",str_to_lower(iso3),"/",loccy),quiet=T)
plot(dADM)
dADM%<>%dplyr::select(ID)

dADM$Allrecords<-sapply(dADM$ID,function(codie){
  sum(grepl(codie,SEN$imp_spat_ID,ignore.case = T))
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=Allrecords), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("No. Impact Records (Desinventar)")+
  scale_fill_gradient(name = "Impact Records",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q



dADM$deaths<-sapply(dADM$ID,function(codie){
  sum(SEN$imp_value[grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
                      SEN$imp_det=="impdetallpeop" & SEN$imp_type=="imptypdeat"])
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=deaths), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Total Deaths (Desinventar)")+
  scale_fill_gradient(name = "Deaths",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q


dADM$builds<-sapply(dADM$ID,function(codie){
  sum(SEN$imp_value[grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
                      SEN$imp_det=="impdetbuild"])
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=builds), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Total Buildings Destroyed/Damaged (Desinventar)")+
  scale_fill_gradient(name = "Buildings",guide="legend", #trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q



dADM$FLdeaths<-sapply(dADM$ID,function(codie){
  sum(SEN$imp_value[grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
                      SEN$imp_det=="impdetallpeop" & SEN$imp_type=="imptypdeat" &
                      SEN$haz_Ab=="FL"])
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=FLdeaths), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Total Flood Deaths (Desinventar)")+
  scale_fill_gradient(name = "Deaths",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q


dADM$FL<-sapply(dADM$ID,function(codie){
  sum(grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
                      SEN$haz_Ab=="FL")
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=FL), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Number of Floods (Desinventar)")+
  theme(legend.position =  c(.75, .75))+
  scale_fill_gradient(name = "Flood Records",guide="legend",# trans = "log10",
                      low="darkblue",high="lightblue",n.breaks=6);q
ggsave("./Plots/SEN/SEN_NoFL_Des.png",q,width=12,height=10)


dADM$WF<-sapply(dADM$ID,function(codie){
  sum(grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
        SEN$haz_Ab=="WF")
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=WF), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Number of Wildfires (Desinventar)")+
  theme(legend.position =  c(.75, .75))+
  scale_fill_gradient(name = "Wildfire Records",guide="legend",trans = "log10",
                      low="darkred",high="red1",n.breaks=6);q
ggsave("./Plots/SEN/SEN_NoWF_Des.png",q,width=12,height=10)

dADM$FLbuilds<-sapply(dADM$ID,function(codie){
  sum(SEN$imp_value[grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
                      SEN$imp_det=="impdetbuild" &
                      SEN$haz_Ab=="FL"])
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=FLbuilds), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Floods - Buildings Destroyed/Damaged (Desinventar)")+
  scale_fill_gradient(name = "Buildings",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q



dADM$WFbuilds<-sapply(dADM$ID,function(codie){
  sum(SEN$imp_value[grepl(codie,SEN$imp_spat_ID,ignore.case = T) & 
                      SEN$imp_det=="impdetbuild" &
                      SEN$haz_Ab=="WF"])
})

# Plot it out!
q<-dADM%>%ggplot()+
  geom_sf(aes(fill=WFbuilds), linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Wildfires - Buildings Destroyed/Damaged (Desinventar)")+
  scale_fill_gradient(name = "Buildings",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q






# EM-DAT 
# eADM<-sf::st_read(paste0("./RawData/SocioPoliticalData/GAUL/gadm40_SEN_shp/gadm40_SEN_",i,".shp"))
eADM<-sf::st_read(paste0("./RawData/SocioPoliticalData/GAUL/SEN_adm_shp_old/SEN_adm",i,".shp"))
# eADM<-sf::st_read(paste0("./RawData/SocioPoliticalData/GAUL/gadm36_SEN_shp/gadm36_SEN_",i,".shp"))

SEN_adm_shp_old

codies<-SEN%>%filter(imp_src_db=="EM-DAT")%>%pull(imp_spat_ID)%>%str_split(";",simplify = T)%>%unlist()%>%c()%>%str_split(",",simplify = T)%>%c()%>%unique()  
  
  
  
  


#@@@@@@@@@@ ADM 1 @@@@@@@@@@#
i<-1
loccy<-str_split(maps$filename[i+1],"/",simplify = T); loccy<-loccy[length(loccy)]
dADM<-ADM<-sf::st_read(paste0("./RawData/MostlyImpactData/Desinventar/",str_to_lower(iso3),"/",loccy),quiet=T)
# EM-DAT 
eADM<-GetGAUL(iso3,lADM=i)  



