# How to choose the next country for an EAP
#     Per country which metric to use - killed, deaths, displaced
#     Using just the DREF data

# Seasonality of risks and compare with timeline for EAPs/s-EAPs

# How to choose the hazard for a specific country
#     Per country which metric to use - killed, deaths, displaced
#     Using just the DREF data

# Exceedance curves - best, middle + worst based on EAPs

# For Costa Rica, pair DREFs with EM-DAT + IDMC and 




procDBfore<-function(Monty){
  # Events database
  ev<-Monty_Ev2Tab(Monty)
  # Impacts database
  imp<-Monty_Imp2Tab(Monty)
  # Housecleaning
  rm(Monty)
  # Select only the required columns for the analysis
  ev_cols<-c(
    "event_ID","ev_sdate","ev_fdate","ev_ISO3s","unregion","worldbankregion",
    "continent","unsubregion","worldbankincomegroup","haz_Ab"
  )
  imp_cols<-c(
    "event_ID","imp_sub_ID","exp_spec_lab","exp_subcat_lab","exp_cat_lab",
    "imp_value","imp_type_lab","imp_unit_lab","imp_sdate","imp_fdate","imp_ISO3",
    "imp_unregion","imp_worldbankregion","imp_continent","imp_unsubregion",
    "imp_worldbankincomegroup","imp_spat_covlab","imp_srcdb_code"
  )
  ev%<>%dplyr::select(all_of(ev_cols))
  imp%<>%dplyr::select(all_of(imp_cols))
  # Left-join event + impact data
  taby<-left_join(ev,imp,by="event_ID"); rm(ev,imp)
  # Also group hazards to make up the numbers
  taby$haz_Ab_grp<-taby$haz_Ab; 
  taby$haz_Ab_grp[taby$haz_Ab_grp%in%c("DR","HW","WF","ET")]<-"HT"
  taby$haz_Ab_grp[taby$haz_Ab_grp%in%c("EQ","LS","VO","TS")]<-"non-CC"
  # Extract the year information
  taby$year<-AsYear(taby$ev_sdate)
  
  return(taby)
}

GetTabImpMonty<-function(){
  # IFRC DREF-EA data
  DREF<-convGOApp_Monty()
  # EM-DAT
  EMDAT<-convEMDAT_Monty()
  # IDMC GIDD
  GIDD<-convGIDD_Monty()
  # IDMC IDU
  IDU<-convIDU_Monty()
  
  return(rbind(procDBfore(DREF),procDBfore(EMDAT),procDBfore(IDU),procDBfore(GIDD)))
}

Monty<-GetTabImpMonty()
  
DREFisos<-unique(Monty$imp_ISO3[Monty$imp_srcdb_code=="GO-DREF"])

Monty%<>%mutate(impact=paste0(imp_type_lab," - ",exp_spec_lab," [",imp_unit_lab,"]"))

Monty%>%group_by(haz_Ab,imp_srcdb_code,imp_ISO3,impact)%>%
  reframe(avgimp=ceiling(mean(imp_value)))%>%arrange(desc(avgimp))%>%
  group_by(haz_Ab,impact)%>%
  slice(1)%>%filter(haz_Ab=="FL")%>%
  setNames(c("Hazard","Database","ISO3C","Impact Type","Average Impact"))%>%
  openxlsx::write.xlsx(paste0("./Analysis_Results/EAP_Country_FL_prioritisation.xlsx"))
# 
unique(Monty$impact)

impies<-Monty%>%filter(imp_srcdb_code=="EM-DAT" & impact=="Deaths - People (All Demographics) [count]")%>%
  mutate(Month=month.abb[month(imp_sdate)])%>%
  group_by(haz_Ab,worldbankregion,Month)%>%
  reframe(numEvs=length(imp_value),
          avg=mean(imp_value),med=median(imp_value),
          Q05=quantile(imp_value,probs=0.05),
          Q95=quantile(imp_value,probs=0.95))

# Make sure the values are periodic and re-connect to one another
impies<-rbind(impies,
              impies%>%filter(Month=="Jan")%>%mutate(Month="Jan "))%>%
  mutate(Month=factor(Month,levels=c("Dec ",month.abb,"Jan ")))


impies%>%filter(haz_Ab%in%c("FL","ST","TC") & worldbankregion=="East Asia & Pacific")%>%
  ggplot()+geom_col(aes(Month,avg,fill=haz_Ab),colour="black")+coord_polar()+scale_y_log10()+facet_wrap(~haz_Ab)

impies%>%filter(haz_Ab%in%c("FL","ST","TC") & worldbankregion=="East Asia & Pacific")%>%
  ggplot()+geom_col(aes(Month,med,fill=haz_Ab),colour="black")+coord_polar()+
  geom_errorbar(aes(Month,ymin=Q05,max=Q95),width=0.2)+
  scale_y_log10()+facet_wrap(~haz_Ab)

p<-impies%>%filter(haz_Ab%in%c("FL","ST","TC") & worldbankregion=="East Asia & Pacific")%>%
  mutate(haz_Ab=factor(haz_Ab,labels=c("Flood","Storm","Tropical Cyclone")))%>%
  ggplot(aes(Month,med,group=haz_Ab))+geom_smooth(aes(ymin=Q05,ymax=Q95,fill=haz_Ab,colour=haz_Ab),alpha=0.3)+
  # geom_ribbon(aes(ymin=Q05,ymax=Q95,fill=haz_Ab),colour="black",alpha=0.3)+
  facet_wrap(~haz_Ab) + ylab("Median Deaths") + labs(fill="Hazard",colour="Hazard") ; p
ggsave("./Plots/HazardSeasonality_Deaths_EastAsiaPacific.png",p,height=5,width=12)

p<-impies%>%filter(haz_Ab%in%c("FL","ST","TC") & worldbankregion=="East Asia & Pacific")%>%
  mutate(haz_Ab=factor(haz_Ab,labels=c("Flood","Storm","Tropical Cyclone")))%>%
  ggplot(aes(Month,numEvs,group=haz_Ab))+geom_smooth(aes(colour=haz_Ab),se=F)+
  # geom_ribbon(aes(ymin=Q05,ymax=Q95,fill=haz_Ab),colour="black",alpha=0.3)+
  facet_wrap(~haz_Ab) + ylab("Number of Events") + labs(fill="Hazard",colour="Hazard") ; p
ggsave("./Plots/HazardSeasonality_Events_EastAsiaPacific.png",p,height=5,width=12)









