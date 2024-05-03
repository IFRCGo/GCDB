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
    "imp_value","imp_type_code","imp_unit_code","imp_type_lab",
    "imp_unit_lab","imp_sdate","imp_fdate","imp_ISO3",
    "imp_unregion","imp_worldbankregion","imp_continent","imp_unsubregion",
    "imp_worldbankincomegroup","imp_spat_covlab","imp_srcdb_code",
    "imp_spat_ID"
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
  # GLIDE
  GLIDE<-convGLIDE_Monty()
  
  return(rbind(procDBfore(DREF),procDBfore(EMDAT),procDBfore(IDU),procDBfore(GIDD),procDBfore(GLIDE)))
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

impies<-Monty%>%filter(imp_srcdb_code=="EMDAT" & impact=="Deaths - People (All Demographics) [count]")%>%
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
  coord_cartesian(ylim = c(0, NA))+
  facet_wrap(~haz_Ab) + ylab("Median Deaths") + labs(fill="Hazard",colour="Hazard") ; p
ggsave("./Plots/HazardSeasonality_Deaths_EastAsiaPacific.png",p,height=5,width=12)

p<-impies%>%filter(haz_Ab%in%c("FL","ST","TC") & worldbankregion=="East Asia & Pacific")%>%
  mutate(haz_Ab=factor(haz_Ab,labels=c("Flood","Storm","Tropical Cyclone")))%>%
  ggplot(aes(Month,numEvs,group=haz_Ab))+geom_smooth(aes(colour=haz_Ab),se=F)+
  # geom_ribbon(aes(ymin=Q05,ymax=Q95,fill=haz_Ab),colour="black",alpha=0.3)+
  facet_wrap(~haz_Ab) + ylab("Number of Events") + labs(fill="Hazard",colour="Hazard") ; p
ggsave("./Plots/HazardSeasonality_Events_EastAsiaPacific.png",p,height=5,width=12)




#####################################################################
######################### EXCEEDANCE CURVES #########################
#####################################################################

#@@@@@@@@@@@@@@@@@ DEATHS @@@@@@@@@@@@@@@@@#

hazcov<-Monty%>%group_by(haz_Ab)%>%
  reframe(coverage=as.numeric(max(year) - min(year)))

# haz_Ab=="FL"
# isos<-c("IDN","PHL","AFG","BGD","COL","KEN","MEX")
# haz_Ab=="TC"
# isos<-c("PHL","MEX","VNM","JPN","BGD","MDG","HTI")
# haz_Ab=="EQ"
# isos<-c("IDN","IRN","AFG","TUR","JPN","PHL","PER","MEX")

isos=c("PHL","MEX","USA","BGD")

exceed<-Monty%>%filter(imp_type_lab=="Deaths" & imp_srcdb_code=="EMDAT" &
                         year>1990 &
                         !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" &
                         haz_Ab%in%c("FL","EQ","TC") &
                         imp_ISO3%in%isos)%>%
  mutate(haz_Ab_lab=factor(haz_Ab,labels=c("Earthquake","Flood","Tropical Cyclone")))%>%
  group_by(haz_Ab_lab,imp_ISO3)%>%
  reframe(coverage=unique(hazcov$coverage[hazcov==unique(haz_Ab)]),
          N=n(),
          impact=sort(imp_value),
          AAL=mean(impact),
          ranking=1:N,
          probability=n():1/unique(coverage))

p<-exceed%>%ggplot(aes(impact,probability,colour=haz_Ab_lab))+
  # geom_smooth(se=F,span=0.6)+
  geom_point(size=2)+geom_line(linewidth=0.3)+
  xlab("Total Deaths")+ylab("No. Events Per Year")+
  ggtitle("EM-DAT Deaths Since 1990")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14))+
  labs(colour="Hazard")+
  scale_y_log10()+
  scale_x_log10(limits=c(1,10000),labels = scales::label_comma())+
  facet_wrap(~imp_ISO3); p
ggsave("./Plots/Example_ExceedanceCurve_EMDAT-Deaths.png",p,height=6,width=10)
# 

GenExceedance_ISO<-function(iso3,imp_db="EMDAT",imp_type="Deaths",exp_spec=NULL,yr=1990,hazs=NULL, loggie=F,rotty=0 ){
  # Extract the data
  out<-Monty%>%filter(imp_srcdb_code==imp_db & 
                   year>yr &
                   imp_ISO3==iso3)
  
  if(!is.null(imp_type)) out%<>%filter(imp_type_lab==imp_type)
  if(!is.null(exp_spec)) out%<>%filter(exp_spec_lab==exp_spec)
  if(!is.null(hazs)) out%<>%filter(haz_Ab%in%hazs)
    
  p<-out%>%
    group_by(haz_Ab)%>%
    reframe(coverage=unique(hazcov$coverage[hazcov==unique(haz_Ab)]),
            N=n(),
            impact=sort(imp_value),
            probability=n():1/unique(coverage))%>%
    filter(N>3)%>%
    ggplot(aes(impact,probability,colour=haz_Ab))+
    # geom_smooth(se=F,span=0.6)+
    geom_point(size=2)+geom_line(linewidth=0.3)+
    xlab(imp_type)+ylab("No. Events Per Year")+
    ggtitle(paste0(iso3," - ",imp_db," ", imp_type, " Since ",yr))+
    theme(plot.title = element_text(hjust = 0.5,face="bold",size=14),
          axis.text.x = element_text(angle = rotty, vjust = 1, hjust=1))+
    scale_x_continuous(labels = scales::label_number())+
    labs(colour="Hazard")
  if(loggie) p <- p + scale_x_log10(labels = scales::label_number()) 
  
  ggsave(paste0("./Plots/",iso3,"_ExceedanceCurve_",imp_db,"_",imp_type,"_",paste0(hazs,collapse = "-"),".png"),p,height=6,width=10)
  
  return(p)
}

GenExceedance_ISO("CRI")
GenExceedance_ISO("PER",loggie=T)
GenExceedance_ISO("PER","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted")
GenExceedance_ISO("CRI","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted")
GenExceedance_ISO("PER","EMDAT",imp_type="Total Affected",hazs=c("CW"),rotty=20)


#@@@@@@@@@@@@@@@@@ DISPLACEMENT @@@@@@@@@@@@@@@@@#

exceed<-Monty%>%filter(imp_type_lab=="Internally Displaced Persons (IDPs)" & imp_srcdb_code=="GIDD" &
                         year>2016 &
                         !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" &
                         haz_Ab%in%c("FL","EQ","TC") &
                         imp_ISO3%in%isos)%>%
  mutate(haz_Ab_lab=factor(haz_Ab,labels=c("Earthquake","Flood","Tropical Cyclone")))%>%
  group_by(haz_Ab_lab,imp_ISO3)%>%
  reframe(coverage=unique(hazcov$coverage[hazcov==unique(haz_Ab)]),
          N=length(imp_value),
          impact=sort(imp_value),
          probability=n():1/unique(coverage))

p<-exceed%>%ggplot(aes(impact,probability,colour=haz_Ab_lab))+
  # geom_smooth(se=F,span=0.6)+
  geom_point(size=2)+geom_line(linewidth=0.3)+
  xlab("Internally Displaced Persons (IDPs)")+ylab("No. Events Per Year")+
  ggtitle("GIDD IDPs Since 2016")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14))+
  # coord_cartesian(xlim = c(10, NA))+
  labs(colour="Hazard")+
  scale_y_log10()+scale_x_log10(labels = scales::label_comma())+
  facet_wrap(~imp_ISO3); p
ggsave("./Plots/Example_ExceedanceCurve_GIDD-IDPs.png",p,height=6,width=10)

#@@@@@@@@@@@@@@@@@ ECONOMIC COST @@@@@@@@@@@@@@@@@#

exceed<-Monty%>%filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted" & imp_srcdb_code=="EMDAT" &
                         year>1990 &
                         !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" &
                         haz_Ab%in%c("FL","EQ","TC") &
                         imp_ISO3%in%isos)%>%
  mutate(haz_Ab_lab=factor(haz_Ab,labels=c("Earthquake","Flood","Tropical Cyclone")))%>%
  group_by(haz_Ab_lab,imp_ISO3)%>%
  reframe(coverage=unique(hazcov$coverage[hazcov==unique(haz_Ab)]),
          N=length(imp_value),
          impact=sort(imp_value),
          probability=n():1/unique(coverage))

p<-exceed%>%ggplot(aes(impact,probability,colour=haz_Ab_lab))+
  # geom_smooth(se=F,span=0.6)+
  geom_point(size=2)+geom_line(linewidth=0.3)+
  xlab("Total Economic Cost [USD 2011 - Infl. Adj.]")+ylab("No. Events Per Year")+
  ggtitle("EM-DAT Total Cost Since 1990")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))+
  # coord_cartesian(xlim = c(10, NA))+
  labs(colour="Hazard")+
  scale_y_log10()+
  # scale_x_log10(labels = scales::label_comma())+
  scale_x_log10(breaks=c(100000,10000000,1000000000,100000000000),
                labels=c("0.1 million","10 million","1 billion","100 billion"))+
  facet_wrap(~imp_ISO3); p
ggsave("./Plots/Example_ExceedanceCurve_EMDAT_Cost.png",p,height=6,width=10)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% DESINVENTAR SUB-NATIONAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
sapply(seq_along(centrams),function(i){
  
  print(centrams[i])
  
  iso3<-centrams[i]
  
  cntimps<-impies%>%filter(imp_src_db=="Desinventar" & ISO3==iso3)
  
  filer<-paste0("./CleanedData/SocioPoliticalData/Desinventar/",
                stringr::str_to_lower(iso3),
                "/ADM_",stringr::str_to_lower(iso3),
                ".geojson")
  
  if(!file.exists(filer)) return(F)
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  if(iso3%in%c("NPL","LKA")){
    ADM<-ADM[nchar(ADM$ADMcode)==9,]
  } else if(iso3=="PAK"){
    ADM<-ADM[nchar(ADM$ADMcode)==3,]
  } 
  
  ADM$Allrecords<-sapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,cntimps$imp_spat_ID,ignore.case = T))
  })
  
  ADM$deathsRP<-sapply(ADM$ADMcode,function(codie){
    output<-tryCatch(cntimps%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                                        ev_sdate>1975 & !is.na(imp_value) &
                                        grepl(codie,imp_spat_ID,ignore.case = T))%>%
                       impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
    ifelse(output$N>30,output$impRP,NA)
  })
  
  ADM$costRP<-sapply(ADM$ADMcode,function(codie){
    output<-tryCatch(cntimps%>%filter(imp_subcats%in%c("impecotot","impecodirtot") & imp_type=="imptypcost" & 
                                        ev_sdate>1975 & !is.na(imp_value) &
                                        grepl(codie,imp_spat_ID,ignore.case = T))%>%
                       impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
    ifelse(output$N>30,output$impRP/1e6,NA)
  })  
  
  # Extract the bounding box of the admin boundaries
  # bbox<-expandBbox(unlist(unname(ExtractBBOXpoly(ADM)[1,2:5])),3)
  
  mad_map <- get_stamenmap(expandBbox(ADM@bbox,3),source = "stamen",maptype = "terrain",zoom=5)
  
  p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
  
  q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=deathsRP), color = "grey30", linewidth=0.1, inherit.aes = FALSE) +
    scale_fill_gradient("Exp. No. Deaths 5Yr RP",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("DeathsRP_Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=costRP), color = "grey30", linewidth=0.1, inherit.aes = FALSE) +
    scale_fill_gradient("Exp. Cost 5Yr RP [Millions Local Curr]",high="chartreuse",low="chartreuse4",trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("CostRP_Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  sapply(lhaz,function(hazzie){
    
    ADM$records<-sapply(ADM$ADMcode,function(codie){
      sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
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

EMDAT<-jsonlite::fromJSON("./CleanedData/MostlyImpactData/CRED/EMDAT_2024-04-15.json")%>%
  procDBfore()

centrams<-c("IDN","NPL","PAK","TLS","LKA","MNG","FJI")

sapply(seq_along(centrams),function(i){
  
  iso3<-centrams[i]
  
  indy<-EMDAT$impact_Data$ID_linkage$event_ID%in%EMDAT$event_Level$ID_linkage$event_ID[EMDAT$event_Level$spatial$ev_ISO3s==iso3]
  
  ADM<-GAUL2Monty(iso3,forcer=T)
  filer<-paste0("./CleanedData/SocioPoliticalData/EMDAT/",
                iso3,"/ADM_",iso3,
                ".geojson")
  
  if(!file.exists(filer)) return(F)
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  ADM1<-aggregate(ADM, by = "ADM1_CODE")
  ADM2<-aggregate(ADM, by = "ADM2_CODE")
  
  ADM2$Allrecords<-sapply(ADM2$ADM2_CODE,function(codie){
    sum(unlist(lapply(EMDAT$impact_Data$spatial$ID_linkage[indy],function(x) {
      any(grepl(paste0("FAO-GAUL-ADM2-",iso3,"-",codie),x$imp_spat_ID,ignore.case = T))
    })))
  })
  
  q<-ADM2%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    xlab("Longitude") + ylab("Latitude") + 
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM2_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",scale=3)  
  
  ADM1$Allrecords<-sapply(ADM2$ADM1_CODE,function(codie){
    sum(unlist(lapply(EMDAT$impact_Data$spatial$ID_linkage[indy],function(x) {
      any(grepl(paste0("FAO-GAUL-ADM1-",iso3,"-",codie),x$imp_spat_ID,ignore.case = T))
    })))
  })
  
  q<-ADM1%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM1_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",scale=3)  
  
  
  sapply(lhaz,function(hazzie){
    
    ADM2$records<-sapply(ADM2$ADM2_CODE,function(codie){
      sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
    })
    
    q<-ADM2%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM2_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",scale=3)  
    
    ADM1$records<-sapply(ADM1$ADM1_CODE,function(codie){
      sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
    })
    
    q<-ADM1%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM1_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",scale=3)  
    
    return(T)},simplify = T)
  
  return(T)},simplify = T)




























#@@@@@@@@@@@@@@@@@@@@@ MONTY WORKSHOP MAY 2024 @@@@@@@@@@@@@@@@@@@@@#
##### Data ##### 
# - Wrangle Desinventar, GDACS + hazard forecast data
# - Get the GAUL data somehow
# - Get the Desinventar map data


##### PowerBI ##### 
# Tables: average country metrics per hazard, per metric type
# For population metrics, use as % of population
# For economic metrics, use as % of GDP-PPP per capita


# Seasonality, per country, per hazard, per impact type (inc. #events)
# 


# Sub-national maps of impact data  


# Loss-exceedance curves



















