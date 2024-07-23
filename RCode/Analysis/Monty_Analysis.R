# How to choose the next country for an EAP
#     Per country which metric to use - killed, deaths, displaced
#     Using just the DREF data

# Seasonality of risks and compare with timeline for EAPs/s-EAPs

# How to choose the hazard for a specific country
#     Per country which metric to use - killed, deaths, displaced
#     Using just the DREF data

# Exceedance curves - best, middle + worst based on EAPs

# For Costa Rica, pair DREFs with EM-DAT + IDMC and 
counties<-openxlsx::read.xlsx("./Taxonomies/IsoContinentRegion.xlsx")%>%
  filter(!is.na(Country))%>%mutate(Continent=convIso3Continent_alt(ISO.Code))%>%
  dplyr::select(ISO.Code,Country,UN.Region,World.Bank.Regions,Continent,UN.Sub.Region,World.Bank.Income.Groups)%>%
  setNames(c("ISO3","country","unregion","worldbankregion","continent","unsubregion","worldbankincomegroup"))

# Get the taxonomy data
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Exposure type class
exp_class<-taxies%>%filter(list_name=="exp_specs")%>%dplyr::select(name,label)%>%
  setNames(c("exp_spec","Exposure_Type"))
# Impact type class
imp_class<-taxies%>%filter(list_name=="imp_type")%>%dplyr::select(name,label)%>%
  setNames(c("imp_type","Impact_Type"))
# Impact units
units_info<-taxies%>%filter(list_name=="measunits")%>%
  dplyr::select(name,label)%>%
  setNames(c("unit_code","Impact_Unit"))%>%na.omit()%>%distinct()

haz_Ab_lab<-c("AV"="Avalanche",
              "CW"="Cold Wave",
              "DR"="Drought",
              "EQ"="Earthquake",
              "EP"="Epidemic",
              "ER"="Erosion",
              "EC"="Extra-Tropical Cyclone",
              "ET"="Extreme Temperature",
              "FR"="Fire",
              "FF"="Flash Flood",
              "FL"="Flood",
              "HW"="Heat Wave",
              "HT"="High Temperature",
              "IN"="Insect Infestation",
              "LS"="Landslide",
              "MM"="Mass Movement",
              "MS"="Mudslide",
              "SL"="Slide",
              "SN"="Snowfall",
              "ST"="Storm",
              "SS"="Storm Surge",
              "TO"="Tornado",
              "TC"="Tropical Cyclone",
              "TS"="Tsunami",
              "WF"="Wildfire",
              "VW"="Violent Wind",
              "VO"="Volcanic Activity",
              "WV"="Wave",
              "WA"="Wave Action")


procDBfore<-function(Monty){
  # Event data
  ev<-cbind(Monty$event_Level$ID_linkage%>%dplyr::select(event_ID),
            Monty$event_Level$temporal%>%dplyr::select(ev_sdate,ev_fdate),
            Monty$event_Level$spatial%>%dplyr::select(ev_ISO3s))
  ev$haz_Ab<-unlist(lapply(Monty$event_Level$allhaz_class,function(x) paste0(unique(x$all_hazs_Ab),collapse = ",")))
  # Add region data
  ev%<>%left_join(counties,by=join_by("ev_ISO3s"=="ISO3"))
  # Multiple sources refer to the same event
  ev%<>%distinct(event_ID,ev_ISO3s,ev_sdate, haz_Ab,.keep_all = T)
  # Impact data
  imp<-cbind(Monty$impact_Data$ID_linkage%>%dplyr::select(event_ID,imp_sub_ID),
             Monty$impact_Data$source%>%dplyr::select(imp_src_db,imp_src_org),
             Monty$impact_Data$impact_detail%>%dplyr::select(-c(imp_est_type,imp_unitdate)))
  imp%<>%cbind(do.call(rbind,parallel::mclapply(Monty$impact_Data$spatial,function(x){
    cbind(x$ID_linkage%>%dplyr::select(imp_spat_ID)%>%distinct(),
          x$spatial_info%>%dplyr::select(imp_spat_res,imp_spat_resunits)%>%distinct())%>%distinct()
  },mc.cores=ncores)))
  
  # # Events database
  # ev<-Monty_Ev2Tab(Monty)
  # # Impacts database
  # imp<-Monty_Imp2Tab(Monty)
  # # Housecleaning
  # rm(Monty)
  # # Select only the required columns for the analysis
  # ev_cols<-c(
  #   "event_ID","ev_sdate","ev_fdate","ev_ISO3s","unregion","worldbankregion",
  #   "continent","unsubregion","worldbankincomegroup","haz_Ab"
  # )
  # imp_cols<-c(
  #   "event_ID","imp_sub_ID","exp_spec_lab","exp_subcat_lab","exp_cat_lab",
  #   "imp_value","imp_type_code","imp_unit_code","imp_type_lab",
  #   "imp_unit_lab","imp_sdate","imp_fdate","imp_ISO3",
  #   "imp_unregion","imp_worldbankregion","imp_continent","imp_unsubregion",
  #   "imp_worldbankincomegroup","imp_spat_covlab","imp_srcdb_code",
  #   "imp_spat_ID"
  # )
  # ev%<>%dplyr::select(all_of(ev_cols))
  # imp%<>%dplyr::select(all_of(imp_cols))
  # Left-join event + impact data
  Monty<-left_join(ev,imp,by="event_ID"); rm(ev,imp)
  
  # Monty%>%filter(ev_ISO3s%in%centrams)%>%group_by(imp_src_db,ev_ISO3s,haz_Ab,exp_spec,imp_type)%>%reframe(Count=n())%>%View()
  
  # Also group hazards to make up the numbers
  Monty$haz_Ab_grp<-Monty$haz_Ab; 
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("HW","CW","HT")]<-"ET"
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("FR")]<-"WF"
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("WV","WA")]<-"SS"
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("MM","MS","SL","AV","ER")]<-"LS"
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("TC,FL","EC","TO")]<-"TC"
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("FF")]<-"FL"
  Monty%<>%filter(!haz_Ab%in%c("EP","IN","SN"))
  
  warning("Don't normally aggregate storm surge into storm, just for now")
  Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("SS")]<-"ST"
  
  # Abbreviated hazard labels
  Monty$haz_Ab_grplab<-unname(haz_Ab_lab[Monty$haz_Ab_grp])
  Monty$haz_Ab_lab<-unname(haz_Ab_lab[Monty$haz_Ab])
  # Extract the year information
  Monty$year<-AsYear(Monty$ev_sdate)
  # Add exposure type label
  Monty%<>%left_join(exp_class,by="exp_spec")
  # Add impact type label
  Monty%<>%left_join(imp_class,by="imp_type")
  # Add impact units label
  Monty%<>%left_join(units_info,by=join_by("imp_units"=="unit_code"))
  # Create a single label out of the three variables above
  Monty%<>%mutate(Impact=paste0(Exposure_Type," ",Impact_Type," [",Impact_Unit,"]"))
  # Database
  Monty%<>%mutate(Database=paste0(imp_src_db," - ",imp_src_org))
  # Neaten up some names
  Monty$Impact<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(Monty$Impact," \\(All Demographics\\)",""),
                                                 " \\(Cost\\)",""),"Inflation-Adjusted","Inf-Adj"),"\\(Unspecified-Inflation-Adjustment\\)","(Unspec. Inf-Adj)")
  
  return(Monty)
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

hazcov<-Monty%>%group_by(imp_srcdb_code,haz_Ab)%>%
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
  reframe(coverage=unique(hazcov$coverage[hazcov$haz_Ab==unique(haz_Ab) & 
                                            hazcov$imp_srcdb_code==hazcov$imp_srcdb_code]),
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

hazcov<-Dessie%>%group_by(imp_src_db,haz_Ab)%>%
  reframe(coverage=as.numeric(max(AsYear(ev_sdate)) - min(AsYear(ev_sdate))))

impRP_calc<-function(indf,retper=NULL){
  
  indf%<>%
    group_by(haz_Ab)%>%
    reframe(coverage=unique(hazcov$coverage[hazcov$haz_Ab==unique(haz_Ab) & 
                                              hazcov$imp_srcdb_code==hazcov$imp_srcdb_code]),
            N=n(),
            impact=sort(imp_value),
            probability=n():1/unique(coverage))
  
  if(is.null(retper)) return(indf)
  
  data.frame(N=indf$N,impRP=(splinefun(x=indf$impact, y=indf$probability, method="hyman"))(1/retper))
}

GenExceedance_ISO<-function(Monty,iso3,imp_db="EMDAT",imp_type="Deaths",exp_spec=NULL,yr=1990,hazs=NULL, plotty=T, loggie=F,rotty=0 ){
  # Extract the data
  out<-Monty%>%filter(imp_srcdb_code==imp_db & 
                   year>yr &
                   imp_ISO3==iso3)
  
  if(!is.null(imp_type)) out%<>%filter(imp_type_lab==imp_type)
  if(!is.null(exp_spec)) out%<>%filter(exp_spec_lab==exp_spec)
  if(!is.null(hazs)) out%<>%filter(haz_Ab%in%hazs)
  
  out%<>%
    group_by(haz_Ab)%>%
    reframe(coverage=unique(hazcov$coverage[hazcov$haz_Ab==unique(haz_Ab) & 
                                              hazcov$imp_srcdb_code==hazcov$imp_srcdb_code]),
            N=n(),
            impact=sort(imp_value),
            probability=n():1/unique(coverage))
  
  if(plotty) {
    p<-out%>%ggplot(aes(impact,probability,colour=haz_Ab))+
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
  } else return(out)
}

GenExceedance_ISO(Monty,"CRI")
GenExceedance_ISO(Monty,"PER",loggie=T)
GenExceedance_ISO(Monty,"PER","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted")
GenExceedance_ISO(Monty,"CRI","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted")
GenExceedance_ISO(Monty,"PER","EMDAT",imp_type="Total Affected",hazs=c("CW"),rotty=20)

lapply(centrams,function(iso3){
  do.call(rbind,lapply(unique(Monty$imp_srcdb_code),function(db){
    do.call(rbind,lapply(unique(Monty$Impact[Monty$imp_srcdb_code==db]),function(db){
      GenExceedance_ISO(Monty,"PER","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted",plotty = F)
    }))
  }))
})

#@@@@@@@@@@@@@@@@@ DISPLACEMENT @@@@@@@@@@@@@@@@@#

exceed<-Monty%>%filter(imp_type_lab=="Internally Displaced Persons (IDPs)" & imp_srcdb_code=="GIDD" &
                         year>2016 &
                         !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" &
                         haz_Ab%in%c("FL","EQ","TC") &
                         imp_ISO3%in%isos)%>%
  mutate(haz_Ab_lab=factor(haz_Ab,labels=c("Earthquake","Flood","Tropical Cyclone")))%>%
  group_by(haz_Ab_lab,imp_ISO3)%>%
  reframe(coverage=unique(hazcov$coverage[hazcov$haz_Ab==unique(haz_Ab) & 
                                            hazcov$imp_srcdb_code==hazcov$imp_srcdb_code]),
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
  reframe(coverage=unique(hazcov$coverage[hazcov$haz_Ab==unique(haz_Ab) & 
                                            hazcov$imp_srcdb_code==hazcov$imp_srcdb_code]),
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
    output<-tryCatch(cntimps%>%filter(exp_spec=="expspec_allpeop" & imp_type=="imptypdeat" &
                                        ev_sdate>1975 & !is.na(imp_value) &
                                        grepl(codie,imp_spat_ID,ignore.case = T))%>%
                       impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
    ifelse(output$N>5,output$impRP,NA)
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

EMDAT<-jsonlite::fromJSON("./CleanedData/MostlyImpactData/CRED/EMDAT_2024-04-25.json")%>%
  procDBfore()

centrams<-c("IDN","NPL","PAK","TLS","LKA","MNG","FJI")

sapply(seq_along(centrams),function(i){
  
  iso3<-centrams[i]
  
  indy<-EMDAT$impact_Data$ID_linkage$event_ID%in%EMDAT$event_Level$ID_linkage$event_ID[EMDAT$event_Level$spatial$ev_ISO3s==iso3]
  
  ADM<-GAUL2Monty(iso3)
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














































desADM<-do.call(rbind,lapply(seq_along(centrams)[1:6],function(i){
  
  print(centrams[i])
  
  iso3<-centrams[i]
  
  cntimps<-Dessie%>%filter(imp_src_db=="Desinventar" & imp_ISO3s==iso3)
  
  if(any(unlist(sapply(cntimps$imp_spat_ID[1:100],function(x) grepl("UNDRR-GovDes",x))))) cntimps$imp_spat_ID<-str_split(unlist(cntimps$imp_spat_ID),paste0("-",iso3,"-"),simplify = T)[,2]
  
  cntimps%<>%filter(!is.na(imp_spat_ID) & imp_spat_ID!="")
  
  filer<-paste0("./CleanedData/SocioPoliticalData/Desinventar/",
                stringr::str_to_lower(iso3),
                "/ADM_",stringr::str_to_lower(iso3),
                ".geojson")
  
  if(!file.exists(filer)) return(F)
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  # ADM$ADMcode<-paste0("UNDRR-GovDes-ADM",ADM$ADMlevel,"-",str_to_upper(iso3),"-",ADM$ADMcode)
  
  # if(iso3%in%c("NPL","LKA")){
  #   ADM<-ADM[nchar(ADM$ADMcode)==9,]
  # } else if(iso3=="PAK"){
  #   ADM<-ADM[nchar(ADM$ADMcode)==3,]
  # } 
  
  ADM$Allrecords<-unlist(parallel::mclapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,unlist(cntimps$imp_spat_ID),ignore.case = T))
  },mc.cores = ncores))
  
  ADM$FLrecords<-unlist(parallel::mclapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,unlist(cntimps$imp_spat_ID[cntimps$haz_Ab=="FL"]),ignore.case = T))
  },mc.cores = ncores))
  
  ADM$EQrecords<-unlist(parallel::mclapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,unlist(cntimps$imp_spat_ID[cntimps$haz_Ab=="EQ"]),ignore.case = T))
  },mc.cores = ncores))
  
  ADM$STrecords<-unlist(parallel::mclapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,unlist(cntimps$imp_spat_ID[cntimps$haz_Ab=="ST"]),ignore.case = T))
  },mc.cores = ncores))
  
  ADM$WFrecords<-unlist(parallel::mclapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,unlist(cntimps$imp_spat_ID[cntimps$haz_Ab=="WF"]),ignore.case = T))
  },mc.cores = ncores))
  
  ADM$ISO3<-iso3
  
  return(ADM)
  
  
  # freqy<-cntimps%>%group_by(exp_spec,imp_type)%>%reframe(N=sum(imp_spat_ID>0))
  # espec<-"expspec_allpeop" #freqy$exp_spec[which.max(freqy$N)]
  # itype<- "imptypdeat" #freqy$imp_type[which.max(freqy$N)]
  # 
  # for(hAb in unique(cntimps$haz_Ab)){
  #   ADM$tmp<-unname(unlist(parallel::mclapply(ADM$ADMcode,function(codie){
  #     tryCatch(cntimps%>%filter(exp_spec==espec & imp_type==itype & haz_Ab==hAb &
  #                                 ev_sdate>1975 & 
  #                                 grepl(codie,imp_spat_ID,ignore.case = T))%>%
  #                reframe(Rate=length(haz_Ab)/unique(hazcov$coverage[hazcov$haz_Ab==hAb & 
  #                                                                                        hazcov$imp_src_db=="Desinventar"]))%>%
  #                pull(Rate),
  #              error=function(e) NA)
  #   },mc.cores=round(ncores/2))))
  #   nomnom<-paste0(hAb,"_Rate")
  #   ADM$tmp[ADM$tmp==0]<-NA
  #   colnames(ADM@data)[ncol(ADM@data)]<-nomnom
  # }
  # 
  # 
  # for(espec in unique(cntimps$exp_spec)){
  #   for(itype in unique(cntimps$imp_type)){
  #     for(hAb in unique(cntimps$haz_Ab)){
  #       ADM$tmp<-unname(unlist(parallel::mclapply(ADM$ADMcode,function(codie){
  #         tryCatch(cntimps%>%filter(exp_spec==espec & imp_type==itype & haz_Ab==hAb &
  #                                     ev_sdate>1975 & 
  #                                     grepl(codie,imp_spat_ID,ignore.case = T))%>%
  #                    reframe(Rate=length(haz_Ab)/unique(hazcov$coverage[hazcov$haz_Ab==hAb & 
  #                                                                         hazcov$imp_src_db=="Desinventar"]))%>%
  #                    pull(Rate),
  #                  error=function(e) NA)
  #       },mc.cores=round(ncores))))
  #       nomnom<-paste0(hAb,"_",espec,"_",itype,"_AAImpact")
  #       ADM$tmp[ADM$tmp==0]<-NA
  #       colnames(ADM@data)[ncol(ADM@data)]<-nomnom
  #     }
  #   }
  # }
  # 
  # lvl<-ADM@data%>%group_by(ADMlevel)%>%reframe(lvl=sum(EP_Rate>0,na.rm = T)); lvl<-lvl$ADMlevel[which.max(lvl$lvl)]
  # 
  # ADM%>%sf::st_as_sf()%>%
  #   filter(ADMlevel==2)%>%
  #   ggplot() + 
  #   geom_sf(aes(fill=EP_Rate))+scale_fill_gradient(name = "Count", trans = "log10")
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # ADM$deathsRP<-sapply(ADM$ADMcode,function(codie){
  #   output<-tryCatch(cntimps%>%filter(exp_spec=="expspec_allpeop" & imp_type=="imptypdeat" &
  #                                       ev_sdate>1975 & !is.na(imp_value) &
  #                                       grepl(codie,imp_spat_ID,ignore.case = T))%>%
  #                      impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
  #   ifelse(output$N>30,output$impRP,NA)
  # })
  # 
  # ADM$costRP<-sapply(ADM$ADMcode,function(codie){
  #   output<-tryCatch(cntimps%>%filter(imp_subcats%in%c("impecotot","impecodirtot") & imp_type=="imptypcost" & 
  #                                       ev_sdate>1975 & !is.na(imp_value) &
  #                                       grepl(codie,imp_spat_ID,ignore.case = T))%>%
  #                      impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
  #   ifelse(output$N>30,output$impRP/1e6,NA)
  # })  
  # 
  # # Extract the bounding box of the admin boundaries
  # # bbox<-expandBbox(unlist(unname(ExtractBBOXpoly(ADM)[1,2:5])),3)
  # 
  # ggplot(ADM) + 
  #   geom_sf(aes(fill=Allrecords))+scale_fill_gradient(name = "Count", trans = "log10")
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # mad_map <- ggmap::get_stadiamap(expandBbox(ADM@bbox,3),maptype = "terrain",zoom=5)
  # 
  # p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
  # 
  # q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
  #   geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
  #   scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  # ggsave(paste0("Allrecords_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  # 
  # q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
  #   geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
  #   scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  # ggsave(paste0("Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  # 
  # q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
  #   geom_sf(aes(fill=deathsRP), color = "grey30", linewidth=0.1, inherit.aes = FALSE) +
  #   scale_fill_gradient("Exp. No. Deaths 5Yr RP",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  # ggsave(paste0("DeathsRP_Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  # 
  # q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
  #   geom_sf(aes(fill=costRP), color = "grey30", linewidth=0.1, inherit.aes = FALSE) +
  #   scale_fill_gradient("Exp. Cost 5Yr RP [Millions Local Curr]",high="chartreuse",low="chartreuse4",trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  # ggsave(paste0("CostRP_Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  # 
  # sapply(lhaz,function(hazzie){
  #   
  #   ADM$records<-sapply(ADM$ADMcode,function(codie){
  #     sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
  #   })
  #   
  #   q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
  #     geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+
  #     scale_fill_gradient(paste0("No. Records - ",hazzie), high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  #   ggsave(paste0(hazzie,"_records_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  #   
  #   q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
  #     geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+
  #     scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  #   ggsave(paste0(hazzie,"_records_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  #   
  #   return(T)},simplify = T)
}))

geojsonio::topojson_write(DesADM,geometry = "polygon",file = "COL_ADM.topojson")
geojsonio::geojson_write(DesADM,file="./CleanedData/MostlyImpactData/COL_ADM.geojson",geometry = "polygon")


Monty<-jsonlite::fromJSON("./CleanedData/Monty_cleaned_2024-04-25.json")

hazcov<-Monty%>%group_by(imp_src_db,haz_Ab)%>%
  reframe(coverage=as.numeric(max(year) - min(year)))


GenExceedance_ISO<-function(Monty,iso3,imp_db="EMDAT",imp_type="Deaths",exp_spec=NULL,yr=1990,hazs=NULL, plotty=T, loggie=F,rotty=0 ){
  # Extract the data
  out<-Dessie%>%filter(imp_src_db==imp_db & 
                        year>yr &
                        imp_ISO3s==iso3)
  
  if(!is.null(imp_type)) out%<>%filter(imp_type==imp_type)
  if(!is.null(exp_spec)) out%<>%filter(exp_spec==exp_spec)
  if(!is.null(hazs)) out%<>%filter(haz_Ab%in%hazs)
  
  out%<>%
    group_by(haz_Ab)%>%
    reframe(coverage=unique(hazcov$coverage[hazcov$haz_Ab==unique(haz_Ab) & 
                                              hazcov$imp_src_db==hazcov$imp_src_db]),
            N=n(),
            impact=sort(imp_value),
            probability=n():1/unique(coverage))
  
  if(plotty) {
    p<-out%>%ggplot(aes(impact,probability,colour=haz_Ab))+
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
  } else return(out)
}

GenExceedance_ISO(Monty,"CRI")
GenExceedance_ISO(Monty,"PER",loggie=T)
GenExceedance_ISO(Monty,"PER","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted")
GenExceedance_ISO(Monty,"CRI","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted")
GenExceedance_ISO(Monty,"PER","EMDAT",imp_type="Total Affected",hazs=c("CW"),rotty=20)

lapply(centrams,function(iso3){
  do.call(rbind,lapply(unique(Dessie$imp_src_db),function(db){
    do.call(rbind,lapply(unique(Dessie$Impact[Dessie$imp_src_db==db]),function(db){
      GenExceedance_ISO(Monty,"PER","EMDAT",imp_type="Loss (Cost)",exp_spec = "Total Direct Costs Inflation-Adjusted",plotty = F)
    }))
  }))
})

























#@@@@@@@@@@@@@@@@@@@@@ MONTY WORKSHOP MAY 2024 @@@@@@@@@@@@@@@@@@@@@#
##### Data ##### 
# - Wrangle Desinventar, GDACS + hazard forecast data
# - Get the GAUL data somehow
# - Get the Desinventar map data
##### PowerBI ##### 
# Tables: average country metrics per hazard, per metric type
# Loss-exceedance curves
# Sub-national maps of impact data  
# Seasonality, per country, per hazard, per impact type (inc. #events)

##### DONE #####
# Loss-exceedance curves (full dataset)
# Tables: average country metrics per hazard, per metric type
# Seasonality, per country, per hazard, per impact type (inc. #events)
# Sub-national maps of impact data 


# APRO workshop data
Monty<-readRDS("./CleanedData/Monty_APROcleaned_2024-05-05.RData")
# Data coverage
hazcov<-Monty%>%group_by(imp_src_db,Hazard)%>%
  reframe(coverage=as.numeric(max(year) - min(year)))
# Loss exceedance curves
lossy<-Monty%>%filter(ev_ISO3s%in%centrams)%>%
  arrange(ev_sdate)%>%
  group_by(Database,ev_ISO3s,Hazard,haz_Ab_grp,Impact)%>%
  reframe(coverage=max(unique(hazcov$coverage[hazcov$Hazard==unique(Hazard) & 
                                            hazcov$imp_src_db==unique(Database)])),
          impact_value=sort(imp_value),
          N=n():1,
          probability=n():1/unique(coverage))%>%
  dplyr::select(-coverage)%>%
  setNames(c("Database","ISO3","Hazard_Type","Hazard_Code","Impact_Type","Impact_Value","No_Impacts","Frequency_Occurrence"))
# Write it out!
write_csv(lossy,"./CleanedData/MostlyImpactData/APRO_Loss.csv")

# Frequency table, Average Annual Impact and Return Periods
freqy<-lossy%>%
  group_by(Database,ISO3,Hazard_Type,Hazard_Code,Impact_Type)%>%
  reframe(Count=max(No_Impacts),
          AAI=mean(Impact_Value),
          RP1=ifelse(min(Frequency_Occurrence)>1 | max(Frequency_Occurrence)<1,min(Impact_Value,na.rm=T),
                         (splinefun(x=Frequency_Occurrence,
                                     y=Impact_Value))(1)),
          RP2=ifelse(min(Frequency_Occurrence)>(1/2) | max(Frequency_Occurrence)<(1/2),min(Impact_Value,na.rm=T),
                     (splinefun(x=Frequency_Occurrence,
                                 y=Impact_Value))(1/2)),
          RP5=ifelse(min(Frequency_Occurrence)>(1/5) | max(Frequency_Occurrence)<(1/5),min(Impact_Value,na.rm=T),
                     (splinefun(x=Frequency_Occurrence,
                                 y=Impact_Value))(1/5)),
          RP10=ifelse(min(Frequency_Occurrence)>(1/10) | max(Frequency_Occurrence)<(1/10),min(Impact_Value,na.rm=T),
                     (splinefun(x=Frequency_Occurrence,
                                 y=Impact_Value))(1/10)),
          RP20=ifelse(min(Frequency_Occurrence)>(1/20) | max(Frequency_Occurrence)<(1/20),min(Impact_Value,na.rm=T),
                     (splinefun(x=Frequency_Occurrence,
                                 y=Impact_Value))(1/20)))%>%
  setNames(c("Database","ISO3","Hazard_Type","Hazard_Code","Impact_Type","No_Impacts",
             "Average_Annual_Impact","Once_in_1_Year",
             "Once_in_2_Year","Once_in_5_Year",
             "Once_in_10_Year","Once_in_20_Year"))
freqy%<>%mutate_at(grep(colnames(freqy),pattern = "Once_in"),function(x) pmax(x,0))

# freqy<-lossy%>%
#   group_by(Database,ISO3,Hazard_Type,Hazard_Code,Impact_Type)%>%
#   reframe(Count=max(No_Impacts), 
#           AAI=mean(Impact_Value),
#           RP1=splinefun(x=Frequency_Occurrence, 
#                                 y=Impact_Value)(1),
#           RP2=splinefun(x=Frequency_Occurrence, 
#                                 y=Impact_Value)(1/2),
#           RP5=splinefun(x=Frequency_Occurrence, 
#                                 y=Impact_Value)(1/5),
#           RP10=splinefun(x=Frequency_Occurrence, 
#                                  y=Impact_Value)(1/10),
#           RP20=splinefun(x=Frequency_Occurrence, 
#                                  y=Impact_Value)(1/20))%>%
#   setNames(c("Database","ISO3","Hazard_Type","Hazard_Code","Impact_Type","No_Impacts",
#              "Average_Annual_Impact","Once_in_1_Year",
#              "Once_in_2_Year","Once_in_5_Year",
#              "Once_in_10_Year","Once_in_20_Year"))
# freqy%<>%mutate_at(grep(colnames(freqy),pattern = "Once_in"),function(x) pmax(x,0))

  # filter(!is.na(Once_in_1_Year))
# Write it out!
write_csv(freqy,"./CleanedData/MostlyImpactData/APRO_FreqTabs.csv")

# For the climate change related hazards, add in the seasonality
Seasonality<-Monty%>%
  mutate(Month=as.integer(month(ev_sdate)))%>%
  group_by(Database,ev_ISO3s,Hazard,haz_Ab_grp,Impact,Month)%>%
  reframe(Evs=length(year),AMI=mean(imp_value))

# Smooth this using a 1-month-either-side rolling average
Seasonality%<>%rbind(Seasonality%>%mutate(Month=Month-12),
                     Seasonality%>%mutate(Month=Month+12))%>%
  arrange(Month)%>%
  group_by(Database,ev_ISO3s,Hazard,haz_Ab_grp,Impact)%>%
  mutate(Month=Month,
         Evs=zoo::rollapply(Evs,3,function(x) mean(x,na.rm = T),align='center',fill=NA),
         AMI=zoo::rollapply(AMI,3,function(x) mean(x,na.rm = T),align='center',fill=NA))%>%
  filter(Month>=1 & Month<=12)%>%
  setNames(c("Database","ISO3","Hazard_Type","Hazard_Code","Impact_Type","Month","No_Impacts","Average_Monthly_Impact"))
# Write it out!
write_csv(Seasonality,"./CleanedData/MostlyImpactData/APRO_Seasonality.csv")

Seasonality%>%filter(Hazard=="Flood" & ev_ISO3s=="NPL")%>%
  ggplot()+geom_smooth(aes(Month,AMI,colour=Database))+
  scale_x_continuous(breaks=1:12,labels=month.abb)+
  scale_y_log10()+
  # geom_line(aes(Month,AMI,colour=ev_ISO3s))+
  facet_wrap(~Impact,scales = "free_y")

Seasonality%>%filter(Impact=="People (All Demographics) Deaths [count]")%>%
  ggplot()+geom_smooth(aes(Month,Evs,colour=Database))+
  scale_x_continuous(breaks=1:12,labels=month.abb)+
  scale_y_log10()+
  # geom_line(aes(Month,AMI,colour=ev_ISO3s))+
  facet_wrap(~Hazard,scales = "free_y")































Monty<-convGOApp_Monty(taby=T)
# EM-DAT
Monty%<>%dplyr::bind_rows(convEMDAT_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# IDMC GIDD
Monty%<>%dplyr::bind_rows(convGIDD_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# IDMC IDU
Monty%<>%dplyr::bind_rows(convIDU_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
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
# Desinventar
Monty%<>%dplyr::bind_rows(convDessie_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))
# DFO
Monty%<>%dplyr::bind_rows(convDFO_Monty(taby=T)%>%
                            mutate_if(is.Date,as.character)%>%
                            mutate(across(c(ext_ID,haz_maxvalue),as.character))%>%
                            dplyr::select(-any_of(c("all_ext_IDs","imp_spat_ID","haz_ext_IDs"))))


Monty%<>%filter(imp_value!=0 & !is.na(haz_spec) & !is.na(imp_value))

saveRDS(Monty,"./CleanedData/Monty_2024-06-11_tab.RData")








Monty<-readRDS("./CleanedData/Monty_2024-06-11_tab.RData")

Monty$year<-AsYear(Monty$imp_sdate)
Monty%<>%filter(!is.na(year))

Monty$haz_Ab_grp<-Monty$haz_Ab; 
Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("HW","CW","HT")]<-"ET"
Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("FR")]<-"WF"
Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("WV","WA")]<-"SS"
Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("MM","MS","SL","AV","ER")]<-"LS"
Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("TC,FL","EC")]<-"TC"
Monty%<>%filter(!haz_Ab%in%c("EP","IN","SN"))


taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Exposure type class
exp_class<-taxies%>%filter(list_name=="exp_specs")%>%dplyr::select(name,label)%>%
  setNames(c("exp_spec","Exposure_Type"))
# Impact type class
imp_class<-taxies%>%filter(list_name=="imp_type")%>%dplyr::select(name,label)%>%
  setNames(c("imp_type","Impact_Type"))
# Impact units
units_info<-taxies%>%filter(list_name=="measunits")%>%
  dplyr::select(name,label)%>%
  setNames(c("unit_code","Impact_Unit"))%>%na.omit()%>%distinct()
# Add impact type label
Monty%<>%left_join(imp_class,by="imp_type")
# Add exposure type label
Monty%<>%left_join(exp_class,by="exp_spec")
# Add impact units label
Monty%<>%left_join(units_info,by=join_by("imp_units"=="unit_code"))
# Create a single label out of the three variables above
Monty%<>%mutate(Impact=paste0(Exposure_Type," ",Impact_Type," [",Impact_Unit,"]"))
# Database
Monty%<>%mutate(Database=paste0(imp_src_db," - ",imp_src_org))
# Neaten up some names
Monty$Impact<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(Monty$Impact," \\(All Demographics\\)",""),
                                                              " \\(Cost\\)",""),"Inflation-Adjusted","Inf-Adj"),"\\(Unspecified-Inflation-Adjustment\\)","(Unspec. Inf-Adj)")

Monty%<>%filter((imp_src_db=="Desinventar" & year>1980) |
                  (imp_src_db=="GLIDE" & year>1980) |
                  (imp_src_db=="EMDAT" & year>1990) |  
                  (imp_src_db=="GIDD" & year>2016) | 
                  (imp_src_db=="DFO" & year>1990) |
                  !imp_src_db%in%c("Desinventar","EMDAT","GIDD","DFO"))

Monty%<>%filter(imp_value!=0 & !is.na(imp_value))

haz_Ab_lab<-c("DR"="Drought",
              "EQ"="Earthquake",
              "ET"="Extreme Temperature",
              "FL"="Flood",
              "LS"="Landslide",
              "ST"="Storm",
              "SS"="Storm Surge",
              "TC"="Tropical Cyclone",
              "TO"="Tornado",
              "TS"="Tsunami",
              "WF"="Wildfire",
              "VW"="Violent Wind",
              "VO"="Volcanic Activity")

hazcov<-data.frame()

for(haz in names(haz_Ab_lab)){
  hazcov%<>%rbind(Monty%>%filter(grepl(haz,haz_Ab_grp))%>%
                    group_by(Database)%>%
                    reframe(Hazard=haz,coverage=as.numeric(max(year) - min(year)))
  )
}

Seasonality <- freqy <- lossy <- data.frame()

for(haz in names(haz_Ab_lab)){
  for(iso in unique(Monty$imp_ISO3s[nchar(Monty$imp_ISO3s)==3])){
    
    print(paste0("haz = ",haz," and ISO = ",iso))
    
    subMon<-Monty%>%filter(grepl(haz,haz_Ab_grp) & grepl(iso,imp_ISO3s))
    
    if(nrow(subMon)==0) next
    
    lossy%<>%rbind(subMon%>%arrange(imp_sdate)%>%
      group_by(Database,Impact)%>%
      reframe(ISO3=iso,haz_Ab=haz,
              coverage=max(unique(hazcov$coverage[hazcov$Hazard==haz & 
                                              hazcov$Database==unique(Database)])),
              impact_value=sort(imp_value),
              N=n():1,
              probability=n():1/unique(coverage))%>%
      dplyr::select(-coverage)%>%
      setNames(c("Database","Impact_Type","ISO3","Hazard_Code",
                 "Impact_Value","No_Impacts","Frequency_Occurrence")))
    
    freqy%<>%rbind(lossy%>%
      group_by(Database,Impact_Type,ISO3,Hazard_Code)%>%
      reframe(Count=max(No_Impacts),
              AAI=mean(Impact_Value),
              RP1=ifelse(min(Frequency_Occurrence)>1 | max(Frequency_Occurrence)<1,min(Impact_Value,na.rm=T),
                         (splinefun(x=Frequency_Occurrence,
                                    y=Impact_Value))(1)),
              RP2=ifelse(min(Frequency_Occurrence)>(1/2) | max(Frequency_Occurrence)<(1/2),min(Impact_Value,na.rm=T),
                         (splinefun(x=Frequency_Occurrence,
                                    y=Impact_Value))(1/2)),
              RP5=ifelse(min(Frequency_Occurrence)>(1/5) | max(Frequency_Occurrence)<(1/5),min(Impact_Value,na.rm=T),
                         (splinefun(x=Frequency_Occurrence,
                                    y=Impact_Value))(1/5)),
              RP10=ifelse(min(Frequency_Occurrence)>(1/10) | max(Frequency_Occurrence)<(1/10),min(Impact_Value,na.rm=T),
                          (splinefun(x=Frequency_Occurrence,
                                     y=Impact_Value))(1/10)),
              RP20=ifelse(min(Frequency_Occurrence)>(1/20) | max(Frequency_Occurrence)<(1/20),min(Impact_Value,na.rm=T),
                          (splinefun(x=Frequency_Occurrence,
                                     y=Impact_Value))(1/20)))%>%
      setNames(c("Database","Impact_Type","ISO3","Hazard_Code","No_Impacts",
                 "Average_Annual_Impact","Once_in_1_Year",
                 "Once_in_2_Year","Once_in_5_Year",
                 "Once_in_10_Year","Once_in_20_Year")))
    
    seasy<-subMon%>%
      mutate(Month=as.integer(month(imp_sdate)))%>%
      group_by(Database,Impact,Month)%>%
      reframe(Evs=length(year),AMI=mean(imp_value))
    
    seasy%<>%rbind(seasy%>%mutate(Month=Month-12),
                         seasy%>%mutate(Month=Month+12))%>%
      arrange(Month)%>%
      group_by(Database,Impact)%>%
      mutate(ISO3=iso, Hazard_Type=haz_Ab_lab[haz], Hazard_Code=haz, Month=Month,
             Evs=zoo::rollapply(Evs,3,function(x) mean(x,na.rm = T),align='center',fill=NA),
             AMI=zoo::rollapply(AMI,3,function(x) mean(x,na.rm = T),align='center',fill=NA))%>%
      filter(Month>=1 & Month<=12)%>%
      setNames(c("Database","Impact_Type","ISO3","Hazard_Type","Hazard_Code",
                 "Month","No_Impacts","Average_Monthly_Impact"))
    
    Seasonality%<>%rbind(seasy)
    
  }
}

lossy$Impact_Value[is.infinite(lossy$Impact_Value)]<-NA_real_
freqy%<>%mutate_if(is.numeric,function(x) {if(any(is.infinite(x))) x[is.infinite(x)]<-NA_real_; return(x)})
Seasonality$Average_Monthly_Impact[is.infinite(Seasonality$Average_Monthly_Impact)]<-NA_real_

freqy%<>%distinct()
lossy%<>%distinct()
Seasonality%<>%distinct()

write_csv(lossy,"./CleanedData/MostlyImpactData/Monty_Loss_premod.csv")
write_csv(freqy,"./CleanedData/MostlyImpactData/Monty_FreqTabs_premod.csv")
write_csv(Seasonality,"./CleanedData/MostlyImpactData/Monty_Seasonality_premod.csv")

lossy%<>%left_join(data.frame(Hazard_Code=names(haz_Ab_lab),
                          Hazard=unname(haz_Ab_lab)),by="Hazard_Code")

freqy%<>%mutate_at(grep(colnames(freqy),pattern = "Once_in"),function(x) pmax(x,0))
freqy%<>%left_join(data.frame(Hazard_Code=names(haz_Ab_lab),
                              Hazard=unname(haz_Ab_lab)),by="Hazard_Code")

freqy %<>%
  mutate(
    Once_in_1_Year = if_else(!is.na(Once_in_2_Year) & Once_in_1_Year == Once_in_2_Year, NA_real_, Once_in_1_Year),
    Once_in_2_Year = if_else(!is.na(Once_in_5_Year) & Once_in_2_Year == Once_in_5_Year, NA_real_, Once_in_2_Year),
    Once_in_5_Year = if_else(!is.na(Once_in_10_Year) & Once_in_5_Year == Once_in_10_Year, NA_real_, Once_in_5_Year),
    Once_in_10_Year = if_else(!is.na(Once_in_20_Year) & Once_in_10_Year == Once_in_20_Year, NA_real_, Once_in_10_Year)
  )%>%
  mutate(
    Once_in_20_Year = if_else(!is.na(Once_in_10_Year) & Once_in_10_Year > Once_in_20_Year, Once_in_10_Year, Once_in_20_Year),
    Once_in_10_Year = if_else(!is.na(Once_in_10_Year) & Once_in_10_Year >= Once_in_20_Year, NA_real_, Once_in_10_Year),
    Once_in_10_Year = if_else(!is.na(Once_in_5_Year) & Once_in_5_Year > Once_in_10_Year, Once_in_5_Year, Once_in_10_Year),
    Once_in_5_Year = if_else(!is.na(Once_in_5_Year) & Once_in_5_Year >= Once_in_10_Year, NA_real_, Once_in_5_Year),
    Once_in_5_Year = if_else(!is.na(Once_in_2_Year) & Once_in_2_Year > Once_in_5_Year, Once_in_2_Year, Once_in_5_Year),
    Once_in_2_Year = if_else(!is.na(Once_in_2_Year) & Once_in_2_Year >= Once_in_5_Year, NA_real_, Once_in_2_Year),
    Once_in_2_Year = if_else(!is.na(Once_in_1_Year) & Once_in_1_Year > Once_in_2_Year, Once_in_1_Year, Once_in_2_Year),
    Once_in_1_Year = if_else(!is.na(Once_in_1_Year) & Once_in_1_Year >= Once_in_2_Year, NA_real_, Once_in_1_Year)
  )

write_csv(lossy,"./CleanedData/MostlyImpactData/Monty_Loss.csv")
write_csv(freqy,"./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")
write_csv(Seasonality,"./CleanedData/MostlyImpactData/Monty_Seasonality.csv")












freqy<-read.csv("./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")%>%
  dplyr::select(Hazard,Hazard_Code,Impact_Type,Database,ISO3,No_Impacts,Once_in_5_Year)
freqy$ISO3[freqy$ISO3=="ZAR"]<-"COD"
# Make the summary from the return period data
lDREF<-freqy%>%filter(Impact_Type=="People Deaths [count]" &
                        !Hazard_Code%in%c("DR","TO","TS","SS","VO","VW"))%>%
  group_by(Hazard,Impact_Type,ISO3)%>%
  reframe(Once_in_5_Year=max(Once_in_5_Year,na.rm = T))
# Where NAs were present in the Once_in_5_Year column
lDREF$Once_in_5_Year[is.infinite(lDREF$Once_in_5_Year)]<-0
# Pair with population data
popy<-wbstats::wb_data(indicator = "SP.POP.TOTL")%>%
  rename("Population"="SP.POP.TOTL",
         "ISO3"="iso3c",
         "Country_Territory"="country",
         "year"="date")%>%
  dplyr::select(ISO3,Country_Territory,Population,year)%>%
  group_by(ISO3,Country_Territory)%>%
  reframe(Population=Population[which.max(year)])
# Merge them
lDREF%<>%left_join(popy,by="ISO3")
# Check for countries that have no population data
unique(lDREF$ISO3[is.na(lDREF$Population)])[is.na(convIso3Country(unique(lDREF$ISO3[is.na(lDREF$Population)])))]
# Where NAs were present in the Once_in_5_Year column
lDREF$Once_in_5_Year[is.infinite(lDREF$Once_in_5_Year)]<-0
# Create the per-capita deaths variable
lDREF%<>%mutate(Once_in_5_Year_pCap=1e6*Once_in_5_Year/Population)

wb<-openxlsx::createWorkbook()
# Create all the worksheets
wb%>%openxlsx::addWorksheet("Deaths")
wb%>%openxlsx::addWorksheet("Deaths Per Capita")
# Reduce to the top-20 worst, wrt deaths per capita
outDREF<-lDREF%>%
  arrange(Hazard,desc(Once_in_5_Year_pCap)) %>%
  group_by(Hazard, Impact_Type) %>%
  slice_head(n = 20) %>%
  mutate(Once_in_5_Year_pCap=round(case_when(
    Once_in_5_Year_pCap==0 ~ runif(n(),0.5), TRUE ~ Once_in_5_Year_pCap
  ),3))%>%
  arrange(Hazard,desc(Once_in_5_Year_pCap)) %>%
  mutate(Ranking=1:20)%>%
  ungroup()%>%
  dplyr::select(Hazard,Impact_Type,ISO3,Country_Territory,Population,Once_in_5_Year_pCap,Ranking)%>%
  setNames(c("Hazard","Impact Type","ISO3 Code","Country/Territory","Population","One-in-Five Year Impact, Per Capita [Per Million]","Ranking"))
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths Per Capita",
                    outDREF, 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Reduce to the top-20 worst, wrt deaths
outDREF<-lDREF%>%
  arrange(Hazard,desc(Once_in_5_Year)) %>%
  group_by(Hazard, Impact_Type) %>%
  slice_head(n = 20) %>%
  mutate(Once_in_5_Year=round(case_when(
    Once_in_5_Year==0 ~ runif(n(),0.5), TRUE ~ Once_in_5_Year
  ),3))%>%
  arrange(Hazard,desc(Once_in_5_Year)) %>%
  mutate(Ranking=1:20)%>%
  ungroup()%>%
  dplyr::select(Hazard,Impact_Type,ISO3,Country_Territory,Population,Once_in_5_Year,Ranking)%>%
  setNames(c("Hazard","Impact Type","ISO3 Code","Country/Territory","Population","One-in-Five Year Impact","Ranking"))
# Write to Workbook
openxlsx::writeData(wb, 
                    sheet="Deaths",
                    outDREF, 
                    headerStyle=openxlsx::createStyle(textDecoration = "Bold"),
                    keepNA = F)
# Save it out!
openxlsx::saveWorkbook(wb,"./CleanedData/MostlyImpactData/DREF_EAP-Country-Prioritisation.xlsx",overwrite = T)



# Pair with country names




# Pair with INFORM data & HDI







# ADM<-geojsonio::geojson_read("./CleanedData/SocioPoliticalData/EMDAT/COL/ADM_COL.geojson", 
#                              what = "sp")
# 
# cntimps$imp_spat_ID<-NA_character_
# 
# for (j in 1:length(ADM@polygons)){
#   
#   if(sum(is.na(cntimps$imp_spat_ID))) next
#   
#   indy<-do.call(cbind,parallel::mclapply(ADM@polygons[[j]]@Polygons, function(poly){
#     any(sp::point.in.polygon(cntimps$imp_lon,
#                          cntimps$imp_lat,
#                          poly@coords[,1],
#                          poly@coords[,2])>0)
#   },mc.cores=ncores))
#   
#   iii<-apply(indy,1,any)
#   
#   cntimps$imp_spat_ID[iii]<-ADM@data$ADM2_CODE[j]  
#     
# }




# imp_spat_ID<-parallel::mclapply(1:length(ADM@polygons), function(j){
#   
#   poly<-ADM@polygons[[j]]
#   
#   imp_spat_ID<-rep(NA_character_,nrow(cntimps))
#   
#   out<-lapply(poly@Polygons, function(ppp){
#     sp::point.in.polygon(cntimps$imp_lon,
#                              cntimps$imp_lat,
#                              ppp@coords[,1],
#                              ppp@coords[,2])>0
#   })
#   
#   if(length(poly@Polygons)==1) {
#     imp_spat_ID[out[[1]]]<-ADM@data$ADM2_CODE[j]  
#     return(imp_spat_ID)
#   } else {
#     iii<-apply(do.call(cbind,out),1,any)
#     imp_spat_ID[iii]<-ADM@data$ADM2_CODE[j]  
#     return(imp_spat_ID)
#   }
# },mc.cores=ncores)
# 
































