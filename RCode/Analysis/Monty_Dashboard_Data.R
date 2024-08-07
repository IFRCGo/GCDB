#@@@@@ extract the current version of the Monty data @@@@@#
# (note that this should be extracting directly from the API when the realtime scripts are in place)
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

# saveRDS(Monty,"./CleanedData/Monty_2024-06-11_tab.RData")






Monty<-readRDS("./CleanedData/Monty_2024-07-29_tab.RData")

Monty$year<-AsYear(Monty$imp_sdate)
Monty%<>%filter(!is.na(year))

Monty$haz_Ab_grp<-Monty$haz_Ab; 
# Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("HW","CW","HT")]<-"ET"
Monty$haz_Ab_grp[Monty$haz_Ab_grp%in%c("HT")]<-"HW"
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
Monty$Database[Monty$imp_src_db=="Desinventar"]<-paste0(Monty$Database[Monty$imp_src_db=="Desinventar"]," - ",Monty$imp_ISO3s[Monty$imp_src_db=="Desinventar"])
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

haz_Ab_lab<-c("CW"="Coldwave",
              "DR"="Drought",
              "EQ"="Earthquake",
              "ET"="Extreme Temperature",
              "FF"="Flash Flood",
              "FL"="Flood",
              "HW"="Heatwave",
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
                    reframe(Hazard_Type=haz,coverage=as.numeric(max(year) - min(year)))
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
                             coverage=max(unique(hazcov$coverage[hazcov$Hazard_Type==haz & 
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
      setNames(c("Database","Impact_Type","Month","No_Impacts",
                 "Average_Monthly_Impact","ISO3","Hazard_Type","Hazard_Code"))
    
    Seasonality%<>%rbind(seasy)
  }
}

lossy$Database[grepl("Desinventar",lossy$Database)]<-"Desinventar - UNDRR"
freqy$Database[grepl("Desinventar",freqy$Database)]<-"Desinventar - UNDRR"
Seasonality$Database[grepl("Desinventar",Seasonality$Database)]<-"Desinventar - UNDRR"

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
                              Hazard_Type=unname(haz_Ab_lab)),by="Hazard_Code")

freqy%<>%mutate_at(grep(colnames(freqy),pattern = "Once_in"),function(x) pmax(x,0))
freqy%<>%left_join(data.frame(Hazard_Code=names(haz_Ab_lab),
                              Hazard_Type=unname(haz_Ab_lab)),by="Hazard_Code")

freqy %<>%
  # mutate(
  #   Once_in_1_Year = if_else(!is.na(Once_in_2_Year) & Once_in_1_Year == Once_in_2_Year, NA_real_, Once_in_1_Year),
  #   Once_in_2_Year = if_else(!is.na(Once_in_5_Year) & Once_in_2_Year == Once_in_5_Year, NA_real_, Once_in_2_Year),
  #   Once_in_5_Year = if_else(!is.na(Once_in_10_Year) & Once_in_5_Year == Once_in_10_Year, NA_real_, Once_in_5_Year),
  #   Once_in_10_Year = if_else(!is.na(Once_in_20_Year) & Once_in_10_Year == Once_in_20_Year, NA_real_, Once_in_10_Year)
  # )%>%
  mutate(
    Once_in_20_Year = if_else(!is.na(Once_in_10_Year) & !is.na(Once_in_20_Year) & Once_in_10_Year > Once_in_20_Year, Once_in_10_Year, Once_in_20_Year),
    # Once_in_10_Year = if_else(!is.na(Once_in_10_Year) & !is.na(Once_in_20_Year) & Once_in_10_Year >= Once_in_20_Year, NA_real_, Once_in_10_Year),
    Once_in_10_Year = if_else(!is.na(Once_in_5_Year) & !is.na(Once_in_10_Year) & Once_in_5_Year > Once_in_10_Year, Once_in_5_Year, Once_in_10_Year),
    # Once_in_5_Year = if_else(!is.na(Once_in_5_Year) & !is.na(Once_in_10_Year) & Once_in_5_Year >= Once_in_10_Year, NA_real_, Once_in_5_Year),
    Once_in_5_Year = if_else(!is.na(Once_in_2_Year) & !is.na(Once_in_5_Year) & Once_in_2_Year > Once_in_5_Year, Once_in_2_Year, Once_in_5_Year),
    # Once_in_2_Year = if_else(!is.na(Once_in_2_Year) & !is.na(Once_in_5_Year) & Once_in_2_Year >= Once_in_5_Year, NA_real_, Once_in_2_Year),
    Once_in_2_Year = if_else(!is.na(Once_in_1_Year) & !is.na(Once_in_2_Year) & Once_in_1_Year > Once_in_2_Year, Once_in_1_Year, Once_in_2_Year)
    # Once_in_1_Year = if_else(!is.na(Once_in_1_Year) & !is.na(Once_in_2_Year) & Once_in_1_Year >= Once_in_2_Year, NA_real_, Once_in_1_Year)
  )

write_csv(lossy,"./CleanedData/MostlyImpactData/Monty_Loss.csv")
write_csv(freqy,"./CleanedData/MostlyImpactData/Monty_FreqTabs.csv")
write_csv(Seasonality,"./CleanedData/MostlyImpactData/Monty_Seasonality.csv")

#@@@@@@@@@@@@@@@@@@@@@@@ SORT OUT THE CONFIDENCE INTERVALS @@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Left join the number of years of each database covering each hazard just to check
5*as.numeric(prop.test(1, 30, conf.level=0.95, correct = FALSE)$conf.int)

stop("Need to modify Desinventar range to be country-wise and also include the end date of the range")




















