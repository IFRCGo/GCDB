WBcall<-function(syear,indicator,fyear=NULL){
  if(is.null(fyear)) value<-wbstats::wb_data(indicator = indicator, 
                                             start_date = as.character(syear-5L), 
                                             date_as_class_date = T)
  value<-wbstats::wb_data(indicator = indicator, 
                          start_date = as.character(syear-5L), 
                          end_date = as.character(fyear),
                          date_as_class_date = T)
  value%>%transmute(iso3=iso3c,date=date,value=get(indicator))
}

# "EN.POP.DNST" -> Population Density
GetWBPDens<-function(syear,fyear){
  return(WBcall(syear,fyear,"EN.POP.DNST"))
}

# "SP.POP.TOTL" -> Total Population
GetWBPop<-function(syear,fyear){
  return(WBcall(syear,fyear,"SP.POP.TOTL"))
}

# "NY.GDP.MKTP.PP.KD" -> GDP-PPP (2011 US$)
GetWBGDP<-function(syear,fyear){
  return(WBcall(syear,fyear,"NY.GDP.MKTP.PP.KD"))
}

# "GCI.2NDPILLAR.XQ" -> Physical Infrastructure
GetWBInfra<-function(syear,fyear){
  return(WBcall(syear,fyear,"GCI.2NDPILLAR.XQ"))
}

# lister<-c("SI.DST.04TH.20","SI.DST.10TH.10","SI.DST.05TH.20","SI.DST.FRST.10","SI.DST.FRST.20","SI.DST.02ND.20","SI.DST.03RD.20")
# for (ind in lister){
#   ttt<-WBcall(2008,2020,ind)
#   sumz<-ttt%>%group_by(iso3)%>%summarise(nans=sum(!is.na(value)),len=length(value))
#   print(ind)
#   print(mean(sumz$nans/sumz$len))
#   print("")
#   # ggplot(sumz,aes(nans/len))+geom_histogram()
# }
# ggplot(sumz,aes(100*nans/len))+geom_histogram() + xlab("Percentage of non-empty values") +
#   ylab("Frequency") + ggtitle("Income Dist. Entries in World Bank 2008-2020") +
#   theme(plot.title = element_text(hjust = 0.5))

normaliseWB<-function(ndata,iso,dater,normdate=as.Date("2015-01-01")){
  
  mindate<-min(ndata$date)
  ndata%<>%filter(iso3%in%iso&!is.na(value))%>%mutate(day=as.numeric(date-min(date)))
  # if(length(ndata$value)<=1) return(data.frame(iso3=iso,factor=1.))
  val<-data.frame()
  for (iso3c in iso){
    nd<-filter(ndata,iso3==iso3c)
    if(all(is.na(nd$value))|length(nd$value)<=1) {
      print(paste0("Not enough data found for country ",iso3c," for normalisation spline for country indicators, factor set as 1"))
      # print(nd$value)
      val%<>%rbind(data.frame(iso3=iso3c,factor=1.))
      next
    }
    func = tryCatch(splinefun(x=nd$day,y=nd$value, method="natural"),error = function(e) NULL)
    if(is.null(func)) {
      stop(paste0("No spline function over WB data possible for country ",iso3c))
    }
    val%<>%rbind(data.frame(iso3=iso3c,factor=func(as.numeric(dater-min(nd$date)))/func(as.numeric(normdate-min(nd$date)))))
  }
  return(val)
}

# if(!is.null(date)) {
#   year<-AsYear(date)
#   nPop<-GetWBPop(directory,(year-5),year)
#   factor<-normaliseWB(nPop,iso = iso,date = date)}
# else factor<-1
# population<-population*factor

InterpWB<-function(iso3c,date,funcy,normdate=as.Date("2015-01-01")){
  year<-AsYear(date)
  dataz<-do.call(funcy,list(syear=(year-5),fyear=(AsYear(Sys.Date())-1)))
  normaliseWB(dataz,iso = iso3c,dater = date, normdate=normdate)
}

InterpPopWB<-function(iso3c,date,normdate=as.Date("2015-01-01")){
  return(InterpWB(iso3c,date,GetWBPop,normdate=normdate))
}
InterpGDPWB<-function(iso3c,date,normdate=as.Date("2015-01-01")){
  return(InterpWB(iso3c,date,GetWBGDP,normdate=normdate))
}


GetWB_Vals<-function(DispData){
  
  fresh_indicators <- wbstats::wb_indicators()
  # View(fresh_indicators)
  # write_csv(fresh_indicators[,1:4],"./IIDIPUS_Input/WB_Indicators.csv")
  
  srcs<-c(
    "Gender Statistics",
    "Global Financial Development",
    "Health Nutrition and Population Statistics",
    "Human Capital Index",
    "Poverty and Equity",
    "Statistical Capacity Indicators",
    "World Development Indicators"
  )
  indies<-fresh_indicators%>%filter(source%in%srcs)
  # write_csv(indies[1:5],"./IIDIPUS_Input/WB-WorldDevelopment_Indicators.csv")
  # View(indies)
  
  syear<-AsYear(min(as.Date(DispData$sdate)))-5
  fyear<-AsYear(max(as.Date(DispData$sdate)))
  
  nn<-ceiling(nrow(indies)/10)
  for (i in 0:(nn-1)){
    iii<-1:10+i*10
    if(max(iii)>nrow(indies)) iii<-(i*10+1):nrow(indies)
    inds<-indies$indicator_id[iii]
    
    dataz<-wbstats::wb_data(indicator = inds, 
                   start_date = as.character(syear), 
                   end_date = as.character(fyear),
                   date_as_class_date = T) %>%
      filter(iso3c%in%DispData$iso3) %>% dplyr::select(-c(iso2c,country,obs_resolution))
    
    if(i==0) CORY<-dataz else CORY%<>%cbind(dataz[3:(ncol(dataz)-1)])
    
  } 
  
  val<-data.frame()
  for(ev in unique(DispData$event_id)){
    subs<-DispData%>%filter(event_id==ev)
    for (is in unique(subs$iso3)){
      nd<-filter(CORY,iso3c==is)
      dater<-as.Date(subs$sdate[1])
      for(k in 3:ncol(nd)){
        inds<-colnames(nd)[k]
        value<-nd[,c(2,k)]; value<-value[!is.na(value[,2]),]
        if(nrow(value)==0) {
          val%<>%rbind(data.frame(eventid=ev,iso3=is,date=dater,npoints=0,
                                  indicator=inds,intrpval=NA,nearval=NA,neardate=NA,intrpsuc=F))
          next
        } else if(nrow(value)==1) {
          id<-which(!is.na(value[,2]))
          vv<-value[id,2]
          val%<>%rbind(data.frame(eventid=ev,iso3=is,date=dater,npoints=1,
                                  indicator=inds,intrpval=vv,nearval=vv,neardate=value[id,1],intrpsuc=F)) 
          next
        } else {
          
          value%<>%mutate(day=as.numeric(date-min(date)))
          id<-which.min(abs(value$date-dater))
          func = tryCatch(splinefun(x=value$day,y=value[,2], method="natural"),error = function(e) NULL)
          if(is.null(func)) {
            print(paste0("No spline for indicator ",inds,", eventid ",ev," in country ",is))
            val%<>%rbind(data.frame(eventid=ev,iso3=is,date=dater,npoints=nrow(value),
                                    indicator=inds,intrpval=value[id,2],nearval=value[id,2],
                                    neardate=value[id,1],intrpsuc=F))
          }
          val%<>%rbind(data.frame(eventid=ev,iso3=is,date=dater,npoints=nrow(value),
                                  indicator=inds,
                                  intrpval=func(as.numeric(dater-min(value$date))),
                                  nearval=value[id,2],
                                  neardate=value[id,1],intrpsuc=T))
          next
        }
      }
    }
  }
  
  val$grouping<-vapply(1:nrow(val),function(i) (strsplit(as.character(val$indicator[i]),split =  ".",fixed = T))[[1]][1],character(1))
  
  return(val)
}