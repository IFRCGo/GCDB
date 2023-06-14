
isos<-unique(impies$ISO3)

arCH<-do.call(rbind,lapply(isos,function(iso){
  ADM<-tryCatch(GetGADM(iso,level=0),error=function(e) NULL)
  if(is.null(ADM)) {
    tmp<-as.data.frame(t(c(iso,matrix(NA,1,10))))
    colnames(tmp)<-c("ISO3","N","max","more500","more100","more50",
                     "more10","Ptop3","Ptop5","Ptop10","Ptop20")
    return(tmp)
  }
  areas<-ExtractIndArea(ADM)
  # disties<-DistPoly(ADM)
  # ndist<-BboxArea(ADM)
  
  data.frame(ISO3=iso,
             N=length(areas),
             max=max(areas),
             more500=sum(areas>500),
             more100=sum(areas>100),
             more50=sum(areas>50),
             more10=sum(areas>10),
             Ptop3=sum(areas>500)
             Ptop5=ifelse(length(areas)>4,100*sort(areas,T)[5]/max(areas),NA),
             Ptop10=ifelse(length(areas)>9,100*sort(areas,T)[10]/max(areas),NA),
             Ptop20=ifelse(length(areas)>19,100*sort(areas,T)[20]/max(areas),NA),
             dist3=ifelse(length(areas)>2,max(disties[areas<sort(areas,T)[3]]),NA),
             dist5=ifelse(length(areas)>4,max(disties[areas<sort(areas,T)[5]]),NA),
             dist10=ifelse(length(areas)>9,max(disties[areas<sort(areas,T)[10]]),NA),
             dist20=ifelse(length(areas)>19,max(disties[areas<sort(areas,T)[20]]),NA))
}))

print(paste0(sum(is.na(arCH$N))," countries with no ADM data from GADM"))

arCH%>%arrange(max)%>%slice(1:10)

arCH%>%ggplot()+geom_histogram(aes(N+1))+scale_x_log10(n.breaks=10)



arCH%>%ggplot()+geom_histogram(aes(max))+scale_x_log10()
arCH%>%ggplot()+geom_point(aes(max,N))+scale_x_log10()+scale_y_log10()
arCH%>%dplyr::select(N,more500,more100,more50,more10)%>%reshape2::melt(id.vars="N")%>%
  ggplot()+geom_point(aes(N,value,colour=variable))+scale_y_log10()+scale_x_log10()

arCH%>%dplyr::select(N,max,Ptop3,Ptop5,Ptop10,Ptop20)%>%
  reshape2::melt(id.vars=c("max","N"))%>%
  mutate(value=value*max/100)%>%
  ggplot()+geom_point(aes(max,value,colour=variable))+
  scale_y_log10(n.breaks=10)+scale_x_log10(n.breaks=10)

arCH%>%filter(!is.na(N))%>%
  dplyr::select(N,max,Ptop3,Ptop5,Ptop10,Ptop20)%>%
  reshape2::melt(id.vars=c("max","N"))%>%
  mutate(value=value*max/100)%>%
  ggplot()+geom_point(aes(max,value,colour=factor(round(log10(N)))))+scale_y_log10(n.breaks=10)+scale_x_log10(n.breaks=10)+
  geom_hline(yintercept = 100)+geom_vline(xintercept = 100)+
  facet_wrap(~variable,nrow = 2)







QarCH<-do.call(rbind,lapply(isos,function(iso){
  ADM<-tryCatch(GetGADM(iso,level=0),error=function(e) NULL)
  if(is.null(ADM)) return(c(iso,matrix(NA,1,10)))
  c(iso,length(ADM@polygons[[1]]@Polygons),quantile(ExtractIndArea(ADM),1:9/10))
}))
colnames(QarCH)<-c("ISO3","N",paste0("q",1:9*10))
QarCH%<>%as.data.frame()
for(i in 3:ncol(QarCH)) QarCH[,i]%<>%as.numeric()


QdiCH<-do.call(rbind,lapply(isos,function(iso){
  ADM<-tryCatch(GetGADM(iso,level=0),error=function(e) NULL)
  if(is.null(ADM)) return(c(iso,matrix(NA,1,10)))
  c(iso,length(ADM@polygons[[1]]@Polygons),quantile(DistPoly(ADM),1:9/10))
}))
colnames(QdiCH)<-c("ISO3","N",paste0("q",1:9*10))
QdiCH%<>%as.data.frame()
for(i in 3:ncol(QdiCH)) QdiCH[,i]%<>%as.numeric()

plotty<-cbind(reshape2::melt(QarCH,id.vars=c("ISO3","N")),
              dplyr::select(reshape2::melt(QdiCH,id.vars=c("ISO3","N")),value))
colnames(plotty)[4:5]<-c("Area","Distance")
plotty%>%ggplot()+geom_point(aes(Distance,Area,colour=ISO3))+
  scale_x_log10()+scale_y_log10()
