ImpactAggADM0<-function(impies, haz="EQ"){
  # Extract Global ADM
  ADM <- rworldmap::getMap(resolution='high')
  ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
  
  impies%<>%filter(!(is.na(impactdetails) | is.na(imptype))) 
  
  impies$impact<-sapply(1:nrow(impies),function(i) paste0(impies$impactsubcats[i],"-",impies$imptype[i]),simplify = T)
  
  ADM@data$N<-sapply(ADM@data$ISO3, function(is){
    length(unique(impies$GCDB_ID[impies$ISO3==is]))
  },simplify = T)
  
  for(imp in unique(impies$impact)){
    # Aggregated per country
    ADM@data$tmp<-sapply(ADM@data$ISO3, function(is){
      sum(impies$impvalue[impies$ISO3==is & impies$impact==imp])
    },simplify = T)
    # Remove all zero counts
    ADM@data$tmp[ADM@data$tmp==0]<-NA
    # Set the column name
    colnames(ADM@data)[ncol(ADM@data)]<-imp
  }
  
  return(ADM)
}

PlotImpAgg<-function(ADM,impact="imptypepopcnt-imptypdeat",loggie=T,bks=NULL,lbs=NULL,guidie="colourbar"){
  
  ADM@data$tmp<-ADM@data[,impact]
  
  if(impact=="N"){
    # Keep as is
    labeller<-"No. Events"
  } else {
    # Extract the correct label for the legend
    taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
    # 
    labeller<-paste0(taxies%>%filter(list_name=="impactsubcats" &
                                       name==str_split(impact,"-",simplify = T)[1])%>%
                       pull(label)," ",
                     taxies%>%filter(list_name=="impacttypes" &
                                       name==str_split(impact,"-",simplify = T)[2])%>%
                       pull(label))  
  }
  # Extract the background map
  # mad_map <- get_stamenmap(ADM@bbox,source = "stamen",maptype = "terrain",zoom=1)
  # 
  # p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
  # 
  # q<-ggmap(mad_map)
  # 
  q<-ggplot()+geom_sf(data=st_as_sf(ADM),aes(fill=tmp), inherit.aes = FALSE)
  if(loggie){
    q+scale_fill_gradient(name = labeller, trans = "log", guide = guidie,
                          breaks=bks,labels=lbs)
  } else {
    q
  }
  
}

ADM<-ImpactAggADM0(impies)
p<-PlotImpAgg(ADM,bks=rev(c(10,100, 1000, 10000, 100000, 1000000, 10000000)),
              lbs=rev(c("10^1","10^2","10^3","10^4","10^5","10^6","10^7")),
              guidie="legend");p

p<-PlotImpAgg(ADM,"N",T,rev(c(1,10,30,50,100,300)),
              rev(c(0,10,30,50,100,300)),"legend");p
