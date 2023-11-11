ImpactAggADM0<-function(impies, haz="EQ"){
  # Extract Global ADM
  ADM <- rworldmap::getMap(resolution='low')
  ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
  
  impies%<>%filter(!(is.na(imp_det) | is.na(imp_type))) 
  
  impies$impact<-sapply(1:nrow(impies),function(i) paste0(impies$imp_subcats[i],"-",impies$imp_type[i]),simplify = T)
  
  ADM@data$N<-sapply(ADM@data$ISO3, function(is){
    length(unique(impies$event_ID[impies$ISO3==is]))
  },simplify = T)
  
  ADM@data$Ncap<-100*ADM@data$N/ADM@data$Population
  
  for(imp in unique(impies$impact)){
    # Aggregated per country
    ADM@data$tmp<-sapply(ADM@data$ISO3, function(is){
      sum(impies$imp_value[impies$ISO3==is & impies$impact==imp])
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
  
  if(impact%in%c("N","Ncap")){
    # Keep as is
    labeller<-"No. Events"
  } else {
    # Extract the correct label for the legend
    taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
    # 
    labeller<-paste0(taxies%>%filter(list_name=="imp_subcats" &
                                       name==str_split(impact,"-",simplify = T)[1])%>%
                       pull(label)," ",
                     taxies%>%filter(list_name=="imp_type" &
                                       name==str_split(impact,"-",simplify = T)[2])%>%
                       pull(label))  
  }
  # Change the projection
  crs_mappy <- "+proj=wintri +x_0=-74 +y_0=0 +datum=WGS84 +no_defs +over" # TRIPEL PROJECTION
  # crs_mappy <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=0" # Lambert azimuthal equal-area projection
  projdat <- lwgeom::st_transform_proj(st_as_sf(ADM), crs = crs_mappy)
  # Add the grid layout to the plot (graticule)
  grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
    lwgeom::st_transform_proj(crs = crs_mappy)
  # To add the blue background to the map
  lats <- c(90:-90, -90:90, 90)
  # longs <- c(rep(c(180-74, -180-74), each = 181), 180-74)
  longs <- c(rep(c(180, -180), each = 181), 180)
  # turn into correctly projected sf collection
  wintri_outline <- 
    list(cbind(longs, lats)) %>%
    st_polygon() %>%
    st_sfc( # create sf geometry list column
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    ) %>% 
    st_sf() %>%
    lwgeom::st_transform_proj(crs = crs_mappy)
  
  # Plot it out!
  q<-ggplot()+
    geom_sf(data = wintri_outline, fill = "#56B4E950", color = "black", alpha=0.5) +
    geom_sf(data = grat_wintri, color = "gray30", linewidth = 0.05, alpha=0.1) +
    geom_sf(data=projdat,aes(fill=tmp), color = "grey30", linewidth=0.1) + #, inherit.aes = FALSE) +
    coord_sf(datum = NULL) + #ylim(c(-80,80)) +
    # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
    ggthemes::theme_map() +
    theme(legend.background = element_rect(fill="white",
                                           linetype="solid", 
                                           colour ="black"),
          legend.position = c(0.1, 0.1))
  # Specific the fill style of the plot
  if(loggie){
    q+scale_fill_gradient(low="pink", high="#ee3224", name = labeller, trans = "log", guide = guidie,
                          breaks=bks,labels=lbs)
  } else {
    q
  }
  
}

# ADM<-ImpactAggADM0(impies)
# p<-PlotImpAgg(ADM,bks=rev(c(10,100, 1000, 10000, 100000, 1000000, 10000000)),
#               lbs=rev(c("10^1","10^2","10^3","10^4","10^5","10^6","10^7")),
#               guidie="legend");p
# 
# p<-PlotImpAgg(ADM,"N",T,rev(c(1,10,30,50,100,300)),
#               rev(c(0,10,30,50,100,300)),"legend");p
