#(re)make Figure 3

#load libraries
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(patchwork)
library(viridis)
library(ggspatial)
library(MarConsNetData) #remotes::install_github("dfo-mar-mpas/MarConsNetData")

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

##download MPAs and OECMs in Canada -----
mpas <- read_sf("data/shapefiles/MPAs.shp")%>%st_transform(CanProj)
tht <- read_sf("data/shapefiles/ThT_MPA.shp")%>%st_transform(CanProj)

mpas[grepl("offshore",tolower(mpas$NAME_E)),"geometry"] <- tht%>%dplyr::select(geometry)

#read in the PAs post the above code ------
CCAs <- mpas%>%
  mutate(name=ifelse(grepl("Eastport",NAME_E),"Eastport Marine Protected Area",NAME_E))%>%
  filter(OWNER_T == 1)%>%
  st_transform(CanProj)#government of conservation areas

naming_convention <- CCAs%>%
  data.frame()%>%
  dplyr::select(name,TYPE_E)%>%
  distinct(name,.keep_all=TRUE)%>%
  mutate(type=case_when(TYPE_E == "Other Effective Area-Based Conservation Measure (OECM)" ~ "Marine Refuge",
                        grepl("Marine Protected Area",TYPE_E) ~ "Marine Protected Area",
                        TRUE ~ "OECM"))%>%
  dplyr::select(-TYPE_E)

CCA_simple <- CCAs%>%
  group_by(name)%>%
  summarise(geometry=st_union(geometry))%>%#gets rid of the zoning
  ungroup()%>%
  left_join(.,naming_convention)

#assign the tkt the right designation 
CCA_simple[grepl("offshore",tolower(CCA_simple$name)),"type"] <- "Marine Protected Area"

#Maritimes Network
#load the network shapefile ----
network <- data_draft_areas()%>%
  st_transform(CanProj)%>%
  mutate(class = ifelse(grepl("Tier",Classification_E),"Draft","Existing"),
         focal = ifelse(SiteName_E %in% c("Jordan Basin Marine Refuge","Eastern Canyons Marine Refuge", #sites named in the network
                                          "Corsair and Georges Canyons Marine Refuge","Cold Seeps"),"focus","network"),
         labs = case_when(SiteName_E == "Jordan Basin Marine Refuge" ~ "JBMR",
                          SiteName_E == "Corsair and Georges Canyons Marine Refuge" ~ "CGCMR",
                          SiteName_E == "Eastern Canyons Marine Refuge" ~ "ECMR",
                          SiteName_E == "Cold Seeps" ~ "CS",
                          TRUE ~ NA)
  )

bioregion <- read_sf("data/shapefiles/MaritimesPlanningArea.shp")%>%
  st_transform(CanProj)

bioregion_lims <- bioregion%>%
  st_transform(utm)%>%
  st_buffer(80)%>%
  st_transform(CanProj)%>%
  st_bbox()

bioregion_poly <- bioregion_lims%>%st_as_sfc()

#basemap
basemap <- ne_countries(scale = "large", # or "medium", "small"
                        country = c("Canada", "United States of America", "Greenland", "Iceland"),
                        returnclass = "sf") %>%
  st_transform(CanProj)

#load CEDI analysis outputs
load("output/dissimilarity_by_mpa.Rdata") #this data isn't quite the same as the final

mpa_names <- names(dissimilarity_by_mpa)
# 
# cedi_df <- data.frame()
# 
# for(i in mpa_names){
#   
#   temp <- data.frame(dissimilarity_by_mpa[[i]]$CEDI)%>%
#           gather("sim","cedi")%>%
#           filter(grepl("CUR",sim))%>%
#           mutate(time_period = ifelse(grepl("MID",sim),"Mid-century","End-century"),
#                  rcp = ifelse(grepl("26",sim),"SSPI-2.6","SSPI-8.5"),
#                  SiteName_E=i)%>%
#           dplyr::select(SiteName_E,rcp,time_period,cedi)
# 
#   cedi_df <- rbind(cedi_df,temp) 
#   
# }

cedi_df <- read.csv("data/TEDI_by_site_scenario.csv")%>%
            data.frame()%>%
            gather("sim","cedi",-MPA_name, -Type)%>%
            filter(grepl("CUR",sim))%>%
            rename(SiteName_E = MPA_name)%>%
            mutate(time_period = ifelse(grepl("MID",sim),"Mid-century","End-century"),
                   time_period = factor(time_period,levels=c("Mid-century","End-century")),
                   rcp = ifelse(grepl("26",sim),"SSPI-2.6","SSPI-8.5"),
                   rcp = factor(rcp,levels=c("SSPI-2.6","SSPI-8.5")),
                   SiteName_E = factor(SiteName_E,levels=mpa_names))%>%
            dplyr::select(SiteName_E,rcp,time_period,cedi)%>%
            arrange(SiteName_E,time_period,rcp)



#add the CEDI values to the network
network2 <- network%>%
           filter(SiteName_E %in%cedi_df$SiteName_E)%>% # "Grand Manan Bird Sanctuary" "Eel Bay" do not have CEDI calculated
           left_join(.,cedi_df)

cedi_plot <- ggplot()+
  geom_sf(data=bioregion,fill="grey95",alpha=0.3,linewidth=0.25)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(name == "Canada"),fill="grey60")+
  geom_sf(data=network2%>%filter(focal !="focus"),aes(fill=cedi),linewidth=0.4,col="black")+
  geom_sf(data=network2%>%filter(focal == "focus"),aes(fill=cedi),linewidth=0.9,col="black")+
  theme_bw()+
  coord_sf(xlim=bioregion_lims[c(1,3)],ylim=bioregion_lims[c(2,4)])+
  labs(fill="CEDI")+
  facet_grid(rcp ~ time_period)+
  theme(legend.position = "inside",
        legend.position.inside = c(0.95,0.11),
        legend.background = element_blank(),
        axis.title = element_blank(),
        strip.background = element_rect(fill="white"))

ggsave("output/Figure3.png",cedi_plot+ scale_fill_gradientn(colors = c("cornflowerblue", "yellow", "coral3")),
       height=8,width=8,units="in",dpi=2400)

#with a viridis scale
ggsave("output/Figure3_viridis.png",cedi_plot+ scale_fill_viridis(direction = -1),
       height=8,width=8,units="in",dpi=2400)

#now get extra fancy with Jordan Basin

jordan_inset <- network%>%
                filter(SiteName_E %in%c("Jordan Basin Marine Refuge", "Western Jordan Basin"))%>% #center on these two adjacent sites
                st_union()%>%
                st_as_sf()%>%
                st_transform(utm)%>%
                st_buffer(3)%>%
                st_bbox()%>%
                st_as_sfc()%>%
                st_transform(CanProj)

jordan_lims <- jordan_inset%>%
               st_bbox()

rcps <- c("SSPI-2.6","SSPI-8.5")
time_periods <- c("Mid-century","End-century")
plot_range <- c(floor(min(network2$cedi)*100)/100,
                ceiling(max(network2$cedi)*100)/100)

plot_output <- list()
for(i in rcps){
  for(j in time_periods){
    
    p1 <- ggplot()+
      geom_sf(data=bioregion,fill="grey95",alpha=0.3,linewidth=0.25)+
      geom_sf(data=basemap)+
      geom_sf(data=basemap%>%filter(name == "Canada"),fill="grey60")+
      geom_sf(data=network2%>%filter(focal !="focus",rcp == i,time_period == j),aes(fill=cedi),linewidth=0.4,col="black")+
      geom_sf(data=network2%>%filter(focal == "focus",rcp == i,time_period == j),aes(fill=cedi),linewidth=0.9,col="black")+
      theme_bw()+
      labs(fill="CEDI")+
      facet_grid(rcp ~ time_period)+
      scale_fill_gradientn(colors = c("cornflowerblue", "yellow", "coral3"),
                           limits = plot_range,
                           guide = guide_colorbar(
                             frame.colour = "black",   # black outline around the colorbar
                             ticks.colour = "black"    # black tick marks
                           ))
    
    temp <- ggplot()+
            geom_sf(data=bioregion,fill="grey95",alpha=0.3,linewidth=0.25)+
            geom_sf(data=basemap)+
            geom_sf(data=basemap%>%filter(name == "Canada"),fill="grey60")+
            geom_sf(data=network2%>%filter(rcp == i,time_period == j),aes(fill=cedi),linewidth=0.4,col="black")+
            theme_bw()+
            labs(fill="CEDI")+
            facet_grid(rcp ~ time_period)+
            scale_fill_gradientn(colors = c("cornflowerblue", "yellow", "coral3"),
                                 limits = plot_range,
                                 guide = guide_colorbar(
                                   frame.colour = "black",   # black outline around the colorbar
                                   ticks.colour = "black"    # black tick marks
                                 ))
    
    
    if(i == "SSPI-2.6" & j == "Mid-century"){
      
      temp2 <- p1+
               theme(legend.position = "none",
                    axis.title = element_blank(),
                    axis.text.x = element_blank(),
                    strip.background.y = element_blank(),
                    strip.text.y = element_blank(),
                    strip.background.x = element_rect(fill="white"))+
                geom_sf(data=jordan_inset,fill=NA,lwd=0.5,linetype=2)+
        coord_sf(xlim=bioregion_lims[c(1,3)],ylim=bioregion_lims[c(2,4)])
      
    }
    
    if(i == "SSPI-2.6" & j == "End-century"){
      
      temp2 <- p1+
                theme(legend.position = "inside",
                      legend.position.inside = c(0.90,0.18),
                      legend.background = element_blank(),
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      strip.background = element_rect(fill="white"))+
                coord_sf(xlim=bioregion_lims[c(1,3)],ylim=bioregion_lims[c(2,4)])
      
    }
    
    if(i == "SSPI-8.5" & j == "Mid-century"){
      
      temp2 <- p1+
              theme(legend.position = "none",
                    axis.title = element_blank(),
                    strip.background= element_blank(),
                    strip.text= element_blank())+
              coord_sf(xlim=bioregion_lims[c(1,3)],ylim=bioregion_lims[c(2,4)])
      
    }
    
    if(i == "SSPI-8.5" & j == "End-century"){
      
      temp2 <- p1+
                theme(legend.position = "none",
                      axis.title = element_blank(),
                      axis.text.y = element_blank(),
                      strip.background.x= element_blank(),
                      strip.background.y = element_rect(fill="white"),
                      strip.text.x= element_blank())+
                annotation_scale(location="br")+
                coord_sf(xlim=bioregion_lims[c(1,3)],ylim=bioregion_lims[c(2,4)])
      
    }
    
    #add the inset
    
    temp_inset <- temp+
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.background = element_blank(),
            strip.background = element_blank(),
            strip.text= element_blank())+
      coord_sf(xlim=jordan_lims[c(1,3)],ylim=jordan_lims[c(2,4)],expand=2)
    
    temp_combo <- temp2 + inset_element(temp_inset,   left = 0.004,        # Keep left close to the edge
                                        bottom = 0.0005,      # Increase this value to move the inset up
                                        right = 0.25,        # Increase right to make the inset wider
                                        top = 0.25,          # Increase top to make the inset taller
                                        align_to = 'panel')   
    
    #save the output
    plot_output[[paste(i,j,sep="-")]] <- temp_combo
    
  } #end j loop
  
} #end i loop

#now construct the plot

p_combo <- (plot_output[[1]] + plot_output[[2]])/(plot_output[[3]] + plot_output[[4]]) 
  
ggsave("output/Fig3_detailed.png",p_combo,height=10,width=10,units="in",dpi=2400)
