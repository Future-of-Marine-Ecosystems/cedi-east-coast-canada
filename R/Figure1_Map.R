#code setup -------
#load libraries
library(sf)
library(tidyr)
library(rnaturalearth)
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
basemap <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(name_en,geometry)%>%
  st_as_sf()%>%
  st_union()%>%
  st_transform(latlong)%>%
  st_as_sf()%>%
  mutate(country="Canada")%>%
  rbind(.,ne_states(country = "United States of America",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="US"),
        ne_states(country = "Greenland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Greenland"),
        ne_states(country = "Iceland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Iceland"))%>%
  st_transform(CanProj)

eez <- read_sf("Data/Shapefiles/Canada_EEZ.shp")%>%
       st_transform(CanProj)

plot_region <- eez%>%st_bbox()

#Create the plots 

#Canada Scale
p1 <- ggplot()+
  geom_sf(data=bioregion,fill="grey95")+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=bioregion_poly,fill=NA,linewidth=0.5)+
  geom_sf(data=eez,fill=NA)+
  geom_sf(data=CCA_simple,fill="deepskyblue1",linewidth=0.4)+ #just existing and implemented sites.
  #geom_sf(data=network%>%filter(Classification_E != "Existing site"),fill="deepskyblue1",linewidth=0.4)+
  theme_bw()+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)])+
  theme(axis.text=element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0), # Remove all outer margins
        panel.spacing = unit(0, "null"),
        panel.background = element_rect(fill="white"),
        plot.background = element_blank())

#Scotian Shelf Bay of Fundy scale  
p2 <- ggplot()+
  geom_sf(data=bioregion,fill="grey95",alpha=0.3)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=network,aes(fill=class),linewidth=0.4)+
  geom_sf(data=network%>%filter(focal == "focus"),linewidth=0.6,fill=NA)+
  theme_bw()+
  coord_sf(xlim=bioregion_lims[c(1,3)],ylim=bioregion_lims[c(2,4)])+
  annotation_scale()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.9,0.1),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank())+
  scale_fill_manual(values=c("coral2","deepskyblue1"))

#use Patchwork to bring together. 
p3 <-  p2 + inset_element(p1,   left = 0.01,        # Keep left close to the edge
                          bottom = 0.65,      # Increase this value to move the inset up
                          right = 0.4,        # Increase right to make the inset wider
                          top = 1.1,          # Increase top to make the inset taller
                          align_to = 'full')   # Ensure it aligns to the full area

ggsave("output/Fig1.png",p3,height=8,width=6.5,units="in",dpi=300)

