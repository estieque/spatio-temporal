
library(sf)
library(tmap)
library(readxl)
library(classInt)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(scales)
library(geodata)
library(extrafont)
library(spdep)
library(dplyr)
library(terra)

data <- read_excel("C:\\Users\\estie\\OneDrive\\Documents\\class 2\\NOISE DATA OF 64 DISTRICT.xlsx", sheet = "sheet1")

head(data)


bd_shape <- gadm(
  country= "BD",
  level = 2,
  path = getwd()
)%>%
  st_as_sf()


bd_shape <- merge(bd_shape, data, by.x="NAME_2", by.y="NAME_2", all.x=T)


breaks <- classIntervals(
  bd_shape$`Noise (dBA)`,
  n=5,
  style = "equal"
)$brks

colors <- hcl.colors(
  n=length(breaks),
  palette= "Temps",
  rev = F
)


ggplot()+
  geom_sf(data = bd_shape,
          aes(fill=`Noise (dBA)`), color="grey90")+
  theme_classic()+
  scale_fill_gradientn(
    name= "Noise level (DB)",
    colors = colors,
    breaks = breaks,
    labels = round(breaks, 0),
    limits = c(
      min(bd_shape$`Noise (dBA)`),
      max(bd_shape$`Noise (dBA)`)
    )
  )+
  guides(
    fill=guide_colorbar(
      direction = "horizontal",
      title.position = "top"
    )
  )+
  theme_classic()+
  theme(legend.position = "bottom")

#Gi-bin, Gi*-bin, LISA 

nb <- poly2nb(bd_shape, snap = 0.001)

lw <- nb2listw(nb, style = "W", zero.policy = T)

bd_shape$variable_of_ineterest <- bd_shape$`Noise (dBA)`




moran_local <- localmoran(bd_shape$variable_of_ineterest, lw, zero.policy = T)

bd_shape$Ii <- moran_local[,1]

bd_shape$Z_Ii <- moran_local[,4]

bd_shape$p_value<- moran_local[,5]


bd_shape$Gi_Bin <- cut(
  bd_shape$p_value,
  breaks = c(0, 0.01, 0.05, 0.1, 1),
  labels = c("Hotspot - 99% Confidence",
             "Hotspot - 95% Confidence",
             "Hotspot - 90% Confidence",
             "Not Significant")
)

palette <- c("red", "orange", "yellow", "white")


tmap_mode('plot')

tm_shape(bd_shape)+
  tm_fill("Gi_Bin", palette= palette,
          legend.hist =F)+
  tm_borders()+
  tm_graticules(lines=F,
                labels.size=3)+
  tm_layout(
    legend.show =T,
    legend.outside = F
  )


mean_column <- attr(moran_local, "quadr")[,"mean"]



bd_shape$high <- mean_column


tm_shape(bd_shape)+
  tm_polygons("high",palette = "-RdYlGn", 
              title = "Moran's I Local Analysis") +                   # Add borders to the shapes
  tm_graticules(lines = FALSE,     # Remove grid lines but keep coordinates
                labels.size = 1.5) + # Adjust label size for the coordinates
  tm_layout(legend.outside = FALSE,  # Keep legend inside the map
            legend.position = c("left", "bottom"),  # Position the legend inside at bottom left
            legend.title.size = 1.2,  # Increase legend title size
            legend.text.size = 1,    # Adjust legend text size
            legend.bg.color = "white",  # Add white background to legend
            legend.bg.alpha = 0.5) 



moran_result <- moran.test(bd_shape$variable_of_ineterest, lw, zero.policy = T)


bd_shape$moran_res <- ifelse(bd_shape$variable_of_ineterest > mean(bd_shape$variable_of_ineterest), "Above Avg", "Bellow Avg")



ggplot()+
  geom_sf(data=bd_shape, aes(fill=moran_res))+
  scale_fill_manual(values = c("Above Avg"="orange", "Bellow Avg"="lightblue"))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    text = element_text(size = 14, family = "Times New Roman"),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(face = "bold", size = 14, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 

nb <- poly2nb(bd_shape, snap = 0.001)

lw <- nb2listw(nb, style = "W", zero.policy = T)

bd_shape$variable_of_ineterest <- bd_shape$`Noise (dBA)`

gi_star <- localG(bd_shape$variable_of_ineterest, lw, zero.policy = T)


bd_shape$gi_star <- as.numeric(gi_star)

bd_shape$hotspot <- case_when(
  bd_shape$gi_star > 2 ~ "Hotspot",
  bd_shape$gi_star < 2 ~ "Coldspot",
  TRUE ~ "Not Significant"
)

ggplot()+
  geom_sf(data = bd_shape, aes(fill=hotspot), color = "black", size = 0.2) +
  scale_fill_manual(values = c(
    "Hotspot" = "red", 
    "Coldspot" = "blue", 
    "Not Significant" = "gray"
  )) +
  scale_y_continuous(breaks = seq(21, 27, by = .5))+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    text = element_text(size = 24),
    axis.text = element_text(face = "bold", size =24, color = "black"),
    axis.title = element_text(face = "bold", size = 24, color = "black"),
    #legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.y = element_text(margin = margin(r = 12))
  ) +
  labs(
    #title = "Getis-Ord Gi* Hotspots and Coldspots - 2023",
    fill = "Cluster Type"
  )


###LISA analysis##
coords <- st_centroid(bd_shape)%>% st_coordinates()

nb <- knn2nb(knearneigh(coords, k=4))

lw <- nb2listw(nb, style = "W", zero.policy = T)

bd_shape$variable_of_ineterest <- bd_shape$`Noise (dBA)`

lisa_result <- localmoran(bd_shape$variable_of_ineterest, lw, zero.policy = T)



bd_shape$lisa_I <- lisa_result[,1]
bd_shape$P_value <- lisa_result[,5]


bd_shape$cluster <- case_when(
  bd_shape$lisa_I > 0 & bd_shape$P_value <= 0.05 ~ "High-High",
  bd_shape$lisa_I < 0 & bd_shape$P_value <= 0.05 ~ "Low-Low",
  bd_shape$lisa_I > 0 & bd_shape$P_value > 0.05 ~ "High-Outlier",
  bd_shape$lisa_I < 0 & bd_shape$P_value > 0.05 ~ "Low-Outlier",
  TRUE ~ "Non-Significant"
)


ggplot()+
  geom_sf(data = bd_shape, aes(fill=cluster ), color = "black", size = 0.2) +
  scale_fill_manual(values = c(
    "High-High" = "red", 
    "Low-Low" = "blue", 
    "High-Outlier" = "orange", 
    "Low-Outlier" = "lightblue", 
    "Non-Significant" = "gray"
  )) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    text = element_text(size = 18),
    axis.text = element_text(face = "bold", size = 18, color = "black"),
    axis.title = element_text(face = "bold", size = 18, color = "black"),
    #legend.title = element_blank(),
    legend.text = element_text(size = 18)
  ) +
  labs(
    #title = "LISA Analysis: Local Moran's I Clusters -2023",
    fill = "Cluster Type"
  )

