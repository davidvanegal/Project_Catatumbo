library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.0
library(GGally) # Extension to 'ggplot2' CRAN v2.2.1
library(CCA) # Canonical Correlation Analysis CRAN v1.2.2
library(MASS) # Support Functions and Datasets for Venables and Ripley's MASS CRAN v7.3-60
library(vegan) # Community Ecology Package CRAN v2.6-4
library(ggforce) # Accelerating 'ggplot2' CRAN v0.4.2

## Análisis bajo parámetros de abundancia

# Carga de base de datos
mx <- read.csv("NMDS_Copros.csv", header = T, sep = ";")
mx <- read.csv("NMDS_Marip.csv", header = T, sep = ";")
mx <- read.csv("NMDS_Abej.csv", header = T, sep = ";")

View(mx)
dim(mx)

# Manipulación de datos
mx2 <- mx[1:13,]
ymx <- mx2[,2:25]
View(ymx)

# NMDS
nmds <- metaMDS(ymx, k = 2, distance = "bray")
nmds

# Cálculo de la bondad de ajuste para NMDS
bdan<-goodness(nmds)
print(bdan)
sum(bdan)
mean(bdan)  

nmds$points
nmds.scores<-scores(nmds)
nmds.scores

Sitio <- nmds$points
spp <- nmds$species
spp

# Determinar sitios
nmdsdata <- data.frame(nmds.scores$sites)
nmdsdata$sites <- c(1:13)
nmdsdata$sites[1:4] <- c("BQ")
nmdsdata$sites[5:8] <- c("PA")
nmdsdata$sites[9:13] <- c("SAF")

# Plot
ggplot(nmdsdata, aes(x = NMDS1, y = NMDS2, color = sites))+
  geom_point(aes(shape = sites), size=4) +
  geom_mark_ellipse(expand = 0, aes(color = sites))+
  theme(axis.text.x = element_text(size = 14, colour = "black"))+
  
  theme(axis.text.y = element_text(size = 14, colour = "black"))+
  theme(axis.title.x = element_text(size = 14, colour = "black"))+
  theme(axis.title.y = element_text(size = 14, colour = "black"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  labs(x = "NMDS1", y = "NMDS2",
       family = "Arial", size = 14)+
  theme(legend.position="right", legend.title=element_blank(),
        text=element_text(size=20), legend.key=element_blank())

stressplot(nmds)

