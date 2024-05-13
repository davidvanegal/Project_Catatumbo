library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.0
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2' CRAN v1.1.3
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2' CRAN v0.9.5

o <- read.csv("GLM_Copros.csv", header = T, sep = ";")
o <- read.csv("GLM_Mariposas.csv", header = T, sep = ";")
o <- read.csv("GLM_Abejas.csv", header = T, sep = ";")

o$Cobertura <- factor(o$Cobertura,
                      levels = c("BQ", "PA", "SA"), ordered = TRUE)

# Plot (A) Abundancia ---------------------------------------------------------
a2 <- ggplot(o, aes(x = Cobertura, y = Abundancia))+
        geom_boxplot(width = 0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Abundancia")+
        theme(axis.text.x = element_text(size = 18))+
        theme(axis.title.x = element_text(size = 18))+
        theme(axis.text.y = element_text(size = 18))+
        theme(axis.title.y = element_text(size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 3.5)+
        annotate("text", label = "A", size = 6, x = 2, y = 5)+
        annotate("text", label = "A", size = 6, x = 3, y = 4.5)
a2

# Plot (B) Especies -----------------------------------------------------------
b2 <- ggplot(o, aes(x = Cobertura, y = Especies))+
        geom_boxplot(width = 0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Número de especies")+
        theme(axis.text.x = element_text(size = 18))+
        theme(axis.title.x = element_text(size = 18))+
        theme(axis.text.y = element_text(size = 18))+
        theme(axis.title.y = element_text(size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "Aa", size = 6, x = 1, y = 1.7)+
        annotate("text", label = "Aa", size = 6, x = 2, y = 1.85)+
        annotate("text", label = "Ab", size = 6, x = 3, y = 2.5)
b2

# Plot (C) Longitud --------------------------------------------------------
ca2 <- ggplot(o, aes(x = Cobertura, y = Longitud))+
  geom_boxplot(width = 0.6)+
  theme(axis.title.x = element_blank())+
  scale_y_continuous("Longitud ponderada (mm)")+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.y = element_text(size = 18))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))+
  theme(panel.background = element_blank())+ 
  theme(legend.position = "none")+ 
  annotate("text", label = "A", size = 6, x = 1, y = 18)+
  annotate("text", label = "A", size = 6, x = 2, y = 20)+
  annotate("text", label = "B", size = 6, x = 3, y = 12)
ca2

# Plot (D) Biomasa -------------------------------------------------------
d2 <- ggplot(o, aes(x = Cobertura, y = Biomasa))+
  geom_boxplot(width = 0.6)+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  scale_y_continuous("Biomasa total (g)")+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.y = element_text(size = 18))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))+
  theme(panel.background = element_blank())+ 
  theme(legend.position = "none")+ 
  annotate("text", label = "A", size = 6, x = 1, y = 16)+
  annotate("text", label = "B", size = 6, x = 2, y = 4)+
  annotate("text", label = "B", size = 6, x = 3, y = 5)
d2

# Plots -------------------------------------------------------------------

##generate multiple plot
final_plot2 <- plot_grid(a2, b2, ca2, d2,
                        labels=c('A', 'B', 'C', 'D'),
                        align='vh', ncol = 2, nrow = 2)
png("grafica2.png", width = 1800, height = 1800, res = 130)
print(final_plot2)
dev.off()


