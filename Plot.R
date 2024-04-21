library(ggplot2)
library(cowplot)
library(ggrepel)

o <- read.csv("Base.csv", header = T, sep = ";")
o$Cobertura <- factor(o$Cobertura,
                      levels = c("BQ", "PA", "SA"), ordered = TRUE)

# Plot (A) Individuos ---------------------------------------------------------
a2 <- ggplot(o, aes(x = Cobertura, y = Abundancia))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Abundancia", limits = c(0, 45),
                           breaks = seq(0, 45, by = 5))+
        theme(axis.text.x = element_text(size = 18))+
        theme(axis.title.x = element_text(size = 18))+
        theme(axis.text.y = element_text(size = 18))+
        theme(axis.title.y = element_text(size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 30)+
        annotate("text", label = "B", size = 6, x = 2, y = 15)+
        annotate("text", label = "A", size = 6, x = 3, y = 45)
a2

# Plot (B) Especies -----------------------------------------------------------
b2 <- ggplot(o, aes(x = Cobertura, y = Especies))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Número de especies", limits = c(0, 10),
                           breaks = seq(0, 10, by = 2))+
        theme(axis.text.x = element_text(size = 18))+
        theme(axis.title.x = element_text(size = 18))+
        theme(axis.text.y = element_text(size = 18))+
        theme(axis.title.y = element_text(size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 8)+
        annotate("text", label = "B", size = 6, x = 2, y = 4)+
        annotate("text", label = "A", size = 6, x = 3, y = 8)
b2

# Plot (C) Biomasa --------------------------------------------------------
ca2 <- ggplot(o, aes(x = factor(sitio), y = biomasa))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Biomasa total (g)", limits = c(0, 4),
                           breaks = seq(0, 4, by = 0.5))+
        theme(axis.title.y = element_text(family = "Arial", size = 18))+
        theme(axis.text.y = element_text(family = "Arial", size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+
        annotate("text", label = "A", size = 6, x = 1, y = 3.82)+
        annotate("text", label = "B", size = 6, x = 2, y = 1.19)+
        annotate("text", label = "B", size = 6, x = 3, y = 0.77)

# Plot (D) Longitud -------------------------------------------------------
d2 <- ggplot(o, aes(x = factor(sitio), y = longitud))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        scale_y_continuous("Longitud ponderada (mm)", limits = c(4, 11),
                           breaks = seq(4, 11, by = 2))+
        theme(axis.text.x = element_text(family = "Arial", size = 18))+
        theme(axis.text.x = element_text(family = "Arial", size = 18))+
        theme(axis.text.y = element_text(family = "Arial", size = 18))+
        theme(axis.title.y = element_text(family = "Arial", size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 8.22)+
        annotate("text", label = "A", size = 6, x = 2, y = 7.89)+
        annotate("text", label = "A", size = 6, x = 3, y = 11)

# Plot (E) Heces ----------------------------------------------------------
e2 <- ggplot(o, aes(x = factor(sitio), y = heces*100))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Remoci?n de heces (%)", limits = c(0, 100),
                           breaks = seq(0, 100, by = 20))+
        theme(axis.title.y = element_text(family = "Arial", size = 18))+
        theme(axis.text.y = element_text(family = "Arial", size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 99.000)+
        annotate("text", label = "B", size = 6, x = 2, y = 87.800)+
        annotate("text", label = "B", size = 6, x = 3, y = 61.350)

# Plot (F) Suelo ----------------------------------------------------------
f2 <- ggplot(o, aes(x = factor(sitio), y = suelo))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Suelo excavado (g)", limits = c(0, 120),
                           breaks = seq(0, 120, by = 20))+
        theme(axis.text.x = element_text(family = "Arial", size = 18))+
        theme(axis.title.y = element_text(family = "Arial", size = 18))+
        theme(axis.text.y = element_text(family = "Arial", size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 117.2)+
        annotate("text", label = "B", size = 6, x = 2, y = 89.1)+
        annotate("text", label = "C", size = 6, x = 3, y = 41.6)

# Plot (G) Dispersi?n -----------------------------------------------------
g2 <- ggplot(o2, aes(sitio, value, fill = disper))+
        geom_boxplot(width=0.5)+
        scale_fill_manual(values=c("gray48", "white", "gray77"),
                          labels = c("Peque?as", "Medianas", "Grandes"))+
        theme(axis.title.x = element_blank())+
        scale_y_continuous("Dispersi?n de semillas (%)", limits = c(0, 110),
                           breaks = seq(0, 120, by = 20))+
        theme(axis.title.y = element_text(family = "Arial", size = 18))+
        theme(axis.text.y = element_text(family = "Arial", size = 18))+
        theme(axis.text.x = element_text(family = "Arial", size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(legend.position = c(0.75, 0.8))+
        theme(panel.background = element_blank())+ 
        annotate("text", label = "A", size = 6, x = 0.84, y = 103)+
        annotate("text", label = "a", size = 6, x = 1.01, y = 95)+
        annotate("text", label = "italic(A)", size = 6, x = 1.17, y = 99, parse=TRUE)+
        annotate("text", label = "B", size = 6, x = 1.84, y = 83)+
        annotate("text", label = "b", size = 6, x = 2.01, y = 77)+
        annotate("text", label = "italic(A)", size = 6, x = 2.17, y = 86, parse=TRUE)+
        annotate("text", label = "C", size = 6, x = 2.84, y = 35)+
        annotate("text", label = "c", size = 6, x = 3.01, y = 33)+
        annotate("text", label = "italic(B)", size = 6, x = 3.17, y = 31, parse=TRUE)+
        theme(legend.key=element_blank())+
        theme(legend.title=element_blank(), 
              legend.text=element_text(size=15,family = "Arial"))

# Plot (H) Dispersi?n total -----------------------------------------------
h2 <- ggplot(o, aes(x = factor(sitio), y = disper*100))+
        geom_boxplot(width=0.6)+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_blank())+
        scale_y_continuous("Dispersi?n semillas\nTotales (%)", limits = c(0, 110),
                           breaks = seq(0, 110, by = 20))+
        theme(axis.title.y = element_text(family = "Arial", size = 18))+
        theme(axis.text.y = element_text(family = "Arial", size = 18))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black", size = 1))+
        theme(panel.background = element_blank())+ 
        theme(legend.position = "none")+ 
        annotate("text", label = "A", size = 6, x = 1, y = 98.148)+
        annotate("text", label = "B", size = 6, x = 2, y = 81.852)+
        annotate("text", label = "C", size = 6, x = 3, y = 32.296)

# Plots -------------------------------------------------------------------

##generate multiple plot
final_plot2 <- plot_grid(a2, e2, b2, f2, ca2, h2, d2, g2,
                        labels=c('A', 'E', 'B', 'F', 'C', 'G', 'D', 'H'),
                        align='vh', ncol = 2, nrow = 4)
png("grafica2.png", width = 1800, height = 1800, res = 130)
print(final_plot2)
dev.off()

