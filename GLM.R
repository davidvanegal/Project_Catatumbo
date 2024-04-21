library(multcomp) # Simultaneous Inference in General Parametric Models CRAN v1.4-25
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4 CRAN v1.1-35.2
library(lmtest) # Testing Linear Regression Models CRAN v0.9-40

# Read data
o <- read.csv("Copros.csv", header = T, sep = ";")
o <- read.csv("Mariposas.csv", header = T, sep = ";")
o <- read.csv("Abejas.csv", header = T, sep = ";")

# Factor sitios
o$sitio <- as.factor(o$Cobertura)

## An?lisis previo
par(mfrow=c(2,4))
print(summary(o))
plot(o)

# Especies ----------------------------------------------------------------
Esp<-glm(Especies~sitio,family=gaussian(),data=o)
print(summary(Esp))
par(mfcol = c(2, 2))
plot(Esp)

anvEsp<-anova(Esp, test="Chisq")
print(anvEsp)
summary(anvEsp)

tukEsp<-glht(Esp, linfct = mcp(sitio = "Tukey"))
summary(tukEsp)

# Individuos --------------------------------------------------------------
Ind<-glm(Abundancia~sitio,family=gaussian(),data=o)
print(summary(Ind))
par(mfcol = c(2, 2))
plot(Indi)

anvInd<-anova(Ind, test="Chisq")
print(anvInd)
summary(anvInd)

tukInd<-glht(Ind, linfct = mcp(sitio = "Tukey"))
summary(tukInd)


# Longitud ----------------------------------------------------------------
Lon<-glm(longitud~sitio,family=gaussian(),data=o)
print(summary(Lon))
par(mfcol = c(2, 2))
plot(Lon)

anvLon<-anova(Lon, test="Chisq")
print(anvLon)
summary(anvLon)

tukLon<-glht(Lon, linfct = mcp(sitio = "Tukey"))
summary(tukLon)
