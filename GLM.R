library(multcomp) # Simultaneous Inference in General Parametric Models CRAN v1.4-25
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4 CRAN v1.1-35.2
library(lmtest) # Testing Linear Regression Models CRAN v0.9-40

# Read data
o <- read.csv("GLM_Copros.csv", header = T, sep = ";")
o <- read.csv("GLM_Mariposas.csv", header = T, sep = ";")
o <- read.csv("GLM_Abejas.csv", header = T, sep = ";")

# Factor sitios
o$Cobertura <- as.factor(o$Cobertura)

## Análisis previo
par(mfrow=c(2,4))
print(summary(o))
plot(o)

# Abundacia --------------------------------------------------------------
Ind <- glm(Abundancia ~ Cobertura, family = gaussian(), data = o)
print(summary(Ind))
par(mfcol = c(2, 2))
plot(Ind)

anvInd <- anova(Ind, test = "Chisq")
print(anvInd)
summary(anvInd)

tukInd <- glht(Ind, linfct = mcp(Cobertura = "Tukey"))
summary(tukInd)

# Especies ----------------------------------------------------------------
Esp <- glm(Especies ~ Cobertura, family = gaussian(), data = o)
print(summary(Esp))
par(mfcol = c(2, 2))
plot(Esp)

anvEsp <- anova(Esp, test = "Chisq")
print(anvEsp)
summary(anvEsp)

tukEsp <- glht(Esp, linfct = mcp(Cobertura = "Tukey"))
summary(tukEsp)

# Longitud ----------------------------------------------------------------
Lon <- glm(Longitud ~ Cobertura, family = gaussian(), data = o)
print(summary(Lon))
par(mfcol = c(2, 2))
plot(Lon)

anvLon <- anova(Lon, test = "Chisq")
print(anvLon)
summary(anvLon)

tukLon <- glht(Lon, linfct = mcp(Cobertura = "Tukey"))
summary(tukLon)

# Biomasa ---------------------------------------------------------------
Biom <- glm(Biomasa ~ Cobertura, family = gaussian(), data = o)
print(summary(Biom))
par(mfcol = c(2, 2))
plot(Biom)

anvBiom <- anova(Biom, test = "Chisq")
print(anvBiom)
summary(anvBiom)

tukBiom <- glht(Biom, linfct = mcp(Cobertura = "Tukey"))
summary(tukBiom)
