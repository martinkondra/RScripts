library(foreign)

insomnio <- read.spss("/home/martinkondra/R/Insomnio_2.sav", to.data.frame=TRUE)
cols <- colnames(data)

# Anova con modelo lineal
model_insomnio <- lm(PrimerNoche ~ as.factor(Edad), data = insomnio)
anova_insomnio <- anova(model_insomnio)

# Anova directo: as.factor = Inter-grupos, Residuals = Intragrupos
anova_insomnio <- aov(PrimerNoche ~ as.factor(Edad), data = insomnio)

# VER COMO HACERLO con grupos!
insomnio1 = subset(insomnio, Edad=='Menor a 20 años')
ks.test(insomnio1$PrimerNoche, "pnorm")

#el p value de schapiro debe ser mayor que alpha
shapiro.test(insomnio$PrimerNoche)

qqnorm(insomnio_groups$PrimerNoche)

tuk <- TukeyHSD(anova_insomnio)
plot(tuk)

# bonferroni
# pairwise.t.test(write, ses, p.adj = "bonf")

# EJERCICIO
setwd("/home/martinkondra/RScripts")
data <- read.csv("Ej08-17.csv")
#1
boxplot(data)
#2
stem(data$placebo)

shapiro.test(data$placebo)
shapiro.test(data$rCognitivo)
shapiro.test(data$apAcertiva)
shapiro.test(data$ejYNut)

ks.test(data$placebo, "pnorm")
ks.test(data$rCognitivo, "pnorm")
ks.test(data$apAcertiva, "pnorm")
ks.test(data$ejYNut, "pnorm")

qqnorm(data$placebo)

library(car)
leveneTest(data$placebo)

