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

shapiro.test(insomnio$PrimerNoche)

qqnorm(insomnio_groups$PrimerNoche)

tuk <- TukeyHSD(anova_insomnio)
plot(tuk)

# bonferroni
# pairwise.t.test(write, ses, p.adj = "bonf")