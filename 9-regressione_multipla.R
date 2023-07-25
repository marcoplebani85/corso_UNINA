# Regressione multipla
rm(list=ls())
setwd("/Users/marcoplebani/Desktop/Corso_R")

df <- read.csv("my_data/multi_reg_data.csv")

str(df)
install.packages("scatterplot3d")
library(scatterplot3d)
par(mfrow=c(1,1))
scatterplot3d(x=df$depth,
              z=df$alga.length,
              y=df$nutrients,
              pch=16,
              highlight.3d=T)

# modelli possibili:

lm.interattivo <- lm(alga.length ~ depth * nutrients, data=df)
lm.additivo <- lm(alga.length ~ depth + nutrients, data=df)
lm.depth <- lm(alga.length ~ depth, data=df)
lm.nutrients <- lm(alga.length ~ nutrients, data=df)
lm0 <- lm(alga.length ~ 1, data=df)

# controlliamo le assunzioni:

par(mfrow=c(2,2))
plot(lm.interattivo)
plot(lm.additivo)
plot(lm.depth) # violazione normalità
plot(lm.nutrients) # violazione normalità
plot(lm0)

# c'è anche l'assunzione di indipendenza dei predittori!

cor.test(df$nutrients, df$depth,
         methods="pearson")

# metodi possibili: "pearson", "kendall", "spearman"
# selezione modello:

AIC(lm.interattivo,
    lm.additivo, # il migliore tra quelli a confronto
    # lm.depth, # già appurato non essere un buon modello
    # lm.nutrients, # già appurato non essere un buon modello
    lm0)

# ...oppure col test F tramite anova().

# estraiamo i parametri del modello:
options(scipen=999) # per evitare l'uso della notazione scientifica
summary(lm.additivo)
# equazione del piano di regressione:
# length = 100 -2 * depth + 0.1 * nutrients

# addiungiamo il piano al grafico:

par(mfrow=c(1,1))
s3d <- scatterplot3d(x=df$depth,
              z=df$alga.length,
              y=df$nutrients,
              pch=16,
              highlight.3d=T)
# E' importante dare un nome allo scatterplot 3D.
# Per aggiungere il piano all'oggetto s3d:
s3d$plane3d(lm.additivo, lty.box="solid")
