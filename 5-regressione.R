# Regressione lineare
# Marco, 8 maggio 2023

rm(list=ls()) # ripulaimo la memoria di R

getwd()
setwd("/Users/marcoplebani/Desktop/Corso_R")

# facciamo una regressione:

df <- read.csv("my_data/regression_data.csv")
# esploriamo il dataset:
str(df)
min(df$depth)
max(df$depth)
par(mfrow=c(1,1))
plot(df)
plot(density(df$alga.length))

plot(x=df$depth, y=df$alga.length)
# sintassi alternativa:
plot(alga.length ~ depth, data=df,
     ylab="lunghezza algale [cm]",
     xlab="profondità [m slm]")

# fittiamo il modello atteso
m1 <- lm(alga.length ~ depth, data=df)

# verifichiamo le assunzioni:
par(mfrow=c(2,2))
plot(m1) # tutto bene

# testiamo m1 contro il modello nullo:
m0 <- lm(alga.length ~ 1, data=df)

anova(m1, m0)

summary(m1)

par(mfrow=c(1,1))
plot(alga.length ~ depth, data=df,
     ylab="lunghezza algale [cm]",
     xlab="profondità [m slm]")
# per disegnare linee sul grafico:
# abline(h=10) # orizzontale
# abline(v=10, lty="dotted") # verticale, punteggiata
# abline(a=0.1, b=0.5) # a e b rappresentano intercetta e coefficiente angolare

# possiamo semplicemente dire ad abline di plottare il modello di regressione m1:
abline(m1)
abline(m1, col="red", lwd=3)


# carichiamo ggplot2:
install.packages("ggplot2")
library(ggplot2)
ggplot(data=df, aes(y=alga.length, x=depth)) + 
  geom_point(color="red", pch=2) +
  geom_smooth(method="lm") +
  theme_classic() + 
  labs(x="Profondità", y="Lunghezza") +
  theme(text=element_text(family="Times", size=15, color="black"))


