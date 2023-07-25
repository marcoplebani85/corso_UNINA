rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages(pacman); library(pacman) # altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)

setwd("/Users/marcoplebani/Desktop/Corso_R")
df <- read.csv("my_data/count_linear.csv", sep=",")
str(df)
par(mfrow=c(1,1))
plot(df)

lm1 <- lm(n.progeny ~ body.mass, data=df)
par(mfrow=c(2,2))
plot(lm1)

glm1 <- glm(n.progeny ~ body.mass, data=df,
            family=poisson(link="identity"))

# GRAFICI DIAGNOSTICI

# Nel caso dei GLM, ottenere i grafici diagnostici con plot() non è adeguato: plot() usa sempre come distribuzione di riferimento quella normale.
# E' meglio usare i grafici ed i test forniti dal pacchetto DHARMa. DHARMa simula i residui attesi secondo il modello e secondo la distribuzione dei residui in esso indicata, quindi confronta i residui attesi con quelli osservati:

glm1Resids <- simulateResiduals(glm1)
par(mfrow = c(1, 2))
# confronto tra residui osservati e quelli attesi secondo una distribuzione poissoniana:
plotQQunif(glm1Resids)
mtext(text = "(a)", side = 3, adj = 0, line = 2)
# andamento dei residui simulati rispetto ai valori medi stimati dal modello:
plotResiduals(glm1Resids, quantreg = T)
mtext(text = "(b)", side = 3, adj = 0, line = 2)


summary(glm1)
summary(lm1)
