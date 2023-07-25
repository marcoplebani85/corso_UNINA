rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages(pacman); library(pacman) # altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa) 


setwd("/Users/marcoplebani/Desktop/Corso_R")
df <- read.csv("my_data/count_data_2groups.csv", sep=",")
str(df)
library(ggplot2)
ggplot(data=df, aes(x=n.eggs, fill=Species)) +
  geom_histogram(alpha = 0.4, position = "identity", breaks=c(0:20), color="black",
                 aes(y = ..density..)) +
    labs(x = "Body mass [kg]", y = "Frequency") +
    scale_x_continuous(limits = c(0, 15)) +
  theme(text = element_text(family = "Times")) +
    theme_classic()


glm1 <- glm(n.eggs ~ Species, data=df, family=poisson)

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

# modello nullo
glm0 <- glm(n.eggs ~ 1, data=df, family=poisson)
# confronto con Chi square:
anova(glm1, glm0, test="Chisq")

# stime dei parametri (a mano):
exp(0.7)
exp(0.7+1.2)

# stime dei parametri (con emmeans):
library(emmeans)
emmeans(glm1, specs=~Species, type="link")
emmeans(glm1, specs=~Species, type="response")
