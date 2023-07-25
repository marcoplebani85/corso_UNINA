# binomial GLM

rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages(pacman); library(pacman) # altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)

setwd("/Users/marcoplebani/Desktop/Corso_R")
df <- read.csv("my_data/egg_predation.csv")
str(df)

# 1. calcoliamo le proporzioni:

df$predation.proportion <- df$egg.predation/df$brood.size

plot(predation.proportion ~ vegetation.cover, data=df)

# 2. ricorda: le proporzioni vanno "pesate" con WEIGHTS. I nidi con più uova rappresentano una frazione più ampia del campione, dunque contano di più per il modello:


glm.binom.full <- glm(predation.proportion ~ vegetation.cover * brood.size,
	data = df,
	weights = brood.size,
	family = binomial(link="logit")
	)

# GRAFICI DIAGNOSTICI

glm1Resids <- simulateResiduals(glm.binom.full)
par(mfrow = c(1, 2))
# confronto tra residui osservati e quelli attesi:
plotQQunif(glm1Resids)
mtext(text = "(a)", side = 3, adj = 0, line = 2)
# andamento dei residui simulati rispetto ai valori medi stimati dal modello:
plotResiduals(glm1Resids, quantreg = T)
mtext(text = "(b)", side = 3, adj = 0, line = 2)

# MODELLI ALTERNATIVI

glm.binom.additivo <- glm(predation.proportion ~ vegetation.cover + brood.size,
                      data = df,
                      weights = brood.size,
                      family = binomial(link="logit")
                      )

glm.binom.cover <- glm(predation.proportion ~ vegetation.cover,
                          data = df,
                          weights = brood.size,
                          family = binomial(link="logit")
)

glm.binom.size <- glm(predation.proportion ~ brood.size,
                          data = df,
                          weights = brood.size,
                          family = binomial(link="logit")
)

glm.binom0 <- glm(predation.proportion ~ 1,
                  data = df,
                  weights = brood.size,
                  family = binomial(link="logit")
)





anova(glm.binom.full, glm.binom.additivo, test="Chisq")

anova(glm.binom.additivo, glm.binom.cover, test="Chisq")

anova(glm.binom.additivo, glm.binom.size, test="Chisq")

anova(glm.binom.cover, glm.binom0, test="Chisq")

anova(glm.binom.size, glm.binom0, test="Chisq")


par(mfrow=c(1,1))
library(ggplot2)
pp1 <- ggplot(data=df, aes(y=predation.proportion, x=vegetation.cover))

pp1 + geom_point() + theme_classic() +
geom_smooth(method = "glm", se = T, color="red",
		aes(weight = brood.size), 
        method.args = list(family = "binomial"))  
        
        
        
        
        
