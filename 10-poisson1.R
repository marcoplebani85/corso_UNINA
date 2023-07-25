# glm - esempio 1
rm(list=ls())
setwd("/Users/marcoplebani/Desktop/Corso_R")
df <- read.csv("my_data/count_data.csv", sep=",")
str(df)
plot(n.progeny ~ body.mass, data=df)
lm1 <- lm(n.progeny ~ body.mass, data=df)
# grafici diagnostici:
par(mfrow=c(2,2))
plot(lm1)
coef(lm1)
# visualizziamo le predizioni:
par(mfrow=c(1,1))
plot(n.progeny ~ body.mass, data=df)
abline(a=-367.088, b=26.7, lty="dashed", col="red")
abline(h=0)

lm.log <- lm(log(n.progeny) ~ body.mass, data=df)
# grafici diagnostici:
par(mfrow=c(2,2))
plot(lm.log)
coef(lm.log)
log(0)

glm1 <- glm(n.progeny ~ body.mass, data=df,
            family=poisson(link="log")
            )
            
# GRAFICI DIAGNOSTICI

# Nel caso dei GLM, ottenere i grafici diagnostici con plot() non è adeguato: plot() usa sempre come distribuzione di riferimento quella normale.
# E' meglio usare i grafici ed i test forniti dal pacchetto DHARMa. DHARMa simula i residui attesi secondo il modello e secondo la distribuzione dei residui in esso indicata, quindi confronta i residui attesi con quelli osservati:

glm1Resids <- simulateResiduals(glm1) par(mfrow = c(1, 2)) # confronto tra residui osservati e quelli attesi secondo una distribuzione poissoniana:
plotQQunif(glm1Resids) mtext(text = "(a)", side = 3, adj = 0, line = 2) # andamento dei residui simulati rispetto ai valori medi stimati dal modello:
plotResiduals(glm1Resids, quantreg = T) mtext(text = "(b)", side = 3, adj = 0, line = 2) 


# come valutare la bontà del modello?
# guardiamo se i parametri sono signif. diversi da zero:
summary(glm1)

# oppure confrontiamo glm1 con glm0

glm0 <- glm(n.progeny ~ 1, data=df,
            family=poisson(link="log")
)
anova(glm1, glm0, test="Chisq")

AIC(glm1, glm0)

# otteniamo le predizioni del modello sulla scala non-trasformata:
dummy <- data.frame(body.mass=c(1:30))

pred <- as.data.frame(predict(glm1,
        newdata=dummy,
        type="response",
        se.fit=T))


pred.link <- as.data.frame(predict(glm1, newdata=dummy, se.fit=TRUE, type="link"))
pred.link$CIL <- pred.link$fit - 1.96*pred.link$se.fit
pred.link$CIH <- pred.link$fit + 1.96*pred.link$se.fit

library(emmeans)
em.predict <- emmeans(glm1, 
        specs=~body.mass,
        type="response",
        at=list(body.mass=1:30)
        )
em.pred <- as.data.frame(em.predict)

par(mfrow=c(1,1))
# predizione
plot(n.progeny ~ body.mass, data=df,
     xlim=c(27,30),
     ylim=c(600, 700))

points(x=em.pred$body.mass,
       y=em.pred$rate,
       type="l", col="red", lwd=3)
# CI
points(x=em.pred$body.mass,
       y=em.pred$asymp.LCL,
       type="l", col="red", lwd=1, lty="dashed")

points(x=em.pred$body.mass,
       y=em.pred$asymp.UCL,
       type="l", col="red", lwd=1, lty="dashed")

