 # ANOVA a due vie


rm(list=ls())
setwd("/Users/marcoplebani/Desktop/Corso_R")

df <- read.csv("my_data/2way_ANOVA_data.csv")

str(df)
df$Species <- as.factor(df$Species)
df$Treatment <- as.factor(df$Treatment)

library(ggplot2)
ggplot(data=df, aes(x=Treatment, y=body.mass, color=Species)) + geom_boxplot() + theme_classic()

# confrontiamo i modelli:

lm.interactive <- lm(body.mass ~ Treatment * Species, data=df)

# corrisponde al modello lineare seguente:
# alga.length_i = mu + d*depth + k*Species_k + w * depth * Species + epsilon_i

lm.additive <- lm(body.mass ~ Treatment + Species, data=df)

lm.species <- lm(body.mass ~ Species, data=df)

lm.Treatment <- lm(body.mass ~ Treatment, data=df)

lm0 <- lm(body.mass ~ 1, data=df)

# controlliamo graficamente che le assunzioni siano rispettate:

par(mfrow=c(2,2))
plot(lm.interactive)
plot(lm.additive)
plot(lm.species)
plot(lm.Treatment) # residui non-normali
plot(lm0) # residui non-normali

# confrontiamo i modelli:

anova(lm.interactive, lm.additive)
anova(lm.additive, lm.species)
anova(lm.additive, lm.Treatment)

anova(lm.species, lm0)

# lm.species è il modello "vincente"

library(emmeans)
emmeans(lm.species, pairwise ~ Species)
