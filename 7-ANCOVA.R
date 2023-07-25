# Lo scenario "ANCOVA"

rm(list=ls())
setwd("/Users/marcoplebani/Desktop/Corso_R")

df <- read.csv("my_data/ANCOVA_data.csv")

plot(df)
str(df)
df$Species <- as.factor(df$Species)
library(ggplot2)
par(mfrow=c(1,1))

# rappresentiamo i dati:

# per definire il colore di riempimento dei punti va indicato fill=Species
ggplot(data=df, aes(x=depth, y=alga.length, fill=Species)) +
  geom_point(shape=21) + # shape indica il tipo di punto da usare
  # colore di riempimento personalizzato:
  scale_fill_manual("Species", values=c("black", "white")) + 
  theme_classic()

# indichiamo i modelli:

lm.interactive <- lm(alga.length ~ depth * Species, data=df)
# è come scrivere il modello completo (qui sotto) in versione "compatta":
lm.interactive.b <- lm(alga.length ~ depth + Species + depth * Species, data=df)

# corrisponde al modello lineare seguente:
# alga.length_i = mu + d*depth + k*Species_k + w * depth * Species + epsilon_i

lm.additive <- lm(alga.length ~ depth + Species, data=df)

lm.species <- lm(alga.length ~ Species, data=df)

lm.depth <- lm(alga.length ~ depth, data=df)

lm0 <- lm(alga.length ~ 1, data=df)

# controlliamo le assunzioni

par(mfrow=c(2,2))
plot(lm.interactive)

# confronto tra modelli

AIC(lm.interactive, 
    lm.additive, 
    lm.species, 
    lm.depth, 
    lm0)

anova(lm.interactive, # lm.interactive è meglio
      lm.additive)

# il problema della tavola ANOVA tradizionale: se il design sperimentale non fosse bilanciato, i risultati dei test statistici cambierebbere a seconda dell'ordine con cui i predittori sono stati definiti nel modello.
# Esempio:
df1 <- df[1:80,] # ho tolto 20 osservazioni per la specie B
lm.interactive.b <- lm(alga.length ~ depth + Species + depth * Species, data=df1)
lm.interactive.c <- lm(alga.length ~ Species + depth + Species * depth, data = df1)

anova(lm.interactive.b)
anova(lm.interactive.c)



# rappresento il modello "vincente":

# per definire il colore di riempimento dei punti va indicato fill=Species
ggplot(data=df, aes(x=depth, y=alga.length, fill=Species, color=Species)) +
  geom_point(shape=21) + # shape indica il tipo di punto da usare
  # colore personalizzato dei punti:
  scale_color_manual("Species", values=c("black", "black")) +
  # colore di riempimento personalizzato:
  scale_fill_manual("Species", values=c("black", "white")) + 
  # linee di regressione personalizzate:
  geom_smooth(method="lm",  fill="grey", aes(linetype=Species)) +
  theme_classic()

# rappresentazione alternativa degli intervalli di confidenza:
ggplot(data=df, aes(x=depth, y=alga.length, fill=Species, color=Species)) +
  geom_point(shape=21) + 
  scale_color_manual("Species", values=c("black", "black")) +
  scale_fill_manual("Species", values=c("black", "white")) + 
  stat_smooth(method="lm",  se=F, aes(linetype=Species)) +
  stat_smooth(method = "lm", colour = "black", geom = "ribbon", fill = NA, linetype="dotted", aes(group=Species)) +
  theme_classic()


# estraiamo i parametri del modello

library(emmeans)
emtrends(lm.interactive, pairwise ~ Species, var="depth")
emmeans(lm.interactive, specs = ~ Species * depth, at=list(depth=13))
