# ANOVA ad una via con lm()
# Marco Plebani, Maggio 2023

getwd() # per vedere la work directory corrente

# In RStudio:
# Session > set working directory > choose directory
# Raccomandazione: RStudio esegue direttamente il comando in console ma non lo salva nello script. Salviamolo noi:
setwd("/Users/marcoplebani/Desktop/Corso_R")

# apri il file di dati:
df <- read.delim("my_data/compare_6_species.txt", dec=".")

head(df)
tail(df)
str(df)
df$Species <- as.factor(df$Species)
# rappresentiamo i dati:
par(mfrow=c(1,1))
boxplot(body.mass ~ Species, data=df)
# la "tilde" significa "varia in dipendenza da"

# sembra ci sia una differenza. Testiamola statisticamente:

m1 <- lm(body.mass ~ Species, data=df)
m1

# verifichiamo le assunzioni del modello:

par(mfrow=c(2,2))
plot(m1)

# definiamo il modello nullo:

m0 <- lm(body.mass ~ 1, data=df)

# produciamo una tavola ANOVA:
anova(m1)

# confrontiamo i due modelli:
# con un test F:
anova(m0, m1)
# con l'Akaike Information Criterion
# (più basso il valore, migliore il modello)
AIC(m1, m0)

# R^2 eccetera:
summary(m1)

# test post-hoc:

# TukeyHSD: https://www.r-bloggers.com/2021/08/how-to-perform-tukey-hsd-test-in-r/

pairwise.t.test(x=df$body.mass, g=df$Species, p.adjust.method="bonferroni")

# per installare un pacchetto:
install.packages("emmeans")
# per caricare un pacchetto:
library(emmeans)

emmeans.pairwise <- emmeans(m1, 
                            pairwise ~ Species, 
                            adjust="bonferroni"
                            )

write.csv(emmeans.pairwise$contrasts, "confronti.csv")



