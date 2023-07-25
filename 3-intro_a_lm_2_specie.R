# Confronto tra due gruppi con lm()
# Marco Plebani, Maggio 2023
rm(list=ls()) # ripuliamo la memoria di R

getwd() # per vedere la work directory corrente

# In RStudio:
# Session > set working directory > choose directory
# Raccomandazione: RStudio esegue direttamente il comando in console ma non lo salva nello script. Salviamolo noi:
setwd("/Users/marcoplebani/Desktop/Corso_R")

# apriamo il file di dati:
df <- read.csv("my_data/compare_2_species.csv")

head(df)
tail(df)
str(df)
df$Species <- as.factor(df$Species)
class(df$Species)
###############
# ESAME GRAFICO

# rappresentiamo i dati:
par(mfrow=c(1,1))
boxplot(body.mass ~ Species, data=df)
# la "tilde" significa "varia in dipendenza da"

# sembra ci sia una differenza. Testiamola statisticamente:

m1 <- lm(body.mass ~ Species, data=df)
m1

######################################
# VERIFICA DELLE ASSUNZIONI DEL MODELLO

# verifichiamo le assunzioni graficamente:

par(mfrow=c(2,2))
plot(m1)

# test quantitativi sulle assunzioni del modello:

# Omogeneità delle varianze
# installiamo lmtest (basta farlo una volta):
install.packages("lmtest")
# carichiamolo:
library(lmtest)
lmtest::bptest(m1)
# "The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression model (by default the same explanatory variables are taken as in the main regression model) and rejects if too much of the variance is explained by the additional explanatory variables."

# un altro test è quello di Goldfeld-Quandt. lmtest::gqtest()
str(m1)
# normalità dei residui:
shapiro.test(m1$residuals)

# outliers:
car::outlierTest(m1) # test di Bonferroni. Ce ne sono tanti altri.


######################################
# TEST DELLA SIGNIFICATIVITà DEL MODELLO

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

summary(m1)






