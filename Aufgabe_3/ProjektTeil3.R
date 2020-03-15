# Als template wurde Aufgabe8.R benutzt
# Dataset vs. Paper notations
# dbh := D
# age := A
# height := H
# hsl := HSL
# biom := W
# Muss noch begründen, wie wir auf die quadratischen Terme kommen

# Setzen des Working Directory
setwd("C:/Users/Matthias/OneDrive/SV/Projekt")
# Laden der Daten aus CSV
buche.data = read.csv("buche.csv", sep=",")
head(buche.data)
# Seed für den Random Number Generator
set.seed(0)

# Aufteilen des Datensatzes in Teildatensätze jedes einzelogen Autors
buche.data.bartelink = subset(buche.data, buche.data[,1] == "Bartelink") ## Verkleinern des Datensatzes
buche.data.heller = subset(buche.data, buche.data[,1] == "Heller")
buche.data.joosten = subset(buche.data, buche.data[,1] == "Joosten")

# Plots
par(mfrow = c(2, 3))
plot(log(buche.data$dbh), log(buche.data$biom),ylim = c(-5, 10))
plot(log(buche.data$age), log(buche.data$biom))
plot(log(buche.data$height), log(buche.data$biom))
plot(log(buche.data$hsl), log(buche.data$biom))
plot((buche.data$hsl), log(buche.data$biom))
plot((buche.data$author), log(buche.data$biom))


## Best subset selection
# 1. maximales Modell festlegen
# 2. alle Teilmodelle sind Kandidatenmodelle
# 3. Entscheidung basierend auf Mallow's Cp
# 4. Schätzer für SPSE = Cp * sigma2.max + n* sigma2.max

## leaps
require("leaps")

# bartelink
buche.bss.bartelink = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.bartelink, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
summary(buche.bss.bartelink)

summary(buche.bss.bartelink)$cp
summary(buche.bss.bartelink)$which[11,] # Modell 11 liefert betragsmäßig besten CP-Wert (-0.08103549)

# heller
buche.bss.heller = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.heller, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
summary(buche.bss.heller)

summary(buche.bss.heller)$cp
summary(buche.bss.heller)$which[12,] # Modell 12 liefert betragsmäßig besten CP-Wert (-0.5130457)

# joosten
buche.bss.joosten = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
summary(buche.bss.joosten)

summary(buche.bss.joosten)$cp
summary(buche.bss.joosten)$which[22,] # Modell 22 liefert betragsmäßig besten CP-Wert (8.344969)


## Simulation
# Genutzter Datensatz und Anzal der Outputs für Kreuzvalidierung
buche.data.used = buche.data.joosten
cross.length.out = 3082

# Anzahl der Beobachtungen für den gewählten Datensatz
numberObs = nrow(buche.data.used)

## Kreuzalidierung
index = rep(1:numberObs, length.out = cross.length.out)
index = sample(1:numberObs, cross.length.out, replace=T)

table(index)
index = rep(1:numberObs, length.out=cross.length.out)
# index = rep(1:numberObs, length.out=250) ## Verkleinerter Datensatz
table(index)
index = sample(index)

# Initialisierung
SPSE1 =SPSE2 =SPSE3 =SPSE4 =SPSE5 =SPSE6 =SPSE7=SPSE8 =0

for(i in 1:numberObs){
  ## Zerlegung
  buche.test= buche.data.used[index==i,]
  buche.train= buche.data.used[index!=i,]

  ## Parameterschaetzung
  m1 <- lm(log(biom)~1+ I(log(dbh)), data =buche.train)
  m2 <- lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2), data =buche.train)
  m3 <- lm(log(biom)~1+ I(log(dbh))+I(log(height)), data =buche.train)
  m4 <- lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(height))+I(log(height)^2)+I(log(age))+hsl, data =buche.train)
  # m5 <- lm(log(biom)~(1+ I(log(dbh))+I(log(height))+I(log(age))+hsl)*as.factor(author), data =buche.train)
 
  ## Prognosefehler
  SPSE1 = SPSE1 + sum((log(buche.test$biom)-predict(m1, newdata=buche.test))^2)
  SPSE2 = SPSE2 + sum((log(buche.test$biom)-predict(m2, newdata=buche.test))^2)
  SPSE3 = SPSE3 + sum((log(buche.test$biom)-predict(m3, newdata=buche.test))^2)
  SPSE4 = SPSE4 + sum((log(buche.test$biom)-predict(m4, newdata=buche.test))^2)
  # SPSE5 = SPSE5 + sum((log(buche.test$biom)-predict(m5, newdata=buche.test))^2)

}
c(SPSE1 ,SPSE2 ,SPSE3 ,SPSE4)


## Schätzung auf Grund von RSS
m1 <- lm(log(biom)~1+ I(log(dbh)), data =buche.train)
m2 <- lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2), data =buche.train)
m3 <- lm(log(biom)~1+ I(log(dbh))+I(log(height)), data =buche.train)
m4 <- lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(height))+I(log(height)^2)+I(log(age))+hsl, data =buche.train)
# m5 <- lm(log(biom)~(1+ I(log(dbh))+I(log(height))+I(log(age))+hsl)*as.factor(author), data =buche.train)

RSS1 = sum(residuals(m1)^2)
RSS2 = sum(residuals(m2)^2)
RSS3 = sum(residuals(m3)^2)
RSS4 = sum(residuals(m4)^2)
# RSS5 = sum(residuals(m5)^2)


c(RSS1, RSS2, RSS3, RSS4)
summary(m4)
sigma2.max = RSS4/(cross.length.out-5) # Wie wird sigma2.max berechnet? woher kam bei aufgabe 8 die -19? Number of coefficients in max model?

SPSE1 = RSS1 + 2*sigma2.max*length(coef(m1))
SPSE2 = RSS2 + 2*sigma2.max*length(coef(m2))
SPSE3 = RSS3 + 2*sigma2.max*length(coef(m3))
SPSE4 = RSS4 + 2*sigma2.max*length(coef(m4))
# SPSE5 = RSS5 + 2*sigma2.max*length(coef(m5))

c(SPSE1 ,SPSE2 ,SPSE3 ,SPSE4)

## Best subset selection
# 1. maximales Modell festlegen
# 2. alle Teilmodelle sind Kandidatenmodelle
# 3. Entscheidung basierend auf Mallow's Cp
# 4. Schätzer für SPSE = Cp * sigma2.max + n* sigma2.max

## leaps
require("leaps")

buche.bss = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(height))+I(log(height)^2)+I(log(age))+hsl, data = buche.data.used, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
summary(buche.bss)

summary(buche.bss)$cp
summary(buche.bss)$which[7,]

summary(buche.bss)$cp*sigma2.max + cross.length.out* sigma2.max
