# Setzen des Working Directory
# setwd("~/Documents/Master Data Science/Statistische Verfahren/Projekt/Statistische-Verfahren--Projekt-4.1-Buche/Aufgabenstellung")
# Laden der Daten aus CSV
buche.data = read.csv("buche.csv", sep=",")
head(buche.data)
# Seed für den Random Number Generator
set.seed(0)

# Aufteilen des Datensatzes in Teildatensätze jedes einzelogen Autors
buche.data.joosten = subset(buche.data, buche.data[,1] == "Joosten")

## Best subset selection
# 1. maximales Modell festlegen
# 2. alle Teilmodelle sind Kandidatenmodelle
# 3. Entscheidung basierend auf Mallow's Cp
# 4. Schätzer für SPSE = Cp * sigma2.max + n* sigma2.max

## leaps
require("leaps")

# joosten
buche.bss.joosten = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
# summary(buche.bss.joosten)

summary(buche.bss.joosten)$cp
summary(buche.bss.joosten)$which[22,] # Modell 22 liefert betragsmäßig besten CP-Wert (8.344969)

m.joosten = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten)

## Simulation
# Genutzter Datensatz und Anzal der Outputs für Kreuzvalidierung
buche.data.used = buche.data.joosten

# Anzahl der Beobachtungen für den gewählten Datensatz
numberObs = nrow(buche.data.used)

cross.length.out = numberObs

# Initialisierung
SPSE1 =SPSE2 =SPSE3 =SPSE4 =SPSE5 =SPSE6 =SPSE7=SPSE8 =0

## Kreuzalidierung
index = rep(1:5, length.out = cross.length.out) # 80 % Trainingsdaten


for(j in 1:100){
  index = sample(index)
  i = 1
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
