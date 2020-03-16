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
SPSE = c(1:4)

## Kreuzalidierung
index = rep(1:10, length.out = cross.length.out)

for(m in 2:5){
  for(j in 1:100){
    index = sample(index)
    i = c(1:m)
    ## Zerlegung
    buche.test= buche.data.used[index %in% i,]
    buche.train= buche.data.used[!(index %in% i),]
    
    ## Simulation
    
  
    ## Parameterschaetzung
    m.joosten = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.train)
   
    ## Prognosefehler
    SPSE[m-1] = SPSE[m-1] + sum((log(buche.test$biom)-predict(m.joosten, newdata=buche.test))^2)
  }
  SPSE[m-1] = SPSE[m-1]/100
}

## Schätzung auf Grund von RSS
m.joosten = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.test)

RSS = sum(residuals(m.joosten)^2)


summary(m.joosten)
sigma2.max = RSS/(cross.length.out-9) # Wie wird sigma2.max berechnet? woher kam bei aufgabe 8 die -19? Number of coefficients in max model?

SPSE1 = RSS + 2*sigma2.max*length(coef(m.joosten))

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

