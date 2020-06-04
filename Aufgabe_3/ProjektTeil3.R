# Laden der Daten aus CSV
buche.data = read.csv("buche.csv", sep=",")
head(buche.data)
# Seed fuer den Random Number Generator
set.seed(0)

# Aufteilen des Datensatzes in Teildatensaetze jedes einzelogen Autors
buche.data.joosten = subset(buche.data, buche.data[,1] == "Joosten")

## Best subset selection
# 1. maximales Modell festlegen
# 2. alle Teilmodelle sind Kandidatenmodelle
# 3. Entscheidung basierend auf Mallow's Cp
# 4. Schaetzer fuer SPSE = Cp * sigma2.max + n* sigma2.max

## leaps
require("leaps")

# joosten
buche.bss.joosten = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
# summary(buche.bss.joosten)

summary(buche.bss.joosten)$cp
summary(buche.bss.joosten)$which[22,] # Modell 22 liefert betragsmaessig besten CP-Wert (8.344969)

m.joosten = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten)

## Simulation
# Genutzter Datensatz und Anzal der Outputs fuer Kreuzvalidierung
buche.data.used = buche.data.joosten

# Anzahl der Beobachtungen fuer den gewaehlten Datensatz
numberObs = nrow(buche.data.used)

cross.length.out = numberObs

# Initialisierung
SPSE = rep(0,4)
SPSE.theo = rep(0,4)
n = 100
reps = 100 
index = rep(1:10, length.out = cross.length.out)

par(mfrow = c(2,4))
hist(log(buche.data.joosten$dbh))
hist(log(buche.data.joosten$age))
hist(buche.data.joosten$hsl)
hist(log(buche.data.joosten$height))
hist(log(buche.data.joosten$dbh) + log(buche.data.joosten$dbh)^2)
hist(log(buche.data.joosten$age))
hist(log(buche.data.joosten$hsl) + log(buche.data.joosten$hsl)^2 + buche.data.joosten$hsl)
hist(log(buche.data.joosten$height) + log(buche.data.joosten$height)^2)


for(m in 1:4){
  for(j in 1:reps){
    index = sample(index)
    i = c(1:(m))
    ## Zerlegung
    buche.test= buche.data.used[index %in% i,]
    buche.design= buche.data.used[!(index %in% i),]
    
    ## Simulation
    biom.mean = mean(buche.design$biom)
    biom.sd = sd(buche.design$biom)
    #biom.sim = rnorm(n, mean = biom.mean, sd = biom.sd)
    
    age.mean = mean(buche.design$age)
    age.sd = sd(buche.design$age)
    age.sim = rnorm(n, mean = age.mean, sd = age.sd)
    
    dbh.mean = mean(buche.design$dbh)
    dbh.sd = sd(buche.design$dbh)
    dbh.sim = rnorm(n, mean = dbh.mean, sd = dbh.sd)
    
    height.mean = mean(buche.design$height)
    height.sd = sd(buche.design$height)
    height.sim = rnorm(n, mean = height.mean, sd = height.sd)
    
    hsl.mean = mean(buche.design$hsl)
    hsl.sd = sd(buche.design$hsl)
    hsl.sim = rnorm(n, mean = hsl.mean, sd = hsl.sd)
    
    m.joosten.design = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.design)
    buche.sim.temp = data.frame(age = age.sim, dbh = dbh.sim, height = height.sim, hsl = hsl.sim)
    biom.sim = exp(predict(m.joosten.design, newdata=buche.sim.temp))
    
    buche.sim = data.frame(hsl = hsl.sim, age = age.sim, dbh = dbh.sim, height = height.sim, biom = biom.sim)
    
    ## Parameterschaetzung
    m.joosten = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.sim)
    
    ## Erwarteter SPSE
    SPSE[m] = SPSE[m] + sum(log((buche.test$biom-predict(m.joosten, newdata=buche.test))^2))
  }
  SPSE[m] = SPSE[m]/reps
}

biom.sd = sd(log(buche.data.joosten$biom))
M = length(coef(m.joosten))
SPSE.theo = nrow(buche.data.joosten)*(biom.sd^2) + M * (biom.sd^2) #Biasterm ist Null

