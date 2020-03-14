# Setzen des Working Directory
setwd("C:/Users/Matthias/OneDrive/SV/Projekt")
# Laden der Daten aus CSV
buche.data = read.csv("buche.csv", sep=",")
head(buche.data)

## Best subset selection
# 1. maximales Modell festlegen
# 2. alle Teilmodelle sind Kandidatenmodelle
# 3. Entscheidung basierend auf Mallow's Cp

# leaps
require("leaps")

buche.bss = regsubsets(log(biom)~(1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2))*as.factor(author), data = buche.data, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
# summary(buche.bss)
summary(buche.bss)$cp
summary(buche.bss)$which[22,] # Modell 22 liefert betragsmaessig besten CP-Wert (4.248051)

# Bestes Modell mit Autor als kategorielle Einflussgroesse
# m = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(height))+I(log(height)^2)+as.factor(author)Joosten+I(log(height)):as.factor(author)Joosten+I(log(height)^2):as.factor(author)Joosten+I(hsl^2):as.factor(author)Joosten, data = buche.data)
m = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(height))+I(log(height)^2)+as.factor(author)+I(log(height)):as.factor(author)+I(log(height)^2):as.factor(author)+I(hsl^2):as.factor(author), data = buche.data)

# Berechnen der Parameter fuer die Modelle
summary(m)
