# Setzen des Working Directory
setwd("~/Documents/Master Data Science/Statistische Verfahren/Projekt/Statistische-Verfahren--Projekt-4.1-Buche/Aufgabenstellung")
# Laden der Daten aus CSV
buche.data = read.csv("buche.csv", sep=",")
head(buche.data)

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

# leaps
require("leaps")

# bartelink
buche.bss.bartelink = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.bartelink, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
# summary(buche.bss.bartelink)

summary(buche.bss.bartelink)$cp
summary(buche.bss.bartelink)$which[11,] # Modell 11 liefert betragsmaessig besten CP-Wert (-0.08103549)

# heller
buche.bss.heller = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.heller, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
# summary(buche.bss.heller)

summary(buche.bss.heller)$cp
summary(buche.bss.heller)$which[12,] # Modell 12 liefert betragsmaessig besten CP-Wert (-0.5130457)

# joosten
buche.bss.joosten = regsubsets(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(age)^2)+I(log(height))+I(log(height)^2)+hsl+I(hsl^2)+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten, nbest = 3) ## nbest = 3, jeweils die drei besten Modelle gleicher Parameterzahl
# summary(buche.bss.joosten)

summary(buche.bss.joosten)$cp
summary(buche.bss.joosten)$which[22,] # Modell 22 liefert betragsmaessig besten CP-Wert (8.344969)


# Beste Modelle pro Autor
m.bartelink = lm(log(biom)~1+ I(log(dbh))+I(log(age)^2)+I(log(height))+I(log(height)^2), data = buche.data.bartelink)
m.heller = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age)^2)+I(log(height)), data = buche.data.heller)
m.joosten = lm(log(biom)~1+ I(log(dbh))+I(log(dbh)^2)+I(log(age))+I(log(height))+I(log(height)^2)+hsl+I(log(hsl))+I(log(hsl)^2), data = buche.data.joosten)

# Berechnen der Parameter für die Modelle
summary(m.bartelink)
summary(m.heller)
summary(m.joosten)
