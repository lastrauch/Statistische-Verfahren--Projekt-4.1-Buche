# Daten einlesen
data.all <- read.csv("buche.csv")

# Daten logarithmieren
data.log <- cbind(subset(data.all, select = author), log(subset(data.all, select = hsl:biom)))

# 3 logarithmierte Teildatensätze erstellen
data.bartelink <- subset(data.log, author == "Bartelink")
data.heller <- subset(data.log, author == "Heller")
data.joosten <- subset(data.log, author == "Joosten")

# Ausgangsmodelle erstellen
m0.bartelink <- lm(biom ~ 1, data = data.bartelink)
m0.heller <- lm(biom ~ 1, data = data.heller)
m0.joosten <- lm(biom ~ 1, data = data.joosten)

# Vorwärts- und Ruckwärtssuche mit Hilfe AIC nach besten Modellen
## Modell Bartelink - Schritte entprechen Reihenfolge: biom ~ dbh + age
m.bartelink <- step(m0.bartelink, scope = ~ (dbh + height + age + hsl) ^ 2)

## Modell Heller - Schritte entprechen Reihenfolge: biom ~ dbh + height
m.heller <- step(m0.heller, scope = ~ (dbh + height + age + hsl) ^ 2)

## Modell Joosten - Schritte entprechen Reihenfolge: biom ~ dbh + height + hsl
m.joosten <- step(m0.joosten, scope = ~ (dbh + height + age + hsl) ^ 2)

## AIC für die gefundenen Modelle
AIC(m.bartelink)
AIC(m.heller)
AIC(m.joosten)

## Devianz der gefundenen Modelle (gutes Modell: Devianz ≈ n - |M|)
deviance(m.bartelink) # n-|M| = 36-2
deviance(m.heller)    # n-|M| = 29-2
deviance(m.joosten)   # n-|M| = 116-3

## Berechnung Varianzen
var.bartelink <- sum((data.bartelink$biom - predict(m.bartelink)) ^ 2) / (length(data.bartelink$biom) - length(coefficients(m.bartelink)))
var.heller <- sum((data.heller$biom - predict(m.heller)) ^ 2) / (length(data.heller$biom) - length(coefficients(m.heller)))
var.joosten <- sum((data.joosten$biom - predict(m.joosten)) ^ 2) / (length(data.joosten$biom) - length(coefficients(m.joosten)))

#### Extra: Daten visualisieren
par(mfrow = c(2, 3))
plot(data.log$dbh, data.log$biom)
plot(data.log$age, data.log$biom)
plot(data.log$height, data.log$biom)
plot(data.log$hsl, data.log$biom)
plot(data.log$author, data.log$biom)

#Residuen vs. dbh im Bartelink-Modell
plot(data.bartelink$dbh, residuals(m.bartelink), ylim = c(-0.6, 0.6))
abline(0, 0)
