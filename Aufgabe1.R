buche.data = read.csv("buche.csv")
head(buche.data)

# Daten logarithmieren
buche.log <- cbind(subset(buche.data, select = author), log(subset(buche.data, select = hsl:biom)))

# Aufteilen des Datensatzes in Teildatensätze jedes einzelogen Autors
buche.bartelink <- subset(buche.log, author == "Bartelink")
buche.heller <- subset(buche.log, author == "Heller")
buche.joosten <- subset(buche.log, author == "Joosten")

# Ausgangsmodelle erstellen
m0.bartelink <- lm(biom ~ 1, data = buche.bartelink)
m0.heller <- lm(biom ~ 1, data = buche.heller)
m0.joosten <- lm(biom ~ 1, data = buche.joosten)

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
var.bartelink <- sum((buche.bartelink$biom - predict(m.bartelink)) ^ 2) / (length(buche.bartelink$biom) - length(coefficients(m.bartelink)))
var.heller <- sum((buche.heller$biom - predict(m.heller)) ^ 2) / (length(buche.heller$biom) - length(coefficients(m.heller)))
var.joosten <- sum((buche.joosten$biom - predict(m.joosten)) ^ 2) / (length(buche.joosten$biom) - length(coefficients(m.joosten)))

#### Extra: Daten visualisieren
par(mfrow = c(2, 3))
plot(buche.log$dbh, buche.log$biom)
plot(buche.log$age, buche.log$biom)
plot(buche.log$height, buche.log$biom)
plot(buche.log$hsl, buche.log$biom)
plot(buche.log$author, buche.log$biom)

#Residuen vs. dbh im Bartelink-Modell
plot(buche.bartelink$dbh, residuals(m.bartelink), ylim = c(-0.6, 0.6))
abline(0, 0)

