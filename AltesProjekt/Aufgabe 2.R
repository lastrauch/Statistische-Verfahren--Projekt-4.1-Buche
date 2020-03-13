### Daten vorbereiten
## Daten einlesen
buche.data <- read.csv("buche.csv")
## Daten logarithmieren
buche.logdata <- cbind(subset(buche.data, select = author),log(subset(buche.data, select = hsl:biom)))

### Ansatz: Einzelne Betrachtung der beiden Autoren Bartelink und Heller im Verhältnis 
### zu Joosten. Es kann auch nur ein Autor mit einer Abhängigkeit mit einer Einflussgröße 
### bei der Suche in das Modell hinein genommen werden.
## Erstellen 2 Spalten für Bartelink und Heller mit binärem Wertebereich für einzelne Einflüsse.
buche.logdata$barte <- ifelse(buche.logdata$author=="Bartelink", 1, 0)
buche.logdata$heller <- ifelse(buche.logdata$author=="Heller", 1, 0)

## Maximal- und Minimalmodell erstellen
m0 <- lm(biom~1, data = buche.logdata)
mmax <- lm(biom~(dbh+height+age+hsl+heller+barte)^2,data= buche.logdata) #Keine Koeffizienten für hsl:heller, hsl:barte, heller:barte

## Vorwärts- und Ruckwärtsselektion - Schritte entprechen Reihenfolge:
## Vorwärts- und Ruckwärssuche
## biom ~ dbh+height+dbh:height+hsl+height:hsl+barte+age+dbh:hsl
result1 <- step(m0,scope = ~(1+dbh+height+age+hsl+heller+barte)^2)
## Rückwärtsselektion - Schritte entprechen Reihenfolge:
## -barte:heller/hsl:heller/hsl:barte-heigth:barte-age:hsl-height:age-age:heller-dbh:barte-age:barte-dbh:age-age-height:heller-dbh:hsl
## --> biom ~ dbh+height+hsl+heller+barte+dbh:height+dbh:heller+height:hsl
result2 <- step(mmax,scope = ~1,direction = "backward")

## AIC, Varianz und Devianz beider Modelle
AIC(result1,result2) #-132.18 & -134.74
sum( (buche.logdata$biom-predict(result1))^2 ) / (length(buche.logdata$biom)-length(coefficients(result1)))
sum( (buche.logdata$biom-predict(result2))^2 ) / (length(buche.logdata$biom)-length(coefficients(result2)))
deviance(result1) # n-|M| = 181-9
deviance(result2) # n-|M| = 181-9

##Koeffizienten der beiden Modelle
coef(result1)
coef(result2)

### Visualisierung Unterschied Daten für Modell 2
## Subsets der logarithmierten und Originaldaten
barte.data <- subset(buche.data, author == "Bartelink")
heller.data <- subset(buche.data, author == "Heller")
joosten.data <- subset(buche.data, author == "Joosten")
barte.logdata <- subset(buche.logdata, author == "Bartelink")
heller.logdata <- subset(buche.logdata, author == "Heller")
joosten.logdata <- subset(buche.logdata, author == "Joosten")

## Vorhersagewerte
barte.pred <- exp(predict(result1, barte.logdata))
heller.pred <- exp(predict(result1, heller.logdata))
joosten.pred <- exp(predict(result1, joosten.logdata))

## Diagramme - rot = Bartelink, grün = Heller, blau = Joosten
par(mfrow=c(2,2))
plot(barte.data$dbh, barte.data$biom, col = 2, ylim = c(0,800), main = "Originaldaten", xlab = "Brusthöhendurchmesser", ylab = "Biomasse")
points(heller.data$dbh, heller.data$biom, col = 3)
points(joosten.data$dbh, joosten.data$biom, col = 4)
plot(barte.logdata$dbh, barte.logdata$biom, col = 2, ylim = c(-0.5,7), main = "Originaldaten - logarithmiert", xlab = "Logarithmierter Brusthöhendurchmesser", ylab = "Logarithmierte Biomasse")
points(heller.logdata$dbh, heller.logdata$biom, col = 3)
points(joosten.logdata$dbh, joosten.logdata$biom, col = 4)
plot(barte.data$dbh, barte.pred, col = 2, ylim = c(0,800), main = "Vorhersagedaten", xlab = "Brusthöhendurchmesser", ylab = "Biomasse gemäß Modell")
points(heller.data$dbh, heller.pred, col = 3)
points(joosten.data$dbh, joosten.pred, col = 4)
plot(barte.logdata$dbh, log(barte.pred), col = 2, ylim = c(-0.5,7), main = "Vorhersagedaten - logarithmiert", xlab = "Logarithmierter Brusthöhendurchmesser", ylab = "Logarithmierte Biomasse gemäß Modell")
points(heller.logdata$dbh, log(heller.pred), col = 3)
points(joosten.logdata$dbh, log(joosten.pred), col = 4)