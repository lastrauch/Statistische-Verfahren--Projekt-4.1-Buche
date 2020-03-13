
##########################################################################
### Task 3.1: Vergleich der Genauigkeit der Schaetzwerte von (M1) und (M2)
##########################################################################

### Daten vorbereiten
buche.data <- read.csv("buche.csv")
joosten.data <- subset(buche.data, author == "Joosten")
joosten.data$logdbh <- log(joosten.data$dbh)
joosten.data$logbiom <- log(joosten.data$biom)

### Modelle erstellen
m1 <- lm(logbiom~1+logdbh, data = joosten.data)
coefficients(m1)
var.m1 <- sum((joosten.data$logbiom-predict(m1))^2) / (length(joosten.data$logbiom)-length(coefficients(m1)))
m2 <- glm(biom~1+logdbh, data=joosten.data, family="Gamma"(link='log'))
coefficients(m2)

### plot der originalen und durch die modelle m1 und m2 vorhergesagten werte auf normaler und log-skala
par(mfrow=c(1,2))
plot(joosten.data$dbh,joosten.data$biom,pch=4,col=1,xlab="Brusthoehendurchmesser",ylab="Biomasse")
points(joosten.data$dbh,exp(predict(m1)),pch=4,col=2)
plot(joosten.data$dbh,joosten.data$biom,pch=4,col=1,xlab="Brusthoehendurchmesser",ylab="Biomasse")
points(joosten.data$dbh,exp(predict(m2)),pch=4,col=2)
par(mfrow=c(1,2))
plot(joosten.data$logdbh,joosten.data$logbiom,pch=4,col=1,xlab="Brusthoehendurchmesser",ylab="Biomasse")
points(joosten.data$logdbh,predict(m1),pch=4,col=2)
plot(joosten.data$logdbh,joosten.data$logbiom,pch=4,col=1,xlab="Brusthoehendurchmesser",ylab="Biomasse")
points(joosten.data$logdbh,predict(m2),pch=4,col=2)

### Differenz der von (M1) und (M2) vorhergesagten Biomassen
par(mfrow=c(1,1))
plot(joosten.data$dbh,exp(predict(m2))-exp(predict(m1)),xlab="Brusthoehendurchmesser",ylab="Biomasse (M2) - Biomasse (M1)")

### Abweichungen von originalen und durch die modelle (M1) und (M2) vorhergesagten werte auf log-skala
diff1.log <- abs( joosten.data$logbiom - predict(m1) )
diff2.log <- abs( joosten.data$logbiom - predict(m2) )

### Abweichungen von originalen und durch die modelle (M1) und (M2) vorhergesagten werte auf urspruenglicher skala
beta1.m1 <- exp(coefficients(m1)["(Intercept)"])
beta2.m1 <- coefficients(m1)["logdbh"]
beta1.m2 <- exp(coefficients(m2)["(Intercept)"])
beta2.m2 <- coefficients(m2)["logdbh"]
diff1 <- abs( beta1.m1*joosten.data$dbh^beta2.m1 - joosten.data$biom )
diff2 <- abs( beta1.m2*joosten.data$dbh^beta2.m2 - joosten.data$biom )
par(mfrow=c(1,1))
plot(joosten.data$dbh,diff1,pch=4,col=2,xlab="Brusthoehendurchmesser",ylab="Biomasse")
points(joosten.data$dbh,diff2,pch=4,col=3)
### nicht so sinnvoll, da schmale staemme natuerlich eine geringere absolute abweichung als dicke haben. deshalb hier die relative abweichung:
diff1.rel <- diff1/joosten.data$biom
diff2.rel <- diff2/joosten.data$biom
par(mfrow=c(1,1))
plot(joosten.data$dbh,diff1.rel,pch=4,col=2,xlab="Brusthoehendurchmesser",ylab="relative Biomassendifferenz")
points(joosten.data$dbh,diff2.rel,pch=4,col=3)
### nach erstem eindruck ist keines der modelle viel besser. wir schauen uns mal noch die summen der relativen abweichungen an
sum(diff1.rel)
sum(diff2.rel)
### ...fast gar kein unterschied, (M1) minimal besser

### quadratische Abweichung des modells (M1) von den Messwerten
var.m1 <- sum(diff1^2)/(length(diff1)-1)
var.m2 <- sum(diff2^2)/(length(diff2)-1)
### quadratische Abweichung des modells (M2) von den Messwerten
sigma.m1 <- sqrt(var.m1)
sigma.m2 <- sqrt(var.m2)
### von diesem standpunkt her ist also (M1) das bessere modell, wobei die unterschiede sehr gering sind




#########################################################################
### Task 3.2: Eigenschaften des Schaetzers fuer beta2 in (M1) und (M2)
#########################################################################

### berechnung der schaetzermatrizen
summary(m2)
phi <- 0.02967372 # Dispersionsparameter
# Berechnung der Schaetzermatrizen
n <- seq(2,100,1)#c(25,50,75,100,150,200,300,400,500) # anzahlen der aus ursprungsdaten verwendeten werte
N <- 0
m <- 1000 # anzahl der pseudobeobachtungen
dbh.N <- sample(joosten.data$dbh,n[1],replace=TRUE) # initialisierung des vektors der verwendeten ursprungs-dbh-werte
beta2.m1.sim <- matrix(0,m,length(n)) # initialisierung der schaetzermatrix fuer (M1)
beta2.m2.sim <- matrix(0,m,length(n)) # initialisierung der schaetzermatrix fuer (M2)
for( j in 1:length(n) )
{
  N <- n[j]
  dbh.N <- c(dbh.N , sample(joosten.data$dbh,N-n[max(c(1,j-1))],replace=TRUE)) # vektor der verwendeten ursprungs-dbh-werte
  mu <- exp(beta1.m2)*dbh.N^beta2.m2 # erwartungswerte um die gammaverteilte pseudobeobachtungen gemacht werden achtung, hier kommen wegen verzerrung bei ruecktransformation des log groessere werte raus, als wir durch preedict(m2) bekommen hatten, obwohl das max der dbh werte hier das selbe wie bei predict(m2) ist!
  for( M in 1:m)
  {
    biom.N.M <- rgamma(length(mu),shape=1/phi,scale=phi*mu) # um mu gammaverteilte pseudobeobachtungen
    sim.N.M.data <- data.frame(biom.N.M,dbh.N,logbiom.N.M=log(biom.N.M),logdbh.N=log(dbh.N))
      #diff <- biom.N.M - mu # nur zum testen
      #plot(dbh.N,diff/biom.N.M) # nur zum testen
    m1.N.M <- lm(logbiom.N.M ~ 1+logdbh.N,data=sim.N.M.data) # ML-schaetzer fuer die simulierten werte nach (M1)
    m2.N.M <- glm(biom.N.M ~ 1+logdbh.N,data=sim.N.M.data,family="Gamma"(link='log')) # ML-schaetzer fuer die simulierten werte nach (M1)
      #par(mfrow=c(1,2)) # nur zum testen
      #plot(sim.N.M.data$logdbh.N,predict(m1.N.M)) # nur zum testen
      #plot(sim.N.M.data$logdbh.N,predict(m2.N.M)) # nur zum testen
    beta2.m1.sim[M,j] <- coefficients(m1.N.M)["logdbh.N"] # koeffizient beta2 nach (M1) in schaetzermatrix fuer (M1) schreiben
    beta2.m2.sim[M,j] <- coefficients(m2.N.M)["logdbh.N"] # koeffizient beta2 nach (M2) in schaetzermatrix fuer (M2) schreiben
  }
}

# untersuchung der genauigkeit der schaetzer fuer beta2 mit boxplots
k <- 5 # anzahl im boxplot auf abszisse dargestellter werte
x <- seq(2,max(n),max(n)/k) # anzahlen von dbh-werten, deren schaetzer fuer beta2 im boxplot verwendet werden sollen
data.m1 <- data.frame(beta2.m1.sim[,x]) # entsprechende daten fuer (M1) auswaehlen
colnames(data.m1) <- seq(1,max(n)-1,max(n)/k)
data.m2 <- data.frame(beta2.m2.sim[,x]) # entsprechende daten fuer (M2) auswaehlen
colnames(data.m2) <- seq(1,max(n)-1,max(n)/k)
# erstellung der boxplots
par(mfrow=c(1,2))
boxplot(data.m1,xlab="Anzahl verwendeter Messwerte",ylab="Schaetzer im Modell (M1)")
abline(a=beta2.m2,b=0,col=2)
boxplot(data.m2,xlab="Anzahl verwendeter Messwerte",ylab="Schaetzer im Modell (M2)")
abline(a=beta2.m2,b=0,col=2)

### untersuchung der erwartungstreue der schaetzer fuer beta2
mean.beta2.m1 <- numeric(0)
mean.beta2.m2 <- numeric(0)
for (i in 1:length(n))
{
  mean.beta2.m1 <- c(mean.beta2.m1,mean(beta2.m1.sim[1:m,i]))
  mean.beta2.m2 <- c(mean.beta2.m2,mean(beta2.m2.sim[1:m,i]))
}
par(mfrow=c(1,2))
plot(n,mean.beta2.m1,xlab="Anzahl verwendeter Messwerte",ylab="Schaetzer im Modell (M1)",ylim=c(2.524,2.537))
abline(a=beta2.m2,b=0,col=2)
plot(n,mean.beta2.m2,xlab="Anzahl verwendeter Messwerte",ylab="Schaetzer im Modell (M2)",ylim=c(2.524,2.537))
abline(a=beta2.m2,b=0,col=2)


###############################################################################################################
# ab hier versucht unterschied zwischen (M1) und (M2) sichtbar zu machen - das fuehrt zu nix aussagekraeftigem!
###############################################################################################################
diffe.1 <- abs(mean.beta2.m1[1:i] - beta2.m2)
diffe.2 <- abs(mean.beta2.m2[1:i] - beta2.m2)
diffe.1-diffe.2
sum(diffe.1[1:20])
par(mfrow=c(1,1))
plot(n,diffe.1)
points(n,diffe.2,col="2")
# berechnung der summe der absoluten differenzen der erwartungswerte zum echten wert fuer (M1) und (M2), 
# Betrachtung des Verhaeltnisses dieser Werte fuer verschiedene Messwertanzahlen
ratio <- numeric(0)
a <- numeric(0)
b <- numeric(0)
for (i in 1:length(n))
{
  diff.beta2.m1 <- sum(abs(mean.beta2.m1[1:i] - beta2.m2))
  diff.beta2.m2 <- sum(abs(mean.beta2.m2[1:i] - beta2.m2))
  a <- c(a,abs(mean.beta2.m1[i:i+5] - beta2.m2))
  b <- c(b,abs(mean.beta2.m2[i:i+5] - beta2.m2))
  ratio <- c(ratio,diff.beta2.m1 / diff.beta2.m2)
}
par(mfrow=c(1,1))
plot(n,ratio,xlab="Anzahl verwendeter Messwerte",ylab="Verhaeltnis summierter Differenzen von (M1) und (M2)")


