dev.off()
plot(multiple_regression)
par(mfrow=c(2,2))
plot(multiple_regression)

#Daten aufteilen
valc <- nalc[1:20]
vmental <- nmental[1:20]
vsuicide <- nsuicide[1:20]
asuicide <- nsuicide[21:40]

#Regressionsmodell erstellen
v_regression <- (lm(vsuicide ~ valc + vmental))
summary(v_regression)

#Vorhersage der Selbstmorde für zweiten Zeitabschnitt 
v_Vorhersage <- data.frame(valc,vmental)
psuicide <- predict(v_regression,v_Vorhersage)

#Vorhersagen vergleichen
bias_vorhersage <- (sum(sqrt(psuicide^2))) - (sum(sqrt(asuicide^2)))
rmse_vorhersage <- sqrt(mean((psuicide-asuicide)^2))

