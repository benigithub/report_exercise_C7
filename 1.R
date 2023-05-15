#Daten einlesen
list.files("C:\\Users\\Roger Moser\\Documents\\Reto\\Unibe\\Quantitative Methoden")
mental <- read.table("C:\\Users\\Roger Moser\\Documents\\Reto\\Unibe\\Quantitative Methoden\\mental.csv" ,header=TRUE ,sep=',')
alc <- read.table("C:\\Users\\Roger Moser\\Documents\\Reto\\Unibe\\Quantitative Methoden\\alc.csv" ,header=TRUE ,sep=',')
suicide <- read.table("C:\\Users\\Roger Moser\\Documents\\Reto\\Unibe\\Quantitative Methoden\\selbstmord.csv" ,header=TRUE ,sep=',')

#Gewünschte Daten aus Tabelle in Vektor speichern
nmental <- mental[,4]
nalc <- alc[,4]
nsuicide <- suicide[,4]
nalc <- nalc[1:198]

#Der Median scheint für mein Vorhaben besser zu passen, da die Daten von vielen verschiedenen Ländern stammen

#Median berechen
medmental <- median(nmental)
medalc <- median(nalc)
medsuicide <- median(nsuicide)

#Standartabweichung besser als Varianz, da auch negativ sein kann

#Standartabweichung berechnen
samental <- sd(nmental)
saalc <- sd(nalc)
sasuicide <- sd(nsuicide)

#Kontingenztabellen
#Eine gute und aussagekräftige KOntingenztabelle zu erstellen, sind meine Daten nicht gut geeignet, da sie aus vielen Ländern aus vielen Jahren stammen.

