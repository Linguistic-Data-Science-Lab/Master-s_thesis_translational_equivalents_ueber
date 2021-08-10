library(xtable)
library(googleVis)
library(readxl)

###### Daten einlesen und Datenstrukturen erstellen #####

# Einlesen der Daten und Erstellen des Data-Frame (Vor Ausführung muss ggf. das working directory mit setwd() auf den Ordner gesetzt werden, der diese Datei enthält)
Annotationen_kombiniert <- read_excel("../Daten/Annotationen_kombiniert.xlsx", col_types = rep("text", times = 10))
#Annotationen_kombiniert <- read_excel("../Daten/Annotationen_kombiniert_bearb.xlsx", col_types = rep("text", times = 10))

# Ersetzen des Zeilennamens, der ein Leerzeichen enthält, damit nicht später immer Anführungszeichen verwendet werden müssen
colnames(Annotationen_kombiniert)[9] <- "Entsprechung"

# Parallele Strukturen für beide Sprachen
de <- Annotationen_kombiniert[Annotationen_kombiniert$Sprache == "de",]
en <- Annotationen_kombiniert[Annotationen_kombiniert$Sprache == "en",]

# Anscheinend wird "5,9" falsch eingelesen. Das wird hier korrigiert
de$Bedeutung <- ifelse(de$Bedeutung == "5.9", "5,9", de$Bedeutung)

# Bei den englischen Alignierungen, NA durch [keine Entsprechung] ersetzen
en$Alignierung <- ifelse(is.na(en$Alignierung), "[keine Entsprechung]", en$Alignierung)

# Wenn Entsprechung leer, einfügen von Wert aus Alignierung
en$Entsprechung <- ifelse(is.na(en$Entsprechung), en$Alignierung, en$Entsprechung)

# Wenn Entsprechung-Wert "AMV", Anhängen des entsprechenden Verbs
en$Entsprechung <- ifelse(en$Entsprechung == "AMV", paste("AMV",en$Alignierung), en$Entsprechung) # paste ist Stringkonkatenierung

# Faktoren statt Strings
de$Entsprechung <- factor(de$Entsprechung)
en$Entsprechung <- factor(en$Entsprechung)

de$Alignierung <- factor(de$Alignierung)
en$Alignierung <- factor(en$Alignierung)

##### Erstellen von Tabellen und Diagrammen #####

# Dataframe mit Spalten für Entsprechung und Anzahl, wobei Anzahl meint, wie viele Beispiele es für die Entsprechung gibt
haeufigkeiten.ent <- data.frame(names(summary(en$Entsprechung)), summary(en$Entsprechung))
# Ordnen nach Häufigkeit
haeufigkeiten.ent <- haeufigkeiten.ent[order(haeufigkeiten.ent$summary.en.Entsprechung., decreasing = T),]
# Spaltennamen ändern
colnames(haeufigkeiten.ent) <- c("Entsprechung", "Anzahl")

# Sankey-Diagramm bauen, das zeigt, welche automatischen Alignierungen letztlich zu welchen Entsprechungen korrigiert wurden
# Auf dem Weg dahin werden auch noch Tabellen erstellt

# Dataframe, der sich von en in der Entsprechungsspalte unterscheidet: Zum Ausschluss führende Metainformation und der 
# Formulierungsnähewert 4 werden zum Entsprechungswert, bei 4n wird es "4n <Entsprechung>"
en.zusatzinfos <- en

en.zusatzinfos$Entsprechung <- as.character(en.zusatzinfos$Entsprechung)

test.meta <- ifelse(is.na(de$Meta), "na", de$Meta)

en.zusatzinfos$Entsprechung <- ifelse(test.meta == "raus", "raus", en.zusatzinfos$Entsprechung)
en.zusatzinfos$Entsprechung <- ifelse(test.meta == "adv", "adv", en.zusatzinfos$Entsprechung)
en.zusatzinfos$Entsprechung <- ifelse(test.meta == "~P", "~P", en.zusatzinfos$Entsprechung)
en.zusatzinfos$Entsprechung <- ifelse(test.meta == "vübz", "vübz", en.zusatzinfos$Entsprechung)

test.form <- ifelse(is.na(en.zusatzinfos$Formulierung), "na", en.zusatzinfos$Formulierung)

en.zusatzinfos$Entsprechung <- ifelse(test.form == "4", "4", en.zusatzinfos$Entsprechung)
en.zusatzinfos$Entsprechung <- ifelse(test.form == "4n", paste("4n", en.zusatzinfos$Entsprechung), en.zusatzinfos$Entsprechung)

en.zusatzinfos$Entsprechung <- factor(en.zusatzinfos$Entsprechung)

# Dataframe mit drei Spalten: Welche Alignierung ist zu welcher Entsprechung/Meta-Wert/4 geworden und wie oft 
# (nur Kombinationen die auch tatsächlich vorkamen enthalten)
input.sankey <- data.frame(table(en.zusatzinfos$Alignierung, 
                                 en.zusatzinfos$Entsprechung))[data.frame(table(en.zusatzinfos$Alignierung, 
                                                                                en.zusatzinfos$Entsprechung))$Freq > 0,]

# Übersicht darüber, auf welche Entsprechungen sich die Beispiele verteilen
verteilungsuebersicht <- data.frame(names(summary(en.zusatzinfos$Entsprechung)), summary(en.zusatzinfos$Entsprechung))

colnames(verteilungsuebersicht) <- c("Kategorie", "Frequenz")

verteilungsuebersicht <- verteilungsuebersicht[order(verteilungsuebersicht$Frequenz, decreasing = T),]

# Ausgabe Verteilungsübersicht (Anhang)
xtab.verteilungsuebersicht <- xtable(verteilungsuebersicht, auto = T, caption = "Die Frequenzen der korrigierten Entsprechungen vor Trennung nach Bedeutung und Ausschluss auszusondernder Beispiele")
print(xtab.verteilungsuebersicht, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Ausgabe Kurzfassung Verteilungsübersicht (3.1)
xtab.verteilungsuebersicht.kurz <- xtable(t(verteilungsuebersicht[1:10,]), auto = T, caption = "Kurzübersicht Frequenzen der korrigierten Entsprechungen", label = "frequenzenvorallemkurz")
print(xtab.verteilungsuebersicht.kurz, include.rownames = FALSE, booktabs = T, include.colnames = F, caption.placement = "top")

# Übersicht darüber, welche Alignierungen zu welchen Entsprechungen wurden, als Tabelle ausgeben (Anhang)
tabelle.al.ent <- input.sankey[order(input.sankey$Var1, input.sankey$Var2),]
colnames(tabelle.al.ent) <- c("Alignierung", "Entsprechung", "Anzahl")
xtab.al.ent <- xtable(tabelle.al.ent, auto = T, caption = "Wie oft wurde welche automatisch alignierte Entsprechung zu was korrigiert?")
print(xtab.al.ent, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Leerzeichen an Werte in Zielspalte anhängen, weil gvis sonst denkt dass der Graph zyklisch ist, weil die Elemente ja gleich heißen
input.sankey$Var2 <- paste0(input.sankey$Var2," ")

# Sankey-Diagramm 
# einführender Blogbeitrag (schlecht geschrieben, aber relevante Infos): https://databreadandbutter.wordpress.com/2017/09/25/erster-blogbeitrag/
# weitere Optionen: https://developers.google.com/chart/interactive/docs/gallery/sankey#top_of_page
sankey <- gvisSankey(input.sankey, from = "Var1", to = "Var2", weight = "Weight", options = list(height = 1300, width = 800, sankey = "{
link: { colorMode: 'source'}, node: { label: { fontName: 'Linux Libertine O', fontSize: 12 }, nodePadding: 7, interactivity: false } }"))

# Diagramm im Browser anzeigen lassen
#plot(sankey)

# Ausgabe in HTML-Datei, die dann genutzt werden kann, um eine PDF-Version zu erstellen (Diagramm im Anhang)
print(sankey, file = "Sankey_urspr_Al.html")

# Ausgabe der Summaries für einige der Kategorien

summary.meta <- t(data.frame(summary(factor(de$Meta))))
summary.bdt <- data.frame(names(summary(factor(de$Bedeutung))), summary(factor(de$Bedeutung)))
colnames(summary.bdt) <- c("Bedeutung", "Anzahl")
summary.bdt <- summary.bdt[order(summary.bdt$Anzahl, decreasing = T),]
summary.passend <- t(data.frame(summary(factor(en$Bedeutung))))
summary.form <- t(data.frame(summary(factor(en$Formulierung))))
summary.al <- t(data.frame(summary(factor(en$Alignierung))))
summary.ent <- (data.frame(names(summary(factor(en$Entsprechung))), summary(factor(en$Entsprechung))))
colnames(summary.ent) <- c("Entsprechung", "Anzahl")

xtab.summary.meta <- xtable(summary.meta, auto = T, caption = "Übersicht über die Häufigkeiten der Werte von Meta in den deutschen Zeilen", label = "summaryMetaDE")
print(xtab.summary.meta, booktabs = T, caption.placement = "top")

xtab.summary.form <- xtable(summary.form, auto = T, caption = "Übersicht über die Häufigkeiten Formulierungsnähewerte", label = "summaryFormEN")
print(xtab.summary.form, booktabs = T, caption.placement = "top")

xtab.summary.passend <- xtable(summary.passend, auto = T, caption = "Übersicht über die Häufigkeiten Bedeutung (en)", label = "summaryPassendEN")
print(xtab.summary.passend, booktabs = T, caption.placement = "top")

xtab.summary.bdt.kurz <- xtable(t(rbind(summary.bdt[1:12,], data.frame(Bedeutung = "andere", Anzahl = sum(summary.bdt$Anzahl[13:length(summary.bdt$Anzahl)])))), auto = T, caption = "Kurzübersicht über die Häufigkeiten Bedeutung (de)", label = "summaryBedeutungDEkurz")
print(xtab.summary.bdt.kurz, caption.placement = "top", booktabs = T, include.colnames = F, include.rownames = F)

xtab.summary.bdt <- xtable(summary.bdt, auto = T, caption = "Übersicht über die Häufigkeiten Bedeutung (de)", label = "summaryBedeutungDE")
print(xtab.summary.bdt, caption.placement = "top", booktabs = T, include.rownames = F)

##### Disambiguierung mit Bayes-Klassifikator (Abschnitt 6.1) #####

# Um die Werte aus dem Paper zu berechnen, Skript mit passenden Werten ausführen, also ggf. oben die eingelesene Datei ändern 
# und unten die Zeile entkommentieren, die bewirkt, dass die korrigierten Entsprechungen verwendet werden

# Dataframes ohne die wegen ihrer Meta-Werte auszuschließenden Beispiele
en.ohne <- en[(! is.na(de$Meta) & de$Meta != "raus" & de$Meta != "adv" & de$Meta != "~P" & de$Meta != "vübz") | is.na(de$Meta),]
de.ohne <- de[(! is.na(de$Meta) & de$Meta != "raus" & de$Meta != "adv" & de$Meta != "~P" & de$Meta != "vübz") | is.na(de$Meta),]

de.ohne$Bedeutung <- factor(de.ohne$Bedeutung)
en.ohne$Alignierung <- factor(en.ohne$Alignierung)

# Dataframe nur mit Bedeutung(en) und Alignierung/Entsprechung
pddf = data.frame(bdt = de.ohne$Bedeutung, al = en.ohne$Alignierung)
#pddf = data.frame(bdt = de.ohne$Bedeutung, al = en.ohne$Entsprechung) # entkommentieren, um Entsprechungsspalte als Alignierungen zu nehmen

library(caret) # für createFolds() für Kreuzvalidierung

library(naivebayes)

# Random-Seed für Reproduzierbarkeit (bei Erstellung der Gruppen für Kreuzvalidierung wird randomisiert)
set.seed(436)

# Gruppen für Kreuzvalidierung (Liste von zehn Integer-Vektoren. Die Integers sind die Indizes der Datenpunkte)
test.groups <- createFolds(pddf$bdt)

# Zehndimensionaler Vektor, der mit der Accuracy für jede Testgruppe gefüllt werden wird
accs <- 1:10
# Das gleiche für die Baseline
bl.accs <- 1:10

# Matrix mit den Bedeutungen in Zeilen und Spalten. Wird später so gefüllt, dass am Ende für jede tatsächliche Bedeutung in den Zeilen
# in den Spalten abzulesen ist, wie oft für die entsprechenden Beispiele die Bedeutung in der Spalte vorhergesagt wurde. 
# Kann genutzt werden, um zu prüfen, welche Fehler der Klassifikator gerne macht
was_als_was <- matrix(rep(0, length(levels(pddf$bdt))^2), nrow = length(levels(pddf$bdt)), ncol = length(levels(pddf$bdt)))
colnames(was_als_was) <- as.character(levels(pddf$bdt))
rownames(was_als_was) <- as.character(levels(pddf$bdt))

for(fold in 1:length(test.groups)){
  group <- test.groups[[fold]]
  train <- pddf[-group,]
  test <- pddf[group,]
  nb.model <- naive_bayes(bdt ~ al, data = train, laplace = 0.1) # Auf Trainingsdaten trainiertes Modell
  nb.pred <- predict(nb.model, test[2], type = "class") # Vorhersage des Modells auf den Testdaten (test[2] ist Dataframe mit ausschließlich den Alignierungen. Nicht test$al, weil die Funktion an dieser Stelle unbedingt eine DF haben möchte)
  acc <- sum(diag(table(nb.pred, test$bdt)))/sum(table(nb.pred, test$bdt)) # Accuracy auf Testdaten
  accs[fold] <- acc
  bl.acc <- table(test$bdt)[names(which.max(table(train$bdt)))] / length(test$bdt) # Baseline-Accuracy
  bl.accs[fold] <- bl.acc
  for(bsp in 1:length(nb.pred)){
    was_als_was[as.character(test$bdt[bsp]),as.character(nb.pred[bsp])] <- was_als_was[as.character(test$bdt[bsp]),as.character(nb.pred[bsp])] + 1
    }
}

# Ausgabe von durchschnittlicher Accuracy und Baseline-Accuracy
print(sum(accs)/10)
print(sum(bl.accs)/10)

##### Unabhängigkeitstests Formulierungsnähe (Abschnitt 4.1.2) ##### 

# Dataframe nur mit den Werten mit Formulierungsnähe 1-3
de.ohne.1_3 <- droplevels.data.frame(de.ohne[en.ohne$Formulierung != "4" & en.ohne$Formulierung != "4n" & (en.ohne$Bedeutung != "n" | is.na(en.ohne$Bedeutung)) & ! startsWith(as.character(en.ohne$Entsprechung), "AMV"),])
en.ohne.1_3 <- droplevels.data.frame(en.ohne[en.ohne$Formulierung != "4" & en.ohne$Formulierung != "4n" & (en.ohne$Bedeutung != "n" | is.na(en.ohne$Bedeutung)) & ! startsWith(as.character(en.ohne$Entsprechung), "AMV"),])

# Einfügen von "n" (für "nicht regiert"), wenn Rektionsstatuszelle leer
de.ohne.1_3$regiert <- ifelse(is.na(de.ohne.1_3$regiert), "n", de.ohne.1_3$regiert)
en.ohne.1_3$regiert <- ifelse(is.na(en.ohne.1_3$regiert), "n", en.ohne.1_3$regiert)

# Übersicht Verteilung Formulierungsnähewerte
summary.form.1_3 <- t(data.frame(summary(factor(en.ohne.1_3$Formulierung))))
xtab.summary.form.1_3 <- xtable(summary.form.1_3, auto = T, caption = "Übersicht über die Häufigkeiten der Formulierungsnähewerte in den ungetrennten I-Daten", label = "summaryForm13")
print(xtab.summary.form.1_3, caption.placement = "top", booktabs = T, include.rownames = F)

# Kreuztabelle Formulierungsnähe Entsprechungen
xtab.table.form.ent.1_3 <- xtable(table(en.ohne.1_3$Formulierung, en.ohne.1_3$Entsprechung), auto = T, caption = "Kreuztabelle Formulierungsnähe Entsprechungen in nicht bedeutungsgetrennten Daten aus I", label = "tableFormEnt13")
print(xtab.table.form.ent.1_3, caption.placement = "top", booktabs = T, rotate.colnames = T)

# Prozentuale Verteilungen der Formulierungsnähewerte für die Rektionsstatus en, de
xtab.table.form.en.reg.1_3 <- xtable(100*prop.table(table(en.ohne.1_3$Formulierung, en.ohne.1_3$regiert),2), digits = 2, auto = T, caption = "Kreuztabellen Formulierungsnähe Rektionsstatus in nicht bedeutungsgetrennten Daten aus I", label = "tableFormReg13en")
print(xtab.table.form.en.reg.1_3, caption.placement = "top", booktabs = T)
xtab.table.form.de.reg.1_3 <- xtable(100*prop.table(table(en.ohne.1_3$Formulierung, de.ohne.1_3$regiert),2), digits = 2, auto = T, caption = "Kreuztabellen Formulierungsnähe Rektionsstatus in nicht bedeutungsgetrennten Daten aus I", label = "tableFormReg13de")
print(xtab.table.form.de.reg.1_3, caption.placement = "top", booktabs = T)

# Anwendung Tests Rektionsstatus (KONZEPTUELL NICHT SINNVOLL)
chisq.test(table(en.ohne.1_3$Formulierung, en.ohne.1_3$regiert), correct = F)
fisher.test(table(en.ohne.1_3$Formulierung, de.ohne.1_3$regiert))

# Tests Entsprechungen
# Erwartete Werte zu klein für Chi^2
chisq.test(table(en.ohne.1_3$Formulierung, en.ohne.1_3$Entsprechung))$expected
# Exakter Test nach Fisher (auskommentiert, weil Ausführung wegen notwendiger Workspaceerhöhung ziemlich lange dauert)
#fisher.test(table(en.ohne.1_3$Formulierung, en.ohne.1_3$Entsprechung), workspace = 200000000)

# Tests für die Bedeutungen 

library(MRCV)

# Erstellen der benötigten Datenstruktur, d.i. einer Tabelle mit Formulierungsnähespalte und für 
# jede Bedeutung einer eigenen Spalte mit Binärwerten. Beispiele in Zeilen

# Wird die einzelnen Bedeutungen enthalten
bdts <- c()

# Hinzufügen aller Bedeutungen, die in einer Bedeutungsannotation genannt werden
for(bdtkomb in names(summary(de.ohne.1_3$Bedeutung))){
  bdts <- c(bdts, strsplit(bdtkomb, ",")[[1]])
}

# Doppelte rausschmeißen
bdts <- unique(bdts)

# Matrix mit Zeilen für die Beispiele, Spalten für die Bedeutungen. Alle Zellen mit 0 initialisiert
bdtmatrix <- matrix(0, nrow = length(de.ohne.1_3$Bedeutung), ncol = length(bdts))

# Belegen der Zelle mit 1, wenn Bedeutung bei Beispiel gegeben
for(i in 1:length(de.ohne.1_3$Bedeutung)){
  for(j in 1:length(bdts)){
    if(as.character(bdts[j]) %in% strsplit(as.character(de.ohne.1_3$Bedeutung[i]), ",")[[1]]){
      bdtmatrix[i,j] <- 1
    }
  }
}

# Umwandeln in Dataframe
bdtspldf <- as.data.frame(bdtmatrix)

colnames(bdtspldf) <- as.character(bdts)
rownames(bdtspldf) <- rownames(de.ohne.1_3)

# Hinzufügen der Formulierungsnähespalte
input.mi.bdt <- cbind(en.ohne.1_3$Formulierung,bdtspldf)

# Random-Seed für Reproduzierbarkeit
set.seed(436)

# Durchführen der Tests (Der Test mit Bootstrapping ist auskommentiert, weil die Ausfühurung sehr lange dauert)
mi.test.bon <- MI.test(input.mi.bdt, 1, ncol(bdtspldf), type = "bon")
#mi.test.boot <- MI.test(input.mi.bdt, 1, ncol(bdtspldf), add.constant = 0.01, type = "boot", B=20000)