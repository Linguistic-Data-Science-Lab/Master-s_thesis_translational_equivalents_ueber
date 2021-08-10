options(OutDec = ",") # Dezimalkomma in der Ausgabe

##### Funktionen #####

# Input: Data-Frame. Potentiell identische Zeilen müssen aufeinander folgen, da die Identität sonst nicht erkannt wird.
# Output: Data-Frame, der um eine Spalte "Anzahl" ergänzt wurde. Der Ergebnis-Data-Frame enthält keine Zeilen mit 
#         identischen Werten mehr. Stattdessen steht in der Spalte "Anzahl", wie oft die jeweilige Zeile vorkam.
make.reg.df <- function(df){
  
  resultdf <- df[1,]
  
  resultdf$Anzahl[1] <- 1
  
  for(i in 2:nrow(df)){
    
    identisch <- TRUE
    
    for(j in 1:ncol(df)){
      if(! identical(df[i,j], df[i-1,j])){
        identisch <- FALSE
        break
      }
    }
    
    if(identisch == TRUE){
      resultdf[nrow(resultdf),ncol(resultdf)] <- resultdf[nrow(resultdf),ncol(resultdf)] + 1
    }
    else{
      neue_zeile <- df[i,]
      neue_zeile$Anzahl <- 1
      resultdf <- rbind(resultdf, neue_zeile)
    }
  }
  
  return(resultdf)
}

##### Daten einlesen und bearbeiten ######

library(xtable)

# Einlesen der Daten und Erstellen des Data-Frame (Vor Ausführung muss ggf. das working directory mit setwd() auf den Ordner gesetzt werden, der diese Datei enthält)
library(readxl)
Annotationen_kombiniert <- read_excel("../Daten/Annotationen_kombiniert.xlsx", col_types = rep("text", times = 10))
#Annotationen_kombiniert <- read_excel("../Daten/Annotationen_kombiniert_bearb.xlsx", col_types = rep("text", times = 10))

# Festlegen des der Schriftart für die Ausgabe. Paket und Schriftart (als TrueType-Font!) müssen installiert sein.
# Es dauert etwas, bis alle Schriftarten geladen sind, also nur laufen lassen, wenn Ausgabe erzeugt werden soll
# Da die PDF-Ausgabe (warum auch immer) trotzdem nicht die richtige Schriftart nutzt, müssen die Bilder als PNG gespeichert werden
# library(extrafont)
# font_import()
# loadfonts()
# par(family = "Linux Libertine") # Legt Schriftart für die Session fest

# Parallele Strukturen für beide Sprachen
de <- Annotationen_kombiniert[Annotationen_kombiniert$Sprache == "de",]
en <- Annotationen_kombiniert[Annotationen_kombiniert$Sprache == "en",]

# Entfernen der auszuschließenden Fälle
en <- en[(de$Meta != "raus" & de$Meta != "adv" & de$Meta != "~P" & de$Meta != "vübz") | is.na(de$Meta),]
de <- de[(de$Meta != "raus" & de$Meta != "adv" & de$Meta != "~P" & de$Meta != "vübz") | is.na(de$Meta),]

# Einfügen von "n", wo nicht regiert
de$regiert <- ifelse(is.na(de$regiert), "n", de$regiert)
en$regiert <- ifelse(is.na(en$regiert), "n", en$regiert)

# Einfügen von "p" für "passend", wenn Zelle bei engl. Bdt. leer
en$Bedeutung <- ifelse(is.na(en$Bedeutung), "p", en$Bedeutung)

# Aus irgendeinem Grund wird "5,9" beim Einlesen durch "5.9" ersetzt. Dies wird hier rückgängig gemacht. (Hässlicher Fix)
de$Bedeutung <- ifelse(de$Bedeutung == "5.9", "5,9", de$Bedeutung)

# Erstellen von Data-Frames, in denen Einträge mit mehreren Bedeutungen geteilt werden. 
# Für jede Bedeutung soll es einen eigenen Eintrag geben.

# Erstellen einer Dummy-Kopie der ersten Zeile (wird hinterher wieder entfernt, garantiert aber, 
# dass das richtige Format vorliegt)
de.dis <- de[1,]
en.dis <- en[1,]

for(i in 1:nrow(de)){
  
  # Komma in Bedeutungszeile -> mehrere Bedeutungen. 
  # fixed = TRUE legt fest, dass es ein fest zu matchender String und kein regulärer Ausdruck ist
  if(grepl(",", de$Bedeutung[i], fixed = TRUE)){
    
    # Vektor mit den einzelnen Bedeutungen
    bdtn <- strsplit(de$Bedeutung[i], ",")[[1]] # [[1]], weil das Ergebnis von strsplit eine Liste ist, die als erstes Elemet den Wortvektor enthält
    
    # Wenn auch in der engl. Bedeutungsspalte ein Komma vorkommt und die Bedeutungen also nicht alle angemessen sind…
    if(grepl(",", en$Bedeutung[i], fixed = TRUE)){
      # …Erstellen eines Vektors der Angemessenheit der Bedeutungen fürs Englische.
      en.bdtn <- strsplit(en$Bedeutung[i], ",")[[1]]
    # Wenn es nur einen Eintrag gibt, enthält der Vektor mit den englischen Entsprechungen diesen so oft, wie es Bedeutungen gibt
    } else {
      en.bdtn <- rep(en$Bedeutung[i], times = length(bdtn))
    }
    
    # Analog für Rektion im Dt.
    if(grepl(",", de$regiert[i], fixed = TRUE)){
      de.rek <- strsplit(de$regiert[i], ",")[[1]]
    } else {
      de.rek <- rep(de$regiert[i], times = length(bdtn))
    }
    
    # Analog für Rektion im En.
    # If-Bedingung momentan nie erfüllt. Trotzdem implementiert, falls sich das ändern sollte
    if(grepl(",", en$regiert[i], fixed = TRUE)){
      en.rek <- strsplit(en$regiert[i], ",")[[1]]
    } else {
      en.rek <- rep(en$regiert[i], times = length(bdtn))
    }
    
    # Für jede Bedeutung wird eine eigene Zeile erstellt, die der ursprünglichen entspricht, 
    # aber immer nur die für die jeweilige Bedeutung passenden Einträge enthält.
    for(b in 1:length(bdtn)){
      
      # de
      neu.d <- de[i,]
      neu.d$Bedeutung <- trimws(bdtn[b]) # trimws: Whitespace entfernen
      neu.d$regiert <- trimws(de.rek[b])
      
      # en
      neu.e <- en[i,]
      neu.e$Bedeutung <- trimws(en.bdtn[b])
      neu.e$regiert <- trimws(en.rek[b])
      
      # Updaten des Data-Frames
      de.dis <- rbind(de.dis, neu.d)
      en.dis <- rbind(en.dis, neu.e)
    }
  
  # Befindet sich kein Komma in der Bedeutungszeile, wird der Eintrag einfach übernommen 
  } else{
    de.dis <- rbind(de.dis, de[i,])
    en.dis <- rbind(en.dis, en[i,])
  }
}

# Entfernen der Dummy-Zeilen am Anfang
de.dis <- de.dis[2:nrow(de.dis),]
en.dis <- en.dis[2:nrow(en.dis),]

# Ändern der Benennungen der Bedeutungskategorien. Es wird auf diese Weise verfahren (anstatt etwa einfach
# später die Namen der Levels zu ändern), da so explizit ist, welche Benennungen durch welche ersetzt werden.
# Dadurch sollte es nicht zu Problemen kommen, wenn sich in der Ausgangsdatei etwas ändert. Würde man einfach
# die Levels umbenennen, könnte es sein, dass es dann zu Verschiebungen kommt und die Benennungen nicht mehr 
# stimmen.
# Eigentlich sollte für Vergleiche bei if-Abfragen lieber identical() verwendet werden, aber das liefert nicht
# das gewünschte Ergebnis, weil statt eines Vektors mit Bool'schen Werten genau ein Wert ausgegeben wird. 
# == sollte keine Probleme bereiten, solange keine NAs vorkommen; und das ist hier nicht der Fall
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "10", "Überschreitung", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1a", "Spatial Traverse Grenzbereich", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "Abd", "Abdeckung", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1b", "Spatial Traverse proximal", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1b +m", "Spatial Traverse proximal +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1c", "Spatial Traverse vertikal", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1c +m", "Spatial Traverse vertikal +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d", "Spatial Traverse <3D", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d1", "Spatial Traverse 2D", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d2", "Spatial Traverse 1D", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d3", "Spatial Traverse Via", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d +m", "Spatial Traverse <3D +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d1 +m", "Spatial Traverse 2D +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d2 +m", "Spatial Traverse 1D +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1d3 +m", "Spatial Traverse Via +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1e", "Spatial Bedeckung", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1f", "Spatial Achsenbezogen", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1f +m", "Spatial Achsenbezogen +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "1f +m +z", "Spatial Achsenbezogen +metaph +ziel", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "2a", "Temporal Tagesteil", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "2b", "Temporal Zeitdauer", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "2c", "Temporal Maßeinheit", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "3a", "Modal Instrumental", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "3b", "Modal Informationsübermittler", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "3c", "Modal Medial", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "3d", "Modal Abstraktes Instrument", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "3e", "Modal Art und Weise", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "4a", "Konditional Kausal / Emotionsgegenstand", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "4a +m", "Konditional Kausal / Emotionsgegenstand +metaph", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "5", "Bezugspunkt", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "6", "Machtverhältnis", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "8", "Rangfolge", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "9", "Thema", de.dis$Bedeutung)
de.dis$Bedeutung <- ifelse(de.dis$Bedeutung == "d", "Desemantisiert", de.dis$Bedeutung)

# Umwandeln der Strings in Faktoren bei Bedeutung, Rektionsstatus, Formulierungsnähe

de.dis$Bedeutung <- factor(de.dis$Bedeutung)
en.dis$Bedeutung <- factor(en.dis$Bedeutung)

de.dis$regiert <- factor(de.dis$regiert)
en.dis$regiert <- factor(en.dis$regiert)

en.dis$Formulierung <- factor(en.dis$Formulierung)

# Dummy-String statt NA bei der Alignierung
en.dis$Alignierung <- ifelse(is.na(en.dis$Alignierung), "<leer>", en.dis$Alignierung)

# Wenn Entsprechung leer, einfügen von Wert aus Alignierung
en.dis$Entsprechung <- ifelse(is.na(en.dis$Entsprechung), en.dis$Alignierung, en.dis$Entsprechung)

# Wenn Entsprechungs-Wert "AMV", Anhängen des entsprechenden Verbs
en.dis$Entsprechung <- ifelse(en.dis$Entsprechung == "AMV", paste("AMV",en.dis$Alignierung), en.dis$Entsprechung) # paste ist Stringkonkatenierung

# Umwandeln der Entsprechungsstrings in Faktoren
en.dis$Entsprechung <- factor(en.dis$Entsprechung)

####### Erstellen verschiedener Datensätze #######

# Data-Frame mit Formulierungsnähe 1 bis 3 ohne AMV-Fälle. Droplevels entfernt Levels, die nicht mehr benutzt werden
de.1_3 <- droplevels.data.frame(de.dis[en.dis$Formulierung != "4" & en.dis$Formulierung != "4n" & en.dis$Bedeutung == "p" & ! startsWith(as.character(en.dis$Entsprechung), "AMV"),])
en.1_3 <- droplevels.data.frame(en.dis[en.dis$Formulierung != "4" & en.dis$Formulierung != "4n" & en.dis$Bedeutung == "p" & ! startsWith(as.character(en.dis$Entsprechung), "AMV"),])

# Dataframe mit Formulierungsnähe 4n
de.4n <- droplevels.data.frame(de.dis[en.dis$Formulierung == "4n" & en.dis$Bedeutung == "p",])
en.4n <- droplevels.data.frame(en.dis[en.dis$Formulierung == "4n" & en.dis$Bedeutung == "p",])

# Dataframe mit Formulierungsnähe 4
de.4 <- droplevels.data.frame(de.dis[en.dis$Formulierung == "4",])
en.4 <- droplevels.data.frame(en.dis[en.dis$Formulierung == "4",])

# Data-Frame mit den unpassenden Beispielen bei Formulierungsnähe 1 bis 3 ohne AMV-Fälle.
de.1_3.np <- droplevels.data.frame(de.dis[en.dis$Formulierung != "4" & en.dis$Formulierung != "4n" & en.dis$Bedeutung == "n" & ! startsWith(as.character(en.dis$Entsprechung), "AMV"),])
en.1_3.np <- droplevels.data.frame(en.dis[en.dis$Formulierung != "4" & en.dis$Formulierung != "4n" & en.dis$Bedeutung == "n" & ! startsWith(as.character(en.dis$Entsprechung), "AMV"),])

# Erstellen von Übersichten, die auch den Rektionsstatus berücksichtigen

# Data-Frame, der nur Bedeutung, Entsprechung und Rektionsstatus und eine ID enthält
interesting_data <- data.frame(de.1_3$Bedeutung, en.1_3$Entsprechung, de.1_3$regiert, en.1_3$regiert, de.1_3$ID)

colnames(interesting_data) <- c("Bedeutung", "Entsprechung", "de_regiert", "en_regiert", "ID")

##### Tabellen und Diagramme #####

# Abschnitt 3.2 

# Erstellen einer Tabelle mit der Frequenz der Bedeutungen, die dann nach Frequenz geordnet und ausgegeben wird
table_int_dat_bdt <- as.data.frame(table(interesting_data$Bedeutung))
colnames(table_int_dat_bdt) <- c("Bedeutung", "Anzahl")
table_int_dat_bdt <- table_int_dat_bdt[order(table_int_dat_bdt$Anzahl, decreasing = T),] # Höchste Frequenzen zuerst -> decreasing
gesamtanzahl <- sum(table_int_dat_bdt$Anzahl)
table_int_dat_bdt <- cbind.data.frame(table_int_dat_bdt, 100 * table_int_dat_bdt$Anzahl/gesamtanzahl)
table_int_dat_bdt$Bedeutung <- as.character(table_int_dat_bdt$Bedeutung)
colnames(table_int_dat_bdt)[3] <- "Anteil (in %)"
xtable_int_dat_bdt <- xtable(table_int_dat_bdt, digits = 2, caption = "Die Häufigkeiten der Bedeutungen nach Bearbeitung", label = "anteilebdt")
# ZEILE MIT GESAMTANZAHL HÄNDISCH ERGÄNZEN
print(xtable_int_dat_bdt, auto = TRUE, include.rownames = FALSE, caption.placement = "top", booktabs = T)

# Ausgabe interesting.data Entsprechungen
table_int_dat_ent <- as.data.frame(table(interesting_data$Entsprechung))
colnames(table_int_dat_ent) <- c("Entsprechung", "Anzahl")
table_int_dat_ent <- table_int_dat_ent[order(table_int_dat_ent$Anzahl, decreasing = T),]
table_int_dat_ent <- cbind.data.frame(table_int_dat_ent, 100 * table_int_dat_ent$Anzahl/gesamtanzahl)
colnames(table_int_dat_ent)[3] <- "Anteil (in %)"
xtable_int_dat_ent <- xtable(table_int_dat_ent, digits = 2, caption = "Die Häufigkeiten der Entsprechungen nach Bearbeitung", label = "anteileent")
print(xtable_int_dat_ent, auto = TRUE, include.rownames = FALSE, caption.placement = "top", booktabs = T)

# Ausgabe en.1_3 Formulierungsnähe
summary.form.1_3 <- t(data.frame(summary(factor(en.1_3$Formulierung))))
xtab.summary.form <- xtable(summary.form.1_3, auto = T, caption = "Übersicht über die Häufigkeiten der Formulierungsnähewerte bei den bearbeiteten Daten", label = "summaryFormEN1_3")
print(xtab.summary.form, caption.placement = "top", booktabs = T)

# Ausgabe interesting.data Rektionsstatus
summary.de_reg.1_3 <- t(data.frame(summary(de.1_3$regiert)))
summary.en_reg.1_3 <- t(data.frame(summary(en.1_3$regiert)))
summary.reg.1_3 <- rbind(summary.de_reg.1_3, summary.en_reg.1_3)
rownames(summary.reg.1_3) <- c("Deutsch", "Englisch")
colnames(summary.reg.1_3) <- c("Nicht regiert", "Regiert")
xtab.summary.reg.1_3 <- xtable(summary.reg.1_3, auto = T, caption = "Übersicht über die Häufigkeiten der Rektionsstatus bei den bearbeiteten Daten", label = "summaryReg1_3")
print(xtab.summary.reg.1_3, caption.placement = "top", booktabs = T)

# Nicht verwendet, aber potenziell für Vergleich interessant:

# Erstellen einer Tabelle mit der Frequenz der Bedeutungen für die Beispiele mit Formulierungsnähe 4, die dann nach Frequenz geordnet und ausgegeben wird
table_de.4_bdt <- as.data.frame(table(de.4$Bedeutung))
colnames(table_de.4_bdt) <- c("Bedeutung", "Anzahl")
table_de.4_bdt <- table_de.4_bdt[order(table_de.4_bdt$Anzahl, decreasing = T),] # Höchste Frequenzen zuerst -> decreasing
gesamtanzahl <- sum(table_de.4_bdt$Anzahl)
table_de.4_bdt <- cbind.data.frame(table_de.4_bdt, 100 * table_de.4_bdt$Anzahl/gesamtanzahl)
table_de.4_bdt$Bedeutung <- as.character(table_de.4_bdt$Bedeutung)
colnames(table_de.4_bdt)[3] <- "Anteil (in %)"
xtable_de.4_bdt <- xtable(table_de.4_bdt, digits = 2, caption = "Die (bei Mehrfachannotation getrennten) Bedeutungen bei den Beispielen mit Formulierungsnähewert 4", label = "anteeilendt4") 
print("Freuenzen der Bedeutungen")
# ZEILE MIT GESAMTANZAHL HÄNDISCH ERGÄNZEN
print(xtable_de.4_bdt, auto = TRUE, include.rownames = FALSE, caption.placement = "top", booktabs = T)

# Abschnitt 4.1

# Erstellen der Bedeutungs-Entsprechungs-Abbildungen

# Data-Frame, der interesting_data ohne ID entspricht, aber geordnet ist nach Bedeutung, Entsprechung, de_regiert, 
# en_regiert (in dieser Reihenfolge)
int.dat.bdt.ent.reg <- interesting_data[order(interesting_data$Bedeutung, interesting_data$Entsprechung, 
                                              interesting_data$de_regiert, interesting_data$en_regiert),1:4]

# Wie int.dat.bdt.ent.reg, aber um eine Spalte "Anzahl" ergänzt. Enthält keine Zeilen mit identischen Werten mehr.
# Stattdessen steht in der Spalte "Anzahl", wie oft die jeweilige Zeile vorkam.
uebersicht.bdt <- make.reg.df(int.dat.bdt.ent.reg)
colnames(uebersicht.bdt) <- c("Bedeutung", "Entsprechung", "regiert de", "regiert en", "Anzahl")

# Ausgabe 
xtab.uebersicht.bdt <- xtable(uebersicht.bdt, auto = TRUE, caption = "Abbildungen nach Bedeutungen geordnet mit Rektionsstatus", label = "bdtent")
print(xtab.uebersicht.bdt, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Kurze Version der Tabelle (ohne Rektionsstatus) erstellen und ausgeben:
uebersicht.bdt.kurz <- make.reg.df(int.dat.bdt.ent.reg[c(1,2)])
xtab.uebersicht.bdt.kurz <- xtable(uebersicht.bdt.kurz, auto = TRUE, caption = "Abbildungen nach Bedeutungen geordnet ohne Rektionsstatus", label = "bdtentkurz")
print(xtab.uebersicht.bdt.kurz, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Entsprechungen nach Bedeutungen

# Geordnet nach Entsprechung, Bedeutung, de_regiert, en_regiert (in dieser Reihenfolge)
int.dat.ent.bdt.reg <- interesting_data[order(interesting_data$Entsprechung, interesting_data$Bedeutung,
                                              interesting_data$de_regiert, interesting_data$en_regiert),1:4]
# Wie int.dat.ent.bdt.reg, aber um eine Spalte "Anzahl" ergänzt. Enthält keine Zeilen mit identischen Werten mehr.
# Stattdessen steht in der Spalte "Anzahl", wie oft die jeweilige Zeile vorkam.
uebersicht.ent <- make.reg.df(int.dat.ent.bdt.reg)
# Ändern der Reihenfolge der Spalten
uebersicht.ent <- data.frame(uebersicht.ent$Entsprechung, uebersicht.ent$Bedeutung, uebersicht.ent$de_regiert, uebersicht.ent$en_regiert, uebersicht.ent$Anzahl)
colnames(uebersicht.ent) <- c("Entsprechung", "Bedeutung", "regiert de", "regiert en", "Anzahl")

# Ausgabe der Tabelle für LaTeX:
xtab.uebersicht.ent <- xtable(uebersicht.ent, auto = TRUE, caption = "Abbildungen nach Entsprechungen geordnet mit Rektionsstatus", label = "entbdt")
print(xtab.uebersicht.ent, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Kurze Version der Tabelle (ohne Rektionsstatus) 

uebersicht.ent.kurz <- make.reg.df(int.dat.ent.bdt.reg[c(1,2)])
# Ändern der Reihenfolge der Spalten
uebersicht.ent.kurz <- uebersicht.ent.kurz[c(2,1,3)]
# Ausgabe
xtab.uebersicht.ent.kurz <- xtable(uebersicht.ent.kurz, auto = TRUE, caption = "Abbildungen nach Entsprechungen geordnet ohne Rektionsstatus", label = "entbdtkurz")
print(xtab.uebersicht.ent.kurz, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Sankey-Diagramm

library(googleVis)

# Erstellen eines Data-Frame im Eingabeformat: from, to, Frequenz
inp.sank <- data.frame(table(int.dat.bdt.ent.reg$Entsprechung, int.dat.bdt.ent.reg$Bedeutung))
colnames(inp.sank) <- c("Entsprechung", "Bedeutung", "Freq")

# height und weight können ggf genutzt werden, um Darstellung lesbar zu machen: , options = list(height = 2000, width = 800)
sank <- gvisSankey(inp.sank, from = "Entsprechung", to = "Bedeutung", weight = "Freq", options = list(height = 600, width = 400, sankey = "{
link: { colorMode: 'source'}, tooltip: { textStyle: { fontName: 'Linux Libertine O', fontSize: 12 } }, node: { label: { fontName: 'Linux Libertine O', fontSize: 12 }, nodePadding: 7, interactivity: false } }"))

# Anzeigen im Browser
#plot(sank)

# Ausgabe der HTML-Datei
#print(sank, file = "Sankey-Plot-bearb.html")

# Abschnitt 4.3

# Data-Frame zum Ankucken für die 4n-Fälle
bdt.ent.4n <- data.frame(de.4n$Bedeutung, en.4n$Entsprechung)

# Ordnen der 4n-Fälle und Erstellen von LaTeX-Tabellen

# Geordnet nach Bedeutung
bdt2ent.4n <- bdt.ent.4n[order(bdt.ent.4n$de.4n.Bedeutung, bdt.ent.4n$en.4n.Entsprechung),]

# Überführen in einen Data-Frame, der keine doppelten Zeilen, aber dafür eine weitere Spalte "Anzahl" enthält,
# in der steht, wie oft die Kombination vorkam
bdt2ent.4n <- make.reg.df(bdt2ent.4n)
colnames(bdt2ent.4n) <- c("Bedeutung", "Entsprechung", "Anzahl")

# Geordnet nach Entsprechung
ent2bdt.4n <- bdt.ent.4n[order(bdt.ent.4n$en.4n.Entsprechung, bdt.ent.4n$de.4n.Bedeutung),]

# Überführen in einen Data-Frame, der keine doppelten Zeilen, aber dafür eine weitere Spalte "Anzahl" enthält,
# in der steht, wie oft die Kombination vorkam
ent2bdt.4n <- make.reg.df(ent2bdt.4n)

# Ändern der Reihenfolge der Spalten: Entsprechungen in der ersten Spalte
ent2bdt.4n <- data.frame(ent2bdt.4n$en.4n.Entsprechung, ent2bdt.4n$de.4n.Bedeutung, ent2bdt.4n$Anzahl)
colnames(ent2bdt.4n) <- c("Entsprechung", "Bedeutung", "Anzahl")

# Erstellen der LaTeX-Tabellen (werden ausgegeben):
xtab.bdt2ent.4n <- xtable(bdt2ent.4n, auto = TRUE, caption = "Abbildungen nach Bedeutungen geordnet 4n", label = "4nbdtent")
print(xtab.bdt2ent.4n, booktabs = T, include.rownames = FALSE, caption.placement = "top")

xtab.ent2bdt.4n <- xtable(ent2bdt.4n, auto = TRUE, caption = "Abbildungen nach Entsprechungen geordnet 4n", label = "4nentbdt")
print(xtab.ent2bdt.4n, booktabs = T, include.rownames = FALSE, caption.placement = "top")

# Abschnitt 4.2

# Data-Frame mit den fürs Englische passenden AMV-Daten (Bedeutung, Entsprechung, Rektionsstatus (de/en)). Enthält nur Fälle, bei denen Formulierungsnähe != 4
amv <- droplevels.data.frame(data.frame(de.dis$Bedeutung[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "p"], 
                  en.dis$Entsprechung[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "p"],
                  en.dis$regiert[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "p"],
                  de.dis$regiert[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "p"]))
colnames(amv) <- c("Bedeutung", "Entsprechung", "en_regiert", "de_regiert")

# Data-Frame mit den fürs Englische nicht passenden AMV-Daten (Bedeutung, Entsprechung, Rektionsstatus (de/en)). Enthält nur Fälle, bei denen Formulierungsnähe != 4
# Wird nicht verwendet, könnte aber gundsätzlich interessant sein
amv.np <- droplevels.data.frame(data.frame(de.dis$Bedeutung[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "n"], 
                                        en.dis$Entsprechung[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "n"],
                                        en.dis$regiert[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "n"],
                                        de.dis$regiert[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "n"]))
colnames(amv.np) <- c("Bedeutung", "Entsprechung", "en_regiert", "de_regiert")


# Ordnen nach Bedeutung, Entsprechung, de_regiert, en_regiert (in dieser Reihenfolge)
amv <- amv[order(amv$Bedeutung, amv$Entsprechung, amv$de_regiert, amv$en_regiert),]

# Entfernen des "AMV " bei den Einträgen in der Entsprechungsspalte
amv$Entsprechung <- substr(as.character(amv$Entsprechung), 4, length(as.character(amv$Entsprechung)))

amv$Entsprechung <- factor(amv$Entsprechung)

# Wie amv, aber um eine Spalte "Anzahl" ergänzt. Enthält keine Zeilen mit identischen Werten mehr.
# Stattdessen steht in der Spalte "Anzahl", wie oft die jeweilige Zeile vorkam.
uebersicht.amv.bdt <- make.reg.df(amv)
colnames(uebersicht.amv.bdt) <- c("Bedeutung", "Entsprechung", "regiert en", "regiert de", "Anzahl")

# Ausgabe Bedeutungs-Entsprechungs-Abbildungen
xtab.uebersicht.amv.bdt <- xtable(uebersicht.amv.bdt, auto = TRUE, caption = "Abbildungen nach Bedeutungen geordnet AMV", label = "amvbdtent")
print(xtab.uebersicht.amv.bdt, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Erstellen der nach Entsprechungen geordneten Tabelle:
# Data-Frame, der nach Entsprechungen geordnet ist
uebersicht.amv.ent <- uebersicht.amv.bdt[order(uebersicht.amv.bdt$Entsprechung, uebersicht.amv.bdt$Bedeutung, uebersicht.amv.bdt$"regiert de", uebersicht.amv.bdt$"regiert en"),]

# Ändern der Reihenfolge der Spalten
uebersicht.amv.ent <- uebersicht.amv.ent[c(2,1,3,4,5)]
colnames(uebersicht.amv.ent) <- c("Entsprechung", "Bedeutung", "regiert en", "regiert de", "Anzahl")

# Ausgabe Latex
xtab.uebersicht.amv.ent <- xtable(uebersicht.amv.ent, auto = TRUE, caption = "Abbildungen nach Entsprechungen geordnet AMV", label = "amventbdt")
print(xtab.uebersicht.amv.ent, tabular.environment = "longtable", booktabs = T, include.rownames = FALSE, floating = FALSE, caption.placement = "top")

# Abschnitt 3.2.2

# Formulierungsnähe bei AMV 
amv.form <- droplevels(en.dis$Formulierung[startsWith(as.character(en.dis$Entsprechung), "AMV") & en.dis$Formulierung != "4" & en.dis$Bedeutung == "p"])
summary.amv.form <- t(data.frame(summary(amv.form)))

xtab.summary.amv.form <- xtable(summary.amv.form, auto = T, caption = "Übersicht über die Häufigkeiten der Formulierungsnähewerte in AMV", label = "summaryFormAMV")
print(xtab.summary.amv.form, caption.placement = "top", booktabs = T)

# Rektionsstatus AMV
summary.amv.de.reg <- t(data.frame(summary(amv$de_regiert)))

xtab.summary.amv.de.reg <- xtable(summary.amv.de.reg, auto = T, caption = "Übersicht über die Häufigkeiten der Rektionsstatus in AMV", label = "summaryDEregAMV")
print(xtab.summary.amv.de.reg, caption.placement = "top", booktabs = T)

# Abschnitt 3.3

# de

# Bedeutung (dargestellt in Abschnitt 3.1)
table_de_bdt <- as.data.frame(table(factor(de$Bedeutung)))
colnames(table_de_bdt) <- c("Bedeutung", "Anzahl")
table_de_bdt <- table_de_bdt[order(table_de_bdt$Anzahl, decreasing = T),] # Höchste Frequenzen zuerst -> decreasing
gesamtanzahl <- sum(table_de_bdt$Anzahl)
table_de_bdt <- cbind.data.frame(table_de_bdt, 100 * table_de_bdt$Anzahl/gesamtanzahl)
table_de_bdt$Bedeutung <- as.character(table_de_bdt$Bedeutung)
colnames(table_de_bdt)[3] <- "Anteil (in %)"
xtable_de_bdt <- xtable(table_de_bdt, digits = 2, caption = "Die Häufigkeiten der Bedeutungen in DE", label = "anteilebdtDE")
print(xtable_de_bdt, auto = TRUE, include.rownames = FALSE, caption.placement = "top", booktabs = T)

summary(factor(de$Meta))

# Rektionsstatus
summary.de.reg <- t(data.frame(summary(factor(de$regiert))))
xtab.summary.de.reg <- xtable(summary.de.reg, auto = T, caption = "Die Häufigkeiten der Rektionsstatuswerte in DE", label = "summaryregDE")
print(xtab.summary.de.reg, caption.placement = "top", booktabs = T, include.rownames = F)


# de.dis

# Bedeutung
table_de.dis_bdt <- as.data.frame(table(de.dis$Bedeutung))
colnames(table_de.dis_bdt) <- c("Bedeutung", "Anzahl")
table_de.dis_bdt <- table_de.dis_bdt[order(table_de.dis_bdt$Anzahl, decreasing = T),] # Höchste Frequenzen zuerst -> decreasing
gesamtanzahl <- sum(table_de.dis_bdt$Anzahl)
table_de.dis_bdt <- cbind.data.frame(table_de.dis_bdt, 100 * table_de.dis_bdt$Anzahl/gesamtanzahl)
table_de.dis_bdt$Bedeutung <- as.character(table_de.dis_bdt$Bedeutung)
colnames(table_de.dis_bdt)[3] <- "Anteil (in %)"
xtable_de.dis_bdt <- xtable(table_de.dis_bdt, digits = 2, caption = "Die Häufigkeiten der Bedeutungen in DE-DIS", label = "anteilebdtDEDIS")
print(xtable_de.dis_bdt, auto = TRUE, include.rownames = FALSE, caption.placement = "top", booktabs = T)

# Rektionsstatus

summary.de.dis.reg <- t(data.frame(summary(factor(de.dis$regiert))))
xtab.summary.de.dis.reg <- xtable(summary.de.dis.reg, auto = T, caption = "Die Häufigkeiten der Rektionsstatuswerte in DE-DIS", label = "summaryregDEDIS")
print(xtab.summary.de.dis.reg, caption.placement = "top", booktabs = T, include.rownames = F)

##### Auswertung Rektionsstatus #####

library(vcd) # Benötigt für Cramérs V

# Zusammenhang zwischen regiert im Deutschen und regiert im Englischen (Ende von 4.1.1)

# Daten ohne sekundäre Präpositionen
int.dat.ohne.sekP <- interesting_data[as.character(interesting_data$Entsprechung) != "as regards" & as.character(interesting_data$Entsprechung) != "as to" & as.character(interesting_data$Entsprechung) != "by means of" & as.character(interesting_data$Entsprechung) != "concerning" & as.character(interesting_data$Entsprechung) != "in relation to" & as.character(interesting_data$Entsprechung) != "in terms of" & as.character(interesting_data$Entsprechung) != "on the subject of" & as.character(interesting_data$Entsprechung) != "regarding" & as.character(interesting_data$Entsprechung) != "relating to" & as.character(interesting_data$Entsprechung) != "with regard to",]

# Rektionsstatus + ID, wobei doppelt vorkommende Kombinationen nur einfach enthalten sind
# (damit es keine doppelten mehr gibt). Entsprechung bleibt erhalten, weil es später zur 
# Suche benötigt wird und nicht stört
int.dat.ohne.dop <- unique(interesting_data[2:5])

# ohne sekundäre P und ohne Doppelte
int.dat.ohne.dop.ohne.sekP <- int.dat.ohne.dop[as.character(int.dat.ohne.dop$Entsprechung) != "as regards" & as.character(int.dat.ohne.dop$Entsprechung) != "as to" & as.character(int.dat.ohne.dop$Entsprechung) != "by means of" & as.character(int.dat.ohne.dop$Entsprechung) != "concerning" & as.character(int.dat.ohne.dop$Entsprechung) != "in relation to" & as.character(int.dat.ohne.dop$Entsprechung) != "in terms of" & as.character(int.dat.ohne.dop$Entsprechung) != "on the subject of" & as.character(int.dat.ohne.dop$Entsprechung) != "regarding" & as.character(int.dat.ohne.dop$Entsprechung) != "relating to" & as.character(int.dat.ohne.dop$Entsprechung) != "with regard to",]

table.int.dat.ohne.dop.ohne.sekP <- table(int.dat.ohne.dop.ohne.sekP[2:3])
colnames(table.int.dat.ohne.dop.ohne.sekP) <- c("en n", "en r")
row.names(table.int.dat.ohne.dop.ohne.sekP) <- c("de n", "de r")

chisq.test(table.int.dat.ohne.dop.ohne.sekP, correct = F)

print(assocstats(table.int.dat.ohne.dop.ohne.sekP))

xtab.table.int.dat.ohne.dop.ohne.sekP <- xtable(table.int.dat.ohne.dop.ohne.sekP, caption = "Rektionsstatus deutsch und Rektionsstatus englisch in bereinigtem I kreuztabuliert", label = "tableintdatohnedopohnesek")
print(xtab.table.int.dat.ohne.dop.ohne.sekP, caption.placement = "top", booktabs = T)

# Unterschiede zwischen den Sprachen (Anfang von 4.1.1)

de_en_reg_ohne_dop <- rbind(summary(int.dat.ohne.dop.ohne.sekP$de_regiert), summary(int.dat.ohne.dop.ohne.sekP$en_regiert))
row.names(de_en_reg_ohne_dop) <- c("de", "en")

barplot(t(de_en_reg_ohne_dop), legend.text = T)

chisq.test(de_en_reg_ohne_dop, correct = F)

print(assocstats(de_en_reg_ohne_dop))

# Wie wäre es, wenn man sek. P drin ließe?:
de_en_reg <- rbind(summary(interesting_data$de_regiert), summary(interesting_data$en_regiert))
row.names(de_en_reg) <- c("de", "en")

barplot(t(de_en_reg), legend.text = T)

# Kreuztabelle Bedeutungen

# Kreuztabellen für Bedeutung und Rektionsstatus aneinandergeklebt: Erst de, dann en
kreuztabelle_bdt_reg <- rbind(t(table(interesting_data$Bedeutung, interesting_data$de_regiert)), t(table(interesting_data$Bedeutung, interesting_data$en_regiert)))

xtab.table.kreuztabelle_bdt_reg <- xtable(kreuztabelle_bdt_reg, auto = T, caption = "Kreuztabelle Rektionsstatus Bedeutungen", label = "tableBdtRek")
print(xtab.table.kreuztabelle_bdt_reg, caption.placement = "top", booktabs = T, rotate.colnames = T)

# Kreuztabelle Entsprechungen

# Kreuztabellen für Entsprechung und Rektionsstatus aneinandergeklebt: Erst de, dann en
kreuztabelle_ent_reg <- rbind(t(table(interesting_data$Entsprechung, interesting_data$de_regiert)), t(table(interesting_data$Entsprechung, interesting_data$en_regiert)))

xtab.table.kreuztabelle_ent_reg <- xtable(kreuztabelle_ent_reg, auto = T, caption = "Kreuztabelle Rektionsstatus Entsprechungen", label = "tableEntRek")
print(xtab.table.kreuztabelle_ent_reg, caption.placement = "top", booktabs = T, rotate.colnames = T)