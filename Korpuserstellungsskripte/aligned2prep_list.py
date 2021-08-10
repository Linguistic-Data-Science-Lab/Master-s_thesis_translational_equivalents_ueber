# -*- coding: utf-8 -*-
"""
Liest die Alignierungen ein. Gegeben ein Wort und eine Korpuslänge (bei
länge = 100000 werden etwa die ersten 100.000 Sätze des alignierten Korpus
betrachtet.) wird eine Liste der 100 häufigsten Übersetzungen (mit Häufigkeiten)
in die Konsole ausgegeben und die Annotationsdatei erstellt.
"""

import random

# Random-Seed. Welche Beispielsätze ausgewählt werden, soll später zufällig
# bestimmt werden. Durch Seedsetzen Ergebnisse reproduzierbar.
random.seed(a=2)

wort = "über"

de_datei = "only_ueber_sentences_de.txt"
en_datei = "only_ueber_sentences_en.txt"
al_datei_en = "efmaral_n4l2_deen.txt"

länge = 104007 # ganzes Korpus
#länge = 1000

# Für die schnitt häufigsten englischen Übersetzungsvarianten werden Beispiele
# übernommen
schnitt = 100

# Anzahl der Beispiele, die pro Übersetzungsmöglichkeit ausgegeben werden
beispielanzahl = 4

# Einlesen
de_daten = open(de_datei, encoding = "utf-8", mode = "r")
en_daten = open(en_datei, encoding = "utf-8", mode = "r")
al_daten_en = open(al_datei_en, encoding = "utf-8", mode = "r")

# Listen der Sätze, die Listen der Tokens sind
de_satzliste = [s.strip().split() for s in de_daten.readlines()[:länge]]
en_satzliste = [s.strip().split() for s in en_daten.readlines()[:länge]]

# Satzlisten der Alignierungen (werden unten noch modifiziert)
al_satzliste_en = [s.strip().split() for s in al_daten_en.readlines()[:länge]]

de_daten.close()
en_daten.close()
al_daten_en.close()

# Umwandeln der Satzlisten der Alignierungen, so dass jeder Satz eine Liste ist
# und jede Alignierung ebenfalls eine (zweielementige) Liste mit den Indizes
# des dt. und des engl. Wortes, z.B. [[0, 0], [1, 1], [1, 2], [2, 3]]
for s,satz in enumerate(al_satzliste_en):
    for p,paar in enumerate(satz):
        al_satzliste_en[s][p] = [int(x) for x in paar.split("-")]


# Dictionary, dessen Schlüssel die Übersetzungen des Wortes (kleingeschrieben) sein werden.
# Werte: Liste der Sätze mit dieser Übersetzung
übers_dict = dict()

# Füllen des Dictionarys
for i in range(len(de_satzliste)):
    #if i % 10000 == 0: print(i)
    # de_ind = de_satzliste[i].index(wort) fände nur das erste Vorkommen, also
    # iterieren wir über den ganzen Satz
    for j in range(len(de_satzliste[i])):
        # Wenn Wort gefunden...
        if de_satzliste[i][j] == wort:
            # Übersetzungsstrings initialisieren
            übers_en = ""
            # Listen der Indizes der Übersetzungen
            übers_ind_en = list()
            # Alle engl. Wörter, die mit über aligniert sind,
            # dem Übersetzungsstring hinzufügen
            for al in al_satzliste_en[i]:
                if al[0] == j:
                    übers_en += en_satzliste[i][al[1]]+" "
                    übers_ind_en += [al[1]]
            # Whitespace außen entfernen und lowercasen
            übers_en = übers_en.strip().lower()
            # Satznummer in Liste unter der Übersetzung im Dict eintragen
            if übers_en not in übers_dict.keys():
                übers_dict[übers_en] = [(i,j,übers_ind_en)]
            else:
                übers_dict[übers_en].append((i,j,übers_ind_en))


# Ausgeben der 100 häufigsten Übersetzungen


# Erstellen einer Häufigkeitsliste, die für jede Übersetzung ein Paar bestehend
# aus Häufigkeit und Übersetzung enthält
häufigkeitsliste = [(len(w),s) for (s,w) in übers_dict.items()]

häufigkeitsliste.sort()

häufigkeitsliste.reverse() # häufigste zuerst

for h,w in häufigkeitsliste[:100]:
    print(w, h)

"""
# Für Graphik zu den am häufigsten alignierten Worten in Masterarbeit
for i,(h,w) in enumerate(häufigkeitsliste[:20]):
    print(str(round(0.3+i*0.7,1))+"/"+str(h/5000)+"/"+w+",")
"""

anzahl_ueber = sum([h for h,w in häufigkeitsliste])

print(wort+" kam insgesamt "+ str(anzahl_ueber) + " Mal vor und es wurden "+str(len(häufigkeitsliste))+" Entsprechungen automatisch aligniert.")

häufigkeitsdatei = open("haeufigkeitsdatei.csv", encoding = "utf-8", mode = "w")

for h,w in häufigkeitsliste:
    print(w, ";", h, sep = "", file = häufigkeitsdatei)

häufigkeitsdatei.close()

"""
# Erstellen einer Datei, in der für alle vom Alignierungstool gefundenen Entsprechungen
# Beispiele enthalten sind (vier, wenn es vier oder mehr Vorkommen gibt, sonst alle).
# (Diese Datei wurde genutzt, um die nicht sinnvollen Entsprechungen zu finden)

fuer_alle_beispiele = open("fuer_alle_beispiele.tsv", encoding = "utf-8", mode = "w")

for h,w in häufigkeitsliste:
    for über in random.sample(übers_dict[w], len(übers_dict[w]) if len(übers_dict[w]) < 4 else 4):
        for i in range(len(de_satzliste[über[0]])):
            if i == über[1]:
                print(de_satzliste[über[0]][i].upper(), file = fuer_alle_beispiele, end = " ")
            else:
                print(de_satzliste[über[0]][i], file = fuer_alle_beispiele, end = " ")
        print("\t"+wort+"\t", file = fuer_alle_beispiele)
        for j in range(len(en_satzliste[über[0]])):
            if j in über[2]:
                print(en_satzliste[über[0]][j].upper(), file = fuer_alle_beispiele, end = " ")
            else:
                print(en_satzliste[über[0]][j], file = fuer_alle_beispiele, end = " ")
        print("\t"+" ".join([en_satzliste[über[0]][üi] for üi in über[2]])+"\t"+str(h), file = fuer_alle_beispiele)

fuer_alle_beispiele.close()

# Erstellen einer Liste mit der Häufigkeitsverteilung (Paare aus (Anzahl der entsprechenden Übersetzungen, Korpusfrequenz))

häufigkeiten = [h for h,w in häufigkeitsliste]

häufigkeitsverteilung = {(häufigkeiten.count(h),h) for h in häufigkeiten}

häufigkeitsverteilung = sorted(häufigkeitsverteilung)

print(häufigkeitsverteilung)
"""


# Erstellen der Annotationdatei

# TSV-Datei zum Annotieren mit Tabellenkalkulationsprogramm
output_tsv = open(wort+"_100-raus-amv-nicht_prep_6_ohne_ann2.tsv", encoding = "utf-8", mode = "w")

# Kopfzeile TSV-Datei
print("Sprache\tID\tSätze\tMeta\tBedeutung\tregiert\tFormulierung\tAlignierung\tEntsprechung\tKommentar", file = output_tsv)

# Wir betrachten nur die schnitt am häufigsten alignierten englischen Ausdrücke

# Liste der Entsprechungen unter den ersten 100, die nicht sinnvoll sind
raus = ["than", ",", "that", "is", "are", "be", "was","being", "what", "on on", "and",
        "put", "which", "must", "see", "know", "will", "hear", "how",
        "find", ":", "report", "informed", "cross", "need", "does", "’s", "where",
        "could", "take", "provide", "setting", "way", "between", "any", "available",
        "one"]

# Liste der Alignierungen mit Verb, bei denen die grundsätzliche Alignierung durchaus
# sinnvoll ist, die aber auch ausgeschlossen werden können, weil es immer wieder
# die gleichen Annotationen sind, die entstehen.
amv = ["have", "has", "had", "having", "discuss", "discussing", "discussed", "discussion", "debating",
       "debate", "debated", "consider", "considering", "aware", "decide", "decision",
       "deciding", "exceed", "exceeds", "determine", "possess", "welcome", "transcends",
       "hold", "enjoy", "dealing", "using", "covering", "adopting", "laying", "going", "given"]

# Liste der Entsprechungen, bei denen klar ist, dass es sich nicht um Präpositionen
# handelt
nicht_prep = ["more", "more than"]

raus = raus + amv + nicht_prep

# Beispiele werden ausgewählt für die Entsprechungen unter den ersten schnitt,
# die nicht in raus sind
entsprechungsauswahl = [(h,w) for (h,w) in häufigkeitsliste[:schnitt] if w not in raus]

# Erstellen einer Liste aller schon annotierten Sätz, um Doppelte vermeiden zu können.
schon_annotiert_datei1 = open("über_100-raus_4.txt", mode = "r", encoding = "utf-16")

schon_annotiert_datei2 = open("über_100-raus-amv-nicht_prep_6_ohne_ann1.txt", mode = "r", encoding = "utf-16")

schon_annotiert = [sent.split("\t")[2].strip().lower() for sent in schon_annotiert_datei1.readlines()] + \
                  [sent.split("\t")[2].strip().lower() for sent in schon_annotiert_datei2.readlines()]

schon_annotiert_datei1.close()
schon_annotiert_datei2.close()

beispielsätze = list()

# Auswahl der Beispielsätz
# Anzahl der Beispiele pro englischer Übersetzungsmöglichkeit = 4
for (h, s) in entsprechungsauswahl:
    for i in range(beispielanzahl):
        beispielsatz = random.sample(übers_dict[s], 1)[0] # Ergebnis ist Liste, also nur das erste Element, das wiederum ein Tupel bestehend aus Satznummer, Index über und Liste der Indices der Entsprechungen ist
        # Ausschluss von Doppelten
        while " ".join(de_satzliste[beispielsatz[0]]).lower() in schon_annotiert:
            beispielsatz = random.sample(übers_dict[s], 1)[0]
        # Hinzufügen zu schon_annotiert, damit das Beispiel nicht nochmal ausgewählt werden kann
        schon_annotiert.append(" ".join(de_satzliste[beispielsatz[0]]).lower())
        beispielsätze.append(beispielsatz)

    # Einfache Variante, für wenn man nicht auf Doppelte achtgeben muss:
    # beispielsätze += random.sample(übers_dict[s], beispielanzahl)

# beispielsätze.sort() # nach Satznummer sortieren

for (s,w,ü) in beispielsätze:
    print("de\t"+str(s), file = output_tsv, end = "\t")
    for i in range(len(de_satzliste[s])):
        if i == w:
            print(de_satzliste[s][i].upper(), file = output_tsv, end = " ")
        else:
            print(de_satzliste[s][i], file = output_tsv, end = " ")
    print("\t\t\t\t\t"+wort+"\t\t", file = output_tsv)
    print("en\t"+str(s), file = output_tsv, end = "\t")
    for j in range(len(en_satzliste[s])):
        if j in ü:
            print(en_satzliste[s][j].upper(), file = output_tsv, end = " ")
        else:
            print(en_satzliste[s][j], file = output_tsv, end = " ")
    print("\t\t\t\t\t"+" ".join([en_satzliste[s][üi] for üi in ü])+"\t\t", file = output_tsv)

output_tsv.close()
