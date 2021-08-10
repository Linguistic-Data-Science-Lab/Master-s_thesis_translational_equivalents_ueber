# -*- coding: utf-8 -*-
"""
Erstellt aus den Hunspell-Alignierungsdateien, die ja nur 1-zu-1-Entsprechungen
enthalten, die Ausgangsdaten für die Wortalignierung, d.h. eine Datei für jede
Sitzung, wobei die Dateien fürs Deutsche und Englische gleich viele Zeilen enthalten.
In jeder Zeile steht ein Satz und dieser Satz ist derjenige, den Hunalign als
(alleinige) Entsprechung zum deutschen Satz gefunden hat.
"""

import os

sent_tok_data_dir = r"D:\Simon\CoStEP_Ziel_nur_deen"

alignment_dir = r"D:\Simon\CoStEP_Ziel_nur_deen\Hunalign_Alignierungen"

result_dir = r"D:\Simon\CoStEP_final_nur_deen"

for file in os.listdir(sent_tok_data_dir+r"\de"):

    # whole.txt contains the data from all files
    if file != "whole.txt":

        print(file) # Fortschrittsmeldungen

        # Dictionary containing the alignments (keys: index sentence de, value: index sentence en)
        en_alignments = {int(l.split("\t")[0]): int(l.split("\t")[1]) for l in open(alignment_dir+"\\"+file).readlines()}

        # German file:
        de_data = open(sent_tok_data_dir+r"\de\\"+file, encoding = "utf-8", mode = "r")

        # list of the German file's sentences
        de_linelist = [line.strip() for line in de_data.readlines()]

        results_de = open(result_dir+r"\de\\"+file, encoding = "utf-8", mode = "w")

        # English
        en_data = open(sent_tok_data_dir+r"\en\\"+file, encoding = "utf-8", mode = "r")
        en_linelist = [line.strip() for line in en_data.readlines()]
        results_en = open(result_dir+r"\en\\"+file, encoding = "utf-8", mode = "w")

        # Going through the indexes of the German sentences that have 1-to-1-correspondents
        # in English
        for ind in sorted(en_alignments.keys()):

            # Hunalign also aligns the empty lines at the end of each file, but
            # they are not considered here, so we have to exclude them. Because
            # of the <p> in the penultimate line, sometimes there are two
            # problematic matches here
            if not (ind == len(de_linelist) or ind == len(de_linelist) + 1) and\
            not (en_alignments[ind] == len(en_linelist) or en_alignments[ind] == len(en_linelist) + 1):

                # We don't want to print the paragraph border indicators Hunalign needed
                # When the original data contains errors (e.g. because some quote was
                # marked as such in the XML of one language but not in that of another),
                # Hunalign sometimes matches normal sentences to <p> (which it normally
                # shouldn't do), so one has to look for <p>s in both languages because
                # otherwise a sentence could be printed in one language's file but not
                # in the other one's which would blow up the following line enumeration
                if de_linelist[ind] != "<p>" and en_linelist[en_alignments[ind]] != "<p>":
                    print(de_linelist[ind], file = results_de)
                    print(en_linelist[en_alignments[ind]], file = results_en)

        de_data.close()
        results_de.close()

        en_data.close()
        results_en.close()
