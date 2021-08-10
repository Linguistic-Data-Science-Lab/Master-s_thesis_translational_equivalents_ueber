# -*- coding: utf-8 -*-
"""
Wir machen das Alignment für jedes Dokument einzeln und fügen die Dokumente
am Ende irgendwann zusammen. Grund dafür ist, dass Hunalign nicht mit sehr großen
Korpora zurechtkommt. Es gibt dafür zwar den Batch-Modus, aber dabei wird das
Korpus auch nur zerstückelt und dann scheibchenweise behandelt. Im Batch-Modus
ginge die Alignierung zwar wohl schneller, da das Wörterbuch nicht immer wieder
neu geladen werden müsste, aber dafür würde man auch eine zusätzliche Fehlerquelle
einbauen, denn Huanalign könnte dann ja auch Alignierungen über die Grenzen der
Ursprungsdateien hinweg machen.
Es werden nur die Sätze übernommen, für die es eindeutige Entsprechungen gibt,
(-bisent), weil für die Wortalignierung Sätze einander gegenübergestellt sein
müssen. Außerdem vereinfacht es später das Erstellen eines Korpus, bei dem immer
von einem dt. Satz ausgegangen wird.
"""

import os
import subprocess

sent_tok_data_dir = r"D:\Simon\CoStEP_Ziel_nur_deen"

alignment_dir = r"D:\Simon\CoStEP_Ziel_nur_deen\Hunalign_Alignierungen"

for file in os.listdir(sent_tok_data_dir+r"\de"):

    # whole.txt contains the data from all files
    if file != "whole.txt":

        print(file)

        # -bisent: nur 1-zu-1-Entsprechungen, -realign: verbessert Performance
        subpr_en = subprocess.run(r"D:\Simon\Hunalign\hunalign-1.1\hunalign.exe -bisent -utf -realign D:\Simon\Hunalign\hunalign-1.1\data\de-en.dic "+sent_tok_data_dir+r"\de\\"+file+r" "+sent_tok_data_dir+r"\en\\"+file+r" > "+alignment_dir+"\\"+file,
                       shell = True, stderr=subprocess.PIPE)
