# -*- coding: utf-8 -*-
"""
Erstellt aus den Dateien für die einzelnen Sitzungen jeweils eine große Datei mit dem Namen whole.txt pro Sprache.
"""

import os

txts_dir = r"D:\Simon\CoStEP_final_nur_deen2\CoStEP_final_nur_deen"

for direc in os.listdir(txts_dir):

    whole = open(txts_dir+r"\\"+direc+r"\\whole.txt",encoding = "utf-8", mode = "w")

    for file in  os.listdir(txts_dir+r"\\"+direc):

        if file != "whole.txt":

            textlist = [t.strip() for t in open(txts_dir+r"\\"+direc+r"\\"+file, encoding = "utf-8", mode = "r")]

            for line in textlist:
                print(line, file = whole)

            print(file)

    whole.close()
