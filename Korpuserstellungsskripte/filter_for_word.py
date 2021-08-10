# -*- coding: utf-8 -*-
"""
Extrahiert die Sätze, bei denen die deutsche Version "über" enthält. Die resultierenden Dateien heißen only_über_sentences.txt.
"""

de_word = "über"

import os

txts_dir = r"D:\Simon\CoStEP_final_nur_deen2\CoStEP_final_nur_deen"

de_data = open(txts_dir+r"\de\whole.txt", mode = "r", encoding = "utf-8")

# nicht auch de_word.capitalize(), weil es ohnehin nicht vorkommt
in_out_list = [True if de_word.lower() in s.split() or de_word.upper() in s.split() else False for s in de_data.readlines()]

de_data.close()

for direc in os.listdir(txts_dir):

    print(direc)

    whole = open(txts_dir+r"\\"+direc+r"\\whole.txt", mode = "r", encoding = "utf-8")

    de_word_file = open(txts_dir+r"\\"+direc+r"\\only_"+de_word+"_sentences.txt", mode = "w", encoding = "utf-8")

    for ind, s in enumerate(whole.readlines()):
        try:
            if in_out_list[ind]:
                print(s, end = "", file = de_word_file)
        except:
            print(ind)

    whole.close()

    de_word_file.close()
