# -*- coding: utf-8 -*-
"""
Erstellt aus den CoStEP-XML-Dateien für beide Sprachen Dateien, in denen jeder
Satz ohne irgendwelche XML-Markierungen in einer Zeile steht und alle Tokens
durch Leerzeichen getrennt sind.

Es wird vorausgesetzt, dass sich die XML-Dateien in den Ordnern befinden, die hier
angegeben sind.
"""

import os
import xml.etree.ElementTree as ET
import nltk.data
from spacy.lang.de import German
from spacy.lang.en import English

# SpaCy-Tokenizer
nlp_de = German()
tokenizer_de = nlp_de.Defaults.create_tokenizer(nlp_de)
nlp_en = English()
tokenizer_en = nlp_en.Defaults.create_tokenizer(nlp_en)

# PUNKT-Satzgrenzenerkennung
sent_detector_de = nltk.data.load('tokenizers/punkt/german.pickle')
sent_detector_en = nltk.data.load('tokenizers/punkt/english.pickle')

# Dateien, die den ganzen Text beinhalten werden
whole_de = open(r"D:\Simon\CoStEP_Ziel_nur_deen\de\whole.txt", encoding = "utf-8", mode = "w")
whole_en = open(r"D:\Simon\CoStEP_Ziel_nur_deen\en\whole.txt", encoding = "utf-8", mode = "w")

for xml in os.listdir(r"D:\Simon\CoStEP+Vorverarbeitungstools\sessions"):

    name = xml.split(".")[0]

    print(name)

    # Ergebnisdateien für jeweilige Sitzung
    output_de = open(r"D:\Simon\CoStEP_Ziel_nur_deen\de\\"+name+".txt", encoding = "utf-8", mode = "w")
    output_en = open(r"D:\Simon\CoStEP_Ziel_nur_deen\en\\"+name+".txt", encoding = "utf-8", mode = "w")

    tree = ET.parse(r"D:\Simon\CoStEP+Vorverarbeitungstools\sessions\\"+xml)

    root = tree.getroot()

    for speaker in root.iter("speaker"): # Jeder Turn hat einen Speaker, der die Texte enthält
        # Wenn Text in beiden Sprachen vorhanden
        if len(set(text.get("language") for text in speaker.findall("text")) & {"de", "en"}) == 2:
            for text in speaker.iter("text"):

                # Für beide Sprachen wird das gleiche gemacht

                if text.get("language") == "de":
                    for p in text.findall("p"): # Paragraphen mit p markiert
                        # itertext findet alle Textabschnitte in dem Element. So wie das hier
                        # gemacht wird, werden die Metainformationen (z.B. <report> oder <url>)
                        # einfach weggeworfen.
                        content = "".join(p.itertext())
                        sentences = sent_detector_de.tokenize(content.strip())
                        out_str = "" # Output-String
                        for sent in sentences:
                            tokens = [tok.text for tok in tokenizer_de(sent)]
                            out_str += " ".join(tokens)+"\n"
                            del tokens
                        # end = <p>\n, weil das bei Hunalign die Absatzgrenze markiert.
                        # Ändern, wenn nicht Hunalign genutzt wird!
                        print(out_str, end = "<p>\n", file = output_de)
                        print(out_str, end = "<p>\n", file = whole_de)

                elif text.get("language") == "en":
                    for p in text.findall("p"):
                        content = "".join(p.itertext())
                        sentences = sent_detector_en.tokenize(content.strip())
                        out_str = ""
                        for sent in sentences:
                            tokens = [tok.text for tok in tokenizer_en(sent)]
                            out_str += " ".join(tokens)+"\n"
                            del tokens
                        print(out_str, end = "<p>\n", file = output_en)
                        print(out_str, end = "<p>\n", file = whole_en)


    output_de.close()
    output_en.close()

whole_de.close()
whole_en.close()
