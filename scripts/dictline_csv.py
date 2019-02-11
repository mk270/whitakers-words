#!/usr/bin/env python3

## dictline_csv.py, by Iain Douglas Scott
##
## Licence?

import csv

TITLES = ("id",
          "word",
          "other-1",
          "other-2",
          "other-3",
          "pos",
          "morphology",
          "number",
          "code",
          "definitions")
FIELD_IDX = (0, 19, 38, 57, 76, 83, 95, 100, 110)
INFILE = 'DICTLINE.GEN'
OUTFILE = 'DICTLINE.GEN.csv'

with open(INFILE, encoding="latin-1") as dictline, \
     open(OUTFILE, 'w', encoding="utf-8") as dictline_csv:

    writer = csv.writer(dictline_csv, delimiter='\t', lineterminator="\n")
    writer.writerow(TITLES)

    for id_, l in enumerate(dictline, 1):
        fields = [l[start:end].strip()
                  for start, end in zip(FIELD_IDX, FIELD_IDX[1:] + (None,))]
        writer.writerow([id_] + fields)
