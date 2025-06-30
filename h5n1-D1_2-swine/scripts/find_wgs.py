#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
from collections import defaultdict

from Bio import SeqIO


def split_fasta_by_strain(input_fasta):
    # Generated with Cursor AI.
    # Step 1: Parse the FASTA file and collect sequences by strain and segment
    strains = defaultdict(dict)
    for record in SeqIO.parse(input_fasta, "fasta"):
        # Header format: accession|strain-id|subtype|segment|location|host|date
        parts = record.description.split('|')
        if len(parts) < 4:
            continue  # skip malformed headers
        strain_id = parts[1]
        segment = parts[3]
        strains[strain_id][segment] = record

    # Step 2: Identify strains with all 8 segments
    complete_strains = {sid: segs for sid, segs in strains.items() if len(segs) == 8 and all(str(i) in segs for i in range(1, 9))}

    # Step 3: Write each complete strain to a separate file
    for sid, segs in complete_strains.items():
        records = [segs[str(i)] for i in range(1, 9)]
        for r in records:
            r.id = r.name = sid
            r.description = ''
        SeqIO.write(records, f"wgs_fastas/{sid.replace('/', '_')}.fasta", "fasta")


if __name__ == '__main__':
    # args = sys.argv[1:]
    input_fasta = 'NCBI_2025-06-24_s2022_NAM.fasta'
    split_fasta_by_strain(input_fasta)
