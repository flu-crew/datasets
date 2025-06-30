# -*- coding: utf-8 -*-

from typing import Dict
import re
import sys

from Bio import SeqIO


segments = ('PB2', 'PB1', 'PA', 'HA', 'NP', 'NA', 'MP', 'NS')


def read_tsv(tsv_path: str) -> Dict[str, str]:
    tsv_map = {}
    with open(tsv_path, 'r') as tsv:
        for line in tsv.readlines():
            line = line.strip('\n').strip()
            if line and line.count('\t') > 0:
                taxon, value = line.split('\t')
                tsv_map[taxon] = value
    return tsv_map


def append_fasta(fasta_path: str, tsv_path: str, capture_regex: str, outpath: str, keep_all=True):
    seqs = list(SeqIO.parse(fasta_path, 'fasta'))
    upd_seqs = []
    tsv_map = read_tsv(tsv_path)
    for seq in seqs:
        res = re.search(capture_regex, seq.id)
        if res:
            key = res[0]
            if key in tsv_map:
                append_value = tsv_map[key]
                seq.id += f'|{append_value}'
                seq.description = ''
                if not keep_all:
                    upd_seqs.append(seq)
        if keep_all:
            upd_seqs.append(seq)

    SeqIO.write(upd_seqs, outpath, 'fasta')


if __name__ == '__main__':
    capture_regex = r'A/[^\|]+'
    fasta_path = 'NCBI_2025-06-24_s2022_NAM.fasta'
    tsv_path = 'genotypes.tsv'
    outpath = 'NCBI_2025-06-24_s2022_NAM_classified.fasta'

    append_fasta(fasta_path, tsv_path, capture_regex, outpath, keep_all=False)
