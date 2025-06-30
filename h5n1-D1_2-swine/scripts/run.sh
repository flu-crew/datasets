#!/usr/bin/bash

# Find whole genome sequenced strains and classify them using Genflu-multi (requires it being installed).
mkdir wgs_fastas
python find_wgs.py
genoflu-multi.py -f wgs_fastas/ -n 12

# Append genoflu-assigned genotypes to the fasta
cat wgs_fastas/results/results.tsv | cut -f 1,2 | grep -v "Not assigned" > genotypes.tsv
python fasta_append.py

# Split out HA and NA sequences, align, and build trees
cat NCBI_2025-06-24_s2022_NAM_classified.fasta | smof grep "|4|" > HA.fasta
cat NCBI_2025-06-24_s2022_NAM_classified.fasta | smof grep "|6|" > NA.fasta
mafft --thread 12 HA.fasta > HA.aln
mafft --thread 12 NA.fasta > NA.aln
iqtree2 -s HA.aln -T 12 -m GTR+F+R5
iqtree2 -s NA.aln -T 12 -m GTR+F+R5
cat HA.aln.treefile | smot grep "H5N1" --newick | treebender -m | treebender -l > HA.iqtree.H5N1.tre
cat NA.aln.treefile | smot grep "H5N1" --newick | treebender -m | treebender -l > NA.iqtree.H5N1.tre

# Color the trees with smot
smot color branch mono --factor-by-field 8 -c colors.tsv HA.iqtree.H5N1.tre > HA.colored.tre
smot color branch mono --factor-by-field 8 -c colors.tsv NA.iqtree.H5N1.tre > NA.colored.tre
