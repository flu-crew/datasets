
**Instructions**
Strain metadata and nucleotide sequence data collected in the United States from January 2022 to April 2025 were queried from GenBank and GISAID databases, including eight vaccine strains (below). The dataset was cleaned, removing duplicates and erroneous sequences, and split by hemagglutinin subtype then cropped to the HA1 subunit before aligning with MAFFT v7.526 and building phylogenetic trees with FastTree v2.1.4. Using PARNAS v0.1.7, the branches of the hemagglutinin trees were rescaled to infer ancestral HA1 amino acid substitutions along each branch, allowing coverage to be determined at a 16 amino acid (~5%) difference threshold.

A/swine/Minnesota/A02861318/2023
A/swine/Iowa/A02751531/2023
A/swine/Iowa/A02751457/2023
A/swine/Kansas/A02861399/2023
A/swine/Ohio/A02751535/2023
A/swine/North_Carolina/A02751517/2023
A/swine/Ohio/A02861299/2023
A/swine/Ohio/A02685126/2023



**Method**
Demo code corresponding to directions below in "demo-code.zip" may be provided upon request
Note that intermediate demo files are reconstructed and may not be 100% identical to published results

1) query with octofludb (000.rq --> 000.fasta)
2) confirm vaccine strains in query output
3) split into H1/H3 (999.sh)
4) remove duplicate strains (999.sh)
5) crop HA1 (999.sh)
6) align with MAFFT (999.sh)
7) manually confirm FASTA quality
8) build trees with FastTree (999.sh)
9) use parnas to build AA HA1 rescaled tree (999.sh)
10) find clusters to calculate total coverage of original selections (999.sh)
	from "H1/H3-HA1-cluster.txt" calculate counts in "vaccine-efficacy--tree-counts.tsv"
	and build tree annotations in "annotation-symbol-H1/H3.txt"
11) color trees (999.sh) and manually incorporate annotations into trees
12) assemble figures (images folder)
