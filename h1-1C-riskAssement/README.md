### Phylogenetic analysis of the H1 1C.2.4 clade between 2021 and 2023 ###


#### File structure ####
- 1C24-noDupes-v4: phylogenetic tree inferred from 1C.2.4 HA gene sequences collected and shared in the OFFLU network between 2021 and 2023.
- CVV-vs-PARNAS: a single strain was selected using PARNAS and compared to the A/Bretagne CVV HA gene at the full HA gene (nt) or HA1 peptide (amino acid)
- parnas-coverage95_CVV: The inferred phylogeny was rescaled to reflect substitutions along the branches and then we asked PARNAS to select representatives that would cover all the observed genetic diversity (within a 5% radius, which equates to approximately 28 amino acids)

#### Step-by-step commands for parnas: ####

1. parnas -t 1C24-noDupes-v4.tre -n 1 (this command imports the inferred phylogeny and selects the most representative strain)

2. parnas -t 1C24-noDupes-v4.new --cover --threshold 95 --nt 1C24-noDupes-v4.fna --color "parnas_95coverage_CVV.tre" --prior "CVV" (this command imports the gene tree, rescales it using the alignment, then selects strains to cover observed diversity)