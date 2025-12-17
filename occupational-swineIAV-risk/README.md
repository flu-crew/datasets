### Data ###
- IAV swine sequences accessed from BVBRC. Sequences from Table 1 were appended with ">REF|".
- Human reference sequences were accessed from https://github.com/flu-crew/offlu-VCM/blob/master/all-human-refs.fna and appended with ">huVaccine|".
- These sequences were manually combined into input.fasta.
- Run `./split_fasta.sh` to generate input_H1.fasta and input_H3.fasta.
- Run `state_counter.R` to count states from which sequences were accessed [optional].

#### Alignment ####
- input_H1.fasta and input_H3.fasta were aligned with Geneious Prime [MAFFT alignment, default settings] and exported as H1_alignment.fasta and H3_alignment.fasta [default settings].

#### Tree ####
- Alignment files were used to make trees by IQ-TREE at http://iqtree.cibiv.univie.ac.at/.
  - Substitution model: Auto
  - FreeRate heterogeneity: Yes
  - Ascertainment bias correction: No
  - Bootstrap analysis: Ultrafast
  - Number of bootstrap alignments: 1000
  - Maximum iterations: 1000
  - Minimum correlation coefficient: 0.99
  - SH-aLRT branch test: Yes, #replicates: 1000
  - Perturbation strength: 0.5
  - IQ-TREE stopping rule: 100
- H1_alignment.fasta.log and H3_alignment.fasta.log files available.

#### related citations: ####

1. Anderson TK, Chang J, Arendsee ZW, Venkatesh D, Souza CK, Kimble JB, Lewis NS, Davis CT, Vincent AL. Swine influenza A viruses and the tangled relationship with humans. Cold Spring Harbor perspectives in medicine. 2021 Mar 1;11(3):a038737.
