### Data ###
- IAV swine sequences accessed from BVBRC. Sequences from Table 1 were appended with ">REF|".
- Human reference sequences were accessed from https://github.com/flu-crew/offlu-VCM/blob/master/all-human-refs.fna and appended with ">huVaccine|".
- These sequences were manually combined into input.fasta.
- Run `./split_fasta.sh` to generate input_H1.fasta and input_H3.fasta.
- Run `state_counter.R` to count states from which sequences were accessed [optional].

#### Alignment ####
- input_H1.fasta and input_H3.fasta were aligned with Geneious Prime [MAFFT alignment, default settings] and exported as H1_alignment.fasta and H3_alignment.fasta [default settings].

#### Tree - IQ-TREE ####
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
- IQ-TREE generated H1_alignment.fasta.log and H3_alignment.fasta.log files.

#### Tree - Processing ####
- Trees were downsampled with PARNAS, https://github.com/flu-crew/parnas.
- `parnas -t "C:\Path\H1_alignment.fasta.treefile" -n 3 --prior "REF" --color "H1_tree.tre"`
  - H1: 13.22% diversity accounted for. 
- `parnas -t "C:\Path\H3_alignment.fasta.treefile" -n 3 --prior "REF" --color "H3_tree.tre"`
  - H3: 19.13% diversity accounted for.
- Because SMOT cannot open nexus files, we open them in FigTree [https://tree.bio.ed.ac.uk/software/figtree/] and save them, converting them to newick format.
- ./smot_runs.sh
(produces HX_smot_fixed.tre)
python3 GJ-replace-iqtree-taxa.py "H1_alignment.fasta" H1_smot_fixed.tre H1_smot_fixed_renamed.tre
python3 GJ-replace-iqtree-taxa.py "H3_alignment.fasta" H3_smot_fixed.tre H3_smot_fixed_renamed.tre
(fixes strain names, from underscores to proper punctuation)

#### related citations: ####

1. Anderson TK, Chang J, Arendsee ZW, Venkatesh D, Souza CK, Kimble JB, Lewis NS, Davis CT, Vincent AL. Swine influenza A viruses and the tangled relationship with humans. Cold Spring Harbor perspectives in medicine. 2021 Mar 1;11(3):a038737.
