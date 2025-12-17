### Data ###
- IAV swine sequences accessed from BVBRC. Sequences from Table 1 were appended with ">REF|".
- Human reference sequences were accessed from https://github.com/flu-crew/offlu-VCM/blob/master/all-human-refs.fna and appended with ">huVaccine|".
- These sequences were manually combined into input.fasta.
- Run `./split_fasta.sh` to generate input_H1.fasta and input_H3.fasta.
- Run `state_counter.R` to count states from which sequences were accessed [optional].

#### Alignment ####
- input_H1.fasta and input_H3.fasta were aligned with Geneious Prime [MAFFT alignment, default settings] and exported as H1_alignment.fasta and H3_alignment.fasta [default settings].

#### IQ-TREE ####
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

#### Tree Processing ####
- Trees were downsampled with PARNAS, https://github.com/flu-crew/parnas.
- `parnas -t "C:\Path\H1_alignment.fasta.treefile" -n 3 --prior "REF" --color "H1_tree.tre"`
  - H1: 13.22% diversity accounted for. 
- `parnas -t "C:\Path\H3_alignment.fasta.treefile" -n 3 --prior "REF" --color "H3_tree.tre"`
  - H3: 19.13% diversity accounted for.
- Because SMOT cannot open nexus files, we open them in FigTree [https://tree.bio.ed.ac.uk/software/figtree/] and save them, converting them to newick format.
- We manually created H1_drop.txt and H3_drop.txt to select sequences to drop from the finished trees. These lists focused on redundant sequences from agricultural fairs.
- `./smot_runs.sh`
- `python3 replace-iqtree-taxa.py "H1_alignment.fasta" H1_smot_fixed.tre H1_smot_fixed_renamed.tre`
- `python3 replace-iqtree-taxa.py "H3_alignment.fasta" H3_smot_fixed.tre H3_smot_fixed_renamed.tre`
  - (fixes strain names, from underscores to proper punctuation)
- `python3 shorten.py H1_smot_fixed_renamed.tre H1_smot_fixed_renamed_shortened.tre 0 7 2`
- `python3 shorten.py H3_smot_fixed_renamed.tre H3_smot_fixed_renamed_shortened.tre 0 7 2`
-   (fixes train names and coloring)
- Final name fixes were completed with Find/Replace in the H1_smot_fixed_renamed_shortened.tre and H3_smot_fixed_renamed_shortened.tre files
-   In both files:
-     Replace "||" with "|"
-     Replace "REF|" with ""
-   In the H1 file:
-    Replace "'REF|huVaccine|lab-22-b|'" with "'REF|huVaccine|A/Hawaii/70/2019|H1N1|2019'"
-    Replace "'REF|huVaccine|'" with "'REF|huVaccine|A/Beijing/262/1995|H1NX|1995'"
-   In the H3 file:
-     Replace "'REF|huVaccine|lab-20-a'" with "'REF|huVaccine|A/Port_Chalmers/1/1973|H3N2|1973'"
-     Replace "'REF|huVaccine|'" with "'REF|huVaccine|A/Hong_Kong/45/2019|H3N2|2018-12-24'"


#### FigTree ####
- 


#### related citations: ####

1. Anderson TK, Chang J, Arendsee ZW, Venkatesh D, Souza CK, Kimble JB, Lewis NS, Davis CT, Vincent AL. Swine influenza A viruses and the tangled relationship with humans. Cold Spring Harbor perspectives in medicine. 2021 Mar 1;11(3):a038737.
