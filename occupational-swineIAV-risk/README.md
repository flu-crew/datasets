### Representative IAV in swine and reference human seasonal virus selection ###

IAV strains were selected to represent the genetic diversity of viruses circulating prior to human sample collection. We queried octofludb https://github.com/flu-crew/octofludb for swine HA genes collected between July 2021 and June 2022. The data included 686 H3 HA and 1,294 H1 HA genes from 25 U.S. states wherein 98.2% of the U.S. swine population resides. Following alignment with mafft v.7.526, maximum likelihood trees were inferred with IQ-Tree v.2.2.2 with automatic model selection and statistical support was assessed using single branch tests and the ultrafast bootstrap algorithm. For each HA clade, a single selection was made using PARNAS v0.1.6, and isolates were requested from the USDA IAV in swine repository at the National Veterinary Services Laboratories. Selection included 7 H1 viruses from classical swine (1A) and human-seasonal (1B) lineages, and 3 H3 viruses from lineages detected in U.S. swine. The inferred phylogenetic trees were down-sampled for visualization using smot v.0.17.4. The smot monophyletic sampling algorithm was applied, maintaining a randomly selected proportion of the HA genes within each clade (H1, 16%; H3, 32%). The virus antigen panel included contemporary H1 and H3 human seasonal influenza vaccine strains (HuVac) derived from the WHO-recommended 2020–2021 Northern Hemisphere influenza vaccine strains and a pre-2009 H1 vaccine strain that shared a common ancestor with the tested swine H1 1B strains.

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
  - (fixes train names and coloring)
- Final name fixes were completed with Find/Replace in the H1_smot_fixed_renamed_shortened.tre and H3_smot_fixed_renamed_shortened.tre files
- In both files:
  - Replace "||" with "|"
  - Replace "REF|" with ""
- In the H1 file:
  - Replace "'REF|huVaccine|lab-22-b|'" with "'REF|huVaccine|A/Hawaii/70/2019|H1N1|2019'"
  - Replace "'REF|huVaccine|'" with "'REF|huVaccine|A/Beijing/262/1995|H1NX|1995'"
- In the H3 file:
  - Replace "'REF|huVaccine|lab-20-a'" with "'REF|huVaccine|A/Port_Chalmers/1/1973|H3N2|1973'"
  - Replace "'REF|huVaccine|'" with "'REF|huVaccine|A/Hong_Kong/45/2019|H3N2|2018-12-24'"
- Tree rendering is completed in FigTree.

#### Cohort Comparisons ####
- R scripts flu54_scripts 1 through 9 processes HI data, producing boxplots demonstrating differences between cohorts for the various strains.
- Odds ratio analyses are produced, calulcating the effect of cohort membership on seropositivity.
- flu54_script_review.R addresses a reviewer suggestion, not present in the final manuscript.

#### Related Citations: ####

Celeste A. Snyder, Garrett M. Janzen, Giovana Ciacci Zanella, Daniel C. A. Moraes, Gustavo S. Silva, Jefferson J. S. Santos, Elizabeth M. Drapeau, Nancy H. L. Leung, Scott E. Hensley, Benjamin J. Cowling, Tavis K. Anderson, Phillip C. Gauger, and Amy L. Baker. Occupationally exposed and general population antibody profiles to influenza A viruses circulating in swine as an indication of zoonotic risk; 2021-22.
