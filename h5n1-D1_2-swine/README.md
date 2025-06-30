**Sequence analysis workflow** for
*Seger, H., et al., 2025. Systemic distribution and protracted detection of clade 2.3.4.4b highly pathogenic avian influenza A (H5N1) D1.2 in swine following experimental inoculation.*


1. Gene segments associated with H5 strains were downloaded from NCBI virus on Jun-24, 2025. We downloaded segments that were collected in North Americas starting 2022.

2. All H5 strains that had all 8 segments were classified using [genoflu-multi](https://github.com/moncla-lab/GenoFLU-multi). The resulting FASTA file containing all segments and genoflu genotypes is in [NCBI_2025-06-24_s2022_NAM_classified.fasta](NCBI_2025-06-24_s2022_NAM_classified.fasta).

3. HA and NA segments were pulled, aligned using MAFFT v7.475, and trees were built using IQ-Tree v2.3.2. The trees were then midpoint rooted and ladderized using [TreeBender](https://github.com/RybergGroup/phylommand/tree/master?tab=readme-ov-file#treebender).

4. [smot](https://github.com/flu-crew/smot) v0.16.0 was used to color the trees' branches based on the genoflu genotypes of the tips. See the resulting colored trees in [HA.colored.tre](HA.colored.tre) and [NA.colored.tre](NA.colored.tre). These trees can be viewed using FigTree.

All scripts to execute the analysis can be found in the [scripts](scripts/) directory.
