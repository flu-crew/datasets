### Maximum likelihood phylogenetic trees ###
This analysis uses:
- MAFFT v7.525: [https://mafft.cbrc.jp/alignment/software/](https://mafft.cbrc.jp/alignment/software/)
- IQTree v2.1.4: [http://www.iqtree.org/](http://www.iqtree.org/)
- TreeTime v0.11.3: [https://treetime.readthedocs.io/](https://treetime.readthedocs.io/)

#### File structure ####
- ML_Trees
    - segments (HA, NA, etc.)
        - FASTA file
        - Alignment file
        - ML Tree file
        - treetime_host_mugration
            - Tree file with host annotations
            - Confidence file
            - GTR file
    - TreePDFs
        - PDF tree files for each segment

#### Step-by-step: ####

1. Align FASTA file (mafft --thread -1 fasta_file > alignment_file)
2. Using deflines from alignment files, make a metadata csv file
3. Build ML phylogenetic tree (iqtree -T AUTO -s alignment_file -m MFP -B 1000 --bnni --alrt 1000)
4. Run TreeTime mugration inference (treetime mugration --tree treefile --states meta.csv --attribute host-category --confidence)
