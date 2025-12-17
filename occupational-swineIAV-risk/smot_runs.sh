#!/bin/bash

sed -i '/^$/d' H1_drop.txt
sed -i '/^$/d' H3_drop.txt
smot tips H1_figtree.tre | grep -v -f H1_drop.txt > H1_keep.txt
smot tips H3_figtree.tre | grep -v -f H3_drop.txt > H3_keep.txt

smot grep --file H1_keep.txt H1_figtree.tre > H1_figtree_pruned.tre
smot grep --file H3_keep.txt H3_figtree.tre > H3_figtree_pruned.tre

#Note: I had trouble with "REF" keep, so using -r to keep REF fixed it.
smot sample mono H1_figtree_pruned.tre --seed 24601 -p 0.16 -k "REF" -r "REF" > H1_smot.tre
smot sample mono H3_figtree_pruned.tre --seed 24601 -p 0.32 -k "REF" -r "REF" > H3_smot.tre

sed 's/\//|/g'  H1_smot.tre > H1_smot_fixed.tre
sed 's/\//|/g'  H3_smot.tre > H3_smot_fixed.tre

rm H1_smot.tre H1_figtree_pruned.tre H3_smot.tre H3_figtree_pruned.tre
