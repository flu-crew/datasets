#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script is designed to undo IQTREE's converting all "|"s and "/"s to "_"s
Created by David E. Hufnagel on Wed Aug 18 07:26:25 2021
"""
import sys

preIQfasta = open(sys.argv[1])
postIQtree = open(sys.argv[2])
out = open(sys.argv[3], "w")





#Go through preIQfasta, replace the "|"s in the names with "_"s and make a
#   dict of key: postName  val: preName
nameChange = {}
for line in preIQfasta:
    if line.startswith(">"):
        preName = line.strip().strip(">")
        postName = preName.replace("|","_").replace("/","_").replace('+', '_').replace('(', '_').replace(')', '_').replace(';', '_')
        print(postName, preName)
        nameChange[postName] = preName


#Go through postIQtree and save its data in a string
postIQtreeStr = ''.join(postIQtree.readlines())



#Go through the dict, replace replace the names in the postIQtree,
#   and output the result
for postName, preName in nameChange.items():
    postIQtreeStr = postIQtreeStr.replace(postName, preName)

out.write(postIQtreeStr)





preIQfasta.close()
postIQtree.close()
out.close()
