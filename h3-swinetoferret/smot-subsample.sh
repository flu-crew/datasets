cat h3-root.tre| 
	#remove international clades and data
	smot grep -v "1970.1" | 
	smot grep -v "Other-Human-1990" | 
	smot grep -v "2000.3" |
	smot grep -vP "variant.*Ohio/(19|20|21|22|23).*2017" |
	#drop some 1980s human vaccines
	#smot grep -vP "huVaccine.*198\d" | # too many removed
	smot grep -vP "(Perth\/16|Wisconsin\/67|Bangkok|Texas\/1|Sichuan\/2|Shanghai|Leningrad|89|Shangdong|Wellington|7\/2004|9715293|2671\/2019|50\/2012)"|
	smot sample mono --factor-by-field=1 --keep-regex="(huVaccine|consensus|variant|CVV|huReference|p2f|keep)" --seed=42 --min-tips=3 --proportion=0.1 >h3-smot-mono.tre

#color
cat h3-smot-mono.tre |
        smot color rm |
        smot color leaf -p "variant" "#FF9D00" |
        smot color leaf -p "CVV" "#FF0000" |
        smot color leaf -Pp "(huReference|consensus|huVaccine)" "#999999" |
        smot color branch para --factor-by-capture '\|(1[ABC]\.[^\|\_]*|\d{4}[^\|\_]*|Other-[^\|\_]*)' --colormap tree-colormap.tab >h3-colored.tre
        #sed 's/|hi-test//g' > ${clade}-color.tre
    mv ${clade}-color.tre ${clade}.tre
done



#removed the following	
huVaccine|A/Texas/50/2012|H3N2|3C|2012-04-15
huVaccine|A/Wellington/01/2004|H3N2|2004-01-01
huVaccine|A/California/7/2004|H3N2|2004-09-16
huVaccine|A/Switzerland/9715293/2013|H3N2|3a|2013-12-06
huVaccine|A/Hong_Kong/2671/2019|H3N2|2a1b2a|2019-06-17
huVaccine|A/Perth/16/2009|H3N2|2009-04-07
huVaccine|A/Wisconsin/67/2005|H3N2|2005-01-01
huVaccine|A/Bangkok/1/1979|H3N2|1979-01-01
huVaccine|A/Texas/1/1977|H3N2|1977-01-01
huVaccine|A/Sichuan/2/1987|H3N2|1987-01-01
