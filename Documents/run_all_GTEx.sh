## Run multiple gene sets through 3.GTEx_valication.Rmd
## and rename the results 

geneSet='Glutamate_Genes'
#geneSet='Parkinson_disease-Mitochondrial_Metabolism_Genes'
#geneSet='Ribosome-Immune_Response_Genes'
#geneSet='Ribosome-Synapse_Genes'

for f in ../Data/Human/GTEx/*.gct; do
    echo "$f"
    fname=$(basename "$f")
    R -e "rmarkdown::render(here::here('Documents', '3.GTEx_validation_one_tissue.Rmd'))" --args "$fname" "$geneSet"
    mv 3.GTEx_validation_one_tissue.html 3.GTEx_validation_"$fname".html
done


#run the summary work flow
R -e "rmarkdown::render(here::here('Documents', '3a.GTEx_Summary.Rmd'))" --args "$geneSet"
mv 3a.GTEx_Summary.html 3a.GTEx_Summary_"$geneSet".html


#check for variation by age in the mice.
R -e "rmarkdown::render(here::here('Documents', '1c_Aging_Gene_Sets.Rmd'))" --args "$geneSet"
mv 1c_Aging_Gene_Sets.html 1c_Aging_Gene_Sets_"$geneSet".html
