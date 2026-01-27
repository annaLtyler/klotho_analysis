## Run multiple gene sets through 3.GTEx_valication.Rmd
## and rename the results 


for f in ../Data/Human/GTEx/*.gct; do
    echo "$f"
    fname=$(basename "$f")
    R -e "rmarkdown::render(here::here('Documents', '3.GTEx_validation_one_tissue.Rmd'))" --args "$fname"
    mv 3.GTEx_validation_one_tissue.html 3.GTEx_validation_"$fname".html
done


#run the summary work flow
R -e "rmarkdown::render(here::here('Documents', '3a.GTEx_Summary.Rmd'))"




