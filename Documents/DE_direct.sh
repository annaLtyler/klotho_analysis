## Run differential gene expression for hets, homs, and carriers


##=====================================================================================##
## Hets 12 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1b.Direct_Variant_Comparison.Rmd'))" --args "Het" "12"
mv 1b.Direct_Variant_Comparison.html 1b.Direct_Variant_Comparison_Het_12mos.html


##=====================================================================================##
## Homs 12 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1b.Direct_Variant_Comparison.Rmd'))" --args "Hom" "12"
mv 1b.Direct_Variant_Comparison.html 1b.Direct_Variant_Comparison_Hom_12mos.html


##=====================================================================================##
## Carriers 12 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1b.Direct_Variant_Comparison.Rmd'))" --args "Carrier" "12"
mv 1b.Direct_Variant_Comparison.html 1b.Direct_Variant_Comparison_Carrier_12mos.html


