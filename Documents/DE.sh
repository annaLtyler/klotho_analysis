## Run differential gene expression for hets, homs, and carriers



##=====================================================================================##
## Hets 12 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1a.Differential_Expression.Rmd'))" --args "Het" "12"
mv 1a.Differential_Expression.html 1a.Differential_Expression_Het_v_WT_12mos.html


##=====================================================================================##
## Homs 12 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1a.Differential_Expression.Rmd'))" --args "Hom" "12"
mv 1a.Differential_Expression.html 1a.Differential_Expression_Hom_v_WT_12mos.html


##=====================================================================================##
## Carriers 12 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1a.Differential_Expression.Rmd'))" --args "Carrier" "12"
mv 1a.Differential_Expression.html 1a.Differential_Expression_Carriers_v_WT_12mos.html

## Run differential gene expression for hets, homs, and carriers



##=====================================================================================##
## Hets 4 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1a.Differential_Expression.Rmd'))" --args "Het" "4"
mv 1a.Differential_Expression.html 1a.Differential_Expression_Het_v_WT_4mos.html


##=====================================================================================##
## Homs 4 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1a.Differential_Expression.Rmd'))" --args "Hom" "4"
mv 1a.Differential_Expression.html 1a.Differential_Expression_Hom_v_WT_4mos.html


##=====================================================================================##
## Carriers 4 months
##=====================================================================================##

R -e "rmarkdown::render(here::here('Documents', '1a.Differential_Expression.Rmd'))" --args "Carrier" "4"
mv 1a.Differential_Expression.html 1a.Differential_Expression_Carriers_v_WT_4mos.html
