rmarkdown::render('README.Rmd', output_file='README.md')
rmarkdown::render('vignettes/vignette_data.Rmd', output_file='vignette_data.md')

rmarkdown::render('vignettes/vignette_subnational.Rmd', output_file='vignette_subnational.md')

rmarkdown::render('vignettes/vignette_married_women.Rmd', output_file='vignette_married_women.md')
rmarkdown::render('vignettes/vignette_married_women_userdata.Rmd', output_file='vignette_married_women_userdata.md')

rmarkdown::render('vignettes/vignette_all_women.Rmd', output_file='vignette_all_women.md')

# rmarkdown::render('vignettes/vignette_aggregate.Rmd', output_file='vignette_aggregate.md')
rmarkdown::render('vignettes/vignette_aggregate_userdata.Rmd', output_file='vignette_aggregate_userdata.md')
