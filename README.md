
<!-- README.md is generated from README.Rmd. Please edit that file -->

This repo contains code to generate results and figures for the
“Gendered male and high-income country authors dominate publication at a
One Health research organization” paper.

This repo uses renv for dependency management. Run `renv::restore` to
install all packages from their recorded versions.

To recreate the results and figures used in the paper, run
`targets::tar_make(main_text)`. Please note that items in the rmd may
not reflect the plos submission guidance for size or other aesthetic
features.

Data used for analysis can be found in the following zenodo deposit:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15226626.svg)](https://doi.org/10.5281/zenodo.15226626)
