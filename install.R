#!/usr/bin/env Rscript
library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc")

rmarkdown::render(input = "notebook/peerreview.Rmd",
                  output_file = "index.html",
                  output_dir = "./")
