source("packages.r")

rmarkdown::render(
  input = "peerreview.rmd", 
  output_file = "index.html", 
  output_dir = "../")