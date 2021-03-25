bookdown::clean_book(clean = getOption("bookdown.clean_book", TRUE))
R.utils::removeDirectory("~/GitHub/ComplexityMethods/content/book", recursive = TRUE)
# Run in terminal
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
# Run after build
source("~/GitHub/CSA-Book/copy2public.R")
