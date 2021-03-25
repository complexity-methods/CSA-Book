cat("\nBuilding gitbook\n\n")
bookdown::render_book(input = 'index.Rmd',output_format = 'bookdown::gitbook', clean_envir = TRUE)
