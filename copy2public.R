if(dir.exists("~/GitHub/ComplexityMethods/static/book")){
  R.utils::removeDirectory("~/GitHub/ComplexityMethods/content/static/book", recursive = TRUE)
}
dir.create("~/GitHub/ComplexityMethods/static/book")
#R.utils::copyDirectory(from = "~/GitHub/ComplexityMethods/content/book", to = "~/Documents/GitHub/ComplexityMethods/public/book",
R.utils::copyDirectory(from = "~/GitHub/CSA-book/_book", to = "~/GitHub/ComplexityMethods/static/book", overwrite = TRUE)
R.utils::copyFile(srcPathname = "~/GitHub/CSA-book/CSA-book.pdf", destPathname = "~/GitHub/ComplexityMethods/static/book", overwrite = TRUE)
