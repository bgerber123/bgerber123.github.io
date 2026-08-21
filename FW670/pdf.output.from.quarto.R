fi = list.file()

quarto::quarto_render("introduction.qmd", output_format = "pdf", quiet=FALSE)

quarto::quarto_render("BigPicture.qmd", output_format = "pdf")

quarto::quarto_render("bayesian.qmd", output_format = "pdf")
