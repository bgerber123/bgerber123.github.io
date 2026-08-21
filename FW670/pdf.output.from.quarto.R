fi = list.file()

quarto::quarto_render("introduction.qmd", output_format = "pdf", quiet=FALSE)

quarto::quarto_render("BigPicture.qmd", output_format = "pdf")

quarto::quarto_render("Probability.qmd", output_format = "pdf")

quarto::quarto_render("likelihood.qmd", output_format = "pdf")

quarto::quarto_render("regression.qmd", output_format = "pdf")

quarto::quarto_render("glm1.qmd", output_format = "pdf")

quarto::quarto_render("glm2.qmd", output_format = "pdf")

quarto::quarto_render("hierarchical.qmd", output_format = "pdf")

quarto::quarto_render("bayesian.qmd", output_format = "pdf")
