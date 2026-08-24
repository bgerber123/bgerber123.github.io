fi = list.file()

quarto::quarto_render("./FW670/introduction.qmd", output_format = "pdf", quiet=FALSE)

quarto::quarto_render("./FW670/BigPicture.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/Probability.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/likelihood.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/regression.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/glm1.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/glm2.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/hierarchical.qmd", output_format = "pdf")

quarto::quarto_render("./FW670/bayesian.qmd", output_format = "pdf")
