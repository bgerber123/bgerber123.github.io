setwd("C:/Users/bgerber/Google Drive/GITHUB/bgerber123.github.io")

knitr::purl("./Week 1/Information.qmd",output="./Week 1/Information.R")

knitr::purl("./Week 3/StudyDesign.qmd",output="./Week 3/StudyDesign.R")

knitr::purl("./Week 4/Probability.qmd",output="./Week 4/Probability.R")

knitr::purl("./Week 6/linear.model.qmd",output="./Week 6/linear.model.R")

# library(quarto)
# library(tinytex)
# quarto_render("./Week 1/Information.qmd", output_format = "pdf")
