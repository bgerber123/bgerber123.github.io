setwd("C:/Users/bgerber/Google Drive/GITHUB/bgerber123.github.io")

knitr::purl("./Week 1/Information.qmd",output="./Week 1/Information.R")

knitr::purl("./Week 3/StudyDesign.qmd",output="./Week 3/StudyDesign.R")

knitr::purl("./Week 4/Probability.qmd",output="./Week 4/Probability.R")

knitr::purl("./Week 5/regression.qmd",output="./Week 5/regression.R")

knitr::purl("./Week 6/linear.model.qmd",output="./Week 6/linear.model.R")

knitr::purl("./Week 7/Bayesian.qmd",output="./Week 7/bayesian.R")

knitr::purl("./Week 8/RandomEffect.qmd",output="./Week 8/random.effect.R")


# library(quarto)
# library(tinytex)
# quarto_render("./Week 1/Information.qmd", output_format = "pdf")
