  


# Load packages, read data
source("R/load.R")

# Fit A-Ci curves, bootstrap, etc.
source("R/analysis.R")


# Make Figures
to.pdf(figure1(),
       filename="output/figures/Figure1.pdf",
       width=6, height=5)

to.pdf(figure2(climdat),
       filename="output/figures/Figure2.pdf",
       width=6, height=5)


to.pdf(figure5(spotagg, fpil, fpop),
       filename="output/figures/Figure5.pdf",
       width=10, height=5)




