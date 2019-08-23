# Load package
library(mcotear)

# list all functions in the package
ls("package:mcotear")

# get help on a function
?sus_to_percentile

# run the function examples
example(sus_to_percentile)


# browseVignettes shows you some of the package
# documentation. I included some of the papers
# I used to create some of the reliability
# (repairable systems analysis) functions within
# the documentation .
browseVignettes("mcotear")


# All of the functions in this package
# are written directly in R.
# If you run the function name without parens
# you can see the specific R code I used.
sus_to_percentile

# Some functions require other packages. You be promted
# to download them when the package is installed,
# if you do not already have them. When you load,
# this package, those should be loaded automatically.
gg_sus()


# End of File