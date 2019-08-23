#install.packages("devtools")
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
#has_devel()
#find_rtools()

# Set working directory to where you want the package stored
# and create the package
setwd("C:/Users/ThinkPad_User_003/Desktop/R")
devtools::create("mcotear")

# package?devtools
# ls("package:devtools")

# Set working directory to current package
setwd("C:/Users/ThinkPad_User_003/Desktop/R Development 3.5.2/mcotear")

# Required packages
use_package("ggplot2")
#use_package("data.table")
#use_package("cowplot")
use_package("gridExtra")
use_package("goftest")


setwd("C:/Users/ThinkPad_User_003/Desktop/R Development 3.5.2/mcotear/data")
#save(amsaa, file = "amsaa.Rdata")

cbPalette <- c("#999999", "#E69F00", "#56B4E9",
  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9",
  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#usethis::use_data(cbPalette, overwrite = T)
#usethis::use_data(cbbPalette, overwrite = T)
#usethis::use_data(amsaa)

load_all()
document()
build_vignettes()
build()
#install()
install.packages("C:/Users/ThinkPad_User_003/Desktop/R Development 3.5.2/mcotear_0.0.0.9000.tar.gz", repos = NULL, type="source")

#build(path = "C:/Users/ThinkPad_User_003/Desktop/R")
#install("C:/Users/ThinkPad_User_003/Desktop/R/mcotear")

library(mcotear)
ls("package:mcotear")
?amsaa

data(amsaa)
example(power_law_process)
?power_law_process

example(df_from_model)
?df_from_model
example(df_from_model)
example(residual_plots)
example(gg_residual_plots)
example(sus_to_percentile)
?sus_to_percentile
example(gg_sus)
gg_sus()
example(sus_suaro)
sus_suaro()

example(mtbf_req)
example(reliability_req)
example(test_duration)
example(exp_mean_lcb)
exp_mean_lcb(n = c(0,1), duration = c(367,400), conf = 0.80)


data(npk)
npk.aov <- aov(yield ~ block + N*P*K, npk)
summary(npk.aov)
npk.aov1 <- aov(yield ~ block + N + K, data = npk)
summary(npk.aov1)
summary.lm(npk.aov1)
model.tables(npk.aov1, type = "means", se = TRUE)
#plot(npk.aov1)

x <- df_from_model(npk.aov1, type = "rstandard")

?grid.layout
residual_plots(x)
gg_residual_plots(x, bins = 10)


# End of File