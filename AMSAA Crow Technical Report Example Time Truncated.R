library(mcotear)
# Loads the package

ls("package:mcotear")
# Lists all of the functions/objects in the package.

browseVignettes("mcotear")
# browseVignettes shows you some of the package
# documentation. I included some of the papers
# I used to create these functions within the
# documentation .

amsaa
cbPalette
cbbPalette
# These objects already loaded from mcotear
# amsaa is a data set, and
# two other are hexcolor values for
# color-blind friendly color palettes


theme_set(theme_bw())
# Sets ggplot2 color theme


(m <- power_law_process(
  t = split(amsaa$Time, amsaa$System),
  T = list(200,200,200),              	  # If any one was failure truncated
  alpha = 0.05,
  fail.trunc = FALSE,                     # then make this true
  iter = 10
))
# Fit Power-Law NHPP (AMSAA-Crow Model)
# the .converge items can be ignored since
# we have time terminated data.
# If the data were failure terminated
# then iterative methods need to be used
# to solve for the parameters, so the
# .converge items are included so you can
# assess if they if the reached convergence.
# So, the iter parameter in the function is
# only applicable to failure terminated data,
# when we would make fail.trunc = TRUE.
# Also, note that the final lambda estimate
# is a transformation of the lambda.converge,
# so they will not match.
# Additionally, alpha is not implemented right
# now. I plan to include confidence intervals
# for the parameters in a future update, which
# will then depend upon alpha.


common_beta(
  t = split(amsaa$Time, amsaa$System),
  T = list(200,200,200),
  fail.trunc = FALSE)
# Large p-value, do not reject null hypothesis
# of a common shape parameter


trend_test(
  t = split(amsaa$Time, amsaa$System),
  T = list(200,200,200),
  fail.trunc = FALSE)
# Low p-values across all tests indicates we do
# not have a Homogeneous Poisson Process
# (failure rate is not constant).


power_law_mcf(t = amsaa$Time, m$est[1], m$est[2])
# power_law_mcf(t = list(amsaa$Time), m$est[1], m$est[2])
# power_law_mcf(t = split(amsaa$Time, amsaa$System), m$est[1], m$est[2])
# Any one of these work. These values themselves
# are not real useful. This function is more
# useful in plotting the function versus the
# observed/emperical mcf estimates so you can 
# visually assess if the model fits the data.


df_mcf <- mcf(t = split(amsaa$Time, amsaa$System), by = NULL)
head(df_mcf)
names(df_mcf)[1] <- "Time"
head(df_mcf)
# Get the nonparametric mcf estimates.
# I show the first 6 rows so you can see
# what it looks like, and you can see the
# column names. Then I change the name
# of "t" to "Time" so it matches the name
# in the amsaa data set.


amsaa1 <- merge(amsaa, df_mcf, by = "Time")
head(amsaa1)
# Merge the nonparametric mcf estimates
# with amsaa into a new data.frame amsaa1


dev.new(height = 4, width = 4)
ggplot(amsaa1, aes(x = Time, y = mcf)) +
  geom_point() +
  labs(
    x = "Operating Hours", y = "MCF",
    title = "Mean Cumulative Function") +
  stat_function(
    fun = function(x){
      power_law_mcf(t = x, lambda = m$e[[1]], beta = m$e[[2]])$power_mcf
    },
    mapping = aes(colour = "Power-Law NHPP")
  ) +
  scale_colour_manual("Functions:",
    breaks = c("Power-Law NHPP"),
    values = c("Power-Law NHPP" = cbPalette[2]),
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid")
    ))
  ) +
  theme(legend.position = c(.225,.875),
    legend.background = element_rect(fill="grey95"),
    legend.key = element_rect(fill="grey95"),
    legend.text = element_text(size=8),
    legend.title=element_text(size=10)
  )
# setwd("C:/Users/ThinkPad_User_003/Desktop/Analysis of Repairable Systems")
# ggsave("AMSAA_MCF.png", dpi = 300, height = 4, width = 4)
# Mean Cumulative Function Plot
# Looks like the data fits pretty well.
# I show how you can set your working directoy
# and save the plot to file.


df_rocof <- rocof(t = split(amsaa$Time, amsaa$System), by = NULL)
head(df_rocof)
names(df_rocof)[1] <- "Time"
head(df_rocof)
# Get the nonparametric rocof and mtbf estimates.
# I show the first 6 rows so you can see
# what it looks like, and you can see the
# column names. Then I change the name
# of "t" to "Time" so it matches the name
# in the amsaa data set. The rocof and mtbf
# are inverses of one another. You can use
# either of these to create a Duane plot.


amsaa2 <- merge(amsaa1, df_rocof, by = "Time")
head(amsaa2)
# Merge our data set with the df_rocof by the Time variable


ggplot(amsaa2,
  aes(
    x = Time,
    y = rocof)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method='lm', se = FALSE) +
  geom_point() +
  labs(y = "Cumulative Failure Rate") +
  scale_colour_manual(values = cbPalette) +
  ggtitle("Duane Plot")
# Create a Duane Plot. A Duane Plot is another
# tool to visually assess if the data appears to follow
# a Power NHPP by assessing if the data appears to follow
# a straight line when plotted on a log-log scale.
# If the data followed a horizontal straight line
# that is an indication of a HPP rather than a NHPP.
# Here since I plotted the rocof vs time, you can 
# see that it appears as though the rocof is decreasing,
# or in other words the failure rate is decreasing, or
# the time between failures is increasing.


ggplot(amsaa2,
  aes(
    x = Time,
    y = mtbf)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method='lm', se = FALSE) +
  geom_point() +
  labs(y = "Cumulative MTBF") +
  scale_colour_manual(values = cbPalette) +
  ggtitle("Duane Plot")
# This is the same Duane Plot, just now using the
# mtbf instead of the rocof. These are equivalent,
# and lead to the same conclusions.


t <- split(amsaa$Time, amsaa$System)
df_rocof2 <- rocof(t = t, by = names(t))
head(df_rocof2)
names(df_rocof2)[1:2] <- c("System", "Time")
amsaa3 <- merge(amsaa1, df_rocof2, by = c("Time", "System"))
head(amsaa3)
# The rocof and mtbf estimates we got before were
# based on grouping all systems together.
# Instead I could do it by System, and create
# a Duane plot with the Systems separated to 
# see if they all seem to follow a similar pattern.
# This would be a visual assessment of a common beta.


ggplot(amsaa3,
  aes(
    x = Time,
    y = rocof,
    colour = System)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method='lm', se = FALSE) +
  geom_point() +
  labs(y = "Cumulative Failure Rate") +
  scale_colour_manual(values = cbPalette) +
  ggtitle("Duane Plot")
# This is a Duane Plot by system. We see that
# the slopes are not exactly equal, but they
# are all indicating a decreasing failure rate.
# The common_beta test/function already told us
# that there was no statistical difference between
# the systems' failure rates.


df_ttt <- ttt(
  t = split(amsaa2$Time, amsaa2$System),
  T = list(200,200,200))
# This ttt function is for the total time on test (TTT).
# The TTT is used in certain trend tests (the
# Military Handbook Test, and Laplace Centroid Tests
# both have TTT versions, see the trend_test function).
# The TTT can also be used in plots to visually assess
# if the data follow a Power Law Process.


ggplot(df_ttt, aes(x = ttt, y = scaled_ttt)) +
   geom_line(colour = "red") + geom_point() +
   geom_abline(intercept = 0, slope = 1) +
   labs(
     x = "Total Time on Test",
     y = "Scaled Total Time on Test") +
   scale_x_continuous(limits = c(0, 1), expand = c(0, 0))  +
   scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
   theme(plot.margin = margin(t = 10, r = 10, unit = "pt")) +
   coord_fixed() +
   ggtitle("TTT Plot")
# A power-law process is appropriate if the TTT plot lies
# close to the diagonal or is a curve that is either concave
# up or concave down. If there is no pattern, or a curve that
# shifts between being concave up and concave down, the
# power-law process is inadequate. We seem to have a pattern
# with a concave up curve. Concave up indicates decreasing
# failure rate. Concave down would indicate increasing
# failure rate, and no curve (i.e. following the straight line)
# would indicate a HPP.


t <- split(amsaa2$Time, amsaa2$System)
T <- list(200,200,200)
ttt_by_system <- purrr::map2(t, T, ttt)
ttt_by_system <- Map(rbind, ttt_by_system, data.frame(0,0,0))
df_ttt_by_system <- dplyr::bind_rows(ttt_by_system, .id = "System")
head(df_ttt_by_system)
names(df_ttt_by_system)[2] <- "Time"
# In the ttt function I did not include a "by"
# arguement that lets you get the TTT per
# system, for example. But, if you install
# the purrr and dplyr packages you can easily get
# the TTT per system as I show above. You could
# also just run the ttt function 3 times by only
# passing in the t and T for each system separately,
# but if you had 10 systems, 100 systems, etc. you
# probably wouldn't want to do that. You could follow
# this appoach in the other functions too (mcf and rocof),
# but the "by" variable in those was included to
# make it a litter easier. (Note that the Map, step
# I included is really not necessary, but I included
# it so the ttt plot lines would start at the origin.)


ggplot(df_ttt_by_system, aes(x = ttt, y = scaled_ttt, colour = System)) +
   geom_line() + geom_point() +
   geom_abline(intercept = 0, slope = 1) +
   labs(
     x = "Total Time on Test",
     y = "Scaled Total Time on Test") +
   scale_x_continuous(limits = c(0, 1), expand = c(0, 0))  +
   scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
   theme(plot.margin = margin(t = 10, r = 10, unit = "pt")) +
   coord_fixed() +
   scale_colour_manual(values = cbPalette) +
   ggtitle("TTT Plot")


# Plot Time Between Failures vs Time
ggplot(amsaa,
  aes(
    x = Time,
    y = ifelse(diff(c(0,amsaa$Time)) < 0, amsaa$Time, diff(c(0,amsaa$Time))),
    colour = System)) +
  geom_point() + geom_line() +
  labs(
    x = "Operating Hours",
    y = "Time Between Failures",
    main = "Time Between Failures vs Time"
  ) + scale_colour_manual(values = cbPalette) +
  ggtitle("Time Between Failures vs. Time")


# Plot Time Between Failures vs Failure Number
ggplot(amsaa,
  aes(
    x = Failure,
    y = ifelse(diff(c(0,amsaa$Time)) < 0, amsaa$Time, diff(c(0,amsaa$Time))),
    colour = System)) +
  geom_point() + geom_line() +
  labs(
    x = "Failure Number",
    y = "Time Between Failures",
    main = "Time Between Failures vs Time"
  ) + scale_colour_manual(values = cbPalette) +
  ggtitle("Time Between Failures vs. Failure Number") +
  scale_x_continuous(breaks = 1:15) +
  theme(panel.grid.minor.x = element_blank())


# End of File