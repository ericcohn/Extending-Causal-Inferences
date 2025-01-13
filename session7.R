#################################################################################
###    Extending Causal Inferences                                            ### 
###    Hands-on Session 7 - SYNTHETIC CONTROL METHOD                          ###
###    Summer 2023                                                            ### 
###    CAUSALab                                                               ### 
#################################################################################

# Set working directory
setwd("/Users/eric/Desktop/Coursework/11 - Summer 2023 coursework/CAUSALab course/Session 7/")
# [EDIT] - Update path to the location where your data and results will be stored

# Load required packages
if (!require("haven")) install.packages("haven")
library(haven)
if (!require("Synth")) install.packages("Synth")
library(Synth)

# Disable printing results in scientific notation
options(scipen=999)

# Defining the `%notin` function
`%notin%` <- Negate(`%in%`)

# Setting random number seed to replicate results
set.seed(1234)

#################################################################################
###     DATA ANALYSIS EXERCISE 1                                              ###
#################################################################################
# Import data
dat = read_dta("Bolzano and provinces.dta")

# Need to convert from a tibble to a data frame and convert the province variable to numeric for the synthetic control functions to work
dat = as.data.frame(dat)
dat$numprovince = as.numeric(dat$numprovince)

# Taking a look at the data
head(dat)

# Selecting the variable weights
dataprep.out.tr = dataprep(foo = dat,
                           predictors = c("totcases_100000",
                                          "logpop",
                                          "density",
                                          "popstud",
                                          "active_cases_100000",
                                          "new_tests_1000",
                                          "positivityrate",
                                          "rt"),
                           special.predictors = list(list("newnew", 25:26, "mean"),
                                                     list("newnew", 27:28, "mean"),
                                                     list("newnew", 29:30, "mean")),
                           predictors.op = "mean",
                           dependent = "newnew",
                           time.variable = "totweek",
                           unit.variable = "numprovince",
                           unit.names.variable = "provincestring",
                           treatment.identifier = 24,
                           controls.identifier = (1:37)[-24],
                           time.predictors.prior = 25:30,
                           time.optimize.ssr = 31:37,
                           time.plot = 23:53)
synth.out.tr = synth(data.prep.obj = dataprep.out.tr)
v.wts = as.numeric(synth.out.tr$solution.v)
tab.tr = synth.tab(synth.out.tr, dataprep.out.tr)

# Assessing the variable weights -- do they make sense?
vweights_tab = cbind(sd = apply(dataprep.out.tr$X0, 1, sd), v.wts)
vweights_tab

# Running the SCM using these weights
dataprep.out = dataprep(foo = dat,
                        predictors = c("totcases_100000",
                                       "logpop",
                                       "density",
                                       "popstud",
                                       "active_cases_100000",
                                       "new_tests_1000",
                                       "positivityrate",
                                       "rt"),
                        special.predictors = list(list("newnew", 32:33, "mean"),
                                                  list("newnew", 34:35, "mean"),
                                                  list("newnew", 36:37, "mean")),
                        predictors.op = "mean",
                        dependent = "newnew",
                        time.variable = "totweek",
                        unit.variable = "numprovince",
                        unit.names.variable = "provincestring",
                        treatment.identifier = 24,
                        controls.identifier = c(1:23, 25:37),
                        time.predictors.prior = 32:37,
                        time.optimize.ssr = 23:37, ## This actually does not matter since we supply the variable weights ourselves
                        time.plot = 23:53)
synth.out = synth(data.prep.obj = dataprep.out, custom.v = v.wts)
tab = synth.tab(synth.out, dataprep.out)

tab$tab.pred
tab$tab.v
tab$tab.loss
tab$tab.w

# Assessing the unit weights -- do they make sense?
uweights_tab = tab$tab.w
uweights_tab = uweights_tab[order(-uweights_tab$w.weights),]
uweights_tab[uweights_tab$w.weights > 0,]

# How well do the weights balance the pre-exposure covariates compared to before?
means.noweight = cbind(dataprep.out$X1 - rowMeans(dataprep.out$X0))
means.weight = cbind(dataprep.out$X1 - dataprep.out$X0 %*% tab$tab.w$w.weights)
cbind(means.noweight, means.weight, tab$tab.v)

# Let's see how the synthetic control units compare to the exposed unit
# prior to the exposure
path.case = dataprep.out$Y1plot
path.synth = dataprep.out$Y0plot %*% synth.out$solution.w 
plot(x = 23:37,
     y = dataprep.out$Y0plot[(23:37) - 22,1],
     col = rgb(1, 0, 0, 0.2),
     type = "l",
     xlab = "Week",
     ylab = "New cases (100000)",
     ylim = c(0, 30))
for (i in 2:nrow(dataprep.out$Y0plot)){
  if (i %in% c(13, 17, 29, 31)){
    color = "red"
    lwd = 2
  }
  else{
    color = rgb(1, 0, 0, 0.2)
    lwd = 1
  }
  lines(x = 23:37,
        y = dataprep.out$Y0plot[(23:37) - 22,i],
        col = color,
        lwd = lwd)
}
lines(x = 23:53,
      y = path.case,
      lwd = 3)
legend(x = 22.75, y = 30,
       legend = c("Bolzano",
                  "Provinces contributing to synthetic Bolzano",
                  "Provinces not contributing to synthetic Bolzano"),
       cex = 0.85,
       col = c("black", "red", rgb(1, 0, 0, 0.2)),
       lwd = c(2, 2, 1),
       lty = 1)

# Plotting the evolution of outcomes
plot(x = 23:53,
     y = dataprep.out$Y0plot[,1],
     col = rgb(1, 0, 0, 0.2),
     type = "l",
     xlab = "Week",
     ylab = "New cases (100000)",
     ylim = c(0, 800))
for (i in 2:nrow(dataprep.out$Y0plot)){
  lines(x = 23:53,
        y = dataprep.out$Y0plot[,i],
        col = rgb(1, 0, 0, 0.2))
}
lines(x = 23:53,
      y = path.case,
      lwd = 3)
lines(x = 23:53,
      y = path.synth,
      type = "l",
      col = "red",
      lwd = 3)
abline(v = 37, lty = 2)
legend(x = 22.75, y = 800,
       legend = c("Bolzano", "Synthetic Bolzano",
                  "All other provinces"),
       cex = 0.85,
       col = c("black", "red", rgb(1, 0, 0, 0.2)),
       lty = 1)

# Store treated post/pre-RMSPE-ratio for comparison with placebo effects later on
eff.pointwise = path.case-path.synth
sq.err = eff.pointwise^2
rmse.pre = sqrt(mean(sq.err[1:14]))
rmse.post = sqrt(mean(sq.err[15:length(sq.err)]))
R_main = rmse.post/rmse.pre
R_main

#################################################################################
###     DATA ANALYSIS EXERCISE 2                                              ###
#################################################################################

## Conducting permutation inference
R.perms = vector("numeric", 37)
for (i in 1:37){
  ## Training model
  dataprep.out.tr.perm = dataprep(foo = dat,
                                  predictors = c("totcases_100000",
                                                 "logpop",
                                                 "density",
                                                 "popstud",
                                                 "active_cases_100000",
                                                 "new_tests_1000",
                                                 "positivityrate",
                                                 "rt"),
                                  special.predictors = list(list("newnew", 25:26, "mean"),
                                                            list("newnew", 27:28, "mean"),
                                                            list("newnew", 29:30, "mean")),
                                  predictors.op = "mean",
                                  dependent = "newnew",
                                  time.variable = "totweek",
                                  unit.variable = "numprovince",
                                  unit.names.variable = "provincestring",
                                  treatment.identifier = i,
                                  controls.identifier = (1:37)[-i],
                                  time.predictors.prior = 25:30,
                                  time.optimize.ssr = 31:37,
                                  time.plot = 23:53)
  synth.out.tr.perm = synth(data.prep.obj = dataprep.out.tr.perm)
  v.wts.perm = as.numeric(synth.out.tr.perm$solution.v)
  
  ## Analysis model
  dataprep.out.perm = dataprep(foo = dat,
                               predictors = c("totcases_100000",
                                              "logpop",
                                              "density",
                                              "popstud",
                                              "active_cases_100000",
                                              "new_tests_1000",
                                              "positivityrate",
                                              "rt"),
                               special.predictors = list(list("newnew", 32:33, "mean"),
                                                         list("newnew", 34:35, "mean"),
                                                         list("newnew", 36:37, "mean")),
                               predictors.op = "mean",
                               dependent = "newnew",
                               time.variable = "totweek",
                               unit.variable = "numprovince",
                               unit.names.variable = "provincestring",
                               treatment.identifier = i,
                               controls.identifier = (1:37)[-i],
                               time.predictors.prior = 32:37,
                               time.optimize.ssr = 23:37, ## This actually does not matter since we supply the variable weights ourselves
                               time.plot = 23:53)
  synth.out.perm = synth(data.prep.obj = dataprep.out.perm, custom.v = v.wts.perm)
  path.case.perm = dataprep.out.perm$Y1plot
  path.synth.perm = dataprep.out.perm$Y0plot %*% synth.out.perm$solution.w
  eff.pointwise.perm = path.case.perm-path.synth.perm
  sq.err.perm = eff.pointwise.perm^2
  rmse.pre.perm = sqrt(mean(sq.err.perm[1:14]))
  rmse.post.perm = sqrt(mean(sq.err.perm[15:length(sq.err.perm)]))
  R.perms[i] = rmse.post.perm/rmse.pre.perm
}
hist(R.perms)
abline(v = R_main)
which(R.perms > R_main)

pval_perm = mean(R.perms >= R_main)

#################################################################################
###     DATA ANALYSIS EXERCISE 3                                              ###
#################################################################################
# Backdating the intervention -- if we backdate the intervention to an earlier date, 
# will we still see that the method produces a reasonable synthetic control? As in 
# Abadie, Diamond, and Hainmueller (2015), will backdate to the 1/2 point of pre-exposure 
# period

# Training model
dataprep.out.tr.backdate = dataprep(foo = dat,
                                    predictors = c("totcases_100000",
                                                   "logpop",
                                                   "density",
                                                   "popstud",
                                                   "active_cases_100000",
                                                   "new_tests_1000",
                                                   "positivityrate",
                                                   "rt"),
                                    special.predictors = list(list("newnew", 23:24, "mean"),
                                                              list("newnew", 25, "mean")),
                                    predictors.op = "mean",
                                    dependent = "newnew",
                                    time.variable = "totweek",
                                    unit.variable = "numprovince",
                                    unit.names.variable = "provincestring",
                                    treatment.identifier = 24,
                                    controls.identifier = (1:37)[-24],
                                    time.predictors.prior = 23:25,
                                    time.optimize.ssr = 26:29,
                                    time.plot = 23:53)
synth.out.tr.backdate = synth(data.prep.obj = dataprep.out.tr.backdate)
v.wts.backdate = as.numeric(synth.out.tr.backdate$solution.v)

# Analysis model
dataprep.out.backdate = dataprep(foo = dat,
                                 predictors = c("totcases_100000",
                                                "logpop",
                                                "density",
                                                "popstud",
                                                "active_cases_100000",
                                                "new_tests_1000",
                                                "positivityrate",
                                                "rt"),
                                 special.predictors = list(list("newnew", 26:27, "mean"),
                                                           list("newnew", 28, "mean")),
                                 predictors.op = "mean",
                                 dependent = "newnew",
                                 time.variable = "totweek",
                                 unit.variable = "numprovince",
                                 unit.names.variable = "provincestring",
                                 treatment.identifier = 24,
                                 controls.identifier = (1:37)[-24],
                                 time.predictors.prior = 23:29,
                                 time.optimize.ssr = 26:29, ## This actually does not matter since we supply the variable weights ourselves
                                 time.plot = 23:53)
synth.out.backdate = synth(data.prep.obj = dataprep.out.backdate, custom.v = v.wts.backdate)

## Plotting the evolution of outcomes
path.case.backdate = dataprep.out.backdate$Y1plot
path.synth.backdate = dataprep.out.backdate$Y0plot %*% synth.out.backdate$solution.w

plot(x = 23:37,
     y = path.synth[(23:37) - 22],
     type = "l",
     col = "red",
     lwd = 3,
     xlab = "Week",
     ylab = "New cases (100000)",
     ylim = c(0, 25))
lines(x = 23:37,
      y = path.case.backdate[(23:37) - 22],
      lwd = 3)
lines(x = 23:53,
      y = path.synth.backdate,
      type = "l",
      col = "red",
      lwd = 3,
      lty = 3)
abline(v = 29, lty = 2)
legend(x = 22.75, y = 25,
       legend = c("Bolzano", "Synthetic Bolzano",
                  "Synthetic Bolzano (backdate)"),
       cex = 0.85,
       col = c("black", "red", "red"),
       lty = c(1, 1, 3))

# Backdated post/pre-RMSPE-ratio for comparison with placebo effects later on
eff.pointwise = path.case.backdate-path.synth.backdate
sq.err = eff.pointwise^2
rmse.pre = sqrt(mean(sq.err[(23:29) - 22]))
rmse.post = sqrt(mean(sq.err[(30:36) - 22]))
R_backdate = rmse.post/rmse.pre

#################################################################################
###     DATA ANALYSIS EXERCISE 4                                              ###
#################################################################################
# Leave-one-out of the donor pool -- is the analysis sensitive to which units are in the donor 
# pool? We will leave one unit out at a time, re-run the analysis, and see how similar it is.

R.leaveout = vector("numeric", 37)
paths.synth.leavout = matrix(nrow = 37, ncol = 31)

for (i in c(13, 29, 17, 31)){
  dat.leaveout = dat[-which(dat$numprovince == i),]
  ## Training model
  dataprep.out.tr.leaveout = dataprep(foo = dat.leaveout,
                                      predictors = c("totcases_100000",
                                                     "logpop",
                                                     "density",
                                                     "popstud",
                                                     "active_cases_100000",
                                                     "new_tests_1000",
                                                     "positivityrate",
                                                     "rt"),
                                      special.predictors = list(list("newnew", 25:26, "mean"),
                                                                list("newnew", 27:28, "mean"),
                                                                list("newnew", 29:30, "mean")),
                                      predictors.op = "mean",
                                      dependent = "newnew",
                                      time.variable = "totweek",
                                      unit.variable = "numprovince",
                                      unit.names.variable = "provincestring",
                                      treatment.identifier = 24,
                                      controls.identifier = (1:37)[-c(24, i)],
                                      time.predictors.prior = 25:29,
                                      time.optimize.ssr = 30:37,
                                      time.plot = 23:53)
  synth.out.tr.leaveout = synth(data.prep.obj = dataprep.out.tr.leaveout)
  v.wts.leaveout = as.numeric(synth.out.tr.leaveout$solution.v)
  
  ## Analysis model
  dataprep.out.leaveout = dataprep(foo = dat.leaveout,
                                   predictors = c("totcases_100000",
                                                  "logpop",
                                                  "density",
                                                  "popstud",
                                                  "active_cases_100000",
                                                  "new_tests_1000",
                                                  "positivityrate",
                                                  "rt"),
                                   special.predictors = list(list("newnew", 32:33, "mean"),
                                                             list("newnew", 34:35, "mean"),
                                                             list("newnew", 36:37, "mean")),
                                   predictors.op = "mean",
                                   dependent = "newnew",
                                   time.variable = "totweek",
                                   unit.variable = "numprovince",
                                   unit.names.variable = "provincestring",
                                   treatment.identifier = 24,
                                   controls.identifier = (1:37)[-c(24, i)],
                                   time.predictors.prior = 32:37,
                                   time.optimize.ssr = 23:37, ## This actually does not matter since we supply the variable weights ourselves
                                   time.plot = 23:53)
  synth.out.leaveout = synth(data.prep.obj = dataprep.out.leaveout, custom.v = v.wts.leaveout)
  path.case.leaveout = dataprep.out.leaveout$Y1plot
  path.synth.leaveout = dataprep.out.leaveout$Y0plot %*% synth.out.leaveout$solution.w
  eff.pointwise.leaveout = path.case.leaveout-path.synth.leaveout
  sq.err.leaveout = eff.pointwise.leaveout^2
  rmse.pre.leaveout = sqrt(mean(sq.err.leaveout[1:14]))
  rmse.post.leaveout = sqrt(mean(sq.err.leaveout[15:length(sq.err.leaveout)]))
  R.leaveout[i] = rmse.post.leaveout/rmse.pre.leaveout
  paths.synth.leavout[i,] = path.synth.leaveout
}

#R.leaveout = R.leaveout[c(1:23, 25:37)]
#paths.synth.leavout = paths.synth.leavout[c(1:23, 25:37),]

# What is the distribution of the RMSPE leave-one-out statistics?
hist(R.leaveout)
abline(v = R_main, lwd = 5)

# How many of the leave-one-out statistics are smaller than the observed?
mean(R.leaveout < R_main)

plot(x = 23:53,
     y = paths.synth.leavout[1,],
     col = rgb(1, 0, 0, 0.2),
     type = "l",
     xlab = "Week",
     ylab = "New cases (100000)",
     ylim = c(0, 800))
for (i in 2:nrow(paths.synth.leavout)){
  lines(x = 23:53,
        y = paths.synth.leavout[i,],
        col = rgb(1, 0, 0, 0.2))
}
lines(x = 23:53,
      y = path.case,
      lwd = 3)
lines(x = 23:53,
      y = path.synth,
      type = "l",
      col = "red",
      lwd = 3)
abline(v = 37, lty = 2)
legend(x = 22.75, y = 800,
       legend = c("Bolzano", "Synthetic Bolzano",
                  "Synthetic Bolzano (leave-one-out)"),
       cex = 0.85,
       col = c("black", "red", rgb(1, 0, 0, 0.2)),
       lty = 1)


#################################################################################
###     BONUS DATA ANALYSIS EXERCISE                                          ###
#################################################################################
if (!require("augsynth")) install.packages("augsynth")
library(augsynth)

# The synthetic control method augments the usual synthetic control method with an outcome 
# model to deal with residual covariate imbalances
dat$treat = ifelse(dat$numprovince == 24 & dat$weeknum > 37, 1, 0)
augsynth.out =  augsynth(form = newnew ~ treat | newnew + totcases_100000 + logpop + density + popstud + active_cases_100000 + new_tests_1000 + positivityrate + rt, 
                         unit = numprovince, 
                         cov_agg = mean,
                         time = weeknum, 
                         data = dat,
                         progfunc = "None", 
                         t_int = 37,
                         scm = TRUE,
                         residualize = TRUE)
plot(augsynth.out)
