sources <- list.files("R/", full.names = TRUE)
for (s in sources) {
  source(s)
}
# Install packages
load_packages(packages = c("dplyr", "rstanarm", "tidyr", "drake", "brms", "loo","future",
                           "cowplot","ggplot2"))

source("drake_plan/plan.R")

# Temporary fix to rstan survival kfold issue
f <- eval(parse(
  text=sub(
    'fit_k_call$subset <- eval(fit_k_call$subset)', 
    'fit_k_call$subset <- if(is.stansurv(x)) NULL else eval(fit_k_call$subset)', 
    deparse(rstanarm:::kfold.stanreg), fixed=TRUE)
), envir=environment(rstanarm:::kfold.stanreg))

assignInNamespace('kfold.stanreg', value=f, ns='rstanarm')

# Set seed

set.seed(45235)
# Set parallel compute
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# BRMS parallel
library(future)
plan(multiprocess)

options(drake_make_menu = FALSE)
# lock_envir must be false in order to run rstanarm in parallel
# https://github.com/ropensci/drake/issues/960#issuecomment-547691147
drake::make(plan, lock_envir = FALSE)


