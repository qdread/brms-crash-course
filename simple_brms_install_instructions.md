# Getting Bayesian model software to work in R

My preferred setup for Bayesian models in R is to use the `brms` modeling package. The `brms` package is a R interface that allows you to write models with simple code that resembles `lmer` code, but "behind the scenes" fits a Bayesian model with the software `Stan` that has a state-of-the-art algorithm coded in C++ for quickly and efficiently sampling posterior distributions. There are multiple choices for intermediaries between `brms` and `Stan`. The best option which makes the models run the fastest is yet another software package called `CmdStanR`. So in order to get `brms` working in R, you need to not only install the `brms` package but also install `Stan` and `cmdstanr` on your system and set up everything so that all those pieces of software can communicate with each other.

I want to give props to Paul B&uuml;rkner, the developer of `brms`, and all the developers of `Stan` including Andrew Gelman, Bob Carpenter, and lots of other hard-working people. Their hard work makes our life easier and our stats better!

# Installing brms, Stan, and CmdStanR

First you have to install the `brms` package and all its dependencies from CRAN:

`install.packages('brms')`

Then you have to install CmdStanR. This is a summary of the [installation instructions](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) on the CmdStanR homepage.

Install `CmdStanR` from its own repository:

`install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))`

Now, because everything will be running in C++, you need to make sure your system is configured to compile C++ programs:

```
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)
```

Once the CmdStan toolchain is configured correctly, you can install Stan (or a version of it called CmdStan):

`install_cmdstan()`

# Running brms

Every time you run an R session with brms, you need to load the package with 

`library(brms)`

Then I prefer to set the following options:

`options(mc.cores = 4, brms.backend = 'cmdstanr', brms.file_refit = 'on_change')`

This tells `brms` to run four chains in parallel (set this to a smaller number if your machine will blow up if you try to run 4 cores in parallel), ensures that `cmdstanr` is being used to fit the models, and also allows you to load pre-fit models from a file if you call the same model again which will save you time!