
# exploquantir

This package goes with PROGEDO-Loire formations
(<https://msh-progedo-loire.univ-nantes.fr/>). It aims to make easier
exploratory analysis of quantitative data in social sciences.

## Installation

You can install the development version of exploquantir from GitHub
with:

``` r
# install.packages("devtools")
devtools::install_github("alietteroux/exploquantir")
```

Packages and data used below :

``` r
library(tidyverse)
library(exploquantir)

library(questionr) # to get "hdv2003" data
data(hdv2003)
```

## acXvars

Calculate and plot Cramer’s V between several categorical variables

``` r
# data managing
d <- hdv2003 %>% select_if(is.factor)
vars_NA <- colnames (d%>% select_if (~sum (is.na(.)) > 0 ))
d <- d %>% mutate_at(vars_NA,fct_na_value_to_level,"ZZ")

# acXvars()
res <- acXvars(d,
        excl=list(qualif=c("Technicien","Autre","ZZ"),
                  nivetud=c("ZZ")),graph=F)
```

## actab

Return a *gt* table displaying contingency analysis results (from two
categorical variables of a dataframe)

``` r
actab(data=d %>% 
        filter(!qualif %in% c("Technicien","Autre","ZZ")),
      var1="qualif",var2="trav.satisf",
      var2.lib="Vous diriez-vous satisfait de votre travail ?",
      option=1)
```

## aovXvars

Calculate and plot proportions of numeric variables variances explained
by categorical variables

``` r
d <- readRDS(file = "data/rp2014_reg52.rds")

res <- aovXvars(d,graph=F,
                vars.quali=c("DEP","NATURE_EPCI","TAU2013","CATAEU2010"),
                vars.quanti=names(d)[grepl("^A1564_CS",names(d))])
```
