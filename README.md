
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
library(exploquantir)

library(tidyverse) # to manage data (select, filter, fct_na_value_to_level...)
library(questionr) # to get "hdv2003" data
data(hdv2003)

# data managing
d <- hdv2003 %>% select_if(is.factor) %>%
  mutate_all(fct_na_value_to_level,"ZZ")
```

## acXvars

Calculates and plots Cramer’s V between several categorical variables

``` r
# acXvars()
res <- acXvars(d,
        excl=list(qualif=c("Technicien","Autre","ZZ"),
                  nivetud=c("ZZ")),graph=F)
```

## actab

Returns a *gt* table displaying contingency analysis results (from two
categorical variables of a dataframe)

``` r
actab(data=d %>% 
        filter(!qualif %in% c("Technicien","Autre","ZZ")),
      var1="qualif",var2="trav.satisf",
      var2.lib="Vous diriez-vous satisfait de votre travail ?",
      option=1)
```

## aovXvars

Calculates and plots proportions of numeric variables variances
explained by categorical variables

``` r
d.com <- readRDS(file = "data/rp2014_reg52.rds")

res <- aovXvars(d.com,graph=F,
                vars.quali=c("DEP","NATURE_EPCI","TAU2013","CATAEU2010"),
                vars.quanti=names(d.com)[grepl("^A1564_CS",names(d.com))])
```

## TabToPlotACM

Returns a dataframe from which we can easily plot a MCA graph using
*ggplot*. This function is inspired by [Anton Perdoncin, “*Représenter
graphiquement les résultats d’une analyse factorielle avec
R*”](https://quanti.hypotheses.org/930).

This dataframe must be named “***modalites***” to work with *code.ex*
argument. Indeed, if *code.ex* is TRUE, the function displays an example
of ggplot code in the Console.

``` r
library(FactoMineR) # to create a MCA object with MCA()

d.acm <- d %>% select(peche.chasse,cuisine,bricol,cinema,sport,
                      sexe,qualif)
res.acm <- MCA(d.acm, quali.sup=c("sexe","qualif"), graph=F)

modalites <- TabToPlotACM(data.ACM=d.acm, res.ACM=res.acm,
                         vars.quali.supp=c("sexe","qualif"),
                         code.ex=F)
```

## TabToGetIndexACM

Returns a *dataframe* from which we can easily get indexes of “junk”
categories, so as to achieve a specific MCA with FactoMineR.

``` r
d.acm <- d %>% select(sexe,qualif,nivetud,occup,clso,trav.imp,trav.satisf)

parts.index <- TabToGetIndexACM(data.vars = d.acm,
                                vars.quali.supp = c("trav.imp","trav.satisf"))

# imagine we'd like to exclude all categories representing less than 5%
excl <- parts.index[parts.index$type.var=="acti" & parts.index$part<5,][["index"]]

# achieving specific MCA with FactoMineR::MCA()
res.acm.s <- MCA(d.acm, quali.sup=c("trav.imp","trav.satisf"), excl=excl, graph=F)
```
