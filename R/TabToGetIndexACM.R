#' Return a helpful *dataframe* for specific MCA with FactoMineR
#'
#' For more details : the Github repository at [https://github.com/alietteroux/exploquantir](https://github.com/alietteroux/exploquantir)
#'
#' @param data.vars a dataframe with *n* rows (individuals) and *p* columns of categorical variables, from which to achieve a MCA.
#' @param vars.quali.supp a vector listing colnames of the categorical supplementary variables in *data.vars*.
#'
#' @return a *dataframe* from which we can easily get indexes of "junk" categories, so as to achieve a specific MCA with FactoMineR.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

TabToGetIndexACM <- function (data.vars, vars.quali.supp){

  # Verification et traitement des arguments manquants ----

  x <- class(data.vars)
  if (!"data.frame" %in% x){
    stop("data.ACM is not a dataframe")
  }

  x <- sapply(data.vars, class)
  if ("numeric" %in% x |"integer" %in% x){
    stop("at least one column of data.ACM is numeric or integer")
  }
  data.vars <- droplevels(data.vars)

  if(missing(vars.quali.supp)){vars.quali.supp <- NULL}

  # Constitution du tableau parts.index ----

  if (is.null(vars.quali.supp)) {
    parts.index <- data.vars %>%
      pivot_longer (cols = everything(),names_to="variable",values_to="modalite") %>%
      group_by (variable,modalite) %>% summarise (effectif=n(),.groups="drop") %>%
      group_by (variable) %>% mutate (part=prop.table(effectif)*100,
                                      nb.mod.ds.var=n()) %>%
      group_by(modalite) %>% mutate(nb.mod.occ=n()) %>%
      mutate(nb.mod.occ.p1 = ifelse(nb.mod.occ>1,"O","N")) %>%
      group_by(variable) %>% mutate(index.concat=ifelse(sum(nb.mod.occ.p1=="O")>0,"O","N")) %>%
      mutate(index=ifelse(index.concat=="O",paste(variable,modalite,sep="_"),
                          as.character(modalite))) %>%
      select(-nb.mod.occ.p1) %>%
      mutate(type.var="acti") %>%
      relocate(type.var,.before=variable)
  }

  if (!is.null(vars.quali.supp)){
    parts.index <- data.vars %>%
      pivot_longer (cols = everything(),names_to="variable",values_to="modalite") %>%
      group_by (variable,modalite) %>% summarise (effectif=n(),.groups="drop") %>%
      group_by (variable) %>% mutate (part=prop.table(effectif)*100,
                                      nb.mod.ds.var=n()) %>%
      group_by(modalite) %>% mutate(nb.mod.occ=n()) %>%
      mutate(nb.mod.occ.p1 = ifelse(nb.mod.occ>1,"O","N")) %>%
      group_by(variable) %>% mutate(index.concat=ifelse(sum(nb.mod.occ.p1=="O")>0,"O","N")) %>%
      mutate(index=ifelse(index.concat=="O",paste(variable,modalite,sep="_"),
                          as.character(modalite))) %>%
      select(-nb.mod.occ.p1) %>%
      mutate(type.var=ifelse(variable %in% vars.quali.supp, "supp","acti")) %>%
      relocate(type.var,.before=variable)
  }

  return(parts.index)

}
