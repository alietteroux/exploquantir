#' Calculate and plot Cramer's V between several categorical variables
#'
#' For more details : the Github repository at [https://github.com/alietteroux/exploquantir](https://github.com/alietteroux/exploquantir)
#'
#' @param data a dataframe with *n* rows (individuals) and *p* columns (categorical variables). If *weight* is given, this dataframe must also contain a numeric variable indicating row weights. To avoid misunderstandings, categorical variables must not contain any NA value
#' @param weight an optional numeric column of *data* indicating row weights
#' @param excl an optional list or dataframe indicating wich levels of some categorical variables shoul be ignored. If it's a dataframe : it must contained a column named "variable" and a column named "modalite". If it's a list : this list must be nested with 2 levels (level 1 = variable, level 2 = modalite). See examples.
#' @param progress boolean, if TRUE displays progress on the console
#' @param graph boolean, if TRUE a graph is displayed
#' @param plot.p.value boolean, if TRUE a p-value threshold (0.05) is displayed on the graph
#'
#' @return a list with two components :
#' \itemize{
#'   \item **d.ac** : a dataframe that contains as many rows as contingency analysis have been realised.
#' \itemize{
#' \item *var1* and *var2* (categorical variables)
#' \item *sum.tc* : sum of the contingency table. Nota Bene : this sum is not necessarily equals to the number of *data*'s individuals when levels are excluded by *excl*
#' \item *var1_mod_m5* and *var2_mod_m5* : levels of *var1* and *var2* matching less than 5 individuals
#' \item *part_cases_sup5* : percent of cells of the contingency table matching less than 5 individuals
#' \item *v.cramer* : Cramer's V calculated by *questionr::chisq.test()* when *weight* is not given (Pearson's Chi-squared test) ; by *survey::svychisq()* when *weight* is given (Pearson's X^2: Rao & Scott adjustment)
#' \item *v.cramer.cl* : category of Cramer's : >= 0.3 ; >= 0.15 et <0.3 ; >= 0.05 et <0.15 ; < 0.5
#' \item *cond.Grasland* : indicates whether Claude Grasland's conditions are met (O) or not (N), that means : *sum.tc*>20 ; *var1_mod_m5* and *var2_mod_m5* are null ; *part_cases_sup5*>=80
#' \item *croisement.ordre.alphab* : allows to differenciate each crossing
#' \item *p.value* : p.value of Chi-squared test
#' }
#'   \item **plot** : a *ggplot* graphic to quickly visualize strongest statistic relationships
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import progress
#' @importFrom tibble remove_rownames
#' @importFrom stats chisq.test
#' @importFrom questionr cramer.v
#' @importFrom survey svydesign svychisq
#'
#' @export

acXvars <- function (data,weight,excl,progress,graph,plot.p.value){

  # Verification et formattage des donnees et arguments ----

  ## Traiter les arguments quand ils ne sont pas renseignes ====

  if(missing(progress)){progress <- F}
  if(missing(plot.p.value)){plot.p.value <- F}
  if(missing(graph)){graph <- T}
  if(missing(excl)){excl <- NULL}
  if(missing(weight)){weight <- NULL}

  ## Verifier que "data" est un dataframe ====

  if(!is.data.frame(data)){
    stop("data is not a dataframe")
  }
  data <- droplevels(data)

  ## Renommer la colonne "weight" dans le dataframe "data" ====

  if(!is.null(weight)){
    names(data)[which(names(data)==weight)] <- "iniquantir.weight"
  }

  ## Verification de la nature des variables ====
  if(!is.null(weight)){
    x <- sapply(data[which(colnames(data)!="iniquantir.weight")], class)
  } else {
    x <- sapply(data, class)
  }

  if ("numeric" %in% x |"integer" %in% x){
    stop("at least one column is numeric or integer")
  } else {
    vars <- names(data)[names(data)!="iniquantir.weight"]
  }

  ## Verification de l'absence des NA ====

  vars_NA <- colnames (data[vars] %>% select_if (~sum (is.na(.)) > 0 ))
  if(length(vars_NA)>0){
    stop(paste0("at least one column contains NA [",paste(vars_NA, collapse=" ; "),"]"))
  }

  ## Formattage des modalites rares ou a exclure (excl) ====

  if (!is.null(excl)) {
    if(is.list(excl) & !is.data.frame(excl)){
      excl <- data.frame(variable=rep(names(excl),lapply(excl,length)),
                         modalite=unlist(excl)) %>%
        remove_rownames()
    } else if (is.data.frame(excl) & "variable" %in% names(excl) & "modalite" %in% names(excl)) {
      excl <- excl
    } else {
      stop("data.frame for 'excl' doesn't contain a colum 'variable'' and a column 'modalite'")
    }
  }

  ## Verification du nombre de modalites par variable ====
  nb.levels <- as.integer()
  for (i in 1:length(vars)) {
    if(!is.null(excl)){
      mod.excl <- excl[excl$variable==vars[i],]
    } else {mod.excl <- NULL}
    if(!is.null(mod.excl)){
      x <- data[!(data[[vars[i]]] %in% mod.excl$modalite),]
      x <- length(unique(x[[vars[i]]]))
      nb.levels <- c(nb.levels,x)
    } else {
      x <- length(unique(data[[vars[i]]]))
      nb.levels <- c(nb.levels,x)
    }
  }

  if (0 %in% nb.levels | 1 %in% nb.levels){
    stop(paste0("at least one column contains less than two levels [",
                paste(vars[which(nb.levels<=1)], collapse=" ; "),"]"))
  }

  # Constitution de la structure du tableau attendu ("d.ac") ----

  d.ac <- data.frame(var1=character(0),var2=character(0),
                     sum.tc=integer(0),
                     var1_mod_m5=character(0),
                     var2_mod_m5=character(0),
                     part_cases_sup5=numeric(0),
                     v.cramer=numeric(0),
                     p.value=numeric(0))

  # Definition de la barre d'avancement ----

  avancement <- progress_bar$new(format = "(:spin) [:bar] :percent [Temps ecoule : :elapsedfull || Estimation du temps restant : :eta]",
                                 total = length(vars), complete = "=",  incomplete = "-",
                                 current = ">", clear = FALSE, width = 140)

  # Realisation du tableau "d.ac" ----

  ## Si "weight" n'est pas renseigne ====

  if(is.null(weight)) {

    for (i in 1:length(vars)){
      var1 <- vars[i]
      for (j in 1:length(vars)){
        var2 <- vars[j]
        if(!is.null(excl)){
          if(var1 %in% excl$variable){
            mod.excl <- excl[excl$variable==var1,]
            td <- data[!(data[[var1]] %in% mod.excl$modalite),]
          } else {td <- data}
        } else {td <- data}
        if(!is.null(excl)){
          if(var2 %in% excl$variable){
            mod.excl <- excl[excl$variable==var2,]
            td <- td[!(td[[var2]] %in% mod.excl$modalite),]
          } else {td <- td}
        } else {td <- td}
        td <- droplevels(td)

        tc <- table(td[[var1]],td[[var2]])
        effmarg_row <- apply(tc,1,sum)
        effmarg_col <- apply(tc,2,sum)

        try(test <- suppressWarnings(chisq.test(tc)),silent=T)

        if(!exists("test")) {
          message(paste0(var1," x ",var2," could not work"))
        } else {
          d.ac <- d.ac %>% bind_rows(
            data.frame(var1=var1,var2=var2,sum.tc=sum(tc),
                       var1_mod_m5=paste(names(effmarg_row[which(effmarg_row<5)]),collapse=","),
                       var2_mod_m5=paste(names(effmarg_col[which(effmarg_col<5)]),collapse=","),
                       part_cases_sup5=length(test$expected[test$expected>=5])/length(test$expected)*100,
                       v.cramer=suppressWarnings(cramer.v(tc)),p.value=test$p.value[[1]]))
          rm(test)
        }
      }
      if(progress==T) avancement$tick()
    }
  }

  ## Si "weight" est renseigne ====

  if (!is.null(weight)){

    for (i in 1:length(vars)){
      var1 <- vars[i]
      for (j in 1:length(vars)){
        var2 <- vars[j]
        if(var2!=var1){
          if(!is.null(excl)){
            if(var1 %in% excl$variable){
              mod.excl <- excl[excl$variable==var1,]
              td <- data[!(data[[var1]] %in% mod.excl$modalite),]
            } else {td <- data}
          } else {td <- data}
          if(!is.null(excl)){
            if(var2 %in% excl$variable){
              mod.excl <- excl[excl$variable==var2,]
              td <- td[!(td[[var2]] %in% mod.excl$modalite),]
            } else {td <- td}
          } else {td <- td}
          td <- droplevels(td)

          td$var1 <- td[[var1]]
          td$var2 <- td[[var2]]
          td_s <- svydesign(ids=~1, data=td, weights=~iniquantir.weight)

          try(test <- svychisq(~var1+var2,td_s,statistic="Chisq"),
              silent=T)

          if(!exists("test")) {
            message(paste0(var1," x ",var2," could not work"))
          } else {
            tc <- test$observed
            effmarg_row <- apply(tc,1,sum)
            effmarg_col <- apply(tc,2,sum)

            d.ac <- d.ac %>% bind_rows(
              data.frame(var1=var1,var2=var2,sum.tc=sum(tc),
                         var1_mod_m5=paste(names(effmarg_row[which(effmarg_row<5)]),collapse=","),
                         var2_mod_m5=paste(names(effmarg_col[which(effmarg_col<5)]),collapse=","),
                         part_cases_sup5=length(test$expected[test$expected>=5])/length(test$expected)*100,
                         v.cramer=suppressWarnings(cramer.v(tc)),p.value=test$p.value[[1]]))
            rm(test)
          }
        }
      }
      if(progress==T) avancement$tick()
    }
  }

  ## Creation des variables "cond.Grasland" et "sign.v.cramer" ====

  d.ac <- d.ac %>%
    mutate(v.cramer.cl=case_when(v.cramer>=0.3 ~ "***",
                                  v.cramer>=0.15 & v.cramer<0.3 ~ "**",
                                  v.cramer>=0.05 & v.cramer<0.15 ~ "*",
                                  v.cramer<0.05 ~ ".")) %>%
    mutate(cond.Grasland=case_when(sum.tc>20 & var1_mod_m5=="" & var2_mod_m5=="" & part_cases_sup5>=80~"O",TRUE~"N"))

  ## Creation de la variable "croisement.ordre.alphab" ====

  # pour chaque croisement de variables, nomme le croisement en respectant l'ordre alphabetique des variables
  # (cela permettra de conserver, si necessaire, une seule ligne par croisement - et non 2)

  for (i in 1:nrow(d.ac)){
    temp <- c(d.ac[i,"var1"],d.ac[i,"var2"])
    d.ac[i,"croisement.ordre.alphab"] <- identical(order(as.character(temp)), seq_along(temp))
  }
  d.ac <- d.ac %>%
    mutate(croisement.ordre.alphab=case_when(croisement.ordre.alphab==TRUE ~ paste(var1,var2,sep=" /x/ "),
                                             croisement.ordre.alphab==FALSE ~ paste(var2,var1,sep=" /x/ ")))


  # Representation graphique ----

  ## Discretisation par quantile ====

  q <- range(d.ac[d.ac$var1!=d.ac$var2,"v.cramer"])
  q <- seq(from=q[1],to=q[2],length.out=5)

  ## Caption ====

  caption <- caption <- "V de Cramer (V) : *** V>=30 ; ** V>=0,15 ; * V>=0,05 ; . V<0,05"

  if(!is.null(weight)) {
    caption <- paste0(caption,"\nMesures effectuees avec la fonction svychisq() du package survey - Pearson's X^2: Rao & Scott adjustment")
  } else {
    caption <- paste0(caption,"\nMesures effectuees avec la fonction cramer.v() du package questionr - Pearson's Chi-squared test")
  }

  if(!is.null(excl)){
    caption <- paste0(caption,"\n(N.B : Les modalites rares ont ete exclues dans les seules analyses impliquant les variables qui les comprenaient)")
  }

  ## Si plot.p.value==F ====

  if(plot.p.value==F){

    g <- d.ac %>%
      filter(var1!=var2) %>%
      ggplot(aes(x=var1,y=var2)) +
      geom_point(aes(size=v.cramer,fill=v.cramer,color=cond.Grasland),shape=21) +
      geom_text(aes(label=v.cramer.cl),show.legend=FALSE) +
      geom_abline(intercept=0, slope=1) +
      scale_size_continuous(name="V de Cramer",breaks=q,labels=round(q,2)) +
      scale_fill_continuous(name="V de Cramer",breaks=q,labels=round(q,2),
                            low="green",high="red") +
      scale_color_manual(name="Conditions\nGrasland",values=c("white","black")) +
      guides(fill=guide_legend(), size = guide_legend()) +
      labs(title="Intensite de la relation statistique entre les variables qualitatives",
           subtitle="d'apres le calcul du V de Cramer",x="",y="",
           caption=caption) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            plot.caption=element_text(hjust=0))
  }

  ## Si plot.p.value==T ====

  if(plot.p.value==TRUE){

    g <- d.ac %>%
      mutate(cond.Grasland_p.value=ifelse(cond.Grasland=="N" | p.value>0.05,
                                    "cond. non ok ou p.value>0.05",
                                    "cond. ok & p.value<=0.05")) %>%
      filter(var1!=var2) %>%
      ggplot(aes(x=var1,y=var2)) +
      geom_point(aes(size=v.cramer,fill=v.cramer,color=cond.Grasland_p.value),shape=21) +
      geom_text(aes(label=v.cramer.cl),show.legend=FALSE) +
      geom_abline(intercept=0, slope=1) +
      scale_size_continuous(name="V de Cramer",breaks=q,labels=round(q,2)) +
      scale_fill_continuous(name="V de Cramer",breaks=q,labels=round(q,2),
                            low="green",high="red") +
      scale_color_manual(name="Remarques",values=c("white","black")) +
      guides(fill=guide_legend(), size = guide_legend()) +
      labs(title="Intensite de la relation statistique entre les variables qualitatives",
           subtitle="d'apres le calcul du V de Cramer",x="",y="",
           caption=caption) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            plot.caption=element_text(hjust=0))
  }

  if(graph==T) {print(g)}

  # Creation de l'objet "res.acXvars" a retourner ----

  res.acXvars <- list()
  res.acXvars$d.ac <- d.ac
  res.acXvars$plot <- g

  return(res.acXvars)
}




