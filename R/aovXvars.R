#' Calculate and plot proportions of numeric variables variances explained by categorical variables
#'
#' For more details : the Github repository at [https://github.com/alietteroux/exploquantir](https://github.com/alietteroux/exploquantir)
#'
#' @param data a dataframe with *n* rows (individuals) and *p* columns (categorical and numeric variables). To avoid misunderstandings, categorical variables must not contain any NA value
#' @param vars.quali names of categorical variables of *data*
#' @param vars.quanti names of numeric variables of *data*
#' @param progress boolean, if TRUE displays progress on the console
#' @param graph indicates which graph to display : *intens* (proportions of variances explained by categorical variables), *detail* (boxplots for each categorical x numeric variables crossing), *none* (no plot is displayed)
#'
#' @return a list with two components :
#' \itemize{
#'   \item **d.aov** : a dataframe that contains as many rows as analysis of variance (ANOVA) have been realised.
#' \itemize{
#' \item *var.quali* : categorical variable of the ANOVA
#' \item *var.quanti* : numeric variable of the ANOVA
#' \item *sd.eff.gr* : standard deviation of individuals'numbers in each group of the categorical variable
#' \item *cv.eff.gr* : coefficient of variation of individuals'numbers in each group of the categorical variable
#' \item *V1* : sum square of intergroup variance
#' \item *V2* : sum square of intragroup variance
#' \item *intensite* : percent of *var.quanti* variance explained by *var.quali*
#' \item *p.value* : p.value of Fisher's test
#' }
#'   \item **plot.intens** : a *ggplot* graphic to quickly visualize strongest statistic relationships
#'   \item **plot.detail** : a *ggplot* graphic that displays a boxplot for each *categorical x numeric* variables crossing
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stats aov sd
#' @importFrom scales rescale_pal
#'
#' @export

aovXvars <- function (data,vars.quali,vars.quanti,progress,graph){

  # Verification et formattage des donnees et arguments ----

  ## Traiter les arguments quand ils ne sont pas renseignes ====

  if(missing(progress)){progress <- F}
  if(missing(graph)){graph <- "intens"}

  ## Verification de la nature des variables ====

  x <- sapply(data[vars.quali], class)
  if ("numeric" %in% x |"integer" %in% x){
    stop("at least one column quali is numeric or integer")
  }

  x <- sapply(data[vars.quanti], class)
  if ("character" %in% x |"factor" %in% x){
    stop("at least one column quanti is character or factor")
  }

  data <- droplevels(data)

  ## Verification de l'absence des NA dans vars.quali ====

  vars.quali_NA <- colnames (data %>% select(vars.quali) %>% select_if (~sum (is.na(.)) > 0 ))
  if(length(vars.quali_NA)>0){
    stop(paste0("at least one column quali contains NA [",paste(vars.quali_NA, collapse=" ; "),"]"))
  }

  ## Presence des NA dans vars.quanti ====

  vars.quanti_NA <- colnames (data %>% select(vars.quanti) %>% select_if (~sum (is.na(.)) > 0 ))
  if(length(vars.quali_NA)>0){
    message(paste0("Nota bene : at least one column quanti contains NA [",paste(vars.quanti_NA, collapse=" ; "),"]"))
  }

  #--- Creation d'un dataframe (t) contenant les valeurs des differentes ANOVA

  # Constitution de la structure du tableau attendu ("d.aov") ----

  d.aov <- data.frame(var.quali=character(), var.quanti=character(),
                      sd.eff.gr=numeric(),cv.eff.gr=numeric(),
                      V1=numeric(), V2=numeric(), p.value=numeric())

  # Definition de la barre d'avancement ----

  avancement <- progress_bar$new(format = "(:spin) [:bar] :percent [Temps ecoule : :elapsedfull || Estimation du temps restant : :eta]",
                                 total = length(vars.quanti), complete = "=",  incomplete = "-",
                                 current = ">", clear = FALSE, width = 140)

  # Realisation du tableau "d.aov" ----

  for (i in 1:length(vars.quanti)){

    for (j in 1:length(vars.quali)){
      sd.eff.gr <- sd(as.numeric(table(data[[vars.quali[j]]])))
      cv.eff.gr <- sd.eff.gr/mean(as.numeric(table(data[[vars.quali[j]]])))
      moy <- mean(as.numeric(table(data[[vars.quali[j]]])))
      anova <- aov(data[[vars.quanti[i]]] ~ data[[vars.quali[j]]])
      d.aov <- d.aov %>% bind_rows(
        data.frame(var.quali=vars.quali[j],
                   var.quanti=vars.quanti[i],
                   sd.eff.gr=sd.eff.gr,
                   cv.eff.gr=cv.eff.gr,
                   V1=summary(anova)[[1]][["Sum Sq"]][1],
                   V2=summary(anova)[[1]][["Sum Sq"]][2],
                   p.value=summary(anova)[[1]][["Pr(>F)"]][1]))
    }
    if(progress==T) avancement$tick()
  }
  d.aov <- d.aov %>%
    mutate (intensite=V1/(V1+V2)*100) %>%
    relocate(intensite,.before=p.value)

  # Representation graphique g.intens ----

  ## Discretisation par quantile ====

  q.i <- range(d.aov[["intensite"]])
  q.i <- seq(from=q.i[1],to=q.i[2],length.out=5)

  q.cv <- range(d.aov[["cv.eff.gr"]])
  q.cv <- seq(from=q.cv[1],to=q.cv[2],length.out=5)

  ## Graphique g.intens ====

  g.intens <- d.aov %>%
    ggplot(aes(x=var.quali,y=var.quanti)) +
    geom_point(aes(size=intensite,fill=intensite,color=cv.eff.gr,stroke=cv.eff.gr),shape=21) +
    scale_size_continuous(name="% de la variance \nde var.quanti\nexpliquee par var.quali",breaks=q.i,labels=round(q.i,2)) +
    scale_fill_continuous(name="% de la variance \nde var.quanti\nexpliquee par var.quali",breaks=q.i,labels=round(q.i,2),
                          low="green",high="red") +
    scale_color_continuous(name="Coefficient de variation \ndes effectifs de chaque groupe \nde la variable quali",
                           low="black",high="grey80",breaks=q.cv,labels=round(q.cv,2)) +
    continuous_scale("stroke", "stroke",
                     name="Coefficient de variation \ndes eff. de chaque groupe \nde la variable quali",
                     palette = rescale_pal(c(2,0)),
                     breaks = q.cv, labels=round(q.cv,2)) +
    guides(fill=guide_legend(), size = guide_legend(),
           color="none", stroke=guide_legend()) +
    labs(title="Intensite de la relation statistique entre les variables",
         subtitle="d'apres le % de la variance de la variable quantitative expliquee par la variable qualitative",x="",y="",
         caption="Mesures effectuees avec la fonction aov()") +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          plot.caption=element_text(hjust=0)) +
    coord_flip()
  if(graph=="intens"){print(g.intens)}

  # Representation graphique g.detail ----

  modalites <- data[vars.quali]  %>%
    pivot_longer (cols = everything(),names_to="var.quali",values_to="modalite") %>%
    group_by (var.quali,modalite) %>% summarise (effectif=n(),.groups="drop") %>%
    group_by (var.quali) %>% mutate (part=prop.table(effectif)*100)

  d.details <- data[c(vars.quali,vars.quanti)] %>%
    pivot_longer(all_of(vars.quali),names_to="var.quali",values_to="modalite") %>%
    pivot_longer(all_of(vars.quanti),names_to="var.quanti",values_to="valeur") %>%
    left_join(modalites,by=c("var.quali","modalite")) %>%
    mutate(modalite=paste0(modalite," (",round(part,0),"%)"))

  caption <- "Les moyennes sont representees par des points rouges"

  if(length(vars.quanti_NA)==0){
    g.detail <- d.details %>%
      ggplot(aes(x=modalite,y=valeur)) +
      geom_boxplot(varwidth = TRUE) +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
      facet_grid(rows=vars(var.quali),cols=vars(var.quanti),scales = "free") +
      coord_flip() +
      labs(x="",y="",caption=caption) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            plot.caption=element_text(hjust=0))

  } else {
    g.detail <- d.details %>% filter(!is.na(valeur)) %>%
      ggplot(aes(x=modalite,y=valeur)) +
      geom_boxplot(varwidth = TRUE) +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
      facet_grid(rows=vars(var.quali),cols=vars(var.quanti),scales = "free") +
      coord_flip() +
      labs(x="",y="",
           caption=paste0(caption,"\nLes valeurs NA pour [",vars.quanti_NA,"] ne sont pas prises en compte")) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            plot.caption=element_text(hjust=0))
  }
  if(graph=="detail"){print(g.detail)}

  # Creation de l'objet "res.aovXvars" a retourner ----

  res.aovXvars <- list()
  res.aovXvars$d.aov <- d.aov
  res.aovXvars$plot.intens <- g.intens
  res.aovXvars$plot.detail <- g.detail

  return(res.aovXvars)
}


