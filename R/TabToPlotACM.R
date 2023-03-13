#' Return a *dataframe* that makes easier MCA-plotting with ggplot
#'
#' This function is inspired by [Anton Perdoncin, "*Représenter graphiquement les résultats d’une analyse factorielle avec R*"](https://quanti.hypotheses.org/930).
#'
#' For more details : the Github repository at [https://github.com/alietteroux/exploquantir](https://github.com/alietteroux/exploquantir)
#'
#' @param data.ACM a dataframe with *n* rows (individuals) and *p* columns of categorical variables.
#' @param res.ACM a "MCA" object : the result of *FactoMineR:MCA(data.ACM)*.
#' @param vars.quali.supp a vector listing colnames of the categorical supplementary variables in *data.ACM*.
#' @param code.ex boolean, if TRUE displays an example of ggplot code in the Console.
#'
#' @return
#' \itemize{
#'   \item a *dataframe* from which we can easily plot a MCA graph (it must be named "modalites" to work with *code.ex*)
#'   \item an example of ggplot code in the Console (if code.ex=T),
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @export

TabToPlotACM <- function (data.ACM, res.ACM, vars.quali.supp, code.ex){

  # Verification et formattage des donnees et arguments ----

  ## Traiter les arguments quand ils ne sont pas renseignes ====

  if(missing(vars.quali.supp) & "quali.sup" %in% names(res.ACM)) {
    stop("res.ACM contains categorical supplementary variables, please inform vars.quali.supp")
  }

  if(!missing(vars.quali.supp) & !"quali.sup" %in% names(res.ACM)) {
    stop("You've informed vars.quali.supp but res.ACM does not contain categorical supplementary variables")
  }

  if(missing(vars.quali.supp)){vars.quali.supp <- NULL}

  ## Verification de la presence et de la nature des objets ====

  if(missing(data.ACM)){
    stop("data.ACM is missing")
  }

  x <- class(data.ACM)
  if (!"data.frame" %in% x){
    stop("data.ACM is not a dataframe")
  }

  x <- sapply(data.ACM, class)
  if ("numeric" %in% x |"integer" %in% x){
    stop("at least one column of data.ACM is numeric or integer")
  }
  data.ACM <- droplevels(data.ACM)

  if(missing(res.ACM)){
    stop("res.ACM is missing")
  }

  if (class(res.ACM)[[1]]!="MCA"){
    stop("res.ACM is not a MCA object")
  }

  if(missing(code.ex)){
    stop("code.ex is missing (boolean, must be T or F")
  }

  ## Verification de l'absence des NA ====

  vars_NA <- colnames (data.ACM %>% select_if (~sum(is.na(.))>0))
  if(length(vars_NA)>0){
    stop(paste0("at least one column of data.ACM contains NA [",paste(vars_NA, collapse=" ; "),"]"))
  }

  # Constitution du tableau modalites ----

  if (is.null(vars.quali.supp)) {
    modalites <- data.ACM %>%
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
    modalites <- data.ACM %>%
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

    # Inserer les elements necessaires a l'interpretation dans "modalites" ----

    if (is.null(vars.quali.supp)) {

      #- informations sur les modalites des variables actives
      infos <- c("coord","contrib","cos2","v.test","eta2")
      for (i in infos) {
        temp <- as.data.frame(round(res.ACM$var[[i]], 2))  %>%
          rename_all(tolower) %>% rename_all(~ gsub(" ", "", .)) %>%
          rename_all(~ paste(., i, sep = "_")) %>% mutate(index = rownames(.))
        if (i!="eta2") {modalites <- modalites %>% left_join(temp,by="index")}
        else {modalites <- modalites %>% left_join(temp,by=c("variable"="index"))}
      }

    }

    if (!is.null(vars.quali.supp)) {

      #- informations sur les modalites des variables actives
      mod_acti <- modalites %>% filter(type.var=="acti")
      infos <- c("coord","contrib","cos2","v.test","eta2")
      for (i in infos) {
        temp <- as.data.frame(round(res.ACM$var[[i]], 2))  %>%
          rename_all(tolower) %>% rename_all(~ gsub(" ", "", .)) %>%
          rename_all(~ paste(., i, sep = "_")) %>% mutate(index = rownames(.))
        if (i!="eta2") {mod_acti <- mod_acti %>% left_join(temp,by="index")}
        else {mod_acti <- mod_acti %>% left_join(temp,by=c("variable"="index"))}
      }

      #- informations sur les modalites des variables supplementaires
      mod_supp <- modalites %>% filter(type.var=="supp")
      infos <- c("coord","cos2","v.test","eta2")
      for (i in infos) {
        temp <- as.data.frame(round(res.ACM$quali.sup[[i]], 2))  %>%
          rename_all(tolower) %>% rename_all(~ gsub(" ", "",.)) %>%
          rename_all(~ paste(., i, sep = "_")) %>% mutate(index = rownames(.))
        if (i!="eta2") {mod_supp <- mod_supp %>% left_join(temp,by="index")}
        else {mod_supp <- mod_supp %>% left_join(temp,by=c("variable"="index"))}
      }

      #- assembler ces deux dataframes "mod_acti" et "mod_supp" pour reconstituer "modalites"
      modalites <- mod_acti %>% bind_rows(mod_supp)
      rm(mod_acti, mod_supp)

    }

  # Inserer d'autres elements sur l'ACM : nb de modalites, eigen values ----

  nb_mod_actives <- nrow(res.ACM$var$contrib)
  modalites <- modalites %>%
    mutate(ACM.nb.mod.acti=nb_mod_actives)

  eig <- round(res.ACM$eig[,2],1)
    if(length(eig>5)){eig <- eig[1:5]}
    for (i in 1:length(eig)) {
      modalites[[paste0("ACM.dim",i,"_eig")]] <- eig[[i]]
    }


    # Verification de la coherence des objets d.ACM <=> res.ACM ----

  temp.mod <- unique(modalites[modalites$type.var=="acti",][["index"]])
  temp.res <- rownames(res.ACM$var$coord)
  if(length(setdiff(temp.mod,temp.res))>0) {
    message("Some indexes do not match beetwen d.ACM and res.ACM. There must be a mistake, unless res.acm is a specific MCA.")
  }

  if (!is.null(vars.quali.supp)) {
    temp <- unique(modalites[modalites$type.var=="supp",][["variable"]])
    if(length(setdiff(temp,vars.quali.supp)>0)) {
      stop("There seems to be a problem with categorical supplementary variables, vars.quali.supp")
    }
  }

    # Première ebauche de plan avec ggplot ----

    ## sans variables supplémentaires ====

  if (is.null(vars.quali.supp)) {

    code <- "modalites %>%
  ggplot(aes(x=dim1_coord, y=dim2_coord)) +
  geom_hline(yintercept=0, color=\"darkgrey\", linetype=\"longdash\") +
  geom_vline(xintercept=0, color=\"darkgrey\", linetype=\"longdash\") +
  geom_point(aes(color=dim1_contrib, size=part)) +
  geom_text(aes(label=index,size=part),hjust=-.1) +
  scale_color_gradient(name=\"Contribution des modalités à la construction de l'Axe 1\",
                       low=\"grey80\",high=\"grey20\") +
  scale_size_continuous(name=\"Part de la modalité parmi l'ensemble des effectifs (%)\") +
  labs(title=\"Plan factoriel - Axes 1 et 2\",
       x=paste0(\"Axe 1 (\",unique(modalites$ACM.dim1_eig),\" %)\"),
       y=paste0(\"Axe 2 (\",unique(modalites$ACM.dim2_eig),\" %)\")) +
  theme_minimal() +
  theme(legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.position=\"bottom\", legend.box=\"vertical\")"
  }

    ## avec variables supplémentaires ====

  if (!is.null(vars.quali.supp)) {

    code <- "modalites %>%
  ggplot(aes(x=dim1_coord, y=dim2_coord)) +
  geom_hline(yintercept=0, color=\"darkgrey\", linetype=\"longdash\") +
  geom_vline(xintercept=0, color=\"darkgrey\", linetype=\"longdash\") +
  geom_point(aes(color=type.var, shape=type.var, size=part)) +
  geom_text(aes(label=index, color=type.var, size=part),hjust=-.1) +
  scale_color_manual(name=\"Type de variable\",values=c(\"black\",\"grey40\")) +
  scale_shape_manual(name=\"Type de variable\",values=c(1,3)) +
  scale_size_continuous(name=\"Part de la modalité parmi l'ensemble des effectifs (%)\",
                        guide=guide_legend(override.aes=list(shape=1))) +
  labs(title=\"Plan factoriel - Axes 1 et 2\",
       subtitle=\"Modalites des variables actives et supplementaires\",
       x=paste0(\"Axe 1 (\",unique(modalites$ACM.dim1_eig),\" %)\"),
       y=paste0(\"Axe 2 (\",unique(modalites$ACM.dim2_eig),\" %)\")) +
  theme_minimal() +
  theme(legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.position=\"bottom\", legend.box=\"vertical\")"
  }

  if(code.ex==T) {cat(code)}
  return(modalites)

}
