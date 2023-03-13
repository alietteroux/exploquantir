#' Return a *gt* table displaying contingency analysis results (from two categorical variables of a dataframe)
#'
#' For more details : the Github repository at [https://github.com/alietteroux/exploquantir](https://github.com/alietteroux/exploquantir)
#'
#' @param data a dataframe with *n* rows (individuals) and *p* columns (including categorical variables). If *weight* is given, this dataframe must also contain a numeric variable indicating row weights. To avoid misunderstandings, categorical variables must not contain any NA value
#' @param var1 colname of the independent variable
#' @param var2 colname of the dependent variable
#' @param weight an optional numeric column of *data* indicating row weights (if weight is informed, only option=0 is allowed)
#' @param option indicates which table to display : *0* (row percentages), *1* (residuals and Chi2 contributions) or *2* (row percentages including some contingency analysis results). Only 0 is allowed for weighted data.
#' @param palette indicates which palette to use for "option=1" : "GreenRed" or "RedGreen"
#' @param title title to display
#' @param subtitle subtitle to display
#' @param var1.lib *var1*'s name to display
#' @param var2.lib *var2*'s name to display
#' @param source *source* to display
#' @param lecture only available if option=0 : an example of how to read values in the table
#'
#' @return a *gt* table : residuals and Chi2 contributions if *option==1* ; row percentages including some contingency analysis results if *option==2*
#'
#' @import dplyr
#' @import gt
#' @importFrom stats chisq.test
#' @importFrom questionr cramer.v lprop wtd.table
#' @importFrom scales col_numeric
#'
#' @export

actab <- function (data,var1,var2,weight,option,palette,
                   title,subtitle,var1.lib,var2.lib,
                   source,lecture){

  # Verification et formattage des donnees et arguments ----

  ## Traiter les arguments quand ils ne sont pas renseignes ====

  if(missing(weight)){weight <- NULL}
  if(missing(palette)){palette <- "GreenRed"}
  if(missing(var1.lib)){var1.lib <- var1}
  if(missing(var2.lib)){var2.lib <- var2}
  if(missing(option)){option <- 0}
  if(missing(title)){
    if(option==0){
      title <- "Pourcentages en lignes"
    }
    if(option==1){
      title <- "Ecarts a l'independance et contributions au Chi2"
    }
    if(option==2){
      title <- "Pourcentages en lignes (+ contrib. Chi2)"
    }
  }
  if(missing(subtitle)){subtitle <- paste0("Analyse de contingence : ",var1," | ",var2)}
  if(missing(source)){source <- "Source : a preciser"} else {source <- paste0("Source : ",source)}

  ## Verification de la nature des variables ====

  x <- sapply(data[c(var1,var2)], class)
  if ("numeric" %in% x |"integer" %in% x){
    stop("at least one column is numeric or integer")
  }

  data <- droplevels(data)

  ## Verification de l'absence des NA ====

  vars_NA <- colnames (data[c(var1,var2)] %>% select_if (~sum (is.na(.)) > 0 ))
  if(length(vars_NA)>0){
    stop("at least one column contains NA")
  }

  ## Verification du nombre de modalites par variable ====

  nb.levels <- c(length(unique(data[[var1]])),
                 length(unique(data[[var2]])))
  if (0 %in% nb.levels | 1 %in% nb.levels){
    stop("at least one column contains less than two levels")
  }

  ## Verification de la contrainte option=0 sur les donnees ponderees ====

  if(!is.null(weight) & option!=0){
    stop("weight has been informed : only option=0 is allowed")
  }

  # Operations preliminaires communes aux differentes options ----

  ## Donnees non ponderees ====

  if(is.null(weight)){

    # realisation du tableau croise
    tc <- table(data[[var1]],data[[var2]])

    if(option!=0){
      # realisation de l'analyse de contingence proprement dite
      ac <- chisq.test(tc)
      # ecarts a l'independance
      ecarts <- ac$observed-ac$expected
      # contributions au Chi2
      cont <- (ac$observed-ac$expected)^2/ac$expected
      # contributions au Chi2 en %
      cont <- cont/sum(cont)*100
    }
  }

  ## Donnees ponderees ====

  if(!is.null(weight)){

    # realisation du tableau croise pour % en lignes (option==0)
    tc <- wtd.table(data[[var1]],data[[var2]],weights=data[[weight]])

    }

  ## Objets utiles pour les tableaux ====

  # colonnes constituant le corps du tableau
  if(is.factor(data[[var2]])) {
    col.corps <- levels(data[[var2]])
  }
  if(is.character(data[[var2]])) {
    col.corps <- unique(data[[var2]])
  }

  if(option!=0){
    # V de Cramer
    vcramer <- cramer.v(tc)
  }

  # Option 0 : "pourcentages en lignes" ----

  if(option==0){

    if(is.null(weight)) {
      # pourcentages en lignes avec une ligne "total" et une colonne "Effectifs"
      res_nb <- as.data.frame.matrix(table(data[[var1]],data[[var2]]))
      res_nb <- rbind(res_nb, total = c(colSums(res_nb)))
      res_p <- as.data.frame.matrix(lprop(table(data[[var1]],data[[var2]]))) %>%
        mutate(Effectifs= apply(res_nb,1,sum)) %>%
        mutate(Effectifs = str_c(format(Effectifs, big.mark=" "),
                                 " (",round(Effectifs/nrow(data)*100,0),"%)")) %>%
        rownames_to_column(var=var1.lib)
    }

    if(!is.null(weight)) {
      # pourcentages en lignes avec une ligne "total" et une colonne "Effectifs"
      res_nb <- as.data.frame.matrix(wtd.table(data[[var1]],data[[var2]],weights=data[[weight]]))
      res_nb <- rbind(res_nb, total = c(colSums(res_nb)))
      tot.pond <- sum(data[[weight]])
      res_p <- as.data.frame.matrix(lprop(wtd.table(data[[var1]],data[[var2]],weights=data[[weight]]))) %>%
        mutate(Effectifs = apply(res_nb,1,sum)) %>%
        mutate(Effectifs = str_c(format(round(Effectifs,0), big.mark=" "),
                                 " (",round(Effectifs/tot.pond*100,0),"%)")) %>%
        rownames_to_column(var=var1.lib)
    }

    # mise en page tab option 0
    tab <- res_p %>%
      gt(rowname_col=var1.lib) %>%
      tab_stubhead(label=var1.lib) %>%
      tab_header (title=md(paste0("**",title,"**")),
                  subtitle=subtitle) %>%
      tab_spanner(label=var2.lib, columns=col.corps) %>%
      tab_source_note(source_note="La valeur maximale de chaque colonne est marquée en gras.")

    if (!missing(lecture)) {
      tab <- tab %>% tab_source_note(source_note = lecture)
    }

    tab <- tab %>%
      tab_source_note(source_note = source) %>%
      fmt_number(columns=col.corps,decimals = 1) %>%
      cols_align(align="center", columns=col.corps) %>%
      tab_options(footnotes.font.size=10, source_notes.font.size=11) %>%
      tab_style (style = list(cell_fill(color="grey85"),cell_text(weight = "bold")),
                 locations=list(cells_stub(rows=nrow(res_p)),cells_body(rows=nrow(res_p)))) %>%
      tab_style(style=cell_text(style="italic"),
                locations=list(cells_column_spanners(),
                               cells_column_labels(columns=all_of(col.corps))))

    # mise en gras du maximum de chaque colonne
    for (i in col.corps){
      tab <- tab_style(tab, style = list(cell_text(weight = "bold")),
                       locations = cells_body(columns=i, rows = res_p[,i]==max(res_p[,i])))
    }
  }


  # Option 1 : "ecarts et contributions" ----

  if(option==1){

    if(palette=="GreenRed") {
      pal <- c("#196f3d","#229954", "#fef9e7","#f1948a","#cb4335")
    }
    if(palette=="RedGreen") {
      pal <- c("#cb4335","#f1948a","#fef9e7","#229954","#196f3d")
    }

    # tableau gardant :
    #-- le signe des ecarts a l'independance
    #-- la valeur des contributions (en %) au Chi2
    res_ac <- cont/sum(cont)*100*(ecarts/abs(ecarts))
    res_ac <- as.data.frame.matrix(res_ac)
    res_ac <- res_ac %>% rownames_to_column(var=var1.lib)

    # tableau option 1
    tab <- res_ac %>%
      gt (rowname_col=var1.lib) %>%
      tab_stubhead (label=var1.lib) %>%
      tab_header (title = md(paste0("**",title,"**")),
                  subtitle=subtitle)  %>%
      tab_spanner(label=var2.lib, columns=col.corps) %>%
      tab_source_note (source_note = md("**Le signe (positif ou negatif) indique respectivement des sur-representations ou sous-representations. La valeur absolue est la contribution (en %) au Chi2-total.**")) %>%
      tab_source_note(source_note=paste0("Pour cette relation, le V de Cramer s'eleve a ",
                                         round(vcramer,2), sep="")) %>%
      tab_source_note (source_note = source)  %>%
      fmt_number (columns=all_of(col.corps),decimals=1) %>%
      cols_align (align="center", columns=all_of(col.corps)) %>%
      tab_options(footnotes.font.size=10, source_notes.font.size=11) %>%
      data_color(
        columns=all_of(col.corps),
        fn=col_numeric(palette = pal,domain = range(res_ac[all_of(col.corps)]))
      ) %>%
      tab_style(style=cell_text(style="italic"),
                locations=list(cells_column_spanners(),
                               cells_column_labels(columns=all_of(col.corps))))
  }

  # Option 2 : "pourcentages en lignes (+ contrib. Chi2)" ----

  if(option==2){

    # pourcentages en lignes avec une ligne "total" et une colonne "Effectifs"
    res_nb <- as.data.frame.matrix(table(data[[var1]],data[[var2]]))
    res_nb <- rbind(res_nb, total = c(colSums(res_nb)))
    res_p <- as.data.frame.matrix(lprop(table(data[[var1]],data[[var2]]))) %>%
      mutate(Effectifs= apply(res_nb,1,sum)) %>%
      mutate(Effectifs = str_c(as.character(Effectifs),
                               " (",round(Effectifs/nrow(data)*100,0),"%)")) %>%
      rownames_to_column(var=var1.lib)

    # contributions "extrêmes" : sup de 0.5 ecart-type a la moyenne des contributions
    cont_extr <- mean(cont) + (sd(cont)/2)

    cont.ext.pos <- as.data.frame.matrix(which(cont>=cont_extr & ecarts>=0, arr.ind=TRUE))
    cont.ext.pos$col <- cont.ext.pos$col+1
    cont.ext.neg <- as.data.frame.matrix(which(cont>=cont_extr & ecarts<0, arr.ind=TRUE))
    cont.ext.neg$col <- cont.ext.neg$col+1

    # note
    note <- md("Les cases surlignees en gris renvoient aux modalites qui tendent le plus a \"s'attirer\" (*ecarts a la situation d'independance positifs, contributions au Chi2 superieures a la moyenne + 0.5 ecart-type des contributions*) ; les cases dont la valeur est en italique et en gris renvoient aux modalites qui tendent le plus a \"se repousser\" (*ecarts a la situation d'independance negatifs, contributions au Chi2 superieures a la moyenne + 0.5 ecart-type des contributions*)")

    # tableau option 2
    tab <- res_p %>%
      gt(rowname_col=var1.lib) %>%
      tab_stubhead(label=var1.lib) %>%
      tab_header (title=md(paste0("**",title,"**")),
                  subtitle=subtitle) %>%
      tab_spanner(label=var2.lib, columns=col.corps) %>%
      tab_source_note(source_note = note) %>%
      tab_source_note(source_note=paste0("Pour cette relation, le V de Cramer s'eleve a ",
                                         round(vcramer,2), sep="")) %>%
      tab_source_note(source_note = source) %>%
      fmt_number(columns=col.corps,decimals = 1) %>%
      cols_align(align="center", columns=col.corps) %>%
      tab_options(footnotes.font.size=10, source_notes.font.size=11) %>%
      tab_style (style = list(cell_fill(color="grey85"),cell_text(weight = "bold")),
                 locations=list(cells_stub(rows=nrow(res_p)),cells_body(rows=nrow(res_p)))) %>%
      tab_style(style=cell_text(style="italic"),
                locations=list(cells_column_spanners(),
                               cells_column_labels(columns=all_of(col.corps))))

    col.corps.num <- which(colnames(res_p) %in% col.corps)
    for (i in col.corps.num){
      tab <- tab_style(tab,
                       style=list(cell_fill(color="grey90")),
                       locations=cells_body(columns=i,
                                            rows=cont.ext.pos[cont.ext.pos$col==i,"row"]))
      tab <- tab_style(tab,
                       style=list(cell_text(style="italic",color="grey60")),
                       locations=cells_body(columns=i,
                                            rows=cont.ext.neg[cont.ext.neg$col==i,"row"]))
    }

  }

  # Resultats ----

  return(tab)

}
