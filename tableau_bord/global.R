#########################################
#LE BILAN DEMOGRAPHIQUE, à l'année n-1  #
# Par Myriam YAHYAOUI, Insee DPS        #
# global.R file                         #
#########################################

# PACKAGES #####################################################################

#Application SHINY
library(bs4Dash)
library(fresh)
library(shinyWidgets)
library(shinyjs)

#Pour utiliser ce package en tant qu'agent Insee : devtools::install_github("InseeFr/R-Insee-Data")
library(insee)

#Pour la manipulation des jeux de données
library(dplyr)
library(tidyr)
library(data.table)

#Pour la manipulation de texte
library(stringr)

#Graphique
library(ggplot2)
library(scales)
library(ggtext)
library(plotly)

#Tableau
library(DT)

# DATA: API Insee.fr  ##########################################################

# Estimations Localisées de la Population --------------------------------------
#Recensement de la population - exploitation principale

list_idbank_elp = 
  get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |> 
  filter(AGE == "00-") |> 
  filter(SEXE %in% c (1, 2, 0) ) |>
  pull(idbank) 

elp = 
  get_insee_idbank(list_idbank_elp) |>
  separate(TITLE_FR, into = c("INDIC_DEMO", "SEXE", "TERRITOIRE"), sep = " - ") |>
  mutate(TERRITOIRE = case_when(grepl("Ville de Paris", TERRITOIRE) ~ "Paris", 
                                TERRITOIRE == "France (inclus Mayotte depuis 2014)" ~ "France",
                                TRUE ~ TERRITOIRE ) ) |>
  select(LAST_UPDATE, TIME_PERIOD, TERRITOIRE, SEXE, INDIC_DEMO, OBS_VALUE, OBS_STATUS) |>
  rename("MaJ_population" = LAST_UPDATE,
         "STATUT_INFO_POP" = OBS_STATUS) |>
  pivot_wider(names_from = SEXE, values_from = OBS_VALUE)

elp

# Naissances et Décès domiciliés -----------------------------------------------
#Statistiques d'Etat-Civil

list_idbank_Naissances_Décès = 
  get_idbank_list("DECES-MORTALITE", "NAISSANCES-FECONDITE") |>
  filter(FREQ == "A") |>
  filter(DEMOGRAPHIE %in% c("NAISS-DOM", "DECES-DOM")) |> 
  pull(idbank)

Naissances_Décès_ec = 
  get_insee_idbank(list_idbank_Naissances_Décès)

naiss = Naissances_Décès_ec |>
  filter( grepl("Naissances", TITLE_FR) ) |>
  rename("MaJ_naissances" = LAST_UPDATE,
         "STATUT_INFO_NAISSANCES" = OBS_STATUS
         )

deces = Naissances_Décès_ec |>
  filter( grepl("Décès", TITLE_FR) ) |>
  rename("MaJ_deces" =  LAST_UPDATE,
         "STATUT_INFO_DECES" = OBS_STATUS)

dat_list = list(naiss = naiss, deces = deces)

for (name in names(dat_list)) {
    setDT(dat_list[[name]]) 
  
  dat_list[[name]][, TERRITOIRE := case_when(grepl("Ville de Paris", TITLE_FR) ~ "Paris",
                                             grepl("France \\(", TITLE_FR) ~ "France", 
                                             TRUE ~ sub("^[^-]*-\\s*", "", TITLE_FR) |> trimws() ) 
                     ] 
    
    dat_list[[name]][, INDIC_DEMO := str_extract(TITLE_FR, "^\\S+\\s+\\S+\\s+\\S+") |> 
                       trimws()
                     ]
    
    dat_list[[name]] <- dat_list[[name]][ !grepl("Autres :", TITLE_FR) ]
    
    dat_list[[name]] <- dat_list[[name]][!grepl("Départements d'outre-mer (inclus Mayotte à partir de 2014)", 
                                                TITLE_FR)]
    
    dat_list[[name]] <- dat_list[[name]] |> pivot_wider(names_from = INDIC_DEMO, 
                                                        values_from = OBS_VALUE)
    
    }

#Vérification des créations de variable 

lapply(dat_list, colnames)

# TAB COMPLETE : Stock et mouvements naturels ----------------------------------

tab_stock_mouvnat = purrr::reduce(
  list( elp |>
          select(-INDIC_DEMO)
        , 
        dat_list[["naiss"]] |>
          select(MaJ_naissances, 
                 TIME_PERIOD, 
                 TERRITOIRE, 
                 `Naissances domiciliées par`,
                 STATUT_INFO_NAISSANCES)
        , 
        dat_list[["deces"]] |>
          select(MaJ_deces, 
                 TIME_PERIOD, 
                 TERRITOIRE, 
                 `Décès domiciliés par`,
                 STATUT_INFO_DECES)
        ),
  ~ left_join(.x, .y, by = c("TIME_PERIOD", "TERRITOIRE"))
  )

tab_stock_mouvnat_travail =  tab_stock_mouvnat |>
  mutate(TIME_PERIOD = as.character(TIME_PERIOD) |> as.integer() ) |>
  rename("Naissances" = `Naissances domiciliées par`,
         "Décès" = `Décès domiciliés par` ) |>
  filter(TIME_PERIOD > 1999)

# Indice de structure ----------------------------------------------------------

RM = tab_stock_mouvnat_travail |>
  mutate( rm = (Hommes/Femmes)*100 )

list_idbank_elp = 
  get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |>
  filter(AGE %in% c("00-24", "25-59", "60-") ) |>
  pull(idbank) 

GRPS_AGES = 
  get_insee_idbank(list_idbank_elp) |>
  rename("MaJ_population" = LAST_UPDATE) |>
  separate(TITLE_FR, into = c("INDIC_DEMO", "GRPS_AGES", "TERRITOIRE"), sep = " - ")

indice_structure = merge(x = RM |>
                           select(TIME_PERIOD, TERRITOIRE, Femmes, Hommes, rm),
                         
                         y = GRPS_AGES |>
                           select(TIME_PERIOD, TERRITOIRE, GRPS_AGES, OBS_VALUE) |>
                           pivot_wider(names_from = GRPS_AGES, values_from = OBS_VALUE),
                         
                         by = c("TIME_PERIOD", "TERRITOIRE")) |>
  rename("Rapport de masculinité" = rm) |>
  mutate(`Indice de jeunesse` = (`Part des 0-24 ans` / `Part des 60 ans ou plus` ) *100,
         `Indice de jeunesse` = round(`Indice de jeunesse`),
         across(c(5:8), ~ round(.x, 1) |> prettyNum(decimal.mark = ",") ),
         across(c(3:4), ~ round(.x, 0) |> prettyNum(big.mark = " ") )
         ) |> 
  rename_with(~ paste0(., " dans l'ensemble de la population"), 
              .cols = starts_with("Part des "))

  
# L'équation du bilan démographique en volume et en masse
#La notion de durée n’a pas de pertinence ici, car nous travaillons sur des pas de temps d’un an.
#Chaque valeur est déjà exprimée sous forme annualisée, 
#ce qui signifie qu’elle correspond à une évolution ou un volume calculé pour une année complète.

tab_calcul = tab_stock_mouvnat_travail |>
  select(-c(Femmes, Hommes)) |>
  arrange(TIME_PERIOD) |>
  arrange(TERRITOIRE) |>
  group_by(TERRITOIRE) |>
  mutate( duree = (lead(TIME_PERIOD) - TIME_PERIOD),
          PERIODE = paste(TIME_PERIOD, lead(TIME_PERIOD), sep = "-"),
          
          popmoy = (lead(Ensemble) + Ensemble) / 2,
          
          sd = lead(Ensemble) - Ensemble,
          tat = ( sd / popmoy ) %>% {.*1000},
          evol_ann_moy = (((lead(Ensemble) / Ensemble) ^ (1/duree) - 1) * 1000),
          evol_ann_moy_naiss = (((lead(Naissances) / Naissances)^(1/duree) - 1) * 1000),
          evol_ann_moy_deces = (((lead(Décès) / Décès)^(1/duree) - 1) * 1000),
          
          tbn = ( (Naissances/duree) / popmoy ) %>% {.*1000},
          tbm = ( (Décès/duree) / popmoy ) %>% {.*1000},
          tan = tbn - tbm,
          sn = Naissances - Décès,
          
          sm = sd - sn,
          tam = tat - tan ) |>
  ungroup()

# TAT sur période plus longue et Evolution annuelle moyenne --------------------
#De la population --------------------------------------------------------------
#' Calcul du taux d'accroissement total (TAT) et de l'évolution moyenne de la population, Naissancesances et des décès
#'
#' Cette fonction permet de calculer le TAT ainsi que l'évolution
#' annuelle moyenne de la population pour chaque territoire (`TERRITOIRE`) sur les années spécifiées.
#'
#' @param data Un data frame contenant au moins les colonnes `TIME_PERIOD`, `TERRITOIRE` et `Ensemble`
#' @param TIME_PERIODs Un vecteur d'années (numérique ou entier) à inclure dans l'analyse
#'
#' @return Un data frame contenant les TATs et les évolutions annuelles moyennes
#' @export
#'
#' @examples
#' df <- data.frame(TIME_PERIOD = c(2000, 2005, 2010, 2000, 2005, 2010),
#'                  TERRITOIRE = c("A", "A", "A", "B", "B", "B"),
#'                  Ensemble = c(1000, 1200, 1400, 500, 600, 750))
#' calcul_tat_evolpop(df, TIME_PERIODs = c(2000, 2005, 2010))
calcul_tat_evolpop <- function(data, TIME_PERIOD_depart, TIME_PERIOD_arrivee) {
  data |>
    select(-c(Femmes, Hommes)) |>
    filter(TIME_PERIOD %in% c(TIME_PERIOD_depart, TIME_PERIOD_arrivee)) |>
    arrange(TIME_PERIOD) |>
    arrange(TERRITOIRE) |>
    group_by(TERRITOIRE) |>
    mutate( 
      duree = lead(TIME_PERIOD) - TIME_PERIOD,
      PERIODE = paste(TIME_PERIOD, lead(TIME_PERIOD), sep = "-"),
      popmoy = (lead(Ensemble) + Ensemble) / 2,
      sd = lead(Ensemble) - Ensemble,
      sd_annuel_moy  = sd / duree,
      tat = (sd_annuel_moy / popmoy) * 1000,
      evol_ann_moy = (((lead(Ensemble) / Ensemble)^(1/duree) - 1) * 1000),
      evol_ann_moy_naiss = (((lead(Naissances) / Naissances)^(1/duree) - 1) * 1000),
      evol_ann_moy_deces = (((lead(Décès) / Décès)^(1/duree) - 1) * 1000)
    ) |>
    filter(!is.na(duree)) |>
    select(-c(Naissances, Décès)) |>
    left_join(
      data |>
        filter(between(TIME_PERIOD, TIME_PERIOD_depart, TIME_PERIOD_arrivee-1)) |>
        group_by(TERRITOIRE) |>
        summarise(naiss_cumulees = sum(Naissances, na.rm = TRUE),
                  deces_cumules = sum(Décès, na.rm = TRUE),
        ),
      by = "TERRITOIRE"
    ) |>
    mutate(
      naiss_annuel_moy = naiss_cumulees / duree,
      deces_annuel_moy = deces_cumules / duree,
      tbn = ( (naiss_annuel_moy) / popmoy) * 1000, 
      tbm = ( (deces_annuel_moy) / popmoy) * 1000,
      tan = tbn - tbm,
      tam = tat - tan
    ) }

# Retravaillez les étiquettes des indicateurs
INDIC_LABELS <- c(
  "Ensemble" = "Population au 1er janvier", 
  "Femmes" = "Population féminine au 1er janvier", 
  "Hommes" = "Population masculine au 1er janvier", 
  "popmoy" = "Population moyenne",
  "duree" = "Durée",
  
  "Naissances" = "Naissances domiciliées", 
  "naiss_cumulees" =  "Naissances dom. sur la période",
  "naiss_annuel_moy" = "Naissances dom. annuelles moyennes",
  "evol_ann_moy_naiss" = "Évolution annuelle moyenne des naissances dom. (accr. relatif)",
  "tbn" = "Taux Brut de Natalité ( ‰)", 
  
  "Décès" = "Décès domiciliés",
  "deces_cumules" =  "Décès dom. sur la période",
  "deces_annuel_moy" = "Décès dom. annuels moyens",
  "evol_ann_moy_deces" = "Évolution annuelle moyenne des décès dom. (accr. relatif)",
  "tbm" = "Taux Brut de Mortalité ( ‰)",
  
  "sn" = "Accroissement Naturel sur la période",
  "tan" = "Taux d'Accroissement Naturel ( ‰)",
  
  "sm" = "Accroissement Migratoire sur la période", 
  "tam" = "Taux d'Accroissement Migratoire ( ‰)",
  
  "sd" = "Accroissement Total sur la période",
  "sd_annuel_moy" = "Accroissement Total annuel moyen",
  "tat" = "Taux d'Accroissement Total ( ‰)",
  "evol_ann_moy" = "Évolution annuelle moyenne (accr. relatif)" )

# En colonne : TIME_PERIOD, PERIODE, TERRITOIRE, INDIC_DEMO, OBS_VALUE
tab_calcul_long = tab_calcul |> 
  pivot_longer(-c(MaJ_population, MaJ_naissances, MaJ_deces,  
                  STATUT_INFO_DECES,  STATUT_INFO_NAISSANCES,  STATUT_INFO_POP,
                  TIME_PERIOD, PERIODE, TERRITOIRE), 
               names_to = "INDIC_DEMO", values_to = "OBS_VALUE") |>
  mutate(INDIC_DEMO_LABEL = factor(recode(INDIC_DEMO, !!!INDIC_LABELS), 
                                   levels = INDIC_LABELS))

#' Générateur de couleurs personnalisées
#'
#' Cette fonction génère un vecteur de couleurs de longueur `n`. Si `n` est inférieur ou égal à 4, 
#' elle renvoie une sélection de couleurs fixes. Pour `n > 4`, elle génère une palette étendue 
#' à l'aide de `colorRampPalette` et des palettes de RColorBrewer.
#'
#' @param n Un entier positif. Le nombre de couleurs à générer.
#'
#' @return Un vecteur de chaînes de caractères représentant des couleurs au format hexadécimal.
#' 
#' @export
#'
#' @examples
#' # Générer 3 couleurs fixes
#' generate_colors(3)
#'
#' # Générer 6 couleurs avec une palette dynamique
#' generate_colors(6)
#'
#' # Utiliser les couleurs dans un graphique
#' barplot(1:6, col = generate_colors(6))
generate_colors <- function(n) {
  # Cas où n <= 4
  if (n <= 4) {
    colors <- c("#136377", "#a1bea0", "#b80c0c", "#e3de27")[1:n]
  } else {
    # Cas où n > 4, avec palette étendue
    colors <- c(colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n))
  }
  
  return(colors)
}
#' Configurer un graphique Plotly avec des paramètres standard
#'
#' Cette fonction applique une configuration commune aux graphiques Plotly, incluant
#' les options de téléchargement, l'affichage de la barre d'outils en français, et
#' d'autres paramètres esthétiques.
#'
#' @param p Un objet `plotly` à configurer.
#'
#' @return Un objet `plotly` modifié avec les configurations appliquées.
#' @export
#'
#' @examples
#' library(plotly)
#' p <- plot_ly(x = ~rnorm(100), y = ~rnorm(100), type = "scatter", mode = "markers")
#' p_configuré <- configurer_plotly(p)
#' p_configuré
config_plotly <- function(p) {
  p <- p |>
    layout(
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#F0F0F0",
      hovermode = "x unified",
      title = list(font = list(color = "#112446"), y=1.25, x = 0),
      showlegend = TRUE,
      legend = list(bordercolor = "#000000",
                    borderwidth = 0.2,
                    font = list(size = 10, color = "black"),
                    bgcolor = "#f9f9f9",
                    title = list(font = list(size = 12, color = "black")),
                    traceorder = "normal"),
      
      xaxis = list(gridcolor = "rgba(89, 91, 99, 0.35)", 
                   tickfont = list(size = 10),
                   showline = TRUE, 
                   linecolor = "#1D3557", 
                   linewidth = 0.75,
                   tickangle = -65,
                   title = list(
                     font = list(size = 14, color = "#617a89"))),
      
      yaxis = list(title = list(font = list(size = 14, color = "#617a89") ),
                   showline = TRUE, 
                   tickangle = -45,
                   linecolor = "#1D3557", 
                   linewidth = 0.75,
                   gridcolor = "rgba(89, 91, 99, 0.35)",
                   tickfont = list(size = 9) ),
      autosize = T, 
      margin = list(l = 25, 
                    r = 25, 
                    b = 25, 
                    t = 35,  
                    pad = 5)
    )
  
  p <- plotly::config(
    p,
    toImageButtonOptions = list(
      format = "png",
      filename = "graphique_plotly",
      scale = 1
    ),
    locale = "fr",
    displayModeBar = TRUE,
    modeBarButtonsToAdd = list("toImage"),
    displaylogo = FALSE
  )
  
  return(p)}

#' Créer un tableau interactif avec DataTables
#'
#' Cette fonction génère un tableau interactif à partir d'un objet `data.frame`
#' ou `tibble`, avec des fonctionnalités avancées comme la recherche, la
#' pagination, l'exportation, et des colonnes fixes. Elle permet également de
#' personnaliser l'apparence du tableau.
#'
#' @param x Un objet de type `data.frame` ou `tibble` à afficher sous forme de tableau interactif.
#' @param rownames Un booléen indiquant si les noms des lignes doivent être affichés (par défaut `FALSE`).
#' @param dom_options Une chaîne de caractères spécifiant la disposition des éléments (par défaut `"Bfrtip"`).
#' @param options Une liste d'options supplémentaires pour personnaliser le tableau (par défaut, vide).
#' @param filter_position Position du filtre dans le tableau, soit `"top"`, `"bottom"`, ou `"none"` (par défaut `"top"`).
#'
#' 
#' @return Un tableau interactif avec les fonctionnalités de DataTables, prêt à être affiché dans un document RMarkdown, un Shiny, ou un script interactif.
#' 
#' @export
#'
#' @examples
#' # Exemple d'utilisation
#' create_dt(mtcars)

create_dt <- function(x, 
                      dom_options = "Bfrtip", 
                      options = list(),
                      filter_position = "top",
                      escape = FALSE,
                      selection = "none") {
  library(DT)
  
  # Options par défaut
  default_options <- list(
    scrollY = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE,
    
    columnDefs = list(
      list(className = 'dt-center', targets = "_all")
    ),
    dom = dom_options,
    buttons = list(
      list(extend = "copy", text = "Copier"),
      list(
        extend = "excel",
        text = "Exporter Excel",
        filename = "data",
        exportOptions = list(modifier = list(page = "all")) # Exporter toutes les données
      ),
      list(
        extend = "pdf",
        text = "Exporter PDF",
        exportOptions = list(modifier = list(page = "all")) # Exporter toutes les données
      )
    ),
    pageLength = 5,
    lengthMenu = list(
      c(5, 10, 25, 50, -1), 
      c("5", "10", "25", "50", "Tout") 
    ),
    paging = TRUE,
    searching = TRUE,
    language = list(
      search = "Rechercher :",
      lengthMenu = "Afficher _MENU_ lignes par page",
      info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
      paginate = list(
        previous = "Précédent",
        `next` = "Suivant"
      )
    ),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '13px', 'padding': '0.75px'});",
      "$(this.api().tables().body()).css({'font-size': '12px'});",
      "$('thead th').css({'font-size': '13px', 'font-family': 'Open sans'});",  
      "$('tbody td').css({'font-size': '12px', 'font-family': 'Open sans'});", 
      "$(this.api().table().container()).find('.dataTables_paginate').css({'font-size': '13px', 'padding': '2px'});",
      "$(this.api().table().container()).find('.dataTables_info').css({'font-size': '12px', 'font-family': 'Open sans'});",
      "$(this.api().table().container()).find('.dataTables_filter input').css({",
      "'font-size': '12px',",  
      "'border': '2px solid #4CAF50',",
      "'padding': '0.75px',",    
      "'width': '250px',",   
      "'border-radius': '4px' 
      });",  
      "}"
    )
  )
  
  # Fusion des options spécifiques avec les options par défaut
  merged_options <- modifyList(default_options, options)
  
  datatable(
    x,
    class = 'cell-border stripe',
    rownames = rownames,
    escape = escape,
    filter = list(position = filter_position),
    callback = JS('
      $(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" ); 
       $("thead input[type=\'search\']").css({
      "font-size": "13px", // Taille du texte des filtres par colonne
      "border": "0.75px solid #4CAF50", // Ajouter une bordure
      "padding": "2px",
    });'),
    extensions = c("Buttons", "FixedColumns"),
    options = merged_options
  )}

# SHINY ELEMENTS & DONNEES #####################################################
#Paramètres:
n = tab_stock_mouvnat$TIME_PERIOD |> as.character() |> as.integer() |> max()

mon_theme_package_fresh = create_theme(
  bs4dash_status(
    primary = "#5E81AC", 
    danger = "#BF616A",
    secondary = "#d8dbde",
  ),
  bs4dash_sidebar_light(
    bg = "#F1F5F9"
  ),
  bs4dash_layout(
    sidebar_width = "420px"
  ) )

# CHOIX INDICATEUR DEMOGRAPHIQUE
choix_indic_demo = list("En termes du volumes" = 
       c ("Naissances"="Naissances", 
          "Décès"="Décès",
          "Accroissement Naturel"="sn",
          "Accroissement Migratoire"="sm",
          "Accroissement Total"="sd" ),
     
     "En termes des nombres \nréduit à l'effectif \nde la population" = 
       c ("Taux Brut de Mortalité ( ‰)" = "tbm",
         "Taux Brut de Natalité ( ‰)" = "tbn",
         "Taux d'Accroissement Naturel ( ‰)" = "tan",
         "Taux d'Accroissement Migratoire ( ‰)" = "tam",
         "Taux d'Accroissement Total ( ‰)" = "tat" ) )



# BLABLA 
texte_explication_calcul = div(style = "padding: 3px; font-size: 12.5px;",
                               HTML("
      <p><strong>Pour comprendre comment on calcule la population d'une année à l'autre,
      il est important de distinguer deux concepts clés :</strong></p>
        <li><strong>Les stocks :</strong>
          <ul>
            <li>Ce sont les effectifs de population mesurés à un instant donné (par exemple, le 1er janvier 2018 ou 2021).</li>
            <li>Ils correspondent à une photographie de la population.</li>
          </ul>
        </li>
        <li><strong>Les mouvements :</strong>
          <ul>
            <li>Les naissances (ajoutent à la population).</li>
            <li>Les décès (retirent de la population).</li>
            <li>Les mouvements migratoires : immigrations (ajoutent) et émigrations (retirent).</li>
          </ul>
        </li>
      <em><strong>Attention :</em> les naissances et décès de l'année 2024 appartiennent au mouvement de l'intervalle 2024-2025.</strong>
      <p style='text-align: center; font-size: 18px;'>
      <code>P<sub>t</sub> = P<sub>t-1</sub> + N<sub>t-1,t</sub> - D<sub>t-1,t</sub> + I<sub>t-1,t</sub> - E<sub>t-1,t</sub></code>
      </p>
      <ul>
        <li><code>P<sub>t</sub></code> : Population au début de l'année <code>t</code>.</li>
        <li><code>P<sub>t-1</sub></code> : Population au début de l'année précédente.</li>
        <li><code>N<sub>t-1,t</sub></code> : Naissances entre <code>t-1</code> et <code>t</code> (mouvement).</li>
        <li><code>D<sub>t-1,t</sub></code> : Décès entre <code>t-1</code> et <code>t</code> (mouvement).</li>
        <li><code>I<sub>t-1,t</sub></code> et <code>E<sub>t-1,t</sub></code> : Immigrations et émigrations entre <code>t-1</code> et <code>t</code>.</li>
      </ul><br>
      <p><strong>Application sur la période de 2018 à 2021</strong></p>
      <ul>
        <li><strong>Stock initial et final :</strong>
          <ul>
            <li>Le stock initial correspond à la population de 2018 (<code>P<sub>2018</sub></code>).</li>
            <li>Le stock final correspond à la population de 2021 (<code>P<sub>2021</sub></code>).</li>
          </ul>
        </li>
        <li><strong>Mouvements cumulés :</strong>
          <ul>
            <li>Les naissances cumulées : On additionne les naissances de 2018, 2019 et 2020, puis on annualise cette somme pour obtenir un flux moyen annuel.</li>
            <li>Les décès cumulés : Même logique que pour les naissances, avec une annualisation des décès cumulés.</li>
            <li>Les mouvements migratoires sont également cumulés sur toute la période.</li>
          </ul>
        </li>
      </ul>
      <br>
      <p><strong>Hypothèse méthodologique :</strong></p>
      <p>Tous les <strong>taux démographiques</strong> sont exprimés en pour mille et sur une base annualisée. Cette approche repose sur une hypothèse forte : la répartition uniforme des événements dans le temps. Autrement dit, on suppose que les naissances, décès et migrations sont répartis <strong>de manière homogène</strong> tout au long de l'année.</p>
                                    ") )



