#########################################
#LE BILAN DEMOGRAPHIQUE, à l'année n-1  #
# Par Myriam YAHYAOUI, Insee DPS        #
# ui.R file                             #
#########################################

# Structure et Affichage de l'application --------------------------------------
dashboardPage(
  
  title = "TABLEAU_DE_BORD_CONJ_DEMO", 
  fullscreen = TRUE, 
  help = NULL,
  
  # En-tête de page ------------------------------------------------------------
  header =  dashboardHeader(
    title = dashboardBrand(title = "Bilan démographique de la France",
                           image = "img_bilan_demo.jpg"  ),
    border = TRUE, fixed = TRUE, status = "navy",
    tags$div(tags$a(style = "color: white;",
                    href = "https://github.com/Myriam-ctrl-lang/STATISTIQUE-HUMAINE", 
                    target = "_blank",
                    icon("github", lib = "font-awesome"),
                    onmouseover = "this.style.color='#ffc107'; this.style.textDecoration='underline';",
                    onmouseout = "this.style.color='white'; this.style.textDecoration='none';",
                    "Code source") ) ),
  
  # Barre latérale -------------------------------------------------------------
  sidebar = dashboardSidebar(skin = "light",
                             status = "warning", 
                             elevation = 4,  
                             collapsed = FALSE,
                             style = "font-size: 90%; transition: width 0.3s ease;",
                             
                             sidebarMenu(id = "sidebarmenu",
                                         
                                         sidebarHeader(title = HTML("
                                           <div class='sidebar-text'>
<p>Comme chaque année, l'Insee et l'Ined publie <br> un article sur l’évolution démographique récente <br>de la population de France.</p>
<p>Ce dernier est réalisé à partir <br> des Recensements de Population (RP), <br> des Estimations Localisées de Population (ELP), <br> et des statistiques et estimations d’État Civil (EC).</p>
<p>En janvier de chaque année n, l'Insee publie des estimations <br> 'précoces' arrêtées à fin n-1.</p>
<ul>
<li>La population au 1er janvier n-3 est recalculée avec <br> les derniers résultats.</li>
<li>Les populations nationales et régionales au 1er janvier <br> n-2 et n-1 sont mises à jour, mais restent provisoires.</li>
</ul>")),
                                         selectizeInput('zone', 
                                                        label = span("Niveau géographique", class='btn btn-warning btn-sm'), 
                                                        choices = unique(tab_stock_mouvnat_travail$TERRITOIRE ), 
                                                        selected = "Île-de-France",
                                                        width = "100%"),
                                         
                                         menuItem("OBSERVER ET MESURER LE MOUVEMENT DE POPULATION", 
                                                  icon = icon("chart-bar") )
                                         )
                             ),
  
  # Corps de l'application avec un thème personnalisé --------------------------
  body = dashboardBody(
    
    #Theme et css --------------------------------------------------------------
    use_theme(mon_theme_package_fresh),
    
    shinyjs::useShinyjs(), 
    
    tags$head( 
    tags$script(
    HTML(" 
    $(document).on('click', '.delete-row', function(event) {
    event.preventDefault();  // Empêche le comportement par défaut du lien
    var rowIndex = $(this).data('id');
    Shiny.setInputValue('delete_row', rowIndex);
    });")),
    
    tags$style(
    HTML("
    $(document).ready(function(){
    $('#toggle-info').click(function(){
    $('#sidebar-info').toggle(); }); });
    
    .sidebar-text {white-space: pre-line;}
    
    .custom-label {
    background-color: #ffc107; 
    color: #1f2d3d; 
    padding: 8px 12px; /* Espace intérieur */
    border-radius: 8px; /* Bords arrondis */
    display: inline-block; }
    
    .tab-buttons {
    display: flex; 
    flex-direction: column; 
    gap: 15px; 
    padding: 10px; 
    background-color: #f5f5f5; 
    border-radius: 8px; }
    
    .tab-buttons button {
    width: 100%; 
    text-align: left; 
    border-radius: 8px; 
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
    transition: all 0.3s ease;
    font-size: 1em;
    color: #333; }
    
    .tab-buttons button:hover {
    background-color: #dbeafe; 
    transform: scale(1.02); }
    
    .tab-buttons button.active {
    background-color: #5E81AC; 
    color: white; }
    
    .box-title {
    font-size: 1.2em;
    color: white; }
    
    .sidebar-panel {
    background-color: #f0f4f8;
    border-radius: 8px;
    padding: 15px;
         }"
         )
    ) 
    ),
    
    bs4TabCard(width = 12, 
               title = "",
               side = "left",
               tabPanel(title =  htmlOutput("message_pop"), 
                        value = "tab1",
                        
                        fluidRow(
                          
                          column(
                            width = 3,
                            
                            div(
                              style = 
                                "
                                height: auto; flex-grow: 1; width: 100%;
          display: flex; flex-direction: column; gap: 2px; 
          background-color: #f0f0f0; 
          border-radius: 10px; 
          padding: 5px; box-sizing: border-box;",
                              
                              div(
                                style = 
                                  "font-size: 18px; color: #D98C07; margin-bottom: 5px;",
                                "🔍 FILTRES"
                              ),
                              
                              div(
                                style = 
                                  "height: 1px; background-color: #D98C07; margin-bottom: 10px;"
                              ),
                              
                              selectInput('indicateur_demo_choix', 
                                          label = span("Les éléments de l'équation fondamental du mouvement de la population"), 
                                          choices = choix_indic_demo, 
                                          selected = c("Naissances", "Décès"),
                                          multiple = TRUE,
                                          width = "100%",
                                          selectize=TRUE),
                              
                              radioButtons("type_graph", 
                                           label = "Type de graphique :", 
                                           choices = list("Lignes" = "line", "Barres" = "bar"),
                                           selected = "line",
                                           inline = TRUE),
                              
                              selectInput("var_y2", 
                                          "Variable sur l’axe secondaire :", 
                                          choices = c("Aucune" = "none", 
                                                      choix_indic_demo), 
                                          selected = "none"),
                              tags$a(href = "https://www.insee.fr/fr/accueil", 
                                     target = "_blank", 
                                     style = "text-decoration: none; display: inline-block; text-align: center; 
                                             background-color: #f8f9fa; border-radius: 10px; padding: 10px; 
                                             transition: transform 0.2s, box-shadow 0.2s; width: 100%;",
                                     div(img(src = 'https://www.insee.fr/static/img/logo_com_externe_semi_bold.png', 
                                             style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"), 
                                         br(),
                                         p("Visitez le site officiel de l'INSEE", 
                                           style = "margin-top: 0px; font-size: 12px; color: #333; font-weight: bold;" ) )
                              )
                            )
                          ),
                          
                          column(width = 9,
                                 
                                 bs4TabCard(type = "tabs", 
                                            width = 12,
                                            maximizable = FALSE,
                                            collapsible = FALSE,
                                            solidHeader = TRUE, 
                                            status = "secondary",
                                            
                                            tabPanel("📈 Graphique :",
                                                     div(
                                                       style = 
                                                         "height: 100%;",
                                                       plotlyOutput("graph_accr",
                                                                    height = "400", 
                                                                    inline = TRUE,
                                                                    width = "100%") 
                                                     ) ),
                                            
                                            tabPanel("🔢 Tableau :",
                                                     div( 
                                                       DTOutput("tab_accr"), 
                                                       style = 
                                                         "font-size:85%; height:600px;") 
                                            )
                                 )
                          )
                        )
                        
                        
                        ),
               tabPanel(title = htmlOutput("message_indic_jeune"), 
                        value = "tab2",
                        
                        div(DTOutput("indice_structure", width = "100%"), 
                            style = "font-size:80%")
                        
                        ),
               
               footer = fluidRow(column(width = 12, 
                                        "Depuis 2004, la méthode de collecte est modifiée, le recensement s'appuie sur une méthode par sondage et vagues annuelles sur un cycle quinquennal."))
               
               
               ),
    
    fluidRow(
      
      column(
        width = 12,
        
        box(width = 12,
            maximizable = FALSE, collapsible = FALSE,
            solidHeader = TRUE, status = "primary",
            
            title = htmlOutput("message_solde_demographique"),
            
            footer = fluidRow(
              column(width = 5,
                     div(style = "text-align: center;")),
              column(width = 2, div(style = "width: 1px; height: 100%; background-color: gray; margin: auto; opacity: 0.7;")),
              column(width = 5, div(style = "text-align: center;" ))
            ),
            
            sidebar = boxSidebar(
              startOpen = TRUE,
              id = "mycardsidebar",
              h4("Démométrie - l'équation du bilan démographique"),
              texte_explication_calcul
            ),
            
            fluidRow(
              column(width = 12,
                     sliderInput(
                       inputId = "choix_periode_calcul",
                       label = "Choisir la période :",
                       min = min(as.numeric(tab_stock_mouvnat_travail$TIME_PERIOD)), 
                       max = max(as.numeric(tab_stock_mouvnat_travail$TIME_PERIOD)),
                       value = c(2015, 2021), 
                       step = 1,
                       animate = FALSE,
                       sep = "",
                       width = "50%"),
                     
                     column(width = 12, 
                            actionButton(inputId = "calcul_variation_population", 
                                         label = "🧮 Lancer le calcul",
                                         class = "btn-primary", 
                                         width = "100%"),
                            
                            br(), br(),
                            uiOutput("message_invitation"),
                            uiOutput("ajout_periode_ui")
                     ),
                     
                     div(DTOutput("tab_variation_population", width = "100%"), 
                         style = "font-size:80%")
                     
              ))
        ) )) 
                        
    
    
    
    )) #FIN
    