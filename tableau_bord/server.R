#########################################
#LE BILAN DEMOGRAPHIQUE, à l'année n-1  #
# Par Myriam YAHYAOUI, Insee DPS        #
# server.R file                         #
#########################################

shinyServer(function(input, output, session) { 
  
  output$message_pop <- renderUI({
    pop_n = tab_calcul |> 
      filter(TERRITOIRE == input$zone, TIME_PERIOD == n) |> 
      pull(Ensemble) 
    # Choix de l'unité selon l'ordre de grandeur
    if (pop_n >= 1e6) {
      population_affichee = pop_n / 1e6
      unite = ifelse(round(population_affichee) == 1, "million", "millions")
    } else {
      population_affichee = pop_n / 1e3
      unite = ifelse(round(population_affichee) == 1, "millier", "milliers")
    }
    # Construction du message avec formatage
    txt_population = paste0( "📈", 
      "<span style='font-size: 18px; font-weight: bold; text-shadow: 2px 2px 5px rgba(0, 0, 0, 0.5);'>", 
      round(population_affichee, 2) |> prettyNum(decimal.mark = ","), 
      "</span> ",
      " ", unite, " d'habitants <br>au 1<sup>er</sup> janvier ", n, ".") |>
      HTML() })
  
  output$message_solde_demographique <- renderUI({
    sd_n = tab_calcul |> 
      filter(TERRITOIRE == input$zone, TIME_PERIOD == n-1) |> 
      pull(sd) 
    # Construction du message de solde démographique
    if (sd_n < 0) {
      txt_solde = paste0(" ⬇️La population est en baisse par rapport à l'an dernier ", 
                         "(- ", sd_n |> abs() |> prettyNum(big.mark = " "), ")" )
    } else if (sd_n == 0) {
      txt_solde = "La population est stable par rapport à l'an dernier"
    } else {  
      txt_solde = paste0("⬆️ La population est en hausse par rapport à l'an dernier ", 
                         "(+ ", sd_n |> prettyNum(big.mark = " "), ")" )
    }
    # Construction du message avec formatage
    txt = paste0("Mesure des variations chronologiques :", "<br>",
                 txt_solde, ".") |>
      HTML() })
  
  output$message_indic_jeune <- renderUI({
    
    indice_jeunesse = indice_structure |>
      filter(TERRITOIRE == input$zone, TIME_PERIOD == n) |>
      pull(`Indice de jeunesse`)
    
    txt = paste0("👥 Pour 100 personnes âgés de 60 ans ou plus, <br>en ",
                 n, " on compte ", 
                 "<span style='font-size: 18px; font-weight: bold; text-shadow: 2px 2px 5px rgba(0, 0, 0, 0.5);'>", 
                 indice_jeunesse,
                 " jeunes de moins de 25 ans</span>", ".") |>
      HTML()
    })

  
  data_fig <- reactive({ 
    
    tab_calcul_long |>
      filter(TERRITOIRE == input$zone, 
             !is.na(OBS_VALUE) ) })
  
  output$graph_accr <- renderPlotly({ 
    df <- data_fig() |> 
      filter(INDIC_DEMO %in% c(input$indicateur_demo_choix))
    
    # Séparer les données pour l'axe primaire et secondaire
    df_y2 <- data_fig() |> 
      filter(INDIC_DEMO == input$var_y2)
    
    title_y = df_y2 |> pull(INDIC_DEMO_LABEL)
    
    # Ajuster l’échelle de l’axe secondaire si nécessaire
    y2_layout <- list(
      title = title_y, 
      overlaying = "y", 
      tickfont = list(size = 9),
      automargin = TRUE,
      side = "right")
    
    # Créer le graphique principal
    
    # Définir le type de graphique en fonction de l'input utilisateur
    
    if (input$type_graph == "bar") {
      p <- plot_ly(
        df,
        x = ~TIME_PERIOD,
        y = ~OBS_VALUE,
        color = ~factor(INDIC_DEMO_LABEL),
        colors = ~unique(df$INDIC_DEMO_LABEL) |> length() |> generate_colors(), 
        type = "bar",
        name = ~INDIC_DEMO_LABEL,
        text = ~paste(
          "<br><b>Année:</b> ", TIME_PERIOD,
          "<br><b>Élément du Bilan Démographique :</b> ", INDIC_DEMO_LABEL,  
          "<br><b>Valeur:</b> ", prettyNum(round(OBS_VALUE, 2), 
                                           big.mark = " ", 
                                           decimal.mark = ",")
        ),
        hoverinfo = "text",
        opacity = 0.45) |> 
        layout(barmode = "overlay", bargap = 0.2)
    } 
    
    else {
      p <- plot_ly(
        data = df,
        x = ~TIME_PERIOD, 
        y = ~OBS_VALUE, 
        color = ~factor(INDIC_DEMO_LABEL),
        colors = unique(df$INDIC_DEMO_LABEL) |> length() |> generate_colors(),
        text = ~paste("<br><b>Année:</b> ", TIME_PERIOD,
                      "<br><b>Élément du Bilan Démographique :</b> ", INDIC_DEMO_LABEL,  
                      "<br><b>Valeur:</b> ", prettyNum(round(OBS_VALUE, 2), 
                                                       big.mark = " ", 
                                                       decimal.mark = ",")),
        hoverinfo = "text",
        type = "scatter",  
        mode = "lines+markers",  
        marker = list(size = 5, opacity = 0.8, symbol = "circle"),
        line = list(width = 1.5)
      )
    }
    
    # Ajouter le tracé secondaire si une variable est sélectionnée
    if (input$var_y2 != "none") {
      p <- p |> 
        add_trace(
          data = df_y2, 
          x = ~TIME_PERIOD, 
          y = ~OBS_VALUE, 
          line = list(color = "#ff3386", width = 2.5),
          marker = list(color = "#ff3386", size = 3, opacity = 0.8, symbol = "circle"),
          type = "scatter", 
          mode = "lines+markers",
          yaxis = "y2"
        )
    }
    
    # Configuration finale
    config_plotly(p) |> 
      layout(xaxis = list(title = "",
                          tickvals = unique(df$TIME_PERIOD) ),
             yaxis = list(title = "", 
                          side = "left"),
             yaxis2 = y2_layout) })
  
  output$tab_accr <- DT::renderDT({
    data_fig() |>
      filter(INDIC_DEMO != "duree") |> 
      select(-c(INDIC_DEMO, PERIODE, MaJ_naissances, MaJ_deces, MaJ_population,
                STATUT_INFO_DECES,  STATUT_INFO_NAISSANCES,  STATUT_INFO_POP,
      )) |>
      mutate(OBS_VALUE = if_else(
        str_detect(INDIC_DEMO_LABEL, "^(Taux|Évolution annuelle)") | INDIC_DEMO_LABEL == "Évolution annuelle moyenne (accr. relatif)", 
        prettyNum(OBS_VALUE |> round(2), decimal.mark = ",") |> paste("( ‰)"), 
        prettyNum(OBS_VALUE |> round(0), big.mark = " ") )) |>
      pivot_wider(names_from = "TIME_PERIOD", values_from = "OBS_VALUE") |>
      arrange(INDIC_DEMO_LABEL) |>
      rename( "Indicateurs de l’évolution de la population" = INDIC_DEMO_LABEL,
              "TERRITOIRE" = TERRITOIRE ) |>
      create_dt(dom_options = "Bfltip",
                options = list(
                  fixedColumns = list(leftColumns = 3),
                  filter = list(position = 'top'))) })
  
  output$indice_structure <- DT::renderDT({
    indice_structure |>
      filter(TERRITOIRE == input$zone)  |>
      create_dt(dom_options = "Bfltip",
                options = list(
                  fixedColumns = list(leftColumns = 2),
                  filter = list(position = 'top'))) })
  
  
  # Stocke les périodes ajoutées dynamiquement
  valeurs <- reactiveValues(resultats = list(), 
                            deletedRows = list(),  # Liste des lignes supprimées
                            calcul_effectue = FALSE)
  
  # Fonction de calcul de la variation démographique
  calcul_variation <- function(annee_debut, annee_fin) {
    calcul_tat_evolpop(tab_stock_mouvnat_travail, 
                       annee_debut, 
                       annee_fin) |> 
      pivot_longer(-c(MaJ_population, MaJ_naissances, MaJ_deces,  
                      STATUT_INFO_DECES,  STATUT_INFO_NAISSANCES,  STATUT_INFO_POP,
                      TIME_PERIOD, PERIODE, TERRITOIRE), 
                   names_to = "INDIC_DEMO", values_to = "OBS_VALUE") |>
      mutate(INDIC_DEMO_LABEL = factor(recode(INDIC_DEMO, !!!INDIC_LABELS), 
                                       levels = INDIC_LABELS)) |>
      filter( !(INDIC_DEMO %in% c("duree", "Estimations de population") ), 
              TERRITOIRE == input$zone,
              !is.na(OBS_VALUE)
      ) |> 
      select(-c(INDIC_DEMO, TIME_PERIOD, MaJ_naissances, MaJ_deces, MaJ_population,
                STATUT_INFO_DECES,  STATUT_INFO_NAISSANCES,  STATUT_INFO_POP)) |>
      mutate(OBS_VALUE = if_else(
        str_detect(INDIC_DEMO_LABEL, "^(Taux|Évolution annuelle)") | INDIC_DEMO_LABEL == "Évolution annuelle moyenne (accr. relatif)", 
        prettyNum(OBS_VALUE |> round(2), decimal.mark = ",") |> paste("( ‰)"), 
        prettyNum(OBS_VALUE |> round(0), big.mark = " ") )) |>
      arrange(INDIC_DEMO_LABEL) |>
      pivot_wider(names_from = "INDIC_DEMO_LABEL", values_from = "OBS_VALUE") |>
      rename("TERRITOIRE" = TERRITOIRE) |>
      mutate(PERIODE = paste(annee_debut, annee_fin, sep = "-"))
  }
  
  observeEvent(input$zone, {
    # Affiche une notification avec un ID unique
    showNotification("Le territoire a changé ! 
                     Veuillez re.lancer votre calcul ! ", 
                     id = "indicateur_changé", type = "message",
                     duration = NULL)
  }, ignoreInit = TRUE)
  
  # Bouton pour lancer le calcul initial
  observeEvent(input$calcul_variation_population, {
    annee_debut <- input$choix_periode_calcul[1]
    annee_fin <- input$choix_periode_calcul[2]
    
    nouvelle_periode <- calcul_variation(annee_debut, annee_fin)
    
    valeurs$resultats <- append(valeurs$resultats, list(nouvelle_periode))
    valeurs$calcul_effectue <- TRUE
    
    # Affiche la boîte de dialogue pour ajouter une nouvelle période
    showModal(modalDialog(
      title = "Souhaitez-vous ajouter une nouvelle période ?",
      "Vous pouvez ajouter une autre période pour comparer les évolutions démographiques.",
      footer = tagList(
        modalButton("Non, terminer"),
        actionButton("ajouter_periode", "➕ Ajouter une période", class = "btn-warning")
      )
    ))
  })
  
  # Gestion de l'ajout de périodes supplémentaires
  observeEvent(input$ajouter_periode, {
    removeModal()
    showModal(modalDialog(
      title = "Sélectionner une nouvelle période",
      sliderInput(
        inputId = "choix_nouvelle_periode",
        label = "Nouvelle période :",
        min = min(as.numeric(tab_stock_mouvnat_travail$TIME_PERIOD)), 
        max = max(as.numeric(tab_stock_mouvnat_travail$TIME_PERIOD)),
        value = c(2016, 2020), 
        step = 1,
        animate = FALSE,
        sep = "",
        width = "100%"
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("valider_nouvelle_periode", "✅ Valider", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$valider_nouvelle_periode, {
    removeModal()
    annee_debut <- input$choix_nouvelle_periode[1]
    annee_fin <- input$choix_nouvelle_periode[2]
    
    nouvelle_periode <- calcul_variation(annee_debut, annee_fin)
    
    valeurs$resultats <- append(valeurs$resultats, list(nouvelle_periode))
  })
  
  
  # Suppression de la dernière période ajoutée
  observeEvent(input$supprimer_periode, {
    if (length(valeurs$resultats) > 0) {
      last_row <- valeurs$resultats[[length(valeurs$resultats)]]
      
      # Sauvegarde de la ligne supprimée
      valeurs$deletedRows <- append(valeurs$deletedRows, list(last_row))
      
      # Suppression de la ligne de resultats
      valeurs$resultats <- valeurs$resultats[-length(valeurs$resultats)]
    }
  })
  
  # Annuler la suppression (restaurer la dernière ligne supprimée)
  observeEvent(input$annuler_suppression, {
    if (length(valeurs$deletedRows) > 0) {
      restored_row <- valeurs$deletedRows[[length(valeurs$deletedRows)]]
      valeurs$resultats <- append(valeurs$resultats, list(restored_row))
      
      # Supprime la ligne de suppression
      valeurs$deletedRows <- valeurs$deletedRows[-length(valeurs$deletedRows)]
    }
  })
  
  # Mise à jour du tableau avec boutons de suppression
  output$tab_variation_population <- DT::renderDT({
    req(input$calcul_variation_population)
    
    df <- bind_rows(valeurs$resultats)
    df$Supprimer <- sapply(seq_len(nrow(df)), function(i) {
      # Ajoute un attribut data-id pour chaque ligne de suppression
      paste0("<a href='#' class='delete-row' data-id='", i, "'>❌</a>")
    })
    
    create_dt(df, escape = FALSE, options = list(
      fixedColumns = list(leftColumns = 2),
      pageLength = 5,
      columnDefs = list(list(targets = ncol(df), orderable = FALSE))
    ))
  }, server = FALSE)
  
  observeEvent(input$delete_row, {
    row_to_delete <- input$delete_row
    if (!is.null(row_to_delete) && row_to_delete <= length(valeurs$resultats)) {
      # Supprime la ligne spécifiée et la sauvegarde dans deletedRows
      last_row <- valeurs$resultats[[row_to_delete]]
      valeurs$deletedRows <- append(valeurs$deletedRows, list(last_row))
      
      # Suppression de la ligne dans resultats
      valeurs$resultats <- valeurs$resultats[-row_to_delete]
    }
  })
  
  # UI pour ajouter et supprimer des périodes
  output$ajout_periode_ui <- renderUI({
    if (valeurs$calcul_effectue) {
      tagList(
        fluidRow(
          column(6,
                 actionButton(
                   inputId = "ajouter_periode",
                   label = "➕ Ajouter une période",
                   class = "btn-warning",
                   style = "background-color:  #74b87f; color: white; width: 100%;"
                 )),
          column(6,
                 actionButton(
                   inputId = "supprimer_periode",
                   label = "➖ Supprimer la dernière période",
                   class = "btn-danger",
                   style = "background-color: #f98b8e; color: white; width: 100%;"
                 )
          ),
          column(6,
                 actionButton(
                   inputId = "annuler_suppression",
                   label = "Annuler la suppression",
                   icon('undo'),
                   class = "btn-info",
                 )
          )
        )
      )
    }
  })
  
  
  
  
  
  })