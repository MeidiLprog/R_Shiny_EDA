# ==========================================================
# CONFIGURATION GÉNÉRALE SHINY
# ==========================================================
options(shiny.maxRequestSize = 50 * 1024^2)  # Upload max : 50 Mo

# ==========================================================
# CHARGEMENT DES MODULES ET LIBRAIRIES
# ==========================================================
source("ML/imports.R")
source("EDA/univariate.R")
source("EDA/Bivariate.R")
source("ML/regression_logistique.R")
source("ML/random_forest.R")
source("ML/SVM.R")

# ==========================================================
# SERVEUR SHINY PRINCIPAL
# ==========================================================
server <- function(input, output, session) {


# ----------------------------------------------------------
# 1️⃣ Gestion dynamique du dataset
# ----------------------------------------------------------
rvals <- reactiveValues(data = NULL)

observe({
  # Si aucun dataset n’est chargé, charger celui par défaut
  if (is.null(rvals$data) && !is.null(df_default)) {
    rvals$data <- df_default
    names(rvals$data) <- make.names(names(rvals$data))
    cat("✅ Données par défaut chargées au démarrage\n")
  }
})

observeEvent(
  {
    list(input$data_choice, input$user_file, input$sep, input$header)
  },
  {
    if (input$data_choice == "default") {
      # --- Cas 1 : dataset par défaut ---
      if (!is.null(df_default)) {
        rvals$data <- df_default
        names(rvals$data) <- make.names(names(rvals$data))
        showNotification("📂 Dataset par défaut chargé.", type = "message")
      } else {
        showNotification("⚠️ Fichier par défaut introuvable dans 'Data/'.", type = "error")
      }

    } else if (!is.null(input$user_file)) {
      # --- Cas 2 : dataset utilisateur ---
      tryCatch({
        newdata <- readr::read_delim(
          input$user_file$datapath,
          delim = input$sep,
          col_names = input$header,
          show_col_types = FALSE
        )
        rvals$data <- newdata
        names(rvals$data) <- make.names(names(rvals$data))
        showNotification("✅ Nouveau dataset importé avec succès.", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur de lecture du fichier :", e$message), type = "error")
      })
    }
  },
  ignoreInit = FALSE
)
observe({
  req(rvals$data)
  output$tablePreview <- DT::renderDataTable({
    DT::datatable(head(rvals$data, 10), options = list(scrollX = TRUE))
  })
})


  # ----------------------------------------------------------
  # 3️⃣ Résumé des données
  # ----------------------------------------------------------
  output$summaryData <- renderPrint({
    req(rvals$data)
    summary(rvals$data)
  })

  output$missingValues <- renderTable({
    req(rvals$data)
    data.frame(
      Variable = names(rvals$data),
      NA_Count = sapply(rvals$data, function(x) sum(is.na(x))),
      Type = sapply(rvals$data, class)
    )
  })

  # ----------------------------------------------------------
  # 4️⃣ Aperçu du dataset
  # ----------------------------------------------------------
  output$tablePreview <- DT::renderDataTable({
    req(rvals$data)
    DT::datatable(head(rvals$data, 10), options = list(scrollX = TRUE))
  })

  observeEvent(input$go, {
    req(rvals$data)
    showModal(modalDialog(
      title = "Aperçu complet du dataset",
      DT::renderDataTable(rvals$data, options = list(scrollX = TRUE)),
      easyClose = TRUE,
      size = 'l'
    ))
  })

  # ----------------------------------------------------------
  # 5️⃣ Sélecteurs dynamiques
  # ----------------------------------------------------------
   # observe({
  #   req(rvals$data)
  #   updateSelectInput(session, "SelectUniv", choices = names(rvals$data))
  #   updateSelectInput(session, "SelectBiv1", choices = names(rvals$data))
  #   updateSelectInput(session, "SelectBiv2", choices = names(rvals$data))
  #   updateSelectInput(session, "target_var", choices = names(rvals$data))
  # })

# ----------------------------------------------------------
# 5️⃣ Sélecteurs dynamiques (filtrage des variables binaires)
# ----------------------------------------------------------
observe({
  req(rvals$data)

  # Sélection des colonnes binaires uniquement
  binary_vars <- names(rvals$data)[sapply(rvals$data, function(x) {
    is.factor(x) && length(unique(na.omit(x))) == 2
  })]

  # Si aucune variable binaire trouvée, on informe l'utilisateur
  if (length(binary_vars) == 0) {
    binary_vars <- c("⚠️ Aucune variable binaire détectée")
  }

  updateSelectInput(session, "SelectUniv", choices = names(rvals$data))
  updateSelectInput(session, "SelectBiv1", choices = names(rvals$data))
  updateSelectInput(session, "SelectBiv2", choices = names(rvals$data))
  updateSelectInput(session, "target_var", choices = binary_vars)
})

  # ----------------------------------------------------------
  # 6️⃣ Analyse univariée
  # ----------------------------------------------------------
  output$oplot_box_bar <- renderPlot({
    req(rvals$data, input$SelectUniv)
    plot_box_bar(rvals$data, input$SelectUniv)
  })

  output$oplot_histo_pie <- renderPlot({
    req(rvals$data, input$SelectUniv)
    plot_histo_pie(rvals$data, input$SelectUniv)
  })

  output$oplot_Cummul <- plotly::renderPlotly({
    req(rvals$data, input$SelectUniv)
    plot_Cummul(rvals$data, input$SelectUniv)
  })

  output$oStat <- renderTable({
    req(rvals$data, input$SelectUniv)
    statQ(rvals$data, input$SelectUniv)
  })

  # ----------------------------------------------------------
  # 7️⃣ Analyse bivariée
  # ----------------------------------------------------------
  output$plt_box_bar <- renderPlot({
    req(rvals$data, input$SelectBiv1, input$SelectBiv2)
    plt_box_bar(rvals$data, input$SelectBiv1, input$SelectBiv2)
  })

  output$describetable <- renderTable({
    req(rvals$data, input$SelectBiv1, input$SelectBiv2)
    describetable(rvals$data, input$SelectBiv1, input$SelectBiv2)
  })

  output$tab_test <- renderTable({
    req(rvals$data, input$SelectBiv1, input$SelectBiv2)
    tab_test(rvals$data, input$SelectBiv1, input$SelectBiv2)
  })

  # ----------------------------------------------------------
  # 8️⃣ Prétraitement
  # ----------------------------------------------------------
  observeEvent(input$normalize, {
    req(rvals$data)
    num_vars <- sapply(rvals$data, is.numeric)
    rvals$data[num_vars] <- scale(rvals$data[num_vars])
    showNotification("✔️ Normalisation effectuée sur les variables numériques.", type = "message")
  })

  observeEvent(input$encode_factors, {
    req(rvals$data)
    cat_vars <- sapply(rvals$data, is.character)
    for (v in names(rvals$data)[cat_vars]) {
      rvals$data[[v]] <- as.factor(rvals$data[[v]])
    }
    showNotification("✔️ Encodage des variables catégorielles effectué.", type = "message")
  })

  observeEvent(input$remove_na, {
    req(rvals$data)
    rvals$data <- na.omit(rvals$data)
    showNotification("✔️ Lignes contenant des valeurs manquantes supprimées.", type = "message")
  })

  # ----------------------------------------------------------
  # 9️⃣ MODÈLES SUPERVISÉS (réactifs)
  # ----------------------------------------------------------
  # --- Régression logistique
  logit_model <- reactive({
    req(rvals$data, input$target_var)
    run_logistic_model(rvals$data, input$target_var)
  })

  # --- Random Forest
  rf_model <- reactive({
    req(rvals$data, input$target_var, input$ka)
    train_rf_model(rvals$data, input$target_var, ntree = input$ka)
  })

  # --- SVM
  svm_model <- reactive({
    req(rvals$data, input$target_var, input$kernel_type)
    train_svm_model(rvals$data, input$target_var, kernel = input$kernel_type)
  })

  # ----------------------------------------------------------
  # 🔹 Sorties : Régression Logistique
  # ----------------------------------------------------------
  output$ROC1 <- renderPlot({ logit_model()$roc_plot() })
  output$AUC1 <- renderPlot({ logit_model()$auc_plot() })
  output$tablemet1 <- renderTable({ logit_model()$metrics })
  output$mc1 <- renderTable({ logit_model()$conf_matrix })

  # ----------------------------------------------------------
  # 🔹 Sorties : Random Forest
  # ----------------------------------------------------------
  output$ROC2 <- renderPlot({ rf_model()$roc_plot() })
  output$AUC2 <- renderPlot({ rf_model()$auc_plot() })
  output$tablemet2 <- renderTable({ rf_model()$metrics })
  output$mc2 <- renderTable({ rf_model()$conf_matrix })

  # ----------------------------------------------------------
  # 🔹 Sorties : SVM
  # ----------------------------------------------------------
  # ----------------------------------------------------------
# 🔹 Sorties : SVM
# ----------------------------------------------------------
output$ROC3 <- renderPlot({
  req(svm_model())
  roc3(svm_model())
})

output$AUC3 <- renderPlot({
  req(svm_model())
  auc3(svm_model())
})

output$tablemet3 <- renderTable({
  req(svm_model())
  svm_model()$metrics
})

output$mc3 <- renderTable({
  req(svm_model())
  cm3(svm_model())
})
}
