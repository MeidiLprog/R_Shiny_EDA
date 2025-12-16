

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united", base_font = bslib::font_google("Montserrat", local = TRUE)),
  titlePanel(span(img(src = "urinary-removebg-preview.png", height = 90), "Plateforme d'analyse et de modélisation")),

  navbarPage("MENU PRINCIPAL",

    # ---- CHARGEMENT ----
    tabPanel("Chargement des données",
      sidebarLayout(
        sidebarPanel(
          h4("📂 Choix du dataset"),
          radioButtons("data_choice", "Source :", choices = c("Inflammation par défaut" = "default", "Importer CSV" = "user")),
          conditionalPanel(
            condition = "input.data_choice == 'user'",
            fileInput("user_file", "Importer un fichier CSV :", accept = ".csv"),
            checkboxInput("header", "Contient une ligne d'en-tête ?", TRUE),
            selectInput("sep", "Séparateur :", choices = c("Virgule" = ",", "Point-virgule" = ";", "Tabulation" = "\t"))
          ),
          actionButton("go", "Afficher tout le dataset", class = "btn-primary")
        ),
        mainPanel(
          h4("Aperçu des données :"), DT::dataTableOutput("tablePreview"),
          br(), h4("Résumé global :"), verbatimTextOutput("summaryData")
        )
      )
    ),

    # ---- PRÉTRAITEMENT ----
    tabPanel("Prétraitement",
      sidebarLayout(
        sidebarPanel(
          h4("🧰 Outils de prétraitement"),
          actionButton("remove_na", "Supprimer les lignes NA", class = "btn-warning"),
          br(), actionButton("normalize", "Normaliser les numériques", class = "btn-success"),
          br(), actionButton("encode_factors", "Encoder les catégorielles", class = "btn-info")
        ),
        mainPanel(h4("🔍 Vérification des valeurs manquantes"), tableOutput("missingValues"))
      )
    ),

    # ---- EXPLORATION ----
    tabPanel("Analyse exploratoire",
      tabsetPanel(
        tabPanel("Univariée",
          sidebarLayout(
            sidebarPanel(selectInput("SelectUniv", "Variable :", choices = NULL)),
            mainPanel(
              fluidRow(column(4, plotOutput("oplot_box_bar")),
                       column(4, plotOutput("oplot_histo_pie")),
                       column(4, plotly::plotlyOutput("oplot_Cummul"))),
              tableOutput("oStat")
            )
          )
        ),
        tabPanel("Bivariée",
          sidebarLayout(
            sidebarPanel(
              selectInput("SelectBiv1", "Variable 1 :", choices = NULL),
              selectInput("SelectBiv2", "Variable 2 :", choices = NULL)
            ),
            mainPanel(
              fluidRow(column(6, plotOutput("plt_box_bar")), column(6, tableOutput("tab_test"))),
              hr(), tableOutput("describetable")
            )
          )
        )
      )
    ),

    # ---- MODÉLISATION ----
    tabPanel("Modèles de classification",
      sidebarLayout(
        sidebarPanel(
          h4("⚙️ Paramètres du modèle"),
          selectInput("target_var", "Variable cible :", choices = NULL),
          sliderInput("ka", "Nombre d'arbres (Random Forest) :", min = 5, max = 200, value = 50, step = 5),
          selectInput("kernel_type", "Noyau SVM :", choices = c("radial", "linear", "polynomial", "sigmoid"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Régression Logistique",
              h4("Courbes ROC & AUC"), fluidRow(column(6, plotOutput("ROC1")), column(6, plotOutput("AUC1"))),
              hr(), fluidRow(column(6, tableOutput("mc1")), column(6, tableOutput("tablemet1")))
            ),
            tabPanel("Random Forest",
              h4("Courbes ROC & AUC"), fluidRow(column(6, plotOutput("ROC2")), column(6, plotOutput("AUC2"))),
              hr(), fluidRow(column(6, tableOutput("mc2")), column(6, tableOutput("tablemet2")))
            ),
            tabPanel("SVM",
              h4("Courbes ROC & AUC"), fluidRow(column(6, plotOutput("ROC3")), column(6, plotOutput("AUC3"))),
              hr(), fluidRow(column(6, tableOutput("mc3")), column(6, tableOutput("tablemet3")))
            )
          )
        )
      )
    ),

    inverse = TRUE
  )
)
