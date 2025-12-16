# ==========================================================
# app.R — Lancement principal de l’application Shiny
# ==========================================================
library(shiny)

# Charger les scripts dans l'environnement global
source("ui.R", local = FALSE)
source("server.R", local = FALSE)

# Lancer l'application
shinyApp(ui = ui, server = server)
