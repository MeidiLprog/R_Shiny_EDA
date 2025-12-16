# ==========================================================
# 📦 Chargement des librairies
# ==========================================================
# Charger plyr avant dplyr pour éviter les conflits
library(plyr)
library(dplyr)

# Outils généraux
library(shiny)
library(shinythemes)
library(bslib)
library(DT)
library(plotly)
library(ggplot2)
library(ggeasy)
library(readr)
library(caret)
library(pROC)
library(randomForest)
library(e1071)
library(caTools)
library(psych)
library(rcompanion)

# Thème global
thematic::thematic_shiny(font = "auto")

# ==========================================================
# 📂 Chargement du dataset par défaut (si disponible)
# ==========================================================
default_path <- "Data/Inflammation.csv"
if (file.exists(default_path)) {
  message("✅ Dataset par défaut trouvé : ", default_path)
  df_default <- read_csv(default_path, show_col_types = FALSE)
} else {
  warning("⚠️ Fichier par défaut introuvable : ", default_path)
  df_default <- NULL
}

# Variables globales (pour compatibilité avec le code)
a <- if (!is.null(df_default)) names(df_default) else NULL
