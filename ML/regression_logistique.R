# ==========================================================
# 📈 MODÈLE 1 : RÉGRESSION LOGISTIQUE
# ==========================================================

# Opérateur de secours
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Chargement des librairies nécessaires
library(caret)
library(pROC)
library(ggplot2)

# ----------------------------------------------------------
# Fonction principale : entraînement et évaluation du modèle
# ----------------------------------------------------------
run_logistic_model <- function(data, target_var) {
  req(data, target_var)

  # Vérification : la variable cible doit exister
  if (!(target_var %in% names(data))) {
    stop("La variable cible sélectionnée n'existe pas dans le dataset.")
  }

  # Conversion de la variable cible en facteur (si nécessaire)
  data[[target_var]] <- as.factor(data[[target_var]])

  # Vérifier qu’il s’agit bien d’un problème binaire
  if (length(unique(data[[target_var]])) != 2) {
    stop("La variable cible doit être binaire (2 modalités).")
  }

  # Supprimer les lignes avec NA
  data <- na.omit(data)

  # Séparation entraînement / test (70 / 30)
  set.seed(123)
  train_index <- createDataPartition(data[[target_var]], p = 0.7, list = FALSE)
  train_data <- data[train_index, ]
  test_data  <- data[-train_index, ]

  # Ajustement du modèle
  formula_str <- as.formula(paste(target_var, "~ ."))
  model <- glm(formula_str, data = train_data, family = binomial)

  # Prédictions
  probs <- predict(model, newdata = test_data, type = "response")

  # Conversion propre des labels pour compatibilité
  true <- as.numeric(factor(test_data[[target_var]])) - 1
  preds <- ifelse(probs > 0.5, 1, 0)

  # ------------------------------------------------------
  # MATRICE DE CONFUSION
  # ------------------------------------------------------
  conf <- table(Prédit = preds, Réel = true)
  conf_matrix <- as.data.frame.matrix(conf)

  # Valeurs (gestion des cas où certaines cases sont manquantes)
  TP <- conf["1", "1"] %||% 0
  TN <- conf["0", "0"] %||% 0
  FP <- conf["1", "0"] %||% 0
  FN <- conf["0", "1"] %||% 0

  # ------------------------------------------------------
  # MÉTRIQUES
  # ------------------------------------------------------
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  f1 <- ifelse(is.na(precision) | is.na(recall), NA,
               2 * (precision * recall) / (precision + recall))
  specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  metrics <- data.frame(
    Métrique = c("Précision", "Rappel", "F1-score", "Spécificité", "Exactitude"),
    Valeur = round(c(precision, recall, f1, specificity, accuracy), 3)
  )

  # ------------------------------------------------------
  # COURBE ROC
  # ------------------------------------------------------
  roc_obj <- tryCatch(
    roc(true, probs, quiet = TRUE),
    error = function(e) NULL
  )

  roc_plot <- function() {
    if (is.null(roc_obj)) return(NULL)
    ggroc(roc_obj, color = "#C60800", size = 1.2) +
      theme_minimal() +
      labs(
        title = "Courbe ROC - Régression Logistique",
        x = "1 - Spécificité",
        y = "Sensibilité"
      ) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray")
  }

  # ------------------------------------------------------
  # COURBE AUC
  # ------------------------------------------------------
  auc_value <- if (!is.null(roc_obj)) round(auc(roc_obj), 3) else NA
  auc_plot <- function() {
    ggplot(data.frame(x = 1), aes(x = x, y = 1)) +
      geom_text(
        aes(label = paste("AUC =", auc_value)),
        size = 8, color = "#C60800"
      ) +
      theme_void() +
      ggtitle("Aire sous la courbe (AUC)")
  }

  # ------------------------------------------------------
  # Sortie de la fonction
  # ------------------------------------------------------
  list(
    model = model,
    conf_matrix = conf_matrix,
    metrics = metrics,
    roc_plot = roc_plot,
    auc_plot = auc_plot
  )
}
