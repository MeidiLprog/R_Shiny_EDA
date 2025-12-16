# ==========================================================
# MODÈLE 3 : SVM (Machine à Vecteurs de Support) — dynamique
# ==========================================================


# ----------------------------------------------------------
# Contexte explicatif
# ----------------------------------------------------------
context3 <- function() {
  c <- "Le modèle SVM (Support Vector Machine) est un classifieur supervisé 
  qui cherche à séparer les classes par un hyperplan optimal.
  Il est particulièrement utile lorsque les classes sont difficiles à séparer linéairement."
  return(c)
}

# ----------------------------------------------------------
# Fonction d'entraînement du modèle
# ----------------------------------------------------------
train_svm_model <- function(data, target_var, kernel = "radial") {
  req(data, target_var)

  if (!target_var %in% names(data)) {
    stop("La variable cible n'existe pas dans le dataset.")
  }

  # Conversion de la cible en facteur
  if (!is.factor(data[[target_var]])) {
    data[[target_var]] <- as.factor(data[[target_var]])
  }

  # Nettoyage
  data <- data[, colSums(is.na(data)) < nrow(data)]
  data <- na.omit(data)

  # Division train/test
  set.seed(123)
  index <- caret::createDataPartition(data[[target_var]], p = 0.75, list = FALSE)
  train <- data[index, ]
  test <- data[-index, ]

  # Construction du modèle
  form <- as.formula(paste(target_var, "~ ."))
  model <- e1071::svm(form, data = train, kernel = kernel, probability = TRUE)

  # Prédictions
  pred <- predict(model, newdata = test, probability = TRUE)
  probs <- attr(pred, "probabilities")
  y_true <- test[[target_var]]

  # Matrice de confusion
  conf_mat <- table(Prédit = pred, Réel = y_true)

  # Calcul des métriques
  levels_y <- levels(y_true)
  positive <- levels_y[length(levels_y)]  # dernière modalité comme "positive"

  TP <- sum(pred == positive & y_true == positive)
  TN <- sum(pred != positive & y_true != positive)
  FP <- sum(pred == positive & y_true != positive)
  FN <- sum(pred != positive & y_true == positive)

  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  fscore <- ifelse(is.na(precision) | is.na(recall), NA, 2 * (precision * recall) / (precision + recall))
  specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  # Retour des résultats
  list(
    model = model,
    test_data = test,
    y_true = y_true,
    y_pred = pred,
    probs = probs,
    conf_mat = conf_mat,
    metrics = data.frame(
      Métrique = c("Précision", "Rappel (Recall)", "F1-score", "Spécificité", "Exactitude (Accuracy)"),
      Valeur = round(c(precision, recall, fscore, specificity, accuracy), 3)
    )
  )
}

# ----------------------------------------------------------
# Fonctions d'affichage
# ----------------------------------------------------------
cm3 <- function(results) {
  if (is.null(results$conf_mat)) return(NULL)
  mat <- as.data.frame.matrix(results$conf_mat)
  return(mat)
}

roc3 <- function(results) {
  req(results)
  if (is.null(results$probs)) {
    message("ℹ️ Pas de probabilités disponibles pour le SVM.")
    return(NULL)
  }
  positive <- levels(results$y_true)[length(levels(results$y_true))]
  prob_pos <- tryCatch(results$probs[, positive], error = function(e) NULL)
  if (is.null(prob_pos)) return(NULL)
  y_true_num <- as.numeric(results$y_true == positive)
  roc_obj <- pROC::roc(y_true_num, prob_pos)
  plot(roc_obj, col = "#C60800", lwd = 3, main = "Courbe ROC — SVM")
  invisible(roc_obj)
}

auc3 <- function(results) {
  req(results)
  if (is.null(results$probs)) return(NULL)
  positive <- levels(results$y_true)[length(levels(results$y_true))]
  prob_pos <- tryCatch(results$probs[, positive], error = function(e) NULL)
  if (is.null(prob_pos)) return(NULL)
  y_true_num <- as.numeric(results$y_true == positive)
  roc_obj <- pROC::roc(y_true_num, prob_pos)
  plot(roc_obj,
       col = "#C60800",
       lwd = 3,
       print.auc = TRUE,
       print.auc.x = 45,
       main = "AUC — SVM",
       percent = TRUE)
  invisible(roc_obj)
}
