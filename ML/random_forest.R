# ==========================================================
# RANDOM FOREST — version générique et stable
# ==========================================================

library(randomForest)
library(pROC)
library(caret)

context2 <- function() {
  "Random Forest : ensemble d'arbres de décision entraînés sur des échantillons bootstrap, 
  robuste aux outliers et au sur-apprentissage, fonctionne avec des données mixtes."
}

train_rf_model <- function(data, target_var, ntree = 100) {
  req(data, target_var)

  # Noms sûrs (espaces -> points)
  names(data) <- make.names(names(data))
  target_var  <- make.names(target_var)

  # Supprimer colonnes constantes/vide
  data <- data[, vapply(data, function(x) length(unique(x[!is.na(x)])) > 1, logical(1))]

  # Limiter les catégorielles trop riches et factoriser les autres
  cat_cols <- vapply(data, is.character, logical(1))
  for (v in names(data)[cat_cols]) {
    n_unique <- length(unique(data[[v]]))
    if (n_unique > 20) {
      data[[v]] <- NULL
    } else {
      data[[v]] <- as.factor(data[[v]])
    }
  }

  # Cible factor (binaire ou multi-classes)
  if (!is.factor(data[[target_var]])) data[[target_var]] <- as.factor(data[[target_var]])

  # Split
  set.seed(123)
  idx <- caret::createDataPartition(data[[target_var]], p = 0.75, list = FALSE)
  train <- data[idx, ]
  test  <- data[-idx, ]

  # Modèle
  form  <- as.formula(paste(target_var, "~ ."))
  model <- randomForest::randomForest(form, data = train, ntree = ntree)

  # Prédictions proba si binaire, sinon vote majoritaire
  if (nlevels(train[[target_var]]) == 2) {
    prob_pos <- predict(model, newdata = test, type = "prob")[, 2]
    y_pred   <- ifelse(prob_pos > 0.5, levels(train[[target_var]])[2], levels(train[[target_var]])[1])
  } else {
    prob_all <- predict(model, newdata = test, type = "prob")
    y_pred   <- factor(colnames(prob_all)[max.col(prob_all)], levels = levels(train[[target_var]]))
    prob_pos <- NULL
  }
  y_true <- test[[target_var]]
  conf_mat <- table(Prédit = y_pred, Réel = y_true)

  # ROC/AUC (seulement binaire)
  roc_obj <- NULL
  if (!is.null(prob_pos)) {
    roc_obj <- pROC::roc(as.numeric(y_true == levels(y_true)[2]), prob_pos)
  }

  # Métriques simples
  acc <- mean(y_pred == y_true)
  auc_val <- if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_

  list(
    model = model,
    conf_matrix = as.data.frame.matrix(conf_mat),
    metrics = data.frame(Accuracy = round(acc, 3), AUC = round(auc_val, 3)),
    roc_plot = function() { if (!is.null(roc_obj)) plot(roc_obj, col = "#1b9e77", lwd = 3) },
    auc_plot = function() { if (!is.null(roc_obj)) plot(roc_obj, col = "#1b9e77", lwd = 3, print.auc = TRUE) }
  )
}

# Aides pour server.R (appelle les closures renvoyées)
roc2 <- function(res) { if (!is.null(res$roc_plot)) res$roc_plot() }
auc2 <- function(res) { if (!is.null(res$auc_plot)) res$auc_plot() }
cm2  <- function(res) { res$conf_matrix }
table_metr2 <- function(res) { res$metrics }
