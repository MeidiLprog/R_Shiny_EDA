# ==========================================================
# ANALYSE BIVARIÉE — version dynamique et robuste
# ==========================================================
library(ggplot2)
library(dplyr)
library(rlang)
library(rcompanion)

# ----------------------------------------------------------
# 1) Table descriptive selon types
# ----------------------------------------------------------
describetable <- function(data, first_feature, second_feature) {
  req(data, first_feature, second_feature)

  # Cas 1 : deux variables qualitatives
  if (!is.numeric(data[[first_feature]]) && !is.numeric(data[[second_feature]])) {
    tab <- table(data[[first_feature]], data[[second_feature]])
    return(as.data.frame.matrix(tab))
  }

  # Cas 2 : une quali et une quanti
  if (xor(is.numeric(data[[first_feature]]), is.numeric(data[[second_feature]]))) {
    temp <- data %>% dplyr::select(all_of(c(first_feature, second_feature)))

    num_var <- names(temp)[sapply(temp, is.numeric)]
    cat_var <- names(temp)[!sapply(temp, is.numeric)]

    # Utiliser rlang::sym pour éviter l’erreur data mask
    desc <- temp %>%
      group_by(!!sym(cat_var)) %>%
      summarise(
        Moyenne    = mean(!!sym(num_var), na.rm = TRUE),
        Médiane    = median(!!sym(num_var), na.rm = TRUE),
        Écart_type = sd(!!sym(num_var), na.rm = TRUE),
        Min        = min(!!sym(num_var), na.rm = TRUE),
        Max        = max(!!sym(num_var), na.rm = TRUE),
        .groups = "drop"
      )

    colnames(desc)[1] <- cat_var
    return(as.data.frame(desc))
  }

  # Cas 3 : deux quantitatives
  if (is.numeric(data[[first_feature]]) && is.numeric(data[[second_feature]])) {
    cor_val <- suppressWarnings(cor(data[[first_feature]], data[[second_feature]], use = "complete.obs"))
    return(data.frame(
      Variable1  = first_feature,
      Variable2  = second_feature,
      Corrélation = round(cor_val, 3)
    ))
  }

  data.frame(Message = "Types non reconnus.")
}

# ----------------------------------------------------------
# 2) Graphique adapté (barplot / boxplot / scatter)
# ----------------------------------------------------------
plt_box_bar <- function(data, first_feature, second_feature) {
  req(data, first_feature, second_feature)

  # Cas quali/quali → barplot groupé
  if (!is.numeric(data[[first_feature]]) && !is.numeric(data[[second_feature]])) {
    ggplot(data, aes(x = .data[[first_feature]], fill = .data[[second_feature]])) +
      geom_bar(position = "dodge") +
      theme_classic() +
      labs(
        title = paste("Comparaison de", first_feature, "et", second_feature),
        x = first_feature, y = "Effectifs", fill = second_feature
      ) +
      ggeasy::easy_center_title()
  }
  # Cas quanti/quali → boxplot
  else if (xor(is.numeric(data[[first_feature]]), is.numeric(data[[second_feature]]))) {
    num_var <- ifelse(is.numeric(data[[first_feature]]), first_feature, second_feature)
    cat_var <- ifelse(!is.numeric(data[[first_feature]]), first_feature, second_feature)

    ggplot(data, aes(x = .data[[cat_var]], y = .data[[num_var]], fill = .data[[cat_var]])) +
      geom_boxplot(alpha = 0.7) +
      theme_classic() +
      labs(
        title = paste("Distribution de", num_var, "selon", cat_var),
        x = cat_var, y = num_var
      ) +
      ggeasy::easy_center_title()
  }
  # Cas quanti/quanti → scatter + droite
  else {
    ggplot(data, aes(x = .data[[first_feature]], y = .data[[second_feature]])) +
      geom_point(color = "#C60800", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "#953553") +
      theme_classic() +
      labs(
        title = paste("Corrélation entre", first_feature, "et", second_feature),
        x = first_feature, y = second_feature
      ) +
      ggeasy::easy_center_title()
  }
}

# ----------------------------------------------------------
# 3) Tests statistiques selon types
# ----------------------------------------------------------
tab_test <- function(data, first_feature, second_feature) {
  req(data, first_feature, second_feature)

  # Cas quali/quali → Khi² + Phi + Cramer V
  if (!is.numeric(data[[first_feature]]) && !is.numeric(data[[second_feature]])) {
    f <- table(data[[first_feature]], data[[second_feature]])
    chi  <- suppressWarnings(chisq.test(f))
    cram <- rcompanion::cramerV(f)
    phi_v <- rcompanion::phi(f)
    return(data.frame(
      Test   = c("Khi²", "Phi", "Cramer V"),
      Valeur = round(c(chi$statistic, phi_v, cram), 3),
      p.value = c(round(chi$p.value, 4), NA, NA)
    ))
  }

  # Cas quanti/quanti → corrélation (Pearson)
  if (is.numeric(data[[first_feature]]) && is.numeric(data[[second_feature]])) {
    cor_val <- suppressWarnings(cor.test(data[[first_feature]], data[[second_feature]], method = "pearson"))
    return(data.frame(
      Test = "Corrélation de Pearson",
      Corrélation = round(cor_val$estimate, 3),
      p.value = round(cor_val$p.value, 4)
    ))
  }

  # Cas quali/quanti → ANOVA si normalité (approx) sinon Kruskal-Wallis
  if (xor(is.numeric(data[[first_feature]]), is.numeric(data[[second_feature]]))) {
    num_var <- ifelse(is.numeric(data[[first_feature]]), first_feature, second_feature)
    cat_var <- ifelse(!is.numeric(data[[first_feature]]), first_feature, second_feature)

    # Shapiro sur échantillon si dataset volumineux
    sh <- tryCatch(shapiro.test(sample(na.omit(data[[num_var]]), min(5000, sum(!is.na(data[[num_var]]))))),
                   error = function(e) NULL)

    if (!is.null(sh) && sh$p.value > 0.05) {
      # ANOVA
      fit <- aov( as.formula(paste0("`", num_var, "` ~ as.factor(`", cat_var, "`)")), data = data )
      p_val <- summary(fit)[[1]][["Pr(>F)"]][1]
      return(data.frame(Test = "ANOVA", p.value = round(p_val, 4)))
    } else {
      # Kruskal-Wallis
      kw <- kruskal.test( as.formula(paste0("`", num_var, "` ~ as.factor(`", cat_var, "`)")), data = data )
      return(data.frame(Test = "Kruskal-Wallis", p.value = round(kw$p.value, 4)))
    }
  }

  data.frame(Message = "Test non applicable.")
}
  