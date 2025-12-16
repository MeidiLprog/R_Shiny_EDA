# ==========================================================
# ANALYSE UNIVARIÉE — version dynamique
# ==========================================================

# ----------------------------------------------------------
# Statistiques descriptives d'une variable
# ----------------------------------------------------------
statQ <- function(data, var) {
  req(data, var)
  
  # Si variable numérique
  if (is.numeric(data[[var]])) {
    temp <- data.frame(
      Statistique = c("Min", "Q1", "Médiane", "Q3", "Max", "Moyenne", "Écart-type"),
      Valeur = c(
        min(data[[var]], na.rm = TRUE),
        quantile(data[[var]], 0.25, na.rm = TRUE),
        median(data[[var]], na.rm = TRUE),
        quantile(data[[var]], 0.75, na.rm = TRUE),
        max(data[[var]], na.rm = TRUE),
        mean(data[[var]], na.rm = TRUE),
        sd(data[[var]], na.rm = TRUE)
      )
    )
    return(temp)
  }
  
  # Si variable qualitative
  else {
    temp <- as.data.frame(table(data[[var]]))
    colnames(temp) <- c("Modalité", "Effectif")
    temp$`Fréquence (%)` <- round(temp$Effectif / sum(temp$Effectif) * 100, 2)
    temp$`Effectif cumulatif` <- cumsum(temp$Effectif)
    temp$`Fréquence cumulée (%)` <- cumsum(temp$`Fréquence (%)`)
    return(temp)
  }
}

# ----------------------------------------------------------
# Boxplot (numérique) ou diagramme en barres (qualitatif)
# ----------------------------------------------------------
plot_box_bar <- function(data, var) {
  req(data, var)
  
  if (is.numeric(data[[var]])) {
    ggplot(data, aes(x = "", y = .data[[var]])) +
      geom_boxplot(fill = "#C6080022", color = "#C60800") +
      labs(title = paste("Boîte à moustaches de", var),
           x = "", y = "Valeur") +
      ggeasy::easy_center_title()
  } else {
    ggplot(data, aes(x = .data[[var]], fill = .data[[var]])) +
      geom_bar(color = "#C60800", fill = "#C6080022") +
      theme_classic() +
      labs(
        title = paste("Diagramme en barres de", var),
        x = var, y = "Fréquence"
      ) +
      ggeasy::easy_center_title()
  }
}

# ----------------------------------------------------------
# Histogramme (numérique) ou diagramme circulaire (qualitatif)
# ----------------------------------------------------------
plot_histo_pie <- function(data, var) {
  req(data, var)
  
  if (is.numeric(data[[var]])) {
    ggplot(data, aes(x = .data[[var]])) +
      geom_histogram(aes(y = ..density..),
                     color = "#C60800", fill = "#C6080022", bins = 20) +
      geom_density(color = "#C60800", lwd = 1, alpha = 0.3) +
      labs(title = paste("Histogramme de", var), x = var, y = "Densité") +
      ggeasy::easy_center_title()
  } else {
    temp <- as.data.frame(table(data[[var]]))
    colnames(temp) <- c("Modalité", "Effectif")
    ggplot(temp, aes(x = "", y = Effectif, fill = Modalité)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = paste("Diagramme circulaire de", var)) +
      scale_fill_manual(values = c("#953553", "#FF8A8A")) +
      ggeasy::easy_center_title()
  }
}

# ----------------------------------------------------------
# Courbe cumulative (pour les variables numériques)
# ----------------------------------------------------------
plot_Cummul <- function(data, var) {
  req(data, var)
  
  if (is.numeric(data[[var]])) {
    tmp.hist <- hist(data[[var]], plot = FALSE, right = FALSE)
    fig <- plot_ly(
      x = tmp.hist$breaks[-1],
      y = cumsum(tmp.hist$counts),
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#C60800"),
      marker = list(color = "black")
    )
    fig <- fig %>% layout(
      title = paste("Courbe cumulative de", var),
      xaxis = list(title = var),
      yaxis = list(title = "Effectifs cumulés")
    )
    return(fig)
  } else {
    return(NULL)
  }
}
