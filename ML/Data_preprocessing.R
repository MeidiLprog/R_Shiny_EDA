# Importer dataset
df <- read_csv("Data/Inflammation.csv")

# Encodage des variables qualitatives

qualitative_var <- c("Occurrence of nausea", "Lumbar pain", "Urine pushing", "Micturition pains",
                     "Burning of urethra", "Inflammation of urinary bladder")

for (q in qualitative_var) {
  df[[q]] = factor(df[[q]],
                  levels = c('no','yes'),
                  labels = c(0, 1)) 
}

# Vérification des outliers 

boxplot(df['Temperature of patient'])

# Vérification des valeurs manquantes

print(apply(df, 2, anyNA))

#Shapiro-Wilk normality test pour choisir entre la normalisation et la standarisation de la variable 'Temperature of patient' 

shapiro.test(df[['Temperature of patient']])

#p-value = 9.702e-08 < 0.05 donc la variable ne suit pas la loi normale donc on choisit la normalisation

# Normalisation de la variable 'Temperature of patient'

process <- preProcess(as.data.frame(df[['Temperature of patient']])
                      , method=c("range"))
df['Temperature of patient'] <- predict(process
                      , as.data.frame(df[['Temperature of patient']]))

# Vérification du déséquilibre des classes

a <- table(df['Inflammation of urinary bladder'])
lbls <- paste(names(a), "\n", a, sep="")
pie(a, labels = lbls,
    main="Diagramme circulaire de l'inflammation de la vessie ")

# Les deux classes sont équilirées

# Exportation de la nouvelle base de données prétraitée "Data_encoded"

write.csv(df,"Data\\Data_encoded.csv", row.names = FALSE)
