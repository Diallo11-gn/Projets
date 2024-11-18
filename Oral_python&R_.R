# Chargement des bibliothèques
library(tidyverse)
library(caret)
library(pROC)

# Importation des données
data <- read.csv("C:\\Users\\mamad\\Desktop\\UniStra\\M1\\S1\\Cours\\Python et R\\framingham.csv", header = TRUE, sep = ",")

#######################################################
#          Description des variables                  #
#######################################################

# Affichage des 5 premières lignes
head(data)

# Résumé des données
summary(data)

# Vérification s'il y a des valeurs manquantes
any(is.na(data))

# Conversion des variables catégorielles en facteur
data$education <- as.factor(data$education)
data$currentSmoker <- as.factor(data$currentSmoker)
data$BPMeds <- as.factor(data$BPMeds)

# Imputation des valeurs manquantes par la moyenne pour les variables numériques
data_imputed <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

#######################################################
#         Réprésentations graphiques                  #
#######################################################

# Graphique à barres groupées pour la répartition du tabagisme en fonction du sexe
table_data_smoker_sex <- table(data_imputed$currentSmoker, data_imputed$male)
barplot(table_data_smoker_sex, beside = TRUE, legend = c("Femme", "Homme"), 
        col = c("pink", "lightblue"), xlab = "Tabagisme", 
        ylab = "Nombre d'individus", main = "Répartition du Tabagisme en fonction du Sexe")

# Graphique à barres empilées pour la répartition de la maladie coronarienne sur 10 ans en fonction du tabagisme
table_data_CHD_smoker <- table(data_imputed$TenYearCHD, data_imputed$currentSmoker)
barplot(table_data_CHD_smoker, beside = TRUE, legend = c("Non Fumeur", "Fumeur"), 
        col = c("lightblue", "salmon"), xlab = "Maladie Coronarienne sur 10 ans", 
        ylab = "Nombre d'individus", main = "Répartition de la Maladie Coronarienne sur 10 ans en fonction du Tabagisme")

# Boxplot pour l'âge en fonction du tabagisme avec différenciation par sexe
boxplot(data_imputed$age ~ data_imputed$currentSmoker, col = ifelse(data_imputed$male == 1, "blue", "red"),
        xlab = "Tabagisme", ylab = "Âge", main = "Âge en fonction du Tabagisme")

######################################################
#        Modèle de régression logistique             #
######################################################

# Partition des données en data train et data test
set.seed(123)
splitIndex <- caTools::sample.split(data_imputed$TenYearCHD, SplitRatio = 0.8)
train_data <- subset(data_imputed, splitIndex == TRUE)
test_data <- subset(data_imputed, splitIndex == FALSE)

# Régression logistique
model <- glm(TenYearCHD ~ ., data = train_data, family = "binomial")

# Prédiction sur les données de test
predictions <- predict(model, newdata = test_data, type = "response")
predictions_class <- ifelse(predictions > 0.5, 1, 0)

# Calcul de la matrice de confusion
conf_matrix <- confusionMatrix(as.factor(predictions_class), as.factor(test_data$TenYearCHD))

# Calcul des métriques
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- conf_matrix$byClass["F1"]

# Affichage des résultats
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")

# Courbe ROC pour la régression logistique
roc_curve <- roc(test_data$TenYearCHD, predictions)
auc_value <- auc(roc_curve)
plot(roc_curve, main = "Courbe ROC", col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lty = 1)
