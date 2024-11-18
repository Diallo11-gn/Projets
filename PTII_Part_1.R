# Importation des packages
library(caTools)
library(ROCR)
library(pROC)
library(dplyr)
library(caret)
library(randomForest)
library(class)

# Importation des données
data <- read.csv('C:\\Users\\mamad\\Desktop\\UniStra\\M1\\S2\\Projet Tutoré II\\Projets\\dataPT2.csv\\dataPT2.csv', header = TRUE, sep = ';', dec = ',')

# Sélection des variables 
dat1 <- data %>% 
  select(-c(apache_4a_hospital_death_prob, apache_4a_icu_death_prob, apache_2_bodysystem,apache_3j_bodysystem))

# Conversion des variables catégorielles en facteur
dat1$ethnicity <- as.factor(dat1$ethnicity)
dat1$gender <- as.factor(dat1$gender)
dat1$icu_admit_source <- as.factor(dat1$icu_admit_source)
dat1$icu_stay_type <- as.factor(dat1$icu_stay_type)
dat1$icu_type <- as.factor(dat1$icu_type)
dat1$hospital_death <- as.factor(dat1$hospital_death)

# Imputation des valeurs manquantes par la moyenne
dat1_imputed <- dat1 %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Conversion des colonnes numériques
dat1_imputed <- dat1_imputed %>% 
  mutate_if(is.numeric, as.numeric)

# Partition en data train et data test
set.seed(123)
splitIndex <- caTools::sample.split(dat1_imputed$hospital_death, SplitRatio = 0.8)
train_data <- subset(dat1_imputed, splitIndex == TRUE)
test_data <- subset(dat1_imputed, splitIndex == FALSE)

# Regression logistique
model <- glm(hospital_death ~ ., data = train_data, family = "binomial")

# Prédiction sur les données de test
predictions <- predict(model, newdata = test_data, type = "response")
predictions_class <- ifelse(predictions > 0.5, 1, 0)

# Matrice de confusion
conf_matrix <- confusionMatrix(as.factor(predictions_class), as.factor(test_data$hospital_death))

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
roc_curve <- roc(test_data$hospital_death, predictions)
auc_value <- auc(roc_curve)
cat("Logistic Regression AUC Value:", auc_value, "\n")

# Random Forest
rf_model <- randomForest(hospital_death ~ ., data = train_data, ntree = 100)
rf_predictions <- as.numeric(predict(rf_model, newdata = test_data, type = "response"))
rf_roc_curve <- roc(ifelse(test_data$hospital_death == 1, 1, 0), rf_predictions)
rf_auc_value <- auc(rf_roc_curve)
cat("Random Forest AUC Value:", rf_auc_value, "\n")

# k-NN
# Remplacer les valeurs manquantes par la moyenne
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# Sélectionner uniquement les colonnes numériques
num_train_data <- train_data[sapply(train_data, is.numeric)]
num_test_data <- test_data[sapply(test_data, is.numeric)]

# Appliquer le modèle k-NN sur les données numériques
knn_model <- knn(num_train_data, num_test_data, train_data$hospital_death, k = 5)
knn_roc_curve <- roc(test_data$hospital_death, as.numeric(knn_model))
knn_auc_value <- auc(knn_roc_curve)
cat("k-NN AUC Value:", knn_auc_value, "\n")

# Tracer la courbe ROC pour tous les modèles
plot(roc_curve, main = "ROC Curve Comparison", col = "blue", lwd = 2)
lines(rf_roc_curve, col = "red", lwd = 2)
lines(knn_roc_curve, col = "green", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Random Forest", "k-NN"),
       col = c("blue", "red", "green"), lwd = 2)

# ACP
# Sélectionnez uniquement les colonnes numériques pour l'ACP
num_train_data_acp <- train_data[sapply(train_data, is.numeric)]
num_test_data_acp <- test_data[sapply(test_data, is.numeric)]

# Combinez les données d'entraînement et de test pour l'ACP
all_data_acp <- rbind(num_train_data_acp, num_test_data_acp)

# Effectuez l'ACP
acp_result <- prcomp(all_data_acp, scale. = TRUE)

# Résumé des résultats
summary(acp_result)

# Affichez les vecteurs propres (loadings) pour chaque composante
print(acp_result$rotation)

# Tracez le graphique de variance expliquée
plot(acp_result, type = "l")

##############################################################

"""# Importation des données
validation_data <- read.csv('C:\\Users\\mamad\\Desktop\\UniStra\\M1\\S2\\Projet Tutoré II\\Projets\\jeu2validation.csv', header = TRUE, sep = ';', dec = ',')

#Selection et conversion des variables 
validation_data <- validation_data %>% 
  select(-c(apache_4a_hospital_death_prob, apache_4a_icu_death_prob, apache_2_bodysystem, apache_3j_bodysystem))

validation_data$ethnicity <- as.factor(validation_data$ethnicity)
validation_data$gender <- as.factor(validation_data$gender)
validation_data$icu_admit_source <- as.factor(validation_data$icu_admit_source)
validation_data$icu_stay_type <- as.factor(validation_data$icu_stay_type)
validation_data$icu_type <- as.factor(validation_data$icu_type)

#Imputation des valeurs manquantes 
validation_data_imputed <- validation_data %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

validation_data_imputed <- validation_data_imputed %>% 
  mutate_if(is.numeric, as.numeric)

# Pour k-NN
validation_data_imputed <- na.omit(validation_data_imputed)
num_validation_data <- validation_data_imputed[sapply(validation_data_imputed, is.numeric)]

#Prédictions pour chaque modele 

# Regression logistique
predictions_logistic <- predict(model, newdata = validation_data_imputed, type = "response")

# Random Forest
rf_predictions <- as.numeric(predict(rf_model, newdata = validation_data_imputed, type = "response"))

# k-NN
# Convertir la variable hospital_death en numérique
train_data$hospital_death <- as.numeric(as.character(train_data$hospital_death)) - 1

# Appliquer le modèle k-NN sur les données numériques
knn_model <- as.numeric(knn(num_train_data, num_validation_data, train_data$hospital_death, k = 5))

# Ajuster les prédictions k-NN pour les rendre binaires (0 ou 1)
kNN_Prediction <- ifelse(knn_model > 0.5, 1, 0)

# Tableau avec identifiant du patient et les prédictions
patient_predictions <- data.frame(
  Patient_ID = validation_data$patient_id,  
  Logistic_Prediction = ifelse(predictions_logistic > 0.5, 1, 0),
  RF_Prediction = ifelse(rf_predictions > 0.5, 1, 0),
  kNN_Prediction = kNN_Prediction
)

# Afficher le tableau
print(patient_predictions)


# Créer un dataframe avec les prédictions
patient_predictions <- data.frame(
  Patient_ID = validation_data$patient_id,  
  Prediction = ifelse(predictions_logistic > 0.5, 1, 0) + ifelse(rf_predictions > 0.5, 1, 0)
)

patient_predictions

# Exporter le dataframe en fichier CSV avec des virgules comme séparateurs
#write.csv(patient_predictions, file = "C:/Users/mamad/Desktop/validation.csv", row.names = FALSE, sep = ',') """

########################################################
########################################################

#Distance euclidienne entre les vecteurs (DCR)

data_a = c(rnorm(10, mean=1:10))
data_b = c(rnorm(10, mean=1:10))

sqrt(sum((data_a - data_b)**2))

calDist <- function(x,y){
  if(length(x)==length(y)){
    res <- sqrt(sum((x-y)**2))
  }else{
    cat('vecteurs de tailles différentes\n')
    res <- NA
  }
  return(res)
}
calDist(x = data_a, y=data_b)


dist
dtf <- data.frame(data_a, data_b)
dist (t(dtf), method = "euclidian")

#Calcul NNDR

data_a = c(rnorm(10, mean=1:10))
data_b = c(rnorm(10, mean=1:10))
data_c = c(rnorm(10, mean=2:11))

calcNNDR <- function (x,y,z) {
  #x point ref
  #y premier voisin
  #z second voisin
  return(calDist(x = x, y = y) / calDist(x = x, y = z))
}

calcNNDR (x=data_a,
          y=data_b,
          z=data_c)

#Lorsqu'il est proche de 1, on considère que c'est bien (selon l'etude) !
 
