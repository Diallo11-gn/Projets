# Chargement des bibliothèques
library(ggplot2)
library(tidyverse)
library(caret)
library(Metrics)

# Chargement des données
data_tot <- readRDS("C:/Users/mamad/Desktop/UniStra/M1/S2/Tech en sciences de données/Projets/data.rds")

# Sélection des variables pertinentes pour la prédiction de la durée de séjour
data_t2 <- data_tot %>% select("hospital_los_day", "icu_los_day", "service_num", "abg_count", "wbc_first",
                               "hgb_first", "platelet_first", "sodium_first", "hosp_exp_flg",
                               "potassium_first", "tco2_first", "chloride_first", "bun_first",
                               "creatinine_first", "po2_first", "pco2_first", "age", "weight_first", "bmi", "day_28_flg")

# Diviser le jeu de données en ensembles d'entraînement et de test
set.seed(45)
indexes <- sample(1:dim(data_t2)[1], 600, replace = FALSE)
data_train2 <- data_t2[indexes, ]
data_test2 <- data_t2[-indexes, ]

# Modèle de régression linéaire pour prédire la durée de séjour à l'hôpital
lm_model_hospital_los <- lm(hospital_los_day ~ ., data = data_train2)
summary(lm_model_hospital_los)

# Prédire sur le jeu de test avec le modèle de régression linéaire
pred_test_hospital_los <- predict(lm_model_hospital_los, newdata = data_test2)
rmse_test_hospital_los <- rmse(pred_test_hospital_los, data_test2$hospital_los_day)
print(paste("RMSE pour la durée de séjour à l'hôpital sur le jeu de test:", rmse_test_hospital_los))

# Modèle de régression logistique pour prédire day_28_flg
logistic_model_day_28 <- glm(day_28_flg ~ ., data = data_train2, family = "binomial")
summary(logistic_model_day_28)

# Prédire sur le jeu de test avec le modèle de régression logistique
pred_test_day_28 <- predict(logistic_model_day_28, newdata = data_test2, type = "response") > 0.5
conf_matrix_day_28 <- table(pred_test_day_28, data_test2$day_28_flg)
accuracy_day_28 <- sum(diag(conf_matrix_day_28)) / sum(conf_matrix_day_28)
print(paste("Accuracy pour day_28_flg sur le jeu de test:", accuracy_day_28))
