# A faire

# Trouver les meilleures régressions  pour prédire hospital_los_day: length of stay in hospital (days, numeric)
#et  day_28_flg: death within 28 days (binary: 1 = yes, 0 = no)




data_tot<- readRDS("data.rds")

# informations sur la base : 

### https://physionet.org/content/mimic2-iaccd/1.0/

## Chargement des données
library(ggplot2)
library(tidyverse)
library(class)





data_tot1<-data_tot%>%select("hosp_exp_flg","censor_flg","sepsis_flg","chf_flg","afib_flg","renal_flg","liver_flg","copd_flg","cad_flg","aline_flg","icu_los_day","hospital_los_day","age","gender_num","icu_exp_flg","day_28_flg","service_num","mort_day_censored","day_icu_intime_num")

data_tot1$hosp_exp_flg<- as.factor(data_tot1$hosp_exp_flg)
set.seed(45)

sample_train<-sample(1:dim(data_tot1)[1],600,replace = F)
data_train <- data_tot1[sample_train,]
data_test <- data_tot1[-sample_train,]

prop.table(table(data_tot1$hosp_exp_flg))
prop.table(table(data_train$hosp_exp_flg))
prop.table(table(data_test$hosp_exp_flg))

# Test de l'entraînement des modèles sur un fold

folds <- cut(seq(1,nrow(data_train)),breaks=10,labels=FALSE)



fit<- glm(hosp_exp_flg ~ aline_flg + censor_flg + sepsis_flg + chf_flg + afib_flg + renal_flg + liver_flg + copd_flg + cad_flg + icu_los_day + age + gender_num + icu_exp_flg + day_28_flg, data = data_train[!folds==1,], family = "binomial")
summary(fit)


predictions<- predict(fit,data_train[!folds==1,],type = "response")
##Seuil à 0.5
table_predictions <- table(predictions>0.5, data_train$hosp_exp_flg[!folds==1] )
print(table_predictions)


acc= (table_predictions[1,1] + table_predictions[2,2]) / sum(table_predictions)
rappel = table_predictions[2,2] / sum(table_predictions[2,])
precision = table_predictions[2,2] / sum(table_predictions[,2])
sensibilite = rappel
specificite = table_predictions[1,1] / sum(table_predictions[1,])
vpn = table_predictions[1,1] / sum(table_predictions[1,])
F1 =  2 * (precision * rappel) / (precision + rappel)
print(F1)

# Cross validation du meilleur seuil glm  : 

seuils <- seq(0.1, 0.9, by = 0.1)
res_data <- data.frame()
meilleur_F1 <- -Inf
meilleur_tableau_contingence <- NULL

for (i in folds) {
  for (seuil in seuils) {
    fit_i <- glm(hosp_exp_flg ~ aline_flg + censor_flg + sepsis_flg + chf_flg + afib_flg + renal_flg + liver_flg + copd_flg + cad_flg + icu_los_day + age + gender_num + icu_exp_flg + day_28_flg, data = data_train[!folds == i,], family = "binomial")
    predictions_i <- predict(fit_i, data_train[!folds == i,], type = "response")
    
    tableau_contingence <- table(predictions_i > seuil, data_train$hosp_exp_flg[!folds==i])
    
    tp <- tableau_contingence[2, 2]  # True Positives
    fp <- tableau_contingence[1, 2]  # False Positives
    tn <- tableau_contingence[1, 1]  # True Negatives
    fn <- tableau_contingence[2, 1]  # False Negatives
    
    precision <- tp / (tp + fp)
    rappel <- tp / (tp + fn)
    F1 =  2 * (precision * rappel) / (precision + rappel)
    
    if (F1 > meilleur_F1) {
      meilleur_F1 <- F1
      meilleur_tableau_contingence <- tableau_contingence
      
      res_data <- data.frame(seuil = seuil, F1 = F1)
    }
  }
}

fit_final <- glm(hosp_exp_flg ~ aline_flg + censor_flg + sepsis_flg + chf_flg + afib_flg + renal_flg + liver_flg + copd_flg + cad_flg + icu_los_day + age + gender_num + icu_exp_flg + day_28_flg, data = data_train, family = "binomial")
predictions_final <- predict(fit_final, data_test, type = "response")

seuil_final <- res_data$seuil
predictions_binary <- predictions_final > seuil_final
tableau_contingence_final <- table(predictions_binary, data_test$hosp_exp_flg)
print(tableau_contingence_final)
tp_final <- tableau_contingence_final[2, 2]  # True Positives
fp_final <- tableau_contingence_final[1, 2]  # False Positives
tn_final <- tableau_contingence_final[1, 1]  # True Negatives
fn_final <- tableau_contingence_final[2, 1]  # False Negatives

precision_final <- tp_final / (tp_final + fp_final)
rappel_final <- tp_final / (tp_final + fn_final)
F1_final =  2 * (precision_final * rappel_final) / (precision_final + rappel_final)
print(F1_final)


## Choix des variables à mettre dans me modèle de prédiction 


# Même chose pour prédire la durée de séjour


library(caret)


install.packages("Metrics")
# Charger la bibliothèque Metrics
library(Metrics)

# Sélectionner les variables nécessaires
data_tot2 <- data_tot %>% select("hospital_los_day","icu_los_day", "service_num", "abg_count", "wbc_first", "hgb_first", "platelet_first", "sodium_first", "hosp_exp_flg", "potassium_first", "tco2_first", "chloride_first", "bun_first", "creatinine_first", "po2_first", "pco2_first", "age", "weight_first", "bmi")

# Diviser le jeu de données en ensembles d'entraînement et de test
set.seed(45)
sample_train <- sample(1:dim(data_tot2)[1], 600, replace = FALSE)
data_train2 <- data_tot2[sample_train, ]
data_test2 <- data_tot2[-sample_train, ]

# Validation croisée à 10 plis avec caret
ctrl <- trainControl(method = "cv", number = 10)

# Créer le modèle avec validation croisée
lm_model_cv <- train(
  as.formula(paste("hospital_los_day ~", paste(c("icu_los_day","service_num","abg_count","wbc_first","hgb_first", "platelet_first", "sodium_first","hosp_exp_flg","potassium_first","tco2_first","chloride_first","bun_first","creatinine_first","po2_first","pco2_first","age", "weight_first", "bmi"), collapse = " + "))),
  data = data_train2,
  method = "lm",
  trControl = ctrl,
  metric = "MAE"
)

# Afficher les résultats de la validation croisée
print(lm_model_cv)

# Prédire sur le jeu de test avec le modèle de validation croisée
predictions_test_cv <- predict(lm_model_cv, newdata = data_test2)
mae_test_cv <- mae(predictions_test_cv, data_test2$hospital_los_day)
print(paste("MAE avec validation croisée sur le jeu de test:", mae_test_cv))

# Afficher le graphique de régression avec MAE
library(ggplot2)

# Créer un data frame avec les prédictions et les valeurs réelles sur le jeu de test
plot_data <- data.frame(
  Predictions = predictions_test_cv,
  Actual = data_test2$hospital_los_day
)

# Tracer le graphique
ggplot(plot_data, aes(x = Actual, y = Predictions)) +
  geom_point(alpha = 0.7) +  # Afficher les points de données avec une légère transparence
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  ggtitle("Graphique de régression avec MAE") +
  xlab("Valeurs réelles (Actual)") +
  ylab("Prédictions (Predictions)") +
  theme_minimal()




# Bonus essayer de standardiser les variables quantitatives pour voir si ça améliore les résultats
https://www.statsoft.fr/concepts-statistiques/glossaire/c/centrer.html

