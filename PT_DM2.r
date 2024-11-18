###### DM 2 #############
library(tidyverse)
library(ggplot2)

# Importation des données dm
dm <- read.csv("C:/Users/Utilisateur/Desktop/Données R proj tuto 1/dm.csv", header = T, dec = ",")
dm_2_ <- read.csv("C:/Users/Utilisateur/Desktop/Données R proj tuto 1/dm (2).csv", header = T, dec = ",")

# Jointure
tabledm <- bind_rows(select(dm, USUBJID, STUDYID, ARM), select(dm_2_, USUBJID, STUDYID, ARM)) %>%
  filter(ARM != "SCREEN FAILURE")
head(tabledm)

# Importation des données ds
spds <- read.csv("C:/Users/Utilisateur/Desktop/Données R proj tuto 1/suppds.csv", header = T, dec = ",")
spds_2_ <- read.csv("C:/Users/Utilisateur/Desktop/Données R proj tuto 1/suppds (2).csv", header = T, dec = ",")

# Jointure
tableds <- bind_rows(
  select(spds, USUBJID, STUDYID, IDVARVAL, QVAL),
  select(spds_2_, USUBJID, STUDYID, IDVARVAL, QVAL)
) %>%
  filter(str_detect(IDVARVAL, 'STUDY DAY')) 

head(tableds)

# Jointure
data <- left_join(tabledm, tableds, by = c("USUBJID", "STUDYID")) %>%
  select(-IDVARVAL)

head(data)

# Construction de la matrice vis

# Initialisation de la matrice
vis <- matrix(data = 0, nrow = nrow(data), ncol = 14)

# Boucle while
i <- 1
while (i <= nrow(data)) {
  if (is.na(data$QVAL[i])) {
    vis[i,] <- rep(NA, 14)
  } else {
    vis[i, 1:data$QVAL[i]] <- 1
  }
  i <- i + 1
}

# Conversion en data frame
vis <- as.data.frame(vis)
colnames(vis) <- paste("Visite", str_pad(1:14, width = 2, pad = 0), sep = '')
rownames(vis) <- data$USUBJID

head(vis)

# Combinaison des données
dataFigLong <- bind_cols(data, vis) %>%
  pivot_longer(cols = starts_with("Visite"), names_to = "VISITE", values_to = "RETAINED")

# Résultats
res <- dataFigLong %>%
  na.omit() %>%
  group_by(VISITE, STUDYID, ARM) %>%
  summarise(
    n = n(),
    n_pres = sum(RETAINED, na.rm = TRUE),
    prop = n_pres / n
  ) %>%
  as.data.frame()

head(res)

# Création du graphique
ggplot(res, aes(x = VISITE, y = prop, color = ARM, linetype = as.factor(STUDYID), group = interaction(STUDYID, ARM))) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion de rétention au fil du temps",
       x = "Visite",
       y = "Proportion de rétention") +
  scale_color_manual(values = c("red", "blue"), name = "") + 
  scale_linetype_discrete(name = "") +  
    theme_minimal()


