#####################################################################
###          Analyse de données pour le papier APC Rings          ###
#####################################################################
rm(list = ls()) #supprimer tous les objets 

### Chargement des packages ----

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(openxlsx)
library(readxl)

### Lecture des données ----

row_data <- read_excel("~/Documents/APC Jaime Texiera/row_data.xlsx")
df <- read_excel("~/Documents/APC Jaime Texiera/data_roles.xlsx")



# Diviser les chaînes de caractères en vecteurs
df <- df %>% 
  separate_rows(Roles, sep = "\\s*,\\s*")

# Grouper le dataframe par "authors" et "DOI" et compter le nombre de "Roles" pour chaque groupe
result <- df %>%
  group_by(authors, DOI) %>%
  summarize(num_roles = n())


# Effectuer un left join entre result et df en utilisant le DOI comme clé
merged_df <- merge(df, result, by = "DOI", all.x = TRUE, all.y = FALSE)


# suspects papers


# Sélectionner les auteurs qui ont num_roles < 3 et "Funding acquisition"
filtered_df <- merged_df %>%
  filter(authors_num < 3, Roles == "Funding acquisition")

suspects_authors <- filtered_df %>%
  select(authors, DOI, num_roles)

