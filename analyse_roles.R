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
library(openalexR)
library(purrr)
library(httr)
library(jsonlite)
library(progressr)

### Lecture des données ----

row_data <- read_excel("~/Documents/APC Jaime Texiera/row_data.xlsx")
data_roles <- read_excel("~/Documents/APC Jaime Texiera/data_roles.xlsx")

# Utilisez la fonction read.table() pour lire le fichier TSV - base SciSciNet juin 2023.
# data_pub <- read_tsv("~/Documents/SciSciNet/SciSciNet_Papers.tsv")

# Garder uniquement les DOI de notre échantillon PlosOne

# Trier le dataframe par "DOI" et "authors_num" pour garantir l'ordre correct
data_roles <- data_roles %>% arrange(DOI, authors_num)

# Créer la nouvelle colonne "roles_corr" en utilisant la fonction lag() et ifelse()
data_roles <- data_roles %>% 
  group_by(DOI) %>% 
  mutate(roles_corr = ifelse(authors_num == 1, "Writing – original draft", lag(Roles))) %>% 
  ungroup()


# Diviser les chaînes de caractères en vecteurs
df <- data_roles %>% 
  separate_rows(roles_corr, sep = "\\s*,\\s*")

# Grouper le dataframe par "authors" et "DOI" et compter le nombre de "Roles" pour chaque groupe
result <- df %>%
  select(authors, DOI, roles_corr) %>%
  group_by(authors, DOI) %>%
  summarize(num_roles = n())


# Effectuer un left join entre result et df en utilisant le DOI comme clé
merged_df <- merge(df, result, by = c("DOI","authors"), all.x = TRUE)

# Selection variables d'intérêt
data_roles <- merged_df %>%
  select(DOI, authors, authors_num, roles_corr, num_roles, funding) 
# Enregistrer ce resultat
write.xlsx(data_roles, "~/Documents/APC Jaime Texiera/data_roles.xlsx")
# Supprimer tables inutiles
rm(df, merged_df, result)

# suspects papers
# Sélectionner les auteurs qui ont num_roles < 3 et "Funding acquisition"
suspects_authors <- data_roles %>%
  filter(num_roles < 2, roles_corr == "Funding acquisition") %>%
  select(authors, DOI, authors_num, num_roles, roles_corr)
write.xlsx(suspects_authors, "~/Documents/APC Jaime Texiera/suspects_authors.xlsx")

#########################################
## Ajouter l'affiliation des auteurs ----
# Remarque importante : Récupérer dans Git les commits avant le 3 aout 2023 pour 
# avoir le script de l'extraction à partir du site de PLOS ONE. 
# Je n'ai gardé dans cette nouvelle version que l'extraction à partir de OpenaAlex 
# car le parsing ne marche pas très bien en faisant du sccraping
#########################################

# Au fait, il y a 3698 DOI qui sautent parce qu'il s'agit en fait de corrections et non des articles originaux. 
# Ce sont des "Corrections" d'autres articles de PLOS.
# Nb : ils sautent parce qu'ils n'ont pas la page "adresse" dans le code source. 
# a <- as.character(df_doi_aff$DOI) %>%
#   unique() 
# a <- subset(df_aff, !DOI %in% a)

# Extraire l'annee et autre metadonées de OpenAlex à partir de la liste des DOI

###################################################################
### !! à exécuter une fois, puis charger le fichier en local !! ###
# Obtenir la liste unique des DOI à partir de df_doi_aff
dois <- unique(df_doi_aff$DOI)

# Fonction pour récupérer les données à partir d'un DOI en toute sécurité
get_data_safe <- safely(function(doi) {
  url <- paste0("https://api.openalex.org/works?filter=doi%3A", doi)
  response <- GET(url)
  
  if (http_type(response) == "application/json") {
    data <- fromJSON(content(response, "text"))
    return(data)
  } else {
    message(paste0("Error: Failed to retrieve data for DOI ", doi))
    return(NULL)
  }
})

# Récupérer les données pour chaque DOI avec affichage de la progression
all_data <- imap(dois, ~{
  data <- get_data_safe(.x)$result
  
  # Affichage de la progression
  cat("Progress:", .y, "out of", length(dois), "\n")
  
  return(data)
})
###################################################################
####           Nom du fichier de sauvegarde.                 ######
nom_fichier <- "~/Documents/APC Jaime Texiera/openalex.rds"
###################################################################
# lire le fichier sauvegardé
openalex <- readRDS(nom_fichier)


###################################################################
# Remarque : comme le fichier "openalex" fait 8.3Go, il est difficile de le manier avec mon orinateur en loclal
#            j'ai donc opté pour des extraction par bouts des informations nécessaires pour l'analyse.
 
# Utiliser map_dfr pour convertir et combiner les données
# openalex[[1]][["results"]][["publication_date"]]
openalex_doi <- map_dfr(openalex[1:91626], ~as.data.frame(.x[["results"]][["doi"]]))
names(openalex_doi) <- "DOI"
openalex_date <- map_dfr(openalex[1:91626], ~as.data.frame(.x[["results"]][["publication_date"]]))
names(openalex_date) <- "date"
openalex_year <- map_dfr(openalex[1:91626], ~as.data.frame(.x[["results"]][["publication_year"]]))
names(openalex_year) <- "annee"
openalex_retrac <- map_dfr(openalex[1:91626], ~as.data.frame(.x[["results"]][["is_retracted"]]))
names(openalex_retrac) <- "is_retracted"
openalex_type <- map_dfr(openalex[1:91626], ~as.data.frame(.x[["results"]][["type"]]))
names(openalex_type) <- "type"
openalex_id <- map_dfr(openalex[1:91626], ~as.data.frame(.x[["results"]][["id"]]))
names(openalex_id) <- "id"

# Fusionner les dataframes en utilisant les noms de lignes (approche Reduce() avec merge())
data_openalex_date <- merge(openalex_doi, openalex_date, by = "row.names")
write.xlsx(data_openalex_date, "~/Documents/APC Jaime Texiera/data_openalex_date.xlsx" , all = TRUE)

data_openalex_id <- merge(openalex_id, openalex_date, by = "row.names")
write.xlsx(data_openalex_id, "~/Documents/APC Jaime Texiera/data_openalex_id.xlsx" , all = TRUE)

data_openalex_py <- merge(openalex_doi, openalex_year, by = "row.names")
write.xlsx(openalex_year, "~/Documents/APC Jaime Texiera/data_openalex_py.xlsx" , all = TRUE)

data_openalex_type <- merge(openalex_doi, openalex_type, by = "row.names")
write.xlsx(data_openalex_type, "~/Documents/APC Jaime Texiera/data_openalex_type.xlsx" , all = TRUE)
###################################################################

# Créer une liste vide pour stocker les éléments extraits
extracted_elements <- list()
# Boucle pour extraire les éléments de 1 à 20
for (i in 1:91626) {
  raw_affiliation_string <- openalex[[i]][["results"]][["authorships"]][[1]][["raw_affiliation_string"]]
  extracted_elements[[i]] <- raw_affiliation_string
}
# Créer un dataframe avec deux colonnes : id et affiliation
df <- data.frame(id = integer(),
                 affiliation = character(),
                 stringsAsFactors = FALSE)
# Remplir le dataframe avec les éléments extraits
for (i in 1:length(extracted_elements)) {
  df <- df %>% 
    add_row(id = i, affiliation = extracted_elements[[i]])
}
df_aff_openalex <- df
write.xlsx(df_aff_openalex, "~/Documents/APC Jaime Texiera/data_institutions_openalex.xlsx" , all = TRUE)

###################################################################
##### Pour les auteurs :
library(tidyr)

# Créer des listes vides pour stocker les éléments extraits
extracted_affiliations <- list()
extracted_ids <- list()
extracted_authors <- list()
extracted_authors_id <- list()

# Boucle pour extraire les éléments
for (i in 1:91626) {
  raw_affiliation_string <- openalex[[i]][["results"]][["authorships"]][[1]][["raw_affiliation_string"]]
  extracted_affiliations[[i]] <- raw_affiliation_string
  
  ids <- openalex[[i]][["results"]][["ids"]]
  extracted_ids[[i]] <- paste(ids, collapse = ", ")
  
  authors <- openalex[[i]][["results"]][["authorships"]][[1]][["author"]][["display_name"]]
  extracted_authors[[i]] <- authors
  
  authors_id <- openalex[[i]][["results"]][["authorships"]][[1]][["author"]][["id"]]
  extracted_authors_id[[i]] <- authors
}

# Créer un dataframe avec plusieurs colonnes : id, affiliation, ids et auteurs
df <- data.frame(id = integer(),
                 affiliation = character(),
                 ids = character(),
                 auteurs = character(),  # Nouvelle colonne pour les auteurs
                 auteurs_id = character(),  # Nouvelle colonne pour les id des auteurs 
                 stringsAsFactors = FALSE)

# Remplir le dataframe avec les éléments extraits
for (i in 1:length(extracted_affiliations)) {
  df <- df %>% 
    add_row(id = i,
            affiliation = extracted_affiliations[[i]],
            ids = extracted_ids[[i]],
            auteurs = paste(extracted_authors[[i]], collapse = ", "),
            auteurs_id = paste(extracted_authors_id[[i]], collapse = ", "))
}

# Séparer la colonne auteurs en plusieurs lignes
df_aut <- df %>%
  select(id, auteurs) %>%
  separate_rows(auteurs, sep = ", ") %>%
  unique()
# Nombre d'auteurs identifiés par id
nb_aut_id <- df_aut %>%
  group_by(id) %>%
  count()
names(nb_aut_id) <- c("id", "nb_auteurs")
# Nombre d'adresses identifiées par id
nb_aff_id <- df %>%
  select(id, affiliation) %>%
  group_by(id) %>%
  count()
names(nb_aut_id) <- c("id", "nb_aff")

# Ne garder que les publications dont la liste des affiliations par ligne concorde au nombre d'auteurs
diff_nb_aut_aff <- merge(nb_aut_id, nb_aff_id, by = "id") %>%
  mutate(diff = nb_aff-n) %>%
  filter(diff==0)

# Faire la jointure
# df auteurs
data_aut <- df_aut %>%
  filter(id %in% diff_nb_aut_aff$id)
# df affiliations
data_aff <- df %>%
  filter(id %in% diff_nb_aut_aff$id)

data <- merge(data_aff, data_aut, by = 0, all = TRUE) %>%
  select(-Row.names) %>%
  separate(ids, into = paste0("id_", 1:5), sep = ", ", fill = "right") %>%
  filter(!is.na(affiliation) & affiliation != "") # Affiliations non disponibles dans les données d'openalex
  
write.xlsx(data, "~/Documents/APC Jaime Texiera/data_aff_openalex.xlsx" , all = TRUE)

###################################################################
# Éclater la colonne "affiliation" au niveau de ";", en supprimant les entrées vides
df_aut_openalex <- df %>% 
  separate_rows(affiliation, sep = ";") %>% 
  filter(affiliation != "")
write.xlsx(df_aut_openalex, "~/Documents/APC Jaime Texiera/data_authors_openalex.xlsx" , all = TRUE)

# Nombre de lignes par id : affiliations
result_aff <- df_aff_openalex %>%
  group_by(id) %>%
  summarise(count = n())

# Nombre de lignes par id : affiliations
result_aut <- df_aut_openalex %>%
  group_by(id) %>%
  summarise(count = n())

result_aff_aut <- left_join(result_aff, result_aut, by = "id") 
names(result_aff_aut) <- c("id", "nb_aff", "nb_aut")

result_aff_aut$diff <- result_aff_aut$nb_aff-result_aff_aut$nb_aut 

# id à exclure car il y a certains auteurs manquants (pas tous)
id_a_exclure <- result_aff_aut %>%
  subset(., .$diff>0) %>%
  select(1)

## merger les deux

# 46 id avec information partielle sur les auteurs à exclure
df_aff_openalex2 <- df_aff_openalex %>%
  subset(., !(df_aff_openalex$id %in% id_a_exclure$id))%>%
  group_by(id) %>%
  mutate(seq = row_number())

# 46 id avec information partielle sur les auteurs à exclure
df_aut_openalex2 <- df_aut_openalex %>%
  subset(., !(df_aut_openalex$id %in% id_a_exclure$id)) %>%
  group_by(id) %>%
  mutate(seq = row_number())

# 975 id sans auteur à exclure
setdif <- setdiff(df_aff_openalex2$id, df_aut_openalex2$id)

df_aff_openalex2 <- df_aff_openalex2 %>%
  subset(., !(df_aff_openalex2$id %in% setdif))

# merge avec rownames pour préserver l'order et non avec left join
df <- merge(df_aut_openalex2, df_aff_openalex2, by = c("id", "seq"))
names(df) <- c("id","seq", "authors", "affiliations")





## Idenfifiants



# Créer une liste vide pour stocker les éléments extraits
extracted_elements <- list()

# Boucle pour extraire les éléments de 1 à 20
for (i in 1:91626) {
  raw_affiliation_string <- openalex[[i]][["results"]][["ids"]]
  extracted_elements[[i]] <- raw_affiliation_string
}

# Combiner les éléments de la liste en un dataframe avec les 5 colonnes
df_id <- bind_rows(extracted_elements)

id_openalex <- df_id$openalex %>%
  unique() %>%
  as.data.frame() %>%
  mutate(id = seq(1:91626))
names(id_openalex) <- c("openalex", "id")

df_id2 <- left_join(id_openalex, df_id, by = "openalex")

write.xlsx(df_id, "~/Documents/APC Jaime Texiera/data_ids_openalex.xlsx" , all = TRUE)

df2 <- left_join(df, df_id2, by = "id")

df <- df2 

# Supprimer les caractères "." et ";" à la fin de la colonne "affiliations"
df$affiliations <- gsub("[.;]+$", "", df$affiliations)

# Éclater la colonne "affiliations" en plusieurs lignes et dupliquer les autres colonnes
df <- separate_rows(df, affiliations, sep = ";")

# Remplacer les motifs "P.R. China", "PRC" et "P.R.C" par "China" dans la colonne "affiliations"
df$affiliations <- gsub("P.R. China|PRC|P.R.C", "China", df$affiliations)

data <- df
# Extraire la dernière chaîne de caractères après la virgule de chaque élément de la colonne "affiliations"
data$pays <- str_extract(data$affiliations, ",\\s*([^,]+)$")
# Supprimer la virgule et l'espace du début de chaque chaîne dans la colonne "pays"
data$pays <- str_replace(data$pays, ",\\s*", "")

# Supprimer les caractères "." et ";" à la fin de la colonne "affiliations"
data$pays <- gsub("[.;]+$", "", data$pays)
# Mettre en majuscule la première lettre de chaque mot dans la colonne "pays"
data$pays <- str_to_title(data$pays)
# Remplacer les motifs "United States Of America", par "USA" dans la colonne "pays"
data$pays <- gsub("United States Of America", "USA", data$pays)
# Remplacer les motifs "Republic Of Korea", par "Korea" dans la colonne "pays"
data$pays <- gsub("Republic Of Korea", "Korea", data$pays)
# Remplacer les motifs "Republic Of Korea", par "Korea" dans la colonne "pays"
data$pays <- gsub("Zoology Section Department Of Biology College Of Sciences Shiraz University Shiraz Iran", "Iran", data$pays)
# Remplacer les motifs "Republic Of Korea", par "Korea" dans la colonne "pays"
data$pays <- gsub("#N#", "", data$pays)
# Supprimer les caractères spéciaux dans la colonne "pays" du DataFrame "df"
data$pays <- gsub("[^[:alnum:] ]", "", data$pays)

# Harmoniser les pays ----
patterns <- read_excel("~/Documents/APC Jaime Texiera/patterns.xlsx")

# Parcourir le dataframe "patterns" pour remplacer les valeurs correspondantes dans "data"
for (i in 1:nrow(patterns)) {
  # Récupérer le motif à rechercher et le remplacement associé
  pattern <- patterns$patern[i]
  replacement <- patterns$replace[i]
  
  # Utiliser grepl pour vérifier si le motif est présent dans chaque valeur de "pays"
  # Si c'est le cas, effectuer le remplacement avec le motif spécifié dans "replace"
  data$pays2[grepl(pattern, data$pays)] <- replacement
}

write.xlsx(data, "~/Documents/APC Jaime Texiera/data_pays.xlsx" , all = TRUE)


# Grouper par année et compter le nombre de DOI
count_by_year <- data_openalex_py %>%
  group_by(annee) %>%
  summarize(count = n())


# Grouper par année et compter le nombre de DOI
count_by_type <- data_openalex_type %>%
  group_by(type) %>%
  summarize(count = n())

# Harmoniser les DOI dans data_openalex_date
data_openalex_date$DOI <- gsub("https://doi.org/", "", data_openalex_date$DOI)

# Fusionner les dataframes en utilisant le DOI
df_doi_aff_py <- merge(data_openalex_date, df_doi_aff, by = "DOI", all.x = TRUE)

# Filtrer les lignes où "Affiliation" n'est pas NA
df_doi_aff_py <- df_doi_aff_py[!is.na(df_doi_aff_py$Affiliation), ]

# Convertir la colonne "date" en format Date
library(lubridate)
df_doi_aff_py$date <- as.Date(df_doi_aff_py$date)


# Extraire l'année à partir de la variable "date"
df_doi_aff_py$annee <- year(df_doi_aff_py$date)


# Compter le nombre de DOI par année
count_by_year <- df_doi_aff_py %>%
  select(DOI, annee) %>%
  unique() %>%
  group_by(annee) %>%
  summarize(count = n())
write.xlsx(count_by_year, "~/Documents/APC Jaime Texiera/count_by_year.xlsx" , all = TRUE)



# Créer une nouvelle colonne "misc_type" avec restriction pour "Do not meet authorship criteria"
# Créer une nouvelle colonne "misc_type" avec les conditions spécifiées
data_roles$misc_type <- ifelse(data_roles$roles_corr == "Funding acquisition" & data_roles$funding == "The authors received no specific funding for this work.",
                               "APC-ring",
                               ifelse(data_roles$roles_corr %in% c("Funding acquisition", "Resources") | 
                                        (data_roles$roles_corr == "Resources" & !grepl("(Conceptualization|Data curation|Supervision|Project administration|Formal analysis|Investigation|Methodology|Software|Validation|Visualization|Writing – review \\& editing|Writing – original draft preparation)", data_roles$roles_corr)),
                                      "Authorship through silver",
                                      ifelse(data_roles$roles_corr %in% c("Funding acquisition", "Resources", "Supervision", "Project administration") & !grepl("(Conceptualization|Data curation|Formal analysis|Investigation|Methodology|Software|Validation|Visualization|Writing – review \\& editing|Writing – original draft preparation)", data_roles$roles_corr),
                                             "Do not meet authorship criteria",
                                             NA)))

## Recoding data_roles$misc_type
data_roles$misc_type <- data_roles$misc_type %>%
  fct_explicit_na("Authorship meets the criteria defined by PLOS One")


# Calculer le nombre distinct de DOI par misc_type
distinct_counts <- data_roles %>%
  group_by(misc_type) %>%
  summarize(distinct_DOI = n_distinct(DOI))
write.xlsx(distinct_counts, "~/Documents/APC Jaime Texiera/misc_type.xlsx" , all = TRUE)



# Compter le nombre de misc_type par DOI
count_misc_type <- data_roles %>%
  group_by(DOI) %>%
  summarize(num_misc_type = n_distinct(misc_type))


# Compter le nombre de DOI avec num_misc_type > 1
count_multiple_misc_type <- data_roles %>%
  group_by(DOI) %>%
  summarize(num_misc_type = n_distinct(misc_type)) %>%
  filter(num_misc_type > 1)


merged_data <- merge(suspects_authors, data_roles, by = c("DOI", "authors"), all.x = TRUE)
