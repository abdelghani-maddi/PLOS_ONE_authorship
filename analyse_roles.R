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
##### extraction des affiliations, ids, auteurs
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
# "data_aff_openalex.xlsx" est ouvert avec read_excel, puis selection des variables d'intérêt
data_aff_openalex <- read_excel("D:/APC Jaime T/data_aff_openalex.xlsx")
data_aff <- data_aff_openalex %>%
  mutate(id = id.x,
         auteur = auteurs.y,
         affiliations = affiliation,
         openalex_id = id_1,
         doi = id_2,
         mag = id_3,
         pmid = id_4,
         pmcid = id_5) %>%
  .[,12:18]
rm(data_aff_openalex) # plus besoin de cette table
####################################################################
# Éclater la colonne "affiliation" au niveau de ";", en supprimant les entrées vides
df_aut_aff <- data_aff %>%
  mutate(affiliations = str_trim(affiliations)) %>%
  separate_rows(affiliations, sep = "(?<=\\.)\\s*;", convert = TRUE) %>%
  mutate(affiliations = str_replace(affiliations, "^\\s+", "")) %>%
  filter(affiliations != "")

# Vérifications : Nombre d'affiliations par auteur
nb_aff_aut <- df_aut_aff %>%
  select(id, auteur, affiliations) %>%
  unique() %>%
  group_by(id, auteur) %>%
  count()
# Parfait ! rm(nb_aff_aut)
# Enregistrer au cas où
write.xlsx(nb_aff_aut, "D:/APC Jaime T/nb_aff_aut.xlsx" , all = TRUE)

#########################################################
### Traitement des affiliations pour extraire les pays ##
#########################################################
data <- df_aut_aff

# Supprimer les caractères "." et ";" à la fin de la colonne "affiliations"
data$affiliations <- gsub("[.;]+$", "", data$affiliations)

# Remplacer les motifs "P.R. China", "PRC" et "P.R.C" par "China" dans la colonne "affiliations"
data$affiliations <- gsub("P.R. China|PRC|P.R.C", "China", data$affiliations)

# Extraire la dernière chaîne de caractères après la virgule ou le point-virgule de chaque élément de la colonne "affiliations"
data$pays <- sapply(strsplit(data$affiliations, "[;,]\\s*"), function(x) tail(x, 1))

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
rm(patterns)
patterns <- read_excel("D:/APC Jaime T/patterns.xlsx")

# Parcourir le dataframe "patterns" pour remplacer les valeurs correspondantes dans "data"
for (i in 1:nrow(patterns)) {
  # Récupérer le motif à rechercher et le remplacement associé
  pattern <- patterns$patern[i]
  replacement <- patterns$replace[i]
  # Utiliser grepl pour vérifier si le motif est présent dans chaque valeur de "pays"
  # Si c'est le cas, effectuer le remplacement avec le motif spécifié dans "replace"
  data$pays_harmo[grepl(pattern, data$pays)] <- replacement
}
#########################################################
# Enregistrer au cas où
write.xlsx(data, "D:/APC Jaime T/data_pays.xlsx" , all = TRUE)
#########################################################
# Repartir directement de data précédemment enregistré (inutile de tout relancer)
data <- read_excel("D:/APC Jaime T/data_pays.xlsx")
#########################################################
# # Harmoniser les DOI dans data
# data$DOI <- gsub("https://doi.org/", "", data$doi)
# data <- data %>%
#   rename(authors = auteur)
#########################################################
# Nombre de publications par année
data_openalex_date <- read_excel("D:/APC Jaime T/data_openalex_date.xlsx") %>%
  select(DOI, date) %>%
  mutate(date = as.Date(date)) %>%
  mutate(annee = year(date))

# Ajout de l'année à data
data <- data %>%
  left_join(., data_openalex_date, by = c("doi" = "DOI")) 

# Nombres
nb_doc_annee <- data %>%
  select(doi, annee) %>%
  unique() %>%
  filter(!is.na(annee)) %>%
  group_by(annee) %>%
  count()
# Exporter
write.xlsx(nb_doc_annee, "D:/APC Jaime T/count_by_year.xlsx" , all = TRUE)
#########################################################
#########################################################
# Enregistrer la nouvelle version de "data" avec les 2 colonnes sur l'année en plus, au cas où
write.xlsx(data, "D:/APC Jaime T/data_pays.xlsx" , all = TRUE)
#########################################################
# Repartir directement de data précédemment enregistré (inutile de tout relancer)
data <- read_excel("D:/APC Jaime T/data_pays.xlsx")
#########################################################
# Créer une nouvelle colonne "misc_type" avec restriction pour "Do not meet authorship criteria"
# Créer une nouvelle colonne "misc_type" avec les conditions spécifiées
data_roles_originale <- data_roles
data_roles <- data_roles_originale
# Ajout flag is_funded
data_roles <- data_roles %>%
  mutate(is_funded = ifelse(grepl("did not receive|no specific funding|any specific grant", funding, ignore.case = TRUE), 0, 1))

# coder les roles
roles <- data_roles_originale %>%
  select(roles_corr) %>%
  unique() %>% 
  filter(!is.na(roles_corr) & !(roles_corr %in% "character(0)")) %>%
  mutate(code_role = paste0("code ", seq(1:length(roles_corr))))

# Ajout code roles 
data_roles <- data_roles %>%
  left_join(., roles, by = "roles_corr")


# concaténer les roles
data_roles <- data_roles %>%
  select(DOI, authors, roles_corr, authors_num, num_roles, funding, is_funded, code_role) %>%
  group_by(DOI, authors,  authors_num, funding, is_funded) %>% # regrouper les roles pour identifier l'authorat inapproprié
  summarize(code_role = paste(code_role, collapse = ", "))


valid_codes <- c("code 30", "code 9", "code 8", "code 4", "code 5")
other_codes <- paste(paste("code", setdiff(1:30, as.numeric(str_extract_all(valid_codes, "\\d+"))), sep = " "), collapse = "|")

data_roles2 <- data_roles %>%
  mutate(
    type_misc = ifelse(
      (code_role == "code 30" | code_role == "code 9") & is_funded == 0,
      "APC ring",
      ifelse(
        str_detect(code_role, "code 30|code 9|code 8") & !(str_detect(code_role, "code 4|code 5")) & is_funded == 1 & !str_detect(code_role, other_codes),
        "Authorship by resources",
        ifelse(
          str_detect(code_role, paste(valid_codes, collapse = "|")) & !str_detect(code_role, other_codes),
          "Not meet authorship criteria",
          "Authorship meets the criteria defined by PLOS One"
        )
      )
    )
  )

write.xlsx(data_roles2, "D:/APC Jaime T/categories authorship.xlsx" , all = TRUE)

###############################################################
###############################################################
# Se limiter aux DOI pour lesquels les infos sur les affiliations sont dispo + >=2 auteurs
###############################################################
###############################################################
doi_study <- data %>%
  filter(!(annee == 2017) & !is.na(annee)) %>%
  select(doi) %>%
  unique() 

# Supprimer la partie "https://doi.org/" de la colonne doi
doi_study$doi <- gsub("^https://doi.org/", "", doi_study$doi)

# Calculer le nombre d'auteurs par doi, car le phénomène concerne focément des copublications
data_roles_originale <- data_roles_originale %>%
  group_by(DOI) %>%
  mutate(nb_aut = max(authors_num))

# selectionner les collab >= auteurs
nb_aut_doi <- data_roles_originale[,c(1,7)] %>%
  unique() %>%
  filter(nb_aut >=2)

doi_study <- doi_study %>%
  subset(doi %in% nb_aut_doi$DOI)

write.xlsx(doi_study, "D:/APC Jaime T/doi_study.xlsx" , all = TRUE)


# appliquer le filtre + ajouter une colonne is_problematic
data_roles_filtre <- data_roles2 %>%
  filter(DOI %in% doi_study$doi) 

# Enregistrer
write.xlsx(data_roles_filtre, "D:/APC Jaime T/categories authorship filtre aux doi etude.xlsx" , all = TRUE)

# Ajouter des flags par type
# Pivoter les données pour obtenir les modalités de type_misc en colonnes
pivot_data <- data_roles_filtre %>%
  select(DOI, type_misc) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = type_misc, values_from = value, values_fill = 0) %>%
  .[,c(3:7)] %>%
  unique() %>%
  group_by(DOI) %>%
  summarise(across(everything(), sum)) %>% # pour avoir sur la même lignes les différentes combinaisons 1 | 0 | 1 
  group_by(across(-DOI)) %>% # Calculer le nombre distinct de DOI par misc_type
  summarise(count = n_distinct(DOI)) 

write.xlsx(pivot_data, "D:/APC Jaime T/misc_type.xlsx" , all = TRUE)

#############################################################################
#############################################################################
###                      Analyse par pays                                ####
#############################################################################
#############################################################################
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

################################################################################
# DOI de l'étude : avec les filtres
doi_study <- read_excel("D:/APC Jaime T/doi_study.xlsx")

# Données roles
df_roles <- read_excel("D:/APC Jaime T/categories authorship.xlsx") %>%
  filter(DOI %in% doi_study$doi)

# données affiliations
df_pays <- read_excel("D:/APC Jaime T/data_pays.xlsx") 

# Supprimer la partie "https://doi.org/" de la colonne doi
df_pays$doi <- gsub("^https://doi.org/", "", df_pays$doi)

df_pays <- df_pays %>%
  filter(doi %in% doi_study$doi) 

# Retravailler le df_pays pour extraire les multiaffilisation qui restent pour certains auteurs
################################################################
# lignes concrnées
df_pays_pointvirgule <- df_pays %>%
  filter(grepl(";", tolower(affiliations), ignore.case = TRUE))

# Diviser la colonne "affiliations" en plusieurs lignes en utilisant "; " comme séparateur
df_split_affiliations <- df_pays_pointvirgule %>%
  separate_rows(affiliations, sep = "; ")

 

# Extraire la dernière chaîne de caractères après la virgule ou le point-virgule de chaque élément de la colonne "affiliations"
df_split_affiliations$pays <- sapply(strsplit(df_split_affiliations$affiliations, "[;,]\\s*"), function(x) tail(x, 1))

# Supprimer les caractères "." et ";" à la fin de la colonne "affiliations"
df_split_affiliations$pays <- gsub("[.;]+$", "", df_split_affiliations$pays)
# Mettre en majuscule la première lettre de chaque mot dans la colonne "pays"
df_split_affiliations$pays <- str_to_title(df_split_affiliations$pays)
# Remplacer les motifs "United States Of America", par "USA" dans la colonne "pays"
df_split_affiliations$pays <- gsub("United States Of America", "USA", df_split_affiliations$pays)
# Remplacer les motifs "Republic Of Korea", par "Korea" dans la colonne "pays"
df_split_affiliations$pays <- gsub("Republic Of Korea", "Korea", df_split_affiliations$pays)
# Remplacer les motifs "Republic Of Korea", par "Korea" dans la colonne "pays"
df_split_affiliations$pays <- gsub("Zoology Section Department Of Biology College Of Sciences Shiraz University Shiraz Iran", "Iran", df_split_affiliations$pays)
# Remplacer les motifs "Republic Of Korea", par "Korea" dans la colonne "pays"
df_split_affiliations$pays <- gsub("#N#", "", df_split_affiliations$pays)
# Supprimer les caractères spéciaux dans la colonne "pays" du DataFrame "df"
df_split_affiliations$pays <- gsub("[^[:alnum:] ]", "", df_split_affiliations$pays)

# Harmoniser les pays ----
# patterns <- read_excel("~/Documents/APC Jaime Texiera/patterns.xlsx")
# rm(patterns)
patterns <- read_excel("D:/APC Jaime T/patterns.xlsx")
df_split_affiliations$pays_harmo <- NA
# Parcourir le dataframe "patterns" pour remplacer les valeurs correspondantes dans "df_split_affiliations"
for (i in 1:nrow(patterns)) {
  # Récupérer le motif à rechercher et le remplacement associé
  pattern <- patterns$patern[i]
  replacement <- patterns$replace[i]
  # Utiliser grepl pour vérifier si le motif est présent dans chaque valeur de "pays"
  # Si c'est le cas, effectuer le remplacement avec le motif spécifié dans "replace"
  df_split_affiliations$pays_harmo[grepl(pattern, df_split_affiliations$pays)] <- replacement
}

# Utiliser bind_rows pour faire l'union entre les deux DataFrames
df_pays_fitre <- df_pays %>%
  filter(!(grepl(";", tolower(affiliations), ignore.case = TRUE)))

df_pays <- bind_rows(df_pays_fitre, df_split_affiliations)

# Faire un left join
# Au fait, j'avais éliminé dans OpenAlex les doi pour lesquels tous les auteus n'ont pas d'institution, ou inversement
# mais la taille des deux df, celui extrait d'openalex et celui de plos laisse entendre que celui de plos contient plus
# d'auteurs identifiés, donc pour l'analyse des pays, il faut soustraire aussi les doi concernés, ou bien analyser les
# différents authorship inaproprié sans lier aux auteurs. Faire le lien uniquement pour les APC ring

# faire un left join
# récupérer l'odre des auteurs dans plos data
df_misc <- df_roles[,c(1,7)] %>%
  unique()

df_pays_misc <- df_pays %>%
  left_join(., df_misc, by = c("doi" = "DOI")) %>%
  .[,4:12] %>%
  unique()
# petite correction d'un vide constaté
# Modifier la colonne pays_harmo en fonction de la condition pour distinguer entre Congo et RD de Congo
df_pays_misc <- df_pays_misc %>%
  mutate(pays_harmo = ifelse(
    is.na(pays_harmo) & grepl("Italia|Padova Italia", pays, ignore.case = TRUE),
    "Italy",
    pays_harmo
  ))

# Modifier la colonne pays_harmo en fonction de la condition pour distinguer entre Congo et RD de Congo
df_pays_misc <- df_pays_misc %>%
  mutate(pays_harmo = ifelse(
    pays_harmo == "Congo" & grepl("Democratic|Dr|Démocratique", pays, ignore.case = TRUE),
    "Democratic Republic of the Congo",
    pays_harmo
  ))

# Modifier la colonne pays_harmo en fonction de la condition pour distinguer entre Congo et RD de Congo
df_pays_misc <- df_pays_misc %>%
  mutate(pays_harmo = ifelse(
    pays_harmo == "Dominica" & grepl("Dominica", pays, ignore.case = TRUE),
    "Dominican Republic",
    pays_harmo
  ))

# essayer de récupérer le max de pays_harmo à partir des noms de villes ou autre (à ce stade il y a 2.7% de NA)
df_na_pays <- df_pays_misc %>%
  filter(is.na(pays_harmo))
# write.xlsx(df_na_pays, "D:/APC Jaime T/df_na_pays.xlsx" , all = TRUE)

####################################################################
# réimporter df_na_pays.xlsx après avoir fait un travail manuel de 
# nettoyage et correction des données
####################################################################
df_na_pays2 <- read_excel("D:/APC Jaime T/df_na_pays.xlsx")


# Parcourir les lignes du df_na_pays
for (i in 1:nrow(df_na_pays)) {
  # Vérifier si le pays existe dans df_na_pays2
  index <- match(df_na_pays$pays[i], df_na_pays2$pays)
  
  # Si le pays existe, mettre à jour pays_harmo
  if (!is.na(index)) {
    df_na_pays$pays_harmo[i] <- df_na_pays2$pays_harmo[index]
  }
}

####
patterns <- read_excel("D:/APC Jaime T/patterns.xlsx")

# Parcourir le dataframe "patterns" pour remplacer les valeurs correspondantes dans "df_split_affiliations"
for (i in 1:nrow(patterns)) {
  # Récupérer le motif à rechercher et le remplacement associé
  pattern <- patterns$patern[i]
  replacement <- patterns$replace[i]
  # Utiliser grepl pour vérifier si le motif est présent dans chaque valeur de "pays"
  # Si c'est le cas, effectuer le remplacement avec le motif spécifié dans "replace"
  df_na_pays$pays_harmo[grepl(pattern, df_na_pays$pays)] <- replacement
}

##
df_pays_misc_corr <-  df_pays_misc %>%
  filter(!(is.na(pays_harmo))) %>%
  rbind(., df_na_pays)
# maintenant 99,1% des adresses sont nettoyées et corrigées (pays de l'institution)
# write.xlsx(df_pays_misc_corr, "D:/APC Jaime T/df_pays_misc_corr.xlsx" , all = TRUE)

# petite correction d'un vide constaté
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays=="Korea", "South Korea", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Korea", "South Korea", df_pays_misc_corr$pays_harmo)

####################################################################
####################################################################
#####################################################
# pays qui ne matchent pas avec countryExData
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Maroc", "Morocco", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="USA", "United States" , df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Central African Republic", "Central African Republic", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Czech Republic", "Czech Rep.", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Dominica", "Dominican Rep.", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="North Macedonia", "Macedonia", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Vietnam", "Viet Nam", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="United Arab Emirates", "United Arab Emirates", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays_harmo=="Korea", "South Korea", df_pays_misc_corr$pays_harmo)
df_pays_misc_corr$pays_harmo <- ifelse(df_pays_misc_corr$pays=="Accra", "Ghana", df_pays_misc_corr$pays_harmo)


####################################################################
####################################################################
# compte fractionnaire
df_pays_misc_corr <- df_pays_misc_corr %>%
  group_by(doi, type_misc) %>%
  mutate(frac = 1/n())

# faire les comptes par pays par type de misc
count_typ_misc <- df_pays_misc_corr %>%
  group_by(pays_harmo, type_misc) %>%
  summarise(nb_frac = sum(frac))

# Nombres
count_typ_misc <- df_pays_misc_corr %>%
  group_by(type_misc) %>%
  mutate(total_type_misc = sum(frac)) %>%
  ungroup() %>%
  group_by(pays_harmo, type_misc) %>%
  summarise(total = sum(frac)) %>%
  pivot_wider(names_from = type_misc, values_from = total, values_fill = 0)
write.xlsx(count_typ_misc, "D:/APC Jaime T/count_typ_misc2.xlsx" , all = TRUE)


# Parts
part_typ_misc <- df_pays_misc_corr %>%
  group_by(type_misc) %>%
  mutate(total_type_misc = sum(frac)) %>%
  ungroup() %>%
  group_by(pays_harmo, type_misc) %>%
  summarise(part = sum(frac) / first(total_type_misc)) %>%
  pivot_wider(names_from = type_misc, values_from = part, values_fill = 0)
write.xlsx(part_typ_misc, "D:/APC Jaime T/part_typ_misc2.xlsx" , all = TRUE)

count_part_typ_misc <- merge(count_typ_misc, part_typ_misc, by = "pays_harmo")
write.xlsx(count_part_typ_misc, "D:/APC Jaime T/count_part_typ_misc2.xlsx" , all = TRUE)

###

#####################################################
#####################################################
# Recuperer pays
require(rworldmap)
data(countryExData)
write.xlsx(countryExData, "D:/APC Jaime T/list_countries.xlsx" , all = TRUE)


# les scripts précédents sur le comptage sont donc relancés après cette correction.
#####################################################

df_nb_part_countries <- left_join(count_part_typ_misc, countryExData, by = c("pays_harmo" = "Country"))

# Il y a 33 pays non disponibles dans le package R, les voici :
df_nb_part_countries_na <- df_nb_part_countries %>%
  filter(is.na(ISO3V10) & !is.na(pays_harmo))
write.xlsx(df_nb_part_countries_na, "D:/APC Jaime T/df_nb_part_countries_na.xlsx" , all = TRUE)
#####################################################
# récupérer les infos géographiques de ces pays manquants
# Créer un dataframe avec les informations
data_pays_na <- data.frame(
  pays_harmo = c("Afghanistan", "Andorra", "Bahrain", "Barbados", "Bhutan", "Bosnia And Hercegovina", "Brunei", "Cabo Verde", "Central African Republic", "Cote Divoire", "Dominican Republic", "Eswatini", "Gambia", "Gibraltar", "Grenada", "Hong Kong", "Lao Pdr", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Macao", "Maldives", "Malta", "Monaco", "Montenegro", "New Caledonia", "Palestine", "Puerto Rico", "Qatar", "Réunion", "San Marino", "Serbia", "Seychelles", "Singapore", "Somalia", "Suriname", "The Bahamas", "Trinidad And Tobago", "United Arab Emirates", "West Africa", "Democratic Republic of the Congo"),
  ISO3V10 = c("AFG", "AND", "BHR", "BRB", "BTN", "BIH", "BRN", "CPV", "CAF", "CIV", "DOM", "SWZ", "GMB", "GIB", "GRD", "HKG", "LAO", "LSO", "LBR", "LBY", "LIE", "MAC", "MDV", "MLT", "MCO", "MNE", "NCL", "PSE", "PRI", "QAT", "REU", "SMR", "SRB", "SYC", "SGP", "SOM", "SUR", "BHS", "TTO", "ARE", "WAF", "COD"),
  EPI_regions = c("Asia & Pacific", "Europe", "Middle East", "Americas", "Asia & Pacific", "Europe", "Asia & Pacific", "Africa", "Africa", "Africa", "Americas", "Africa", "Africa", "Europe", "Americas", "Asia & Pacific", "Asia & Pacific", "Africa", "Africa", "Africa", "Europe", "Asia & Pacific", "Asia & Pacific", "Europe", "Europe", "Europe", "Europe", "Middle East", "Americas", "Middle East", "Africa", "Europe", "Europe", "Africa", "Asia & Pacific", "Africa", "Americas", "Americas", "Middle East", "Africa", "Africa", "Sub-Saharan Africa")
)
write.xlsx(data_pays_na, "D:/APC Jaime T/data_pays_na.xlsx" , all = TRUE)

# Mettre à jour les valeurs de df_nb_part_countries

# Itérer à travers les valeurs de pays_harmo dans data_pays_na
for (i in 1:length(data_pays_na$pays_harmo)) {
  pays_harmo <- data_pays_na$pays_harmo[i]
  
  # Mettre à jour les valeurs de df_nb_part_countries
  df_nb_part_countries$ISO3V10[df_nb_part_countries$pays_harmo == pays_harmo & is.na(df_nb_part_countries$ISO3V10)] <- data_pays_na$ISO3V10[i]
  df_nb_part_countries$EPI_regions[df_nb_part_countries$pays_harmo == pays_harmo & is.na(df_nb_part_countries$EPI_regions)] <- data_pays_na$EPI_regions[i]
}

write.xlsx(df_nb_part_countries[,1:11], "D:/APC Jaime T/df_nb_part_countries3.xlsx" , all = TRUE)

###########################################################
###########################################################
# Pivoter les données pour obtenir les modalités de type_misc en colonnes
pivot_data <- df_pays_misc_corr %>%
  select(doi, type_misc) %>%
  unique() %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = type_misc, values_from = value, values_fill = 0) %>%
  # .[,c(3:7)] %>%
  # unique() %>%
  group_by(doi) %>%
  summarise(across(everything(), sum)) %>% # pour avoir sur la même lignes les différentes combinaisons 1 | 0 | 1 
  group_by(across(-doi)) %>% # Calculer le nombre distinct de DOI par misc_type
  summarise(count = n_distinct(doi)) 
###########################################################
###########################################################
algeria <- df_pays_misc_corr %>% filter(pays_harmo=="Algeria") %>% select(frac)

sum(algeria$frac)
df_pays_algeria <- df_pays %>%
     filter(grepl("algeria|algérie", affiliations, ignore.case = TRUE))
df_pays_algeria <- df_pays %>%
     filter(grepl("algeria|algérie", tolower(affiliations), ignore.case = TRUE))

n_distinct(df_pays_algeria$doi)

n_distinct(algeria$doi)

setdiff(df_pays_algeria$doi, algeria$doi)

sum(pivot_data$count)


#######################################################################
# > sum(pivot_data$count)
# [1] 81823
# > sum(df_nb_part_countries$`Authorship meets the criteria defined by PLOS One.x`)
# [1] 81823
# > sum(df_nb_part_countries$`Not meet authorship criteria.x`)
# [1] 5448
# > sum(df_nb_part_countries$`Authorship by resources.x`)
# [1] 2593
# > sum(df_nb_part_countries$`APC ring.x`)
# [1] 35
# > sum(df_nb_part_countries$`Authorship meets the criteria defined by PLOS One.y`)
# [1] 1
# > sum(df_nb_part_countries$`Not meet authorship criteria.y`)
# [1] 1
# > sum(df_nb_part_countries$`Not meet authorship criteria.y`)
# [1] 1
# > sum(df_nb_part_countries$`Authorship by resources.y`)
# [1] 1
# > sum(df_nb_part_countries$`APC ring.y`)
# [1] 1
####################################################################################