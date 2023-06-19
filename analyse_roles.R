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
data_roles <- read_excel("~/Documents/APC Jaime Texiera/data_roles.xlsx")

# Utilisez la fonction read.table() pour lire le fichier TSV - base SciSciNet juin 2023.
# data_pub <- read_tsv("~/Documents/SciSciNet/SciSciNet_Papers.tsv")

# Garder uniquement les DOI de notre échantillon PlosOne



# Triez le dataframe par "DOI" et "authors_num" pour garantir l'ordre correct
data_roles <- data_roles %>% arrange(DOI, authors_num)

# Créez la nouvelle colonne "roles_corr" en utilisant la fonction lag() et ifelse()
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


# suspects papers
# Sélectionner les auteurs qui ont num_roles < 3 et "Funding acquisition"
filtered_df <- merged_df %>%
  filter(num_roles < 2, roles_corr == "Funding acquisition")

suspects_authors <- filtered_df %>%
  select(authors, DOI, authors_num, num_roles)




## Ajouter l'affiliation des auteurs ----

# Load the rvest library
library(rvest)

# Create an empty list to store the results
results <- list()

# Define the function to extract information from a page
extract_info <- function(url) {
  # Read the web page
  webpage <- read_html(url)
  
  # Extract the DOI
  doi <- webpage %>% html_nodes('meta[name="citation_doi"]') %>% html_attr("content")
  
  # Extract the funding information
  funding <- webpage %>% html_nodes('p:contains("Funding:")') %>% html_text() %>% gsub("^.*Funding: ", "", .)
  
  # Extract the affiliations
  affiliations <- webpage %>% html_nodes('p:contains("Affiliation")') %>% html_text() %>% gsub("^.*Affiliation", "", .) %>% trimws()
  
  # Extract the list of authors and their roles
  authors <- webpage %>% html_nodes('a.author-name') %>% html_text()
  
  roles <- list()
  
  # Extract the roles for each author
  for (i in seq_along(authors)) {
    author_roles <- webpage %>%
      html_nodes(paste0('a[data-author-id="', i, '"] ~ div.author-info p#authRoles')) %>%
      html_text() %>%
      gsub("^Roles", "", .) %>%
      trimws()
    
    roles <- append(roles, list(author_roles))
  }
  
  # Return the results as a list
  result <- list(DOI = doi, Funding = funding, Affiliations = affiliations, Authors = authors, Roles = roles)
  return(result)
}

# List of DOI
dois <- suspects_authors$DOI

# Loop through the DOI list
for (doi in dois) {
  # Construct the URL of the page
  url <- paste0("https://journals.plos.org/plosone/article?id=", doi)
  
  # Try to extract the information from the page
  tryCatch({
    info <- extract_info(url)
    
    # Add the extracted information to the results list
    results[[doi]] <- info
    
    # Display the progress
    cat("DOI", doi, "extracted\n")
  }, error = function(e) {
    # Ignore the HTTP 404 error (page not found)
    if (grepl("404", e$message)) {
      cat("DOI", doi, "not found\n")
    } else {
      # Other errors not related to page not found
      stop(e)
    }
  })
}

# Create a dataframe from the found results
df_aff <- do.call(rbind, results)

# Convert to dataframe
df_aff <- as.data.frame(df_aff)

# Eclater le dataframe


# Convertir la colonne Affiliations en type caractère
df_aff$Affiliations <- as.character(df_aff$Affiliations)

write.xlsx(df_aff, "~/Documents/APC Jaime Texiera/df_aff.xlsx")
df_aff <- read_excel("~/Documents/APC Jaime Texiera/df_aff3.xlsx")



# Eclater le dataframe en utilisant la fonction map2 et strsplit
df_eclate <- df_aff %>%
  mutate(
    Authors = map2(Authors, Affiliations, ~ strsplit(.x, ",")),
    Affiliations = map2(Affiliations, Authors, ~ strsplit(.x, ",")) %>%
      map(~ trimws(.x)) %>%
      map(~ ifelse(.x == "", NA, .x))
  ) %>%
  unnest_longer(Authors) %>%
  unnest(Affiliations) %>%
  select(DOI, Authors, Affiliations)



# Create an empty list to store the results
results <- list()

# Define the function to extract information from a page
extract_info <- function(url) {
  # Read the web page
  webpage <- read_html(url)
  
  # Extract the DOI
  doi <- webpage %>% html_nodes('meta[name="citation_doi"]') %>% html_attr("content")
  
  # Extract the funding information
  funding <- webpage %>% html_nodes('p:contains("Funding:")') %>% html_text() %>% gsub("^.*Funding: ", "", .)
  
  # Extract the affiliations
  affiliation_nodes <- webpage %>% html_nodes("p[id^='authAffiliations-']")
  affiliations <- lapply(affiliation_nodes, function(node) {
    affiliation <- node %>% html_text() %>% gsub("^.*Affiliation", "", .) %>% trimws()
    affiliation <- gsub("\n", "", affiliation) # Supprimer les retours à la ligne éventuels
    return(affiliation)
  })
  
  # Extract the list of authors and their roles
  authors <- webpage %>% html_nodes('a.author-name') %>% html_text()
  
  roles <- list()
  
  # Extract the roles for each author
  for (i in seq_along(authors)) {
    author_roles <- webpage %>%
      html_nodes(paste0('a[data-author-id="', i, '"] ~ div.author-info p#authRoles')) %>%
      html_text() %>%
      gsub("^Roles", "", .) %>%
      trimws()
    
    roles <- append(roles, list(author_roles))
  }
  
  # Return the results as a list
  result <- list(DOI = doi, Funding = funding, Affiliations = affiliations, Authors = authors, Roles = roles)
  return(result)
}

# List of DOI
# dois <- suspects_authors$DOI
# dois <- row_data$DOI

#df_aff$DOI <- as.character(df_aff$DOI)
#doi_restant <- filter(row_data, !DOI %in% (df_aff$DOI))
dois <- doi_restant$DOI

# Loop through the DOI list
for (doi in dois) {
  # Construct the URL of the page
  url <- paste0("https://journals.plos.org/plosone/article?id=", doi)
  
  # Try to extract the information from the page
  tryCatch({
    info <- extract_info(url)
    
    # Add the extracted information to the results list
    results[[doi]] <- info
    
    # Display the progress
    cat("DOI", doi, "extracted\n")
  }, error = function(e) {
    # Ignore the HTTP 404 error (page not found)
    if (grepl("404", e$message)) {
      cat("DOI", doi, "not found\n")
    } else {
      # Other errors not related to page not found
      stop(e)
    }
  })
}

# Create a dataframe from the found results
df_aff <- do.call(rbind, results)

# Create a dataframe from the found results
df_aff <- data.frame(do.call(rbind, results), stringsAsFactors = FALSE)

# Filtrer les lignes où "Affiliation" n'est pas NA
df_aff <- df_aff[!is.na(df_aff$Affiliations), ]


df_doi_aff <- df_aff%>%
  select(DOI, Affiliations) %>%
  unnest(Affiliations) %>%
  group_by(DOI) %>%
  mutate(authors_num = row_number()) %>%
  ungroup() 

# country <- read_excel("~/Documents/APC Jaime Texiera/countries.xlsx")

df_doi_aff <- df_doi_aff %>%
  mutate(Affiliations = purrr::map(Affiliations, unlist)) %>%
  unnest(Affiliations) %>%
  group_by(DOI, authors_num) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(cols = starts_with("Affiliations"), names_to = "Affiliation_num", values_to = "Affiliation") %>%
  ungroup() %>%
  select(-row_num)



df_doi_aff <- df_doi_aff %>%
  mutate(Affiliation = ifelse(substr(Affiliation, 1, 5) != "s    ", Affiliation, gsub(",     ", "\n", Affiliation))) %>%
  separate_rows(Affiliation, sep = "\n") %>%
  mutate(Affiliation = gsub("^s    ", "", Affiliation))



df_doi_aff <- df_doi_aff %>%
  separate_rows(Affiliation, sep = ", s\\s{2,}")


# Au fait, il y a 3698 DOI qui sautent parce qu'il s'agit en fait de corrections et non des articles originaux. 
# Ce sont des "Corrections" d'autres articles de PLOS.
# Nb : ils sautent parce qu'ils n'ont pas la page "adresse" dans le code source. 
# a <- as.character(df_doi_aff$DOI) %>%
#   unique() 
# a <- subset(df_aff, !DOI %in% a)

# Fonction pour extraire la ville
extract_city <- function(affiliation) {
  # Recherche du motif ville
  city_match <- str_match(affiliation, ",\\s*(.*?),\\s*[^,]*$")[,2]
  
  # Vérification si la correspondance est trouvée
  if (!is.na(city_match)) {
    return(city_match)
  } else {
    return(NA)
  }
}

# Fonction pour extraire le pays
extract_country <- function(affiliation) {
  # Recherche du motif pays
  country_match <- str_match(affiliation, "\\s(\\S+)$")[,2]
  
  # Vérification si la correspondance est trouvée
  if (!is.na(country_match)) {
    return(country_match)
  } else {
    return(NA)
  }
}

# Appliquer les fonctions aux affiliations du dataframe
df_doi_aff$Country <- sapply(df_doi_aff$Affiliation, extract_country)

## harminiser les graphies des pays
harmo_pays <- read_excel("~/Documents/APC Jaime Texiera/harmo_pays.xlsx")
# Modification de la colonne "Country" dans df_doi_aff
df_doi_aff$Country <- ifelse(df_doi_aff$Country %in% harmo_pays$Country, harmo_pays$remplacement[match(df_doi_aff$Country, harmo_pays$Country)], df_doi_aff$Country)


### Faire les premières statistiques ---

# Compter le nombre de pays par DOI
country_counts <- df_doi_aff %>%
  group_by(DOI) %>%
  summarise(Num_Pays = n_distinct(Country))



# Calculer le nombre de lignes par DOI
num_rows <- df_doi_aff %>%
  group_by(DOI) %>%
  summarise(Num_Lignes = n())

# Ajouter une nouvelle colonne avec le calcul 1/Num_Lignes
df_doi_aff <- df_doi_aff %>%
  left_join(num_rows, by = "DOI") %>%
  mutate(frac_geo = 1 / Num_Lignes)



# Calculer la somme de frac_geo par Country
sum_by_country <- df_doi_aff %>%
  group_by(Country) %>%
  summarise(Somme_frac_geo = sum(frac_geo))

write.xlsx(sum_by_country, "~/Documents/APC Jaime Texiera/sum_by_country5.xlsx")

# Désactiver la notation scientifique
# options(scipen = 999)

# left join des pays des auteurs suspects
df_doi_aff$DOI <- as.character(df_doi_aff$DOI)

suspects_authors <- suspects_authors %>%
  left_join(df_doi_aff, by = c("DOI", "authors_num")) 

suspects_authors <- suspects_authors %>%
  select(DOI, authors, authors_num, Country, frac_geo) %>%
  unique()

# Compter le nombre de lignes par pays
country_counts <- table(suspects_authors$Country) %>%
  as.data.frame()


# Calculer la somme de frac_geo par pays
sum_by_country2 <- aggregate(frac_geo ~ Country, data = suspects_authors, FUN = sum)
write.xlsx(sum_by_country2, "~/Documents/APC Jaime Texiera/sum_by_country_suspec.xlsx")

# Extraire l'annee et autre metadonées
library(purrr)
library(httr)
library(jsonlite)
library(progressr)

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

# Nom du fichier de sauvegarde
nom_fichier <- "~/Documents/APC Jaime Texiera/openalex.rds"

# Sauvegarder la liste (au cas où:-))
# saveRDS(all_data, nom_fichier)

# lire le fichier sauvegardé
openalex <- readRDS(nom_fichier)

# 
data_openalex <- as.data.frame(openalex[[3]][["results"]])

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

# Fusionner les dataframes en utilisant les noms de lignes (approche Reduce() avec merge())
data_openalex_date <- merge(openalex_doi, openalex_date, by = "row.names")
write.xlsx(data_openalex_date, "~/Documents/APC Jaime Texiera/data_openalex_date.xlsx" , all = TRUE)

data_openalex_py <- merge(openalex_doi, openalex_year, by = "row.names")
write.xlsx(openalex_year, "~/Documents/APC Jaime Texiera/data_openalex_py.xlsx" , all = TRUE)

data_openalex_type <- merge(openalex_doi, openalex_type, by = "row.names")
write.xlsx(data_openalex_type, "~/Documents/APC Jaime Texiera/data_openalex_type.xlsx" , all = TRUE)

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



library(dplyr)





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
