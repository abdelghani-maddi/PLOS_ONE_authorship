library(rvest)
library(tidyverse)

# Fonction pour extraire les affiliations pour un auteur donné avec l'index
extract_affiliations_with_index <- function(page, index) {
  affiliation_nodes <- page %>%
    html_nodes(xpath = paste0('//p[starts-with(@id, "authAffiliations-', index, '")]/span[@class="type"][contains(text(), "Affiliations")]/following-sibling::text()[preceding-sibling::span[@class="type"][contains(text(), "Affiliations")]]'))
  
  affiliations <- affiliation_nodes %>%
    html_text() %>%
    strsplit("\n") %>%
    unlist() %>%
    trimws()
  
  return(list(Index = index, Affiliations = affiliations))
}

# Lien de la page avec le code source
doi <- "10.1371/journal.pone.0286102"
url <- paste0("https://journals.plos.org/plosone/article/authors?id=", doi)

# Lecture du code source HTML
page <- read_html(url)

# Récupération du nombre d'auteurs (n)
n <- page %>%
  html_nodes(xpath = '//*[@id="author-summary"]/p[starts-with(@id, "authAffiliations")]/span[@class="type"][contains(text(), "Affiliations")]/preceding-sibling::text()') %>%
  length()

# Extraction des lignes entre "Affiliations</span>" et </p> pour chaque index
df <- map_dfr(0:10, ~extract_affiliations_with_index(page, .x), .id = "RowID")

df <- df %>%
  filter(., !(Affiliations=="")) %>%
  unique()

# Réinitialisation des indices du dataframe
df <- df %>% 
  rowid_to_column(var = "Index") %>%
  rename_with(~make.unique(.))

# Affichage du dataframe
print(df)
















library(rvest)
library(tidyverse)

# Fonction pour extraire les affiliations d'un auteur donné
extract_affiliations <- function(node) {
  affiliation <- node %>%
    html_text() %>%
    str_remove("^.*Affiliation") %>%
    trimws()
  
  affiliation <- str_remove_all(affiliation, "\n") # Supprimer les retours à la ligne éventuels
  
  return(affiliation)
}

# Lien de la page avec le code source
doi <- "10.1371/journal.pone.0285383"
url <- paste0("https://journals.plos.org/plosone/article/authors?id=", doi)

# Lecture du code source HTML
page <- read_html(url)

# Extraction des nœuds d'affiliations
affiliation_nodes <- page %>%
  html_nodes("p[id^='authAffiliations-']")

# Extraction des affiliations pour chaque auteur
affiliations <- map(affiliation_nodes, extract_affiliations)

# Création du dataframe
df <- tibble(
  Auteur = paste("Auteur", seq_along(affiliations)),
  Affiliation = unlist(affiliations)
)

# Réinitialisation des indices du dataframe
df <- df %>% rowid_to_column("Index")

# Affichage du dataframe
print(df)


