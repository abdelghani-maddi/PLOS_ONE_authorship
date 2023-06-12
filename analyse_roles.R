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
  select(authors, DOI, num_roles)




## Ajouter l'affiliation des auteurs ----

# Load the rvest library
library(rvest)
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


# Eclater le dataframe
library(tidyverse)


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


