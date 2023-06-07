#####################################################################
### Analyse de données plos one ### 
#####################################################################
rm(list = ls()) #supprimer tous les objets 

library(rvest)
library(xml2)
library(rvest)
library(dplyr)


# Spécifier l'URL de la page
url <- "https://journals.plos.org/plosone/article/authors?id=10.1371/journal.pone.0286102"

# Lisez le contenu de la page
page <- read_html(url)

# Fonction utilitaire pour extraire les métadonnées
extract_metadata <- function(css_selector, attribute = "content") {
  nodes <- page %>% html_nodes(css_selector)
  if (length(nodes) > 0) {
    return(xml_attr(nodes, attribute))
  } else {
    return(NA)
  }
}

# Fonction utilitaire pour extraire les rôles des auteurs
extract_author_roles <- function(css_selector) {
  nodes <- page %>% html_nodes(css_selector)
  if (length(nodes) > 0) {
    return(html_text(nodes, trim = TRUE))
  } else {
    return(NA)
  }
}

# Extraire les métadonnées
metadata <- data.frame(
  Title = extract_metadata('meta[property="og:title"]', "content"),
  Authors = extract_metadata('meta[name="citation_author"]', "content"),
  Roles = extract_author_roles('#authRoles'),
  DOI = extract_metadata('meta[name="citation_doi"]', "content"),
  Journal = extract_metadata('meta[name="citation_journal_title"]', "content"),
  Publication_Date = extract_metadata('meta[name="citation_publication_date"]', "content"),
  Abstract = extract_metadata('meta[name="description"]', "content")
)

metadata <- metadata %>%
  unique()


## Approche plus extensive :
library(rvest)
library(tibble)

# Liste pour stocker les informations extraites
articles_data <- list()

# Boucle pour parcourir les liens des articles
for (i in 286016:286816) {
  # Construire l'URL de l'article
  article_url <- paste0("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.", sprintf("%07d", i))
  
  # Tentative de lecture du contenu de la page de l'article
  tryCatch({
    # Lire le contenu de la page de l'article
    article_page <- read_html(article_url)
    
    # Extraire les informations de l'article
    title <- article_page %>%
      html_node("meta[property='og:title']") %>%
      html_attr("content")
    
    authors <- article_page %>%
      html_nodes("meta[name='citation_author']") %>%
      html_attr("content") %>%
      toString()  # Convertir en chaîne de caractères
    
    roles <- article_page %>%
      html_nodes("#authRoles") %>%
      html_text(trim = TRUE)
    
    doi <- article_page %>%
      html_node("meta[name='citation_doi']") %>%
      html_attr("content")
    
    funding <- article_page %>%
      html_nodes("p:contains('Funding:')") %>%
      html_text(trim = TRUE)
    
    # Stocker les informations de l'article dans la liste
    articles_data[[length(articles_data) + 1]] <- list(
      Title = title,
      Authors = authors,
      Roles = roles,
      DOI = doi,
      Funding = funding
    )
    
    # Afficher le progrès
    cat("Article", i, "processed\n")
  }, error = function(e) {
    # Ignorer les erreurs 404 et continuer
    if (inherits(e, "simpleError") && grepl("HTTP error 404", conditionMessage(e))) {
      cat("Article", i, "not found\n")
    } else {
      stop(e)
    }
  })
}

# Convertir la liste en tibble
articles_tbl <- tibble::tibble(
  Title = sapply(articles_data, `[[`, "Title"),
  Authors = sapply(articles_data, `[[`, "Authors"),
  Roles = sapply(articles_data, `[[`, "Roles"),
  DOI = sapply(articles_data, `[[`, "DOI"),
  Funding = sapply(articles_data, `[[`, "Funding")
)

# Afficher le tibble
print(articles_tbl)














rm(list = ls()) #supprimer tous les objets 



# Charger la bibliothèque rvest
library(rvest)

# Créer une liste vide pour stocker les résultats
results <- list()

# Définir la fonction pour extraire les informations d'une page
extract_info <- function(url) {
  # Lire la page web
  webpage <- read_html(url)
  
  # Extraire le DOI
  doi <- webpage %>% html_nodes('meta[name="citation_doi"]') %>% html_attr("content")
  
  # Extraire l'information sur le financement
  funding <- webpage %>% html_nodes('p:contains("Funding:")') %>% html_text() %>% gsub("^.*Funding: ", "", .)
  
  # Extraire la liste des auteurs et leurs rôles
  authors <- webpage %>% html_nodes('a.author-name') %>% html_text()
  roles <- webpage %>% html_nodes('p#authRoles') %>% html_nodes('span.type') %>% html_text()
  
  # Retourner les résultats sous forme de liste
  result <- list(DOI = doi, Funding = funding, Authors = authors, Roles = roles)
  return(result)
}

# Parcourir les numéros d'article
for (i in 286716:286816) {
  # Construire l'URL de la page
  url <- paste0("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.", sprintf("%07d", i))
  
  # Essayer d'extraire les informations de la page
  tryCatch({
    info <- extract_info(url)
    
    # Ajouter les informations extraites à la liste des résultats
    results[[i]] <- info
    
    # Afficher le progrès
    cat("Article", i, ":", info$DOI, "extracted\n")
  }, error = function(e) {
    # Ignorer l'erreur HTTP 404 (page non trouvée)
    if (grepl("404", e$message)) {
      cat("Article", i, "not found\n")
    } else {
      # Autres erreurs non liées à la page non trouvée
      stop(e)
    }
  })
}

# Créer un dataframe à partir des résultats trouvés
df <- do.call(rbind, results)

# Afficher le dataframe avec les résultats
print(df)










rm(list = ls()) #supprimer tous les objets 

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
  result <- list(DOI = doi, Funding = funding, Authors = authors, Roles = roles)
  return(result)
}

# Loop through the article numbers
for (i in 0189266:290000) {
  # Construct the URL of the page
  url <- paste0("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.", sprintf("%07d", i))
  
  # Try to extract the information from the page
  tryCatch({
    info <- extract_info(url)
    
    # Add the extracted information to the results list
    results[[i]] <- info
    
    # Display the progress
    cat("Article", i, ":", info$DOI, "extracted\n")
  }, error = function(e) {
    # Ignore the HTTP 404 error (page not found)
    if (grepl("404", e$message)) {
      cat("Article", i, "not found\n")
    } else {
      # Other errors not related to page not found
      stop(e)
    }
  })
}

# Create a dataframe from the found results
df <- do.call(rbind, results)

# transformer en dataframe
df <- as.data.frame(df)

# Supprimer les lignes avec une liste vide dans la colonne "Roles"
df3 <- df[lapply(df$Roles, length) > 0, ]

# Éclater la colonne Authors
df3 <- df3 %>%
  mutate(Authors = map(Authors, ~strsplit(.x, ","))) %>%
  unnest(Authors)
write.xlsx(df3, "~/Documents/APC Jaime Texiera/df_0189266_a_290000_v2.xlsx")


# Créer un groupe distinct pour chaque DOI
df3 <- df3 %>%
  group_by(DOI) %>%
  mutate(Auteur_Num = row_number())

# Modifier les noms des auteurs en fonction du numéro d'auteur
df3 <- df3 %>%
  mutate(Auteur = Auteur_Num)

# Supprimer la colonne Auteur_Num si nécessaire
df3 <- df3 %>%
  select(-Auteur_Num)


# Utiliser la fonction unnest pour éclater la colonne Roles
df2 <- df3 %>%
  select(Roles) %>%
  unique() %>%
  unnest(., cols = Roles, keep_empty = FALSE)


# Joindre les dataframes df et df2 à partir du rowname
df4 <- merge(df3, df2, by = "row.names", all = TRUE) 

df4 <- df4[,c(2,3,4,6,8)] 
names(df4) = c("DOI", "funding", "authors", "authors_num", "Roles")

df <- df %>%
  mutate(ID = match(DOI, unique(DOI)))

# Supprimer la chaîne de caractères "Roles" et les espaces vides qui le suivent de la colonne Roles
df$Roles <- gsub("Roles\\s+", "", df$Roles)

write.xlsx(df, "~/Documents/APC Jaime Texiera/df_plos.xlsx")



























