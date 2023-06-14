library(httr)
library(jsonlite)

# Fonction pour extraire les données
extract_plos_data <- function(year) {
  url <- sprintf("http://api.plos.org/search/?wt=json&hl=false&facet=false&q=received_date:[%d-01-01T00:00:00Z+TO+%d-01-01T00:00:00Z]&fq=doc_type:full+AND+cross_published_journal_key:PLoSONE&rows=1000&fl=id,publication_date", year, year + 1)
  response <- GET(url)
  
  if (http_type(response) == "application/json") {
    data <- content(response, as = "text")
    parsed_data <- fromJSON(data, flatten = TRUE)
    
    return(parsed_data$response$docs)
  } else {
    stop("Error: Failed to retrieve data from PLOS ONE API")
  }
}

# Extraction des données pour chaque année
years <- 2018:2023
all_data <- list()

for (year in years) {
  data <- extract_plos_data(year)
  all_data[[as.character(year)]] <- data
}

# Affichage des DOI et années de publication
for (year in years) {
  data <- all_data[[as.character(year)]]
  cat("Year:", year, "\n")
  
  for (i in seq_along(data)) {
    doi <- data[[i]]$id
    publication_date <- data[[i]]$publication_date
    
    cat("DOI:", doi, "\n")
    cat("Publication Year:", substr(publication_date, 1, 4), "\n")
    cat("\n")
  }
}
