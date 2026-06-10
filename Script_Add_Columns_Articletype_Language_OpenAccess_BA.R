# Caricare le librerie
library(readxl)
library(writexl)
library(httr)
library(jsonlite)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_AfterRDuplicatesRemoval_BA_Step2_WithArticleType.xlsx")

# Funzione per ottenere il tipo di articolo da OpenAlex
get_openalex_article_type <- function(doi) {
  # Gestire valori mancanti
  if (is.na(doi) || doi == "" || is.null(doi)) {
    return(list(
      type = NA_character_,
      type_crossref = NA_character_,
      is_retracted = NA,
      is_paratext = NA
    ))
  }
  
  # Pulire il DOI
  doi <- gsub("https://doi.org/", "", doi)
  doi <- gsub("http://dx.doi.org/", "", doi)
  doi <- trimws(doi)
  
  # URL per OpenAlex API (usa il DOI come identificatore)
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  
  result <- tryCatch({
    response <- GET(
      url,
      add_headers(
        "User-Agent" = "mailto:your-email@example.com",  # OpenAlex chiede un contatto
        "Accept" = "application/json"
      ),
      timeout(10)
    )
    
    if (status_code(response) == 200) {
      content_text <- content(response, as = "text", encoding = "UTF-8")
      data <- fromJSON(content_text, flatten = TRUE)
      
      # OpenAlex fornisce il campo "type" (article, review, etc.)
      type <- ifelse(is.null(data$type), NA_character_, as.character(data$type))
      
      # Anche "type_crossref" che è più dettagliato
      type_crossref <- ifelse(is.null(data$type_crossref), 
                              NA_character_, 
                              as.character(data$type_crossref))
      
      # Informazioni aggiuntive utili
      is_retracted <- ifelse(is.null(data$is_retracted), 
                             NA, 
                             as.logical(data$is_retracted))
      
      is_paratext <- ifelse(is.null(data$is_paratext), 
                            NA, 
                            as.logical(data$is_paratext))
      
      return(list(
        type = type,
        type_crossref = type_crossref,
        is_retracted = is_retracted,
        is_paratext = is_paratext
      ))
      
    } else if (status_code(response) == 404) {
      return(list(
        type = "Not_found_in_OpenAlex",
        type_crossref = NA_character_,
        is_retracted = NA,
        is_paratext = NA
      ))
    } else {
      return(list(
        type = paste0("Error_", status_code(response)),
        type_crossref = NA_character_,
        is_retracted = NA,
        is_paratext = NA
      ))
    }
  }, error = function(e) {
    return(list(
      type = "Error_request",
      type_crossref = NA_character_,
      is_retracted = NA,
      is_paratext = NA
    ))
  })
  
  # OpenAlex è gentile ma chiede di non fare più di 10 richieste/secondo
  # Usiamo una pausa conservativa
  Sys.sleep(0.15)  # ~6-7 richieste al secondo
  
  return(result)
}

# Creare vettori vuoti per i risultati
openalex_types <- character(nrow(df))
openalex_types_crossref <- character(nrow(df))
openalex_retracted <- logical(nrow(df))
openalex_paratext <- logical(nrow(df))

# Recuperare i tipi di articolo da OpenAlex
cat("Inizio recupero dei tipi di articolo da OpenAlex...\n")
cat(paste0("Totale articoli da processare: ", nrow(df), "\n"))
cat("Tempo stimato: circa 3-4 minuti\n\n")

start_time <- Sys.time()

for (i in 1:nrow(df)) {
  if (i %% 100 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    avg_time_per_item <- elapsed / i
    remaining <- (nrow(df) - i) * avg_time_per_item
    cat(paste0("Processati ", i, " articoli su ", nrow(df), 
               " - Tempo rimanente: ", round(remaining, 1), " minuti\n"))
  }
  
  result <- get_openalex_article_type(df$DOI[i])
  openalex_types[i] <- result$type
  openalex_types_crossref[i] <- result$type_crossref
  openalex_retracted[i] <- result$is_retracted
  openalex_paratext[i] <- result$is_paratext
}

cat("\nCompletato!\n")

# Aggiungere le colonne al dataframe
# COLONNA PRINCIPALE con il nome richiesto
df$Articletype_OpenAlex <- openalex_types

# Colonne aggiuntive per maggiori dettagli
df$OpenAlex_Type_Crossref <- openalex_types_crossref
df$OpenAlex_Is_Retracted <- openalex_retracted
df$OpenAlex_Is_Paratext <- openalex_paratext

# Visualizzare sommari
cat("\n=== ARTICLETYPE_OPENALEX ===\n")
summary_types <- df %>% 
  group_by(Articletype_OpenAlex) %>% 
  summarise(Numero = n()) %>% 
  arrange(desc(Numero))
print(summary_types)

cat("\n=== TIPO CROSSREF (più dettagliato) ===\n")
summary_crossref <- df %>% 
  group_by(OpenAlex_Type_Crossref) %>% 
  summarise(Numero = n()) %>% 
  arrange(desc(Numero))
print(summary_crossref)

cat("\n=== ARTICOLI RITRATTATI ===\n")
print(table(df$OpenAlex_Is_Retracted, useNA = "ifany"))

# Mostrare alcuni esempi
cat("\n=== ESEMPI (prime 15 righe) ===\n")
print(df[1:15, c("DOI", "Articletype_OpenAlex", "OpenAlex_Type_Crossref")])

# Salvare il risultato
write_xlsx(df, "Total_AfterRDuplicatesRemoval_BA_Step2_WithOpenAlexType.xlsx")

cat("\n✓ File salvato con successo come 'Total_AfterRDuplicatesRemoval_BA_Step2_WithOpenAlexType.xlsx'!\n")

# Statistiche finali
cat("\n=== STATISTICHE FINALI ===\n")
cat(paste0("Totale articoli: ", nrow(df), "\n"))
cat(paste0("Trovati in OpenAlex: ", sum(df$Articletype_OpenAlex != "Not_found_in_OpenAlex" & 
                                          !is.na(df$Articletype_OpenAlex)), "\n"))
cat(paste0("Non trovati: ", sum(df$Articletype_OpenAlex == "Not_found_in_OpenAlex", na.rm = TRUE), "\n"))
cat(paste0("Errori: ", sum(grepl("Error", df$Articletype_OpenAlex), na.rm = TRUE), "\n"))

# Mostrare la lista delle colonne finali
cat("\n=== COLONNE NEL FILE FINALE ===\n")
cat(paste(names(df), collapse = "\n"))
cat("\n")


# ============================================
# AGGIUNTA: RECUPERO LANGUAGE E OPEN ACCESS
# ============================================

cat("\n\n=== INIZIO RECUPERO LANGUAGE E OPEN ACCESS ===\n\n")

# Funzione per ottenere Language e Open Access da OpenAlex
get_openalex_language_oa <- function(doi) {
  # Gestire valori mancanti
  if (is.na(doi) || doi == "" || is.null(doi)) {
    return(list(
      language = NA_character_,
      open_access = NA_character_
    ))
  }
  
  # Pulire il DOI
  doi <- gsub("https://doi.org/", "", doi)
  doi <- gsub("http://dx.doi.org/", "", doi)
  doi <- trimws(doi)
  
  # URL per OpenAlex API
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  
  result <- tryCatch({
    response <- GET(
      url,
      add_headers(
        "User-Agent" = "mailto:your-email@example.com",
        "Accept" = "application/json"
      ),
      timeout(10)
    )
    
    if (status_code(response) == 200) {
      content_text <- content(response, as = "text", encoding = "UTF-8")
      data <- fromJSON(content_text, flatten = TRUE)
      
      # LINGUA
      language <- ifelse(is.null(data$language), 
                         NA_character_, 
                         as.character(data$language))
      
      # OPEN ACCESS
      open_access <- if (!is.null(data$open_access$is_oa)) {
        ifelse(data$open_access$is_oa == TRUE, "Yes", "No")
      } else {
        NA_character_
      }
      
      return(list(
        language = language,
        open_access = open_access
      ))
      
    } else {
      return(list(
        language = NA_character_,
        open_access = NA_character_
      ))
    }
  }, error = function(e) {
    return(list(
      language = NA_character_,
      open_access = NA_character_
    ))
  })
  
  Sys.sleep(0.15)
  
  return(result)
}

# Creare vettori vuoti
openalex_language <- character(nrow(df))
openalex_open_access <- character(nrow(df))

# Recuperare Language e Open Access
cat("Inizio recupero Language e Open Access da OpenAlex...\n")
cat(paste0("Totale articoli da processare: ", nrow(df), "\n\n"))

start_time2 <- Sys.time()

for (i in 1:nrow(df)) {
  if (i %% 100 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time2, units = "mins"))
    avg_time_per_item <- elapsed / i
    remaining <- (nrow(df) - i) * avg_time_per_item
    cat(paste0("Processati ", i, " articoli su ", nrow(df), 
               " - Tempo rimanente: ", round(remaining, 1), " minuti\n"))
  }
  
  result <- get_openalex_language_oa(df$DOI[i])
  openalex_language[i] <- result$language
  openalex_open_access[i] <- result$open_access
}

cat("\nCompletato!\n")

# Aggiungere le nuove colonne al dataframe
df$Language <- openalex_language
df$Open_Access <- openalex_open_access

# Visualizzare sommari delle nuove colonne
cat("\n=== LINGUA ===\n")
summary_language <- df %>% 
  group_by(Language) %>% 
  summarise(Numero = n()) %>% 
  arrange(desc(Numero))
print(summary_language)

cat("\n=== OPEN ACCESS ===\n")
summary_oa <- df %>% 
  group_by(Open_Access) %>% 
  summarise(Numero = n()) %>% 
  arrange(desc(Numero))
print(summary_oa)

# Mostrare esempi con le nuove colonne
cat("\n=== ESEMPI CON LANGUAGE E OPEN ACCESS (prime 15 righe) ===\n")
print(df[1:15, c("DOI", "Articletype_OpenAlex", "Language", "Open_Access")])

# Salvare il file aggiornato
write_xlsx(df, "Total_AfterRDuplicatesRemoval_BA_Step2_WithOpenAlexComplete.xlsx")

cat("\n✓ File finale salvato con successo come 'Total_AfterRDuplicatesRemoval_BA_Step2_WithOpenAlexComplete.xlsx'!\n")

# Statistiche finali complete
cat("\n=== STATISTICHE FINALI COMPLETE ===\n")
cat(paste0("Totale articoli: ", nrow(df), "\n"))
cat(paste0("\nArticoli in Open Access: ", sum(df$Open_Access == "Yes", na.rm = TRUE), "\n"))
cat(paste0("Articoli NON in Open Access: ", sum(df$Open_Access == "No", na.rm = TRUE), "\n"))
cat(paste0("Open Access non disponibile: ", sum(is.na(df$Open_Access)), "\n"))

cat("\n=== TUTTE LE COLONNE NEL FILE FINALE ===\n")
cat(paste(names(df), collapse = "\n"))
cat("\n")






