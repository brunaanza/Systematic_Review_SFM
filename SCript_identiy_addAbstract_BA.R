# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)
library(httr)
library(jsonlite)

# Leggere il file Excel
df <- read_excel("Total_OpenAccessOnly_BA_Step10.xlsx")

cat("=== RECUPERO ABSTRACT TRAMITE OPENALEX API ===\n\n")
cat(paste0("Totale articoli: ", nrow(df), "\n\n"))

# Verificare se esiste già una colonna Abstract
if ("Abstract" %in% names(df)) {
  cat("⚠️  Colonna 'Abstract' già presente nel file!\n")
  cat("Verranno recuperati solo gli abstract mancanti (NA).\n\n")
  
  na_count <- sum(is.na(df$Abstract) | df$Abstract == "")
  cat(paste0("Abstract mancanti: ", na_count, "\n\n"))
  
  df$Abstract_Original <- df$Abstract  # Backup
} else {
  cat("✓ Colonna 'Abstract' non presente. Verrà creata.\n\n")
  df$Abstract <- NA_character_
}

# ===== FUNZIONE PER RECUPERARE ABSTRACT DA OPENALEX =====

get_abstract_from_openalex <- function(doi) {
  # Gestire valori mancanti
  if (is.na(doi) || doi == "" || is.null(doi)) {
    return(list(
      abstract = NA_character_,
      abstract_inverted_index = NA,
      status = "no_doi"
    ))
  }
  
  # Pulire il DOI
  doi_clean <- gsub("https://doi.org/", "", doi)
  doi_clean <- gsub("http://dx.doi.org/", "", doi_clean)
  doi_clean <- trimws(doi_clean)
  
  # URL per OpenAlex API
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi_clean)
  
  result <- tryCatch({
    response <- GET(
      url,
      add_headers(
        "User-Agent" = "mailto:your-email@example.com",  # OpenAlex richiede un contatto
        "Accept" = "application/json"
      ),
      timeout(15)
    )
    
    if (status_code(response) == 200) {
      content_text <- content(response, as = "text", encoding = "UTF-8")
      data <- fromJSON(content_text, flatten = TRUE)
      
      # OpenAlex fornisce abstract in formato "inverted index"
      # Dobbiamo ricostruirlo
      
      if (!is.null(data$abstract_inverted_index) && length(data$abstract_inverted_index) > 0) {
        # Ricostruire abstract da inverted index
        inv_index <- data$abstract_inverted_index
        
        # Creare vettore di parole con posizioni
        max_pos <- max(unlist(inv_index))
        abstract_words <- character(max_pos + 1)
        
        for (word in names(inv_index)) {
          positions <- inv_index[[word]]
          for (pos in positions) {
            abstract_words[pos + 1] <- word  # +1 perché R è 1-indexed
          }
        }
        
        # Unire parole
        abstract_text <- paste(abstract_words[abstract_words != ""], collapse = " ")
        
        return(list(
          abstract = abstract_text,
          abstract_inverted_index = TRUE,
          status = "success"
        ))
        
      } else {
        # Nessun abstract disponibile
        return(list(
          abstract = NA_character_,
          abstract_inverted_index = FALSE,
          status = "no_abstract"
        ))
      }
      
    } else if (status_code(response) == 404) {
      return(list(
        abstract = NA_character_,
        abstract_inverted_index = NA,
        status = "not_found"
      ))
    } else {
      return(list(
        abstract = NA_character_,
        abstract_inverted_index = NA,
        status = paste0("error_", status_code(response))
      ))
    }
  }, error = function(e) {
    return(list(
      abstract = NA_character_,
      abstract_inverted_index = NA,
      status = "error_request"
    ))
  })
  
  # Pausa per rispettare limiti API
  Sys.sleep(0.15)
  
  return(result)
}

# ===== RECUPERARE ABSTRACT =====

cat("=== INIZIO RECUPERO ABSTRACT ===\n\n")
cat("Questo processo richiederà circa", round(nrow(df) * 0.15 / 60, 1), "minuti\n")
cat("(~0.15 secondi per articolo)\n\n")

# Creare vettori per risultati
abstracts <- character(nrow(df))
status_codes <- character(nrow(df))

start_time <- Sys.time()

for (i in 1:nrow(df)) {
  # Se l'abstract esiste già, saltalo
  if ("Abstract_Original" %in% names(df) && 
      !is.na(df$Abstract_Original[i]) && 
      df$Abstract_Original[i] != "") {
    abstracts[i] <- df$Abstract_Original[i]
    status_codes[i] <- "already_present"
    next
  }
  
  # Recupera abstract
  result <- get_abstract_from_openalex(df$DOI[i])
  abstracts[i] <- result$abstract
  status_codes[i] <- result$status
  
  # Progress indicator
  if (i %% 50 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    avg_time <- elapsed / i
    remaining <- (nrow(df) - i) * avg_time
    
    cat(paste0("Processati ", i, " articoli su ", nrow(df), 
               " | Tempo rimanente: ", round(remaining, 1), " minuti\n"))
  }
}

cat("\nCompletato!\n\n")

# Aggiungere risultati al dataframe
df$Abstract <- abstracts
df$Abstract_Status <- status_codes

# ===== STATISTICHE =====

cat("=== STATISTICHE RECUPERO ABSTRACT ===\n\n")

status_summary <- df %>%
  group_by(Abstract_Status) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(status_summary)

# Calcolare percentuali
total <- nrow(df)
success_count <- sum(df$Abstract_Status == "success", na.rm = TRUE)
no_abstract_count <- sum(df$Abstract_Status == "no_abstract", na.rm = TRUE)
not_found_count <- sum(df$Abstract_Status == "not_found", na.rm = TRUE)
error_count <- sum(grepl("error", df$Abstract_Status))

cat("\n=== SUMMARY ===\n")
cat(paste0("Totale articoli: ", total, "\n"))
cat(paste0("Abstract recuperati con successo: ", success_count, " (", 
           round(success_count/total*100, 1), "%)\n"))
cat(paste0("Abstract non disponibili in OpenAlex: ", no_abstract_count, " (", 
           round(no_abstract_count/total*100, 1), "%)\n"))
cat(paste0("DOI non trovati: ", not_found_count, " (", 
           round(not_found_count/total*100, 1), "%)\n"))
cat(paste0("Errori: ", error_count, " (", 
           round(error_count/total*100, 1), "%)\n"))

# ===== ESEMPI =====

cat("\n=== ESEMPI ABSTRACT RECUPERATI (primi 5) ===\n\n")

examples_success <- df %>%
  filter(Abstract_Status == "success") %>%
  select(Title, Abstract) %>%
  head(5)

for (i in 1:min(5, nrow(examples_success))) {
  cat(paste0("--- Articolo ", i, " ---\n"))
  cat(paste0("Titolo: ", examples_success$Title[i], "\n"))
  cat(paste0("Abstract: ", substr(examples_success$Abstract[i], 1, 200), "...\n\n"))
}

# ===== VERIFICARE LUNGHEZZA ABSTRACT =====

df$Abstract_Length <- nchar(df$Abstract)

cat("\n=== STATISTICHE LUNGHEZZA ABSTRACT ===\n")
cat(paste0("Lunghezza media: ", round(mean(df$Abstract_Length, na.rm = TRUE), 0), " caratteri\n"))
cat(paste0("Lunghezza mediana: ", median(df$Abstract_Length, na.rm = TRUE), " caratteri\n"))
cat(paste0("Lunghezza minima: ", min(df$Abstract_Length, na.rm = TRUE), " caratteri\n"))
cat(paste0("Lunghezza massima: ", max(df$Abstract_Length, na.rm = TRUE), " caratteri\n"))

# ===== SALVARE FILE =====

# Rimuovere colonne temporanee
df_output <- df %>%
  select(-Abstract_Length)

if ("Abstract_Original" %in% names(df_output)) {
  df_output <- df_output %>% select(-Abstract_Original)
}

write_xlsx(df_output, "Total_WithAbstracts_BA_Step11.xlsx")

cat("\n✓ File salvato: 'Total_WithAbstracts_BA_Step11.xlsx'\n")
cat(paste0("  Contiene: ", nrow(df_output), " articoli con colonna Abstract\n"))
cat(paste0("  Colonne totali: ", ncol(df_output), "\n\n"))

# File solo con abstract recuperati (per verifica)
df_with_abstracts <- df %>%
  filter(Abstract_Status == "success") %>%
  select(DOI, Title, Abstract)

write_xlsx(df_with_abstracts, "Abstracts_Retrieved_Success.xlsx")
cat("✓ Abstract recuperati salvati: 'Abstracts_Retrieved_Success.xlsx'\n")

# File con abstract mancanti (per eventuale recupero manuale)
df_missing_abstracts <- df %>%
  filter(Abstract_Status %in% c("no_abstract", "not_found", "no_doi")) %>%
  select(DOI, Title, Abstract_Status)

if (nrow(df_missing_abstracts) > 0) {
  write_xlsx(df_missing_abstracts, "Abstracts_Missing.xlsx")
  cat("✓ Abstract mancanti salvati: 'Abstracts_Missing.xlsx'\n")
  cat(paste0("  (", nrow(df_missing_abstracts), " articoli senza abstract)\n"))
}

# ===== RIEPILOGO FINALE =====

cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli iniziali: ", nrow(df), "\n"))
cat(paste0("Abstract recuperati: ", success_count, "\n"))
cat(paste0("Abstract non disponibili: ", nrow(df) - success_count, "\n"))
cat(paste0("Tasso di successo: ", round(success_count/nrow(df)*100, 1), "%\n"))

cat("\n=== FILE CREATI ===\n")
cat("1. 'Total_WithAbstracts_BA_Step11.xlsx' - Dataset completo con Abstract\n")
cat("2. 'Abstracts_Retrieved_Success.xlsx' - Solo abstract recuperati\n")
cat("3. 'Abstracts_Missing.xlsx' - Articoli senza abstract (per recupero manuale)\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. ✓ Hai aggiunto la colonna Abstract!\n")
cat("2. Ora puoi eseguire il CLUSTERING usando Title + Abstract\n")
cat("3. Il clustering sarà molto più accurato con gli abstract!\n")

time_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat(paste0("\n⏱️  Tempo totale: ", round(time_elapsed, 1), " minuti\n"))

cat("\n✅ RECUPERO ABSTRACT COMPLETATO!\n")