# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel - USA IL FILE PRIMA DELLA RIMOZIONE CONSUMER
df <- read_excel("Total_AfterRArticleTypeRemoval_BA_Step3.xlsx")

cat("=== RIMOZIONE ARTICOLI CONSUMER-RELATED (TUTTE LE FORME) ===\n\n")
cat(paste0("Totale articoli iniziali: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== CATTURARE TUTTE LE FORME DI "CONSUMER" =====
# Questa regex cattura:
# - consumer / consumers (minuscolo)
# - Consumer / Consumers (maiuscolo)
# - CONSUMER / CONSUMERS (tutto maiuscolo)
# - CoNsUmEr / CoNsUmErS (qualsiasi combinazione)

df$has_consumer <- grepl("\\bconsumers?\\b", tolower(df$Title), perl = TRUE)
count_consumer <- sum(df$has_consumer, na.rm = TRUE)

# Mostrare esempi di tutte le forme trovate
cat("=== ESEMPI DI FORME 'CONSUMER' TROVATE ===\n")
consumer_examples <- df %>%
  filter(has_consumer == TRUE) %>%
  select(Title) %>%
  head(20)

for (i in 1:nrow(consumer_examples)) {
  title <- consumer_examples$Title[i]
  cat(paste0(i, ". ", title, "\n"))
  
  # Estrarre e mostrare la forma esatta trovata
  matches <- regmatches(tolower(title), 
                        gregexpr("\\bconsumers?\\b", tolower(title), perl = TRUE))
  if (length(matches[[1]]) > 0) {
    cat(paste0("   Forma trovata: '", paste(matches[[1]], collapse = "', '"), "'\n\n"))
  }
}

# ===== KEYWORDS SPECIFICHE =====
keywords_consumer_specific <- c(
  # Consumer-related
  "consumer acceptance", "consumer acceptability", "consumer preference",
  "consumer perception", "consumer attitude", "consumer behavior",
  "consumer behaviour", "consumer choice", "consumer liking",
  "consumer response", "consumer test", "consumer evaluation",
  "consumer study", "consumer survey",
  
  # Plurale esplicito per sicurezza
  "consumers acceptance", "consumers preference", "consumers perception",
  
  # Sensory
  "sensory evaluation", "sensory analysis", "sensory acceptance",
  "sensory attributes", "sensory quality", "hedonic", "hedonic scale",
  "taste preference", "flavor preference", "flavour preference", "palatability",
  "sensory properties", "sensory characteristic", "sensory profile",
  
  # Purchase
  "willingness to buy", "willingness to purchase", "purchase intention",
  "buying intention", "purchasing behavior", "purchasing behaviour",
  "market acceptance",
  
  # Acceptability
  "acceptability study", "acceptance test", "preference test",
  "organoleptic", "taste panel", "tasting panel", "sensory panel",
  "discriminatory test", "triangle test"
)

# Verificare keywords specifiche (case-insensitive)
df$has_specific_keywords <- FALSE
df$found_keywords <- ""

cat("\n=== PROCESSANDO KEYWORDS SPECIFICHE ===\n")

for (i in 1:nrow(df)) {
  if (!is.na(df$Title[i]) && df$Title[i] != "") {
    title_lower <- tolower(df$Title[i])
    found <- c()
    
    for (keyword in keywords_consumer_specific) {
      if (grepl(tolower(keyword), title_lower, fixed = TRUE)) {
        found <- c(found, keyword)
        df$has_specific_keywords[i] <- TRUE
      }
    }
    
    if (length(found) > 0) {
      df$found_keywords[i] <- paste(found, collapse = "; ")
    }
  }
  
  if (i %% 100 == 0) {
    cat(paste0("Processati ", i, " su ", nrow(df), "\n"))
  }
}

cat("\nCompletato!\n\n")

# ===== COMBINARE: rimuovere se ha qualsiasi forma di "consumer/consumers" O keywords specifiche =====
df$to_remove <- df$has_consumer | df$has_specific_keywords

# Creare etichetta per capire perché è stato rimosso
df$removal_reason <- ""
df$removal_reason[df$has_consumer & !df$has_specific_keywords] <- "consumer/consumers (any form)"
df$removal_reason[!df$has_consumer & df$has_specific_keywords] <- "keywords only"
df$removal_reason[df$has_consumer & df$has_specific_keywords] <- "both"

# Identificare articoli rimossi e da mantenere - MANTENENDO TUTTE LE COLONNE
articles_to_remove <- df %>% filter(to_remove == TRUE)
articles_to_keep <- df %>% filter(to_remove == FALSE)

# ===== STATISTICHE =====
cat("=== STATISTICHE RIMOZIONE (TUTTE LE FORME) ===\n")
cat(paste0("Articoli con 'consumer/consumers' (any case): ", sum(df$has_consumer), "\n"))
cat(paste0("Articoli con keywords specifiche: ", sum(df$has_specific_keywords), "\n"))
cat(paste0("Articoli in entrambe le categorie: ", sum(df$has_consumer & df$has_specific_keywords), "\n"))
cat(paste0("\nTOTALE da rimuovere: ", nrow(articles_to_remove), "\n"))
cat(paste0("Articoli rimanenti: ", nrow(articles_to_keep), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(articles_to_remove) / nrow(df)) * 100, 2), "%\n\n"))

# Breakdown per motivo
cat("=== BREAKDOWN PER MOTIVO ===\n")
removal_summary <- articles_to_remove %>%
  group_by(removal_reason) %>%
  summarise(count = n())
print(removal_summary)

# ===== VERIFICA: Contare varianti specifiche =====
cat("\n=== VERIFICA VARIANTI DI 'CONSUMER' TROVATE ===\n")

# Contare forme diverse
singular_lower <- sum(grepl("\\bconsumer\\b", df$Title, perl = TRUE))
plural_lower <- sum(grepl("\\bconsumers\\b", df$Title, perl = TRUE))
singular_upper <- sum(grepl("\\bConsumer\\b", df$Title, perl = TRUE))
plural_upper <- sum(grepl("\\bConsumers\\b", df$Title, perl = TRUE))
all_caps_singular <- sum(grepl("\\bCONSUMER\\b", df$Title, perl = TRUE))
all_caps_plural <- sum(grepl("\\bCONSUMERS\\b", df$Title, perl = TRUE))

cat(paste0("'consumer' (minuscolo): ", singular_lower, "\n"))
cat(paste0("'consumers' (minuscolo): ", plural_lower, "\n"))
cat(paste0("'Consumer' (prima maiuscola): ", singular_upper, "\n"))
cat(paste0("'Consumers' (prima maiuscola): ", plural_upper, "\n"))
cat(paste0("'CONSUMER' (tutto maiuscolo): ", all_caps_singular, "\n"))
cat(paste0("'CONSUMERS' (tutto maiuscolo): ", all_caps_plural, "\n"))

# ===== ESEMPI ARTICOLI RIMOSSI =====
cat("\n=== ESEMPI ARTICOLI RIMOSSI (primi 30) ===\n")
examples <- articles_to_remove %>%
  select(Title, removal_reason, found_keywords) %>%
  head(30)

for (i in 1:nrow(examples)) {
  cat(paste0(i, ". ", examples$Title[i], "\n"))
  cat(paste0("   Motivo: ", examples$removal_reason[i], "\n"))
  if (examples$found_keywords[i] != "") {
    cat(paste0("   Keywords: ", examples$found_keywords[i], "\n"))
  }
  cat("\n")
}

# Keywords più frequenti
cat("\n=== KEYWORDS PIÙ FREQUENTI ===\n")
all_keywords_found <- unlist(strsplit(articles_to_remove$found_keywords[articles_to_remove$found_keywords != ""], "; "))
if (length(all_keywords_found) > 0) {
  keyword_freq <- sort(table(all_keywords_found), decreasing = TRUE)
  print(head(keyword_freq, 20))
}

# ===== SALVARE I FILE - MANTENENDO TUTTE LE COLONNE =====

# File principale - RIMUOVERE le colonne di analisi temporanee, mantenere solo originali
articles_to_keep_clean <- articles_to_keep %>%
  select(-has_consumer, -has_specific_keywords, -found_keywords, -to_remove, -removal_reason)

write_xlsx(articles_to_keep_clean, "Total_AfterConsumerRemoval_BA_Step4_AllForms.xlsx")
cat("\n✓ File principale salvato: 'Total_AfterConsumerRemoval_BA_Step4_AllForms.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_to_keep_clean), " articoli con TUTTE le ", 
           ncol(articles_to_keep_clean), " colonne originali\n"))

# File articoli rimossi - con informazioni aggiuntive per analisi
write_xlsx(articles_to_remove, "Removed_Consumer_Step4_AllForms.xlsx")
cat("✓ Articoli rimossi salvati: 'Removed_Consumer_Step4_AllForms.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_to_remove), " articoli con colonne originali + info rimozione\n"))

# File con breakdown per forma di consumer
consumer_variants <- data.frame(
  Variant = c("consumer", "consumers", "Consumer", "Consumers", "CONSUMER", "CONSUMERS"),
  Count = c(singular_lower, plural_lower, singular_upper, plural_upper, all_caps_singular, all_caps_plural)
)
write_xlsx(consumer_variants, "Consumer_Variants_Found.xlsx")
cat("✓ Statistiche varianti salvate: 'Consumer_Variants_Found.xlsx'\n")

# ===== VERIFICA FINALE RIGOROSA =====
cat("\n=== VERIFICA FINALE (RIGOROSA) ===\n")
cat(paste0("Articoli nel file finale: ", nrow(articles_to_keep_clean), "\n"))
cat(paste0("Colonne nel file finale: ", ncol(articles_to_keep_clean), "\n"))

# Verificare che le colonne siano uguali a quelle originali
original_cols <- ncol(df)
final_cols <- ncol(articles_to_keep_clean)
cat(paste0("Colonne originali: ", original_cols, "\n"))
cat(paste0("Colonne finali: ", final_cols, "\n"))

if (original_cols == final_cols) {
  cat("✅ Tutte le colonne originali mantenute!\n")
} else {
  cat("⚠️  Differenza nel numero di colonne!\n")
}

# Test TUTTI i pattern possibili di consumer
test_patterns <- c(
  "\\bconsumer\\b",
  "\\bconsumers\\b",
  "\\bConsumer\\b",
  "\\bConsumers\\b",
  "\\bCONSUMER\\b",
  "\\bCONSUMERS\\b"
)

remaining_total <- 0
for (pattern in test_patterns) {
  count <- sum(grepl(pattern, articles_to_keep_clean$Title, perl = TRUE))
  remaining_total <- remaining_total + count
  if (count > 0) {
    pattern_name <- gsub("\\\\b", "", pattern)
    cat(paste0("⚠️  Articoli con '", pattern_name, "' rimasti: ", count, "\n"))
  }
}

# Test case-insensitive generale
remaining_any_case <- sum(grepl("\\bconsumers?\\b", tolower(articles_to_keep_clean$Title), perl = TRUE))

cat(paste0("\nTOTALE articoli con qualsiasi forma di 'consumer/consumers': ", remaining_any_case, "\n"))

if (remaining_any_case > 0) {
  cat("\n⚠️  ATTENZIONE: Ci sono ancora articoli con 'consumer/consumers'!\n\n")
  leftover <- articles_to_keep_clean %>%
    filter(grepl("\\bconsumers?\\b", tolower(Title), perl = TRUE))
  
  cat("Esempi (primi 10):\n")
  for (i in 1:min(10, nrow(leftover))) {
    cat(paste0(i, ". ", leftover$Title[i], "\n"))
    # Mostrare quale forma è stata trovata
    matches <- regmatches(leftover$Title[i], 
                          gregexpr("\\b[Cc][Oo][Nn][Ss][Uu][Mm][Ee][Rr][Ss]?\\b", 
                                   leftover$Title[i], perl = TRUE))
    if (length(matches[[1]]) > 0) {
      cat(paste0("   Forma trovata: '", paste(matches[[1]], collapse = "', '"), "'\n"))
    }
    cat("\n")
  }
  
  # Salvare per debug - TUTTE LE COLONNE
  write_xlsx(leftover, "DEBUG_RemainingConsumerArticles.xlsx")
  cat("✓ Articoli rimasti salvati per debug: 'DEBUG_RemainingConsumerArticles.xlsx'\n")
  
} else {
  cat("\n✅✅✅ PERFETTO! Nessun articolo con 'consumer/consumers' rimasto!\n")
  cat("Tutte le forme (minuscole, maiuscole, singolare, plurale) sono state rimosse.\n")
}

# Mostrare nomi delle colonne mantenute
cat("\n=== COLONNE NEL FILE FINALE ===\n")
cat(paste(names(articles_to_keep_clean), collapse = "\n"))
cat("\n")

cat("\n✅ Rimozione completata!\n")
cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli originali: ", nrow(df), "\n"))
cat(paste0("Articoli rimossi: ", nrow(articles_to_remove), "\n"))
cat(paste0("Articoli finali: ", nrow(articles_to_keep_clean), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(articles_to_remove) / nrow(df)) * 100, 2), "%\n"))
cat(paste0("Colonne mantenute: ", ncol(articles_to_keep_clean), " (tutte le originali)\n"))