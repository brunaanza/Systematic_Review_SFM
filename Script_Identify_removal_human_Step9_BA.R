# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_AfterNonEnglishRemoval_Step8.xlsx")

cat("=== IDENTIFICAZIONE E RIMOZIONE ARTICOLI CON 'HUMAN' ===\n\n")
cat(paste0("Totale articoli iniziali: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== IDENTIFICARE ARTICOLI CON "HUMAN" (tutte le forme) =====

# Cercare tutte le forme di "human" (case-insensitive)
# Cattura: human, humans, Human, Humans, HUMAN, HUMANS, etc.
df$has_human <- grepl("\\bhuman\\b|\\bhumans\\b", tolower(df$Title), perl = TRUE)

# Separare articoli
articles_with_human <- df %>% filter(has_human == TRUE)
articles_without_human <- df %>% filter(has_human == FALSE)

# ===== STATISTICHE =====
cat("=== STATISTICHE ===\n")
cat(paste0("Articoli con 'human/humans': ", nrow(articles_with_human), "\n"))
cat(paste0("Articoli senza 'human/humans': ", nrow(articles_without_human), "\n"))
cat(paste0("Percentuale con 'human': ", round((nrow(articles_with_human) / nrow(df)) * 100, 2), "%\n\n"))

# ===== CONTARE VARIANTI SPECIFICHE =====
cat("=== VARIANTI DI 'HUMAN' TROVATE ===\n\n")

# Contare forme diverse (case-sensitive per vedere le varianti)
human_lower <- sum(grepl("\\bhuman\\b", df$Title, perl = TRUE, ignore.case = FALSE))
humans_lower <- sum(grepl("\\bhumans\\b", df$Title, perl = TRUE, ignore.case = FALSE))
human_upper <- sum(grepl("\\bHuman\\b", df$Title, perl = TRUE, ignore.case = FALSE))
humans_upper <- sum(grepl("\\bHumans\\b", df$Title, perl = TRUE, ignore.case = FALSE))
human_allcaps <- sum(grepl("\\bHUMAN\\b", df$Title, perl = TRUE, ignore.case = FALSE))
humans_allcaps <- sum(grepl("\\bHUMANS\\b", df$Title, perl = TRUE, ignore.case = FALSE))

cat("Varianti trovate:\n")
cat(paste0("  'human' (minuscolo singolare): ", human_lower, "\n"))
cat(paste0("  'humans' (minuscolo plurale): ", humans_lower, "\n"))
cat(paste0("  'Human' (prima maiuscola singolare): ", human_upper, "\n"))
cat(paste0("  'Humans' (prima maiuscola plurale): ", humans_upper, "\n"))
cat(paste0("  'HUMAN' (tutto maiuscolo singolare): ", human_allcaps, "\n"))
cat(paste0("  'HUMANS' (tutto maiuscolo plurale): ", humans_allcaps, "\n\n"))

# ===== ESEMPI ARTICOLI CON "HUMAN" =====
if (nrow(articles_with_human) > 0) {
  cat("=== ESEMPI ARTICOLI CON 'HUMAN' (primi 50) ===\n\n")
  examples_human <- articles_with_human %>%
    select(Title) %>%
    head(50)
  
  for (i in 1:nrow(examples_human)) {
    # Evidenziare la parola human nel titolo
    title <- examples_human$Title[i]
    cat(paste0(i, ". ", title, "\n"))
    
    # Mostrare quale forma è stata trovata
    if (grepl("\\bhuman\\b", tolower(title), perl = TRUE)) {
      matches <- regmatches(title, gregexpr("\\b[Hh][Uu][Mm][Aa][Nn][Ss]?\\b", title, perl = TRUE))
      if (length(matches[[1]]) > 0) {
        cat(paste0("   Forma trovata: '", paste(matches[[1]], collapse = "', '"), "'\n"))
      }
    }
    cat("\n")
  }
}

# ===== ANALISI CONTESTO "HUMAN" =====
cat("\n=== ANALISI CONTESTO 'HUMAN' NEI TITOLI ===\n\n")

# Cercare pattern comuni con "human"
common_patterns <- c(
  "human health" = "human health",
  "human consumption" = "human consumption",
  "human nutrition" = "human nutrition",
  "human food" = "human food",
  "human diet" = "human diet",
  "human subjects" = "human subjects",
  "human trials" = "human trials",
  "human welfare" = "human welfare",
  "human safety" = "human safety",
  "human population" = "human population"
)

pattern_counts <- list()

for (pattern_name in names(common_patterns)) {
  pattern <- common_patterns[[pattern_name]]
  count <- sum(grepl(pattern, tolower(articles_with_human$Title), fixed = TRUE))
  if (count > 0) {
    pattern_counts[[pattern_name]] <- count
  }
}

if (length(pattern_counts) > 0) {
  cat("Pattern comuni trovati:\n")
  pattern_df <- tibble(
    Pattern = names(pattern_counts),
    Count = unlist(pattern_counts)
  ) %>%
    arrange(desc(Count))
  
  print(pattern_df)
  cat("\n")
}

# ===== SALVARE I FILE - MANTENENDO TUTTE LE COLONNE ORIGINALI =====

# FILE 1: Dataset SENZA "human" (pulito)
articles_without_human_clean <- articles_without_human %>%
  select(-has_human)

write_xlsx(articles_without_human_clean, "Total_WithoutHuman_BA_Step9.xlsx")
cat("✓ FILE 1 salvato: 'Total_WithoutHuman_BA_Step9.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_without_human_clean), " articoli SENZA 'human'\n"))
cat(paste0("  Colonne: ", ncol(articles_without_human_clean), " (tutte le originali)\n\n"))

# FILE 2: Solo articoli CON "human"
articles_with_human_clean <- articles_with_human %>%
  select(-has_human)

write_xlsx(articles_with_human_clean, "WithHuman_Only_BA_Step9.xlsx")
cat("✓ FILE 2 salvato: 'WithHuman_Only_BA_Step9.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_with_human_clean), " articoli CON 'human/humans'\n"))
cat(paste0("  Colonne: ", ncol(articles_with_human_clean), " (tutte le originali)\n\n"))

# ===== VERIFICA FINALE RIGOROSA =====
cat("=== VERIFICA FINALE (RIGOROSA) ===\n")
cat(paste0("Articoli iniziali (Step 8): ", nrow(df), "\n"))
cat(paste0("Articoli con 'human': ", nrow(articles_with_human_clean), "\n"))
cat(paste0("Articoli senza 'human': ", nrow(articles_without_human_clean), "\n"))
cat(paste0("Verifica somma: ", nrow(articles_with_human_clean) + nrow(articles_without_human_clean), 
           " = ", nrow(df), 
           ifelse(nrow(articles_with_human_clean) + nrow(articles_without_human_clean) == nrow(df), " ✓", " ✗"), "\n\n"))

# Verificare colonne
cat(paste0("Colonne originali: ", ncol(df), "\n"))
cat(paste0("Colonne file SENZA human: ", ncol(articles_without_human_clean), "\n"))
cat(paste0("Colonne file CON human: ", ncol(articles_with_human_clean), "\n"))

if (ncol(articles_without_human_clean) == ncol(df)) {
  cat("✅ Tutte le colonne originali mantenute!\n\n")
} else {
  cat("⚠️  Differenza nel numero di colonne!\n\n")
}

# Test finale: verificare che non ci siano "human" rimasti nel file pulito
remaining_human <- sum(grepl("\\bhuman\\b|\\bhumans\\b", 
                             tolower(articles_without_human_clean$Title), perl = TRUE))

cat("Test finale sul file SENZA human:\n")
cat(paste0("  Articoli con 'human/humans' rimasti: ", remaining_human, "\n"))

if (remaining_human > 0) {
  cat("  ⚠️  ATTENZIONE: Ci sono ancora articoli con 'human'!\n\n")
  
  leftover <- articles_without_human_clean %>%
    filter(grepl("\\bhuman\\b|\\bhumans\\b", tolower(Title), perl = TRUE))
  
  cat("Esempi (primi 10):\n")
  for (i in 1:min(10, nrow(leftover))) {
    cat(paste0(i, ". ", leftover$Title[i], "\n"))
  }
  
  write_xlsx(leftover, "DEBUG_RemainingHumanArticles.xlsx")
  cat("\n✓ Salvati per debug: 'DEBUG_RemainingHumanArticles.xlsx'\n\n")
  
} else {
  cat("  ✅ Nessun articolo con 'human/humans' rimasto!\n\n")
}

# Mostrare nomi delle colonne
cat("=== COLONNE NEL FILE FINALE (SENZA HUMAN) ===\n")
cat(paste(names(articles_without_human_clean), collapse = "\n"))
cat("\n")

# ===== RIEPILOGO FINALE =====
cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli iniziali (Step 8): ", nrow(df), "\n"))
cat(paste0("Articoli con 'human/humans' rimossi: ", nrow(articles_with_human_clean), "\n"))
cat(paste0("Articoli finali SENZA 'human' (Step 9): ", nrow(articles_without_human_clean), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(articles_with_human_clean) / nrow(df)) * 100, 2), "%\n"))
cat(paste0("Percentuale mantenuta: ", round((nrow(articles_without_human_clean) / nrow(df)) * 100, 2), "%\n"))

cat("\n=== FILE CREATI ===\n")
cat("1. 'Total_WithoutHuman_BA_Step9.xlsx' - Dataset FINALE SENZA 'human' ✨\n")
cat("2. 'WithHuman_Only_BA_Step9.xlsx' - Solo articoli CON 'human'\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. 'Total_WithoutHuman_BA_Step9.xlsx' è il tuo dataset FINALE!\n")
cat("2. Rivedi 'WithHuman_Only_BA_Step9.xlsx' per verificare se ci sono falsi positivi\n")
cat("3. Se necessario, recupera alcuni articoli e reintegrali nel dataset principale\n")
cat("4. Procedi con le analisi quantitative (PCA, bibliometrics, word frequency, etc.)\n")

cat("\n✅ Identificazione e rimozione articoli con 'HUMAN' completata!\n")