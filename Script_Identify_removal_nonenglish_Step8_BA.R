# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_AfterSocialRemoval_BA_Step7.xlsx")

cat("=== IDENTIFICAZIONE E RIMOZIONE ARTICOLI NON IN INGLESE ===\n\n")
cat(paste0("Totale articoli iniziali: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== VERIFICARE SE ESISTE GIA' LA COLONNA LANGUAGE =====

if ("Language" %in% names(df)) {
  cat("✓ Colonna 'Language' trovata nel file!\n\n")
  
  # Analizzare le lingue presenti
  cat("=== LINGUE PRESENTI NEL FILE ===\n")
  language_summary <- df %>%
    group_by(Language) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  print(language_summary)
  
  # Identificare articoli non in inglese
  # Considerare come inglese: "en", "eng", "English", "english", NA (assumiamo inglese se mancante)
  df$is_english <- tolower(df$Language) %in% c("en", "eng", "english") | is.na(df$Language)
  
  # Articoli non inglese
  non_english <- df %>% filter(!is_english)
  english_only <- df %>% filter(is_english)
  
  cat(paste0("\nArticoli in inglese: ", nrow(english_only), "\n"))
  cat(paste0("Articoli NON in inglese: ", nrow(non_english), "\n\n"))
  
  if (nrow(non_english) > 0) {
    cat("=== ESEMPI ARTICOLI NON IN INGLESE ===\n")
    examples <- non_english %>%
      select(Title, Language) %>%
      head(20)
    
    for (i in 1:nrow(examples)) {
      cat(paste0(i, ". [", examples$Language[i], "] ", examples$Title[i], "\n\n"))
    }
  }
  
} else {
  cat("⚠️  Colonna 'Language' NON trovata nel file!\n")
  cat("Procedo con identificazione euristica basata sui titoli...\n\n")
  
  # ===== METODO EURISTICO: IDENTIFICARE LINGUA DAL TITOLO =====
  
  # Caratteri non-ASCII o parole in altre lingue comuni
  # Pattern per identificare lingue non-inglesi
  
  # Pattern per caratteri speciali di lingue specifiche
  pattern_chinese <- "[\\u4e00-\\u9fff]"  # Caratteri cinesi
  pattern_cyrillic <- "[\\u0400-\\u04ff]"  # Cirillico (russo, etc.)
  pattern_arabic <- "[\\u0600-\\u06ff]"  # Arabo
  pattern_hebrew <- "[\\u0590-\\u05ff]"  # Ebraico
  pattern_thai <- "[\\u0e00-\\u0e7f]"  # Thai
  pattern_korean <- "[\\uac00-\\ud7af]"  # Coreano
  pattern_japanese <- "[\\u3040-\\u309f\\u30a0-\\u30ff]"  # Giapponese (hiragana, katakana)
  
  # Accenti comuni in lingue europee (spagnolo, francese, portoghese, tedesco, etc.)
  # Ma NON in inglese
  pattern_accents <- "[àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ]"
  
  # Pattern combinato
  pattern_non_english <- paste(
    pattern_chinese, pattern_cyrillic, pattern_arabic, pattern_hebrew,
    pattern_thai, pattern_korean, pattern_japanese,
    sep = "|"
  )
  
  # Identificare articoli con caratteri non-inglesi
  df$has_non_english_chars <- grepl(pattern_non_english, df$Title, perl = TRUE, ignore.case = TRUE)
  
  # Identificare articoli con accenti (possibili false positives, quindi li segnaleremo separatamente)
  df$has_accents <- grepl(pattern_accents, df$Title, perl = TRUE, ignore.case = TRUE)
  
  # Parole chiave comuni in altre lingue (sample)
  keywords_other_languages <- c(
    # Spagnolo
    "\\bdel\\b", "\\bde\\b", "\\bla\\b", "\\bel\\b", "\\bpara\\b", "\\ben\\b", "\\bcon\\b",
    "\\bdesarrollo\\b", "\\bestudio\\b", "\\banálisis\\b",
    
    # Francese
    "\\bdes\\b", "\\bles\\b", "\\bune\\b", "\\bdu\\b", "\\bau\\b", "\\bet\\b",
    "\\bétude\\b", "\\banalyse\\b", "\\bdéveloppement\\b",
    
    # Portoghese
    "\\bda\\b", "\\bdo\\b", "\\bpara\\b", "\\bem\\b", "\\bcom\\b",
    "\\bestudo\\b", "\\banálise\\b", "\\bdesenvolvimento\\b",
    
    # Tedesco
    "\\bder\\b", "\\bdie\\b", "\\bdas\\b", "\\bein\\b", "\\beine\\b", "\\bund\\b",
    "\\bstudie\\b", "\\buntersuchung\\b", "\\bentwicklung\\b",
    
    # Italiano
    "\\bdel\\b", "\\bdella\\b", "\\bdi\\b", "\\bper\\b", "\\bcon\\b",
    "\\bstudio\\b", "\\banalisi\\b", "\\bsviluppo\\b"
  )
  
  # Controllare presenza di parole in altre lingue
  df$has_other_language_words <- FALSE
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$Title[i]) && df$Title[i] != "") {
      title_lower <- tolower(df$Title[i])
      
      for (pattern in keywords_other_languages) {
        if (grepl(pattern, title_lower, perl = TRUE)) {
          df$has_other_language_words[i] <- TRUE
          break
        }
      }
    }
  }
  
  # Combinare i criteri
  df$is_english <- !(df$has_non_english_chars | df$has_other_language_words)
  
  # Articoli identificati
  non_english <- df %>% filter(!is_english)
  english_only <- df %>% filter(is_english)
  
  # Articoli con solo accenti (per revisione manuale)
  accents_only <- df %>% 
    filter(has_accents & is_english)
  
  cat(paste0("Articoli identificati come inglese: ", nrow(english_only), "\n"))
  cat(paste0("Articoli identificati come NON inglese: ", nrow(non_english), "\n"))
  cat(paste0("Articoli con accenti (possibili false positives): ", nrow(accents_only), "\n\n"))
  
  if (nrow(non_english) > 0) {
    cat("=== ESEMPI ARTICOLI NON IN INGLESE IDENTIFICATI ===\n")
    examples <- non_english %>%
      select(Title, has_non_english_chars, has_other_language_words) %>%
      head(30)
    
    for (i in 1:nrow(examples)) {
      cat(paste0(i, ". ", examples$Title[i], "\n"))
      if (examples$has_non_english_chars[i]) {
        cat("   Motivo: Caratteri non-ASCII/non-inglesi\n")
      }
      if (examples$has_other_language_words[i]) {
        cat("   Motivo: Parole chiave in altre lingue\n")
      }
      cat("\n")
    }
  }
  
  if (nrow(accents_only) > 0) {
    cat("\n=== ARTICOLI CON ACCENTI (da rivedere manualmente se necessario) ===\n")
    accents_examples <- accents_only %>%
      select(Title) %>%
      head(10)
    
    for (i in 1:nrow(accents_examples)) {
      cat(paste0(i, ". ", accents_examples$Title[i], "\n"))
    }
    cat("\n")
  }
}

# ===== STATISTICHE =====
cat("\n=== STATISTICHE FINALI ===\n")
cat(paste0("Articoli in inglese: ", nrow(english_only), "\n"))
cat(paste0("Articoli NON in inglese: ", nrow(non_english), "\n"))
cat(paste0("Percentuale inglese: ", round((nrow(english_only) / nrow(df)) * 100, 2), "%\n"))
cat(paste0("Percentuale NON inglese: ", round((nrow(non_english) / nrow(df)) * 100, 2), "%\n\n"))

# ===== SALVARE I FILE - MANTENENDO TUTTE LE COLONNE ORIGINALI =====

# FILE 1: Solo articoli in inglese (dataset pulito)
english_only_clean <- english_only %>%
  select(-is_english)  # Rimuovo solo il flag

# Rimuovere anche le colonne euristiche se esistono
if ("has_non_english_chars" %in% names(english_only_clean)) {
  english_only_clean <- english_only_clean %>%
    select(-has_non_english_chars, -has_accents, -has_other_language_words)
}

write_xlsx(english_only_clean, "Total_EnglishOnly_BA_Step8.xlsx")
cat("✓ FILE 1 salvato: 'Total_EnglishOnly_BA_Step8.xlsx'\n")
cat(paste0("  Contiene: ", nrow(english_only_clean), " articoli in INGLESE\n"))
cat(paste0("  Colonne: ", ncol(english_only_clean), " (tutte le originali)\n"))

# FILE 2: Solo articoli NON in inglese (per riferimento)
non_english_clean <- non_english %>%
  select(-is_english)

if ("has_non_english_chars" %in% names(non_english_clean)) {
  # Tenere le colonne diagnostiche per capire perché sono stati rimossi
  write_xlsx(non_english_clean, "Removed_NonEnglish_Step8.xlsx")
} else {
  write_xlsx(non_english_clean, "Removed_NonEnglish_Step8.xlsx")
}

cat("✓ FILE 2 salvato: 'Removed_NonEnglish_Step8.xlsx'\n")
cat(paste0("  Contiene: ", nrow(non_english_clean), " articoli NON in inglese\n\n"))

# ===== VERIFICA FINALE =====
cat("=== VERIFICA FINALE ===\n")
cat(paste0("Articoli iniziali (Step 7): ", nrow(df), "\n"))
cat(paste0("Articoli in inglese (Step 8): ", nrow(english_only_clean), "\n"))
cat(paste0("Articoli rimossi: ", nrow(non_english_clean), "\n"))
cat(paste0("Verifica somma: ", nrow(english_only_clean) + nrow(non_english_clean), 
           " = ", nrow(df), 
           ifelse(nrow(english_only_clean) + nrow(non_english_clean) == nrow(df), " ✓", " ✗"), "\n\n"))

# Verificare colonne
cat(paste0("Colonne originali: ", ncol(df), "\n"))
cat(paste0("Colonne file finale inglese: ", ncol(english_only_clean), "\n"))

if (ncol(english_only_clean) == ncol(df) || ncol(english_only_clean) == ncol(df) - 1) {
  cat("✅ Tutte le colonne originali mantenute!\n\n")
} else {
  cat("⚠️  Differenza nel numero di colonne\n\n")
}

cat("=== FILE CREATI ===\n")
cat("1. 'Total_EnglishOnly_BA_Step8.xlsx' - Dataset SOLO articoli in inglese (FINALE)\n")
cat("2. 'Removed_NonEnglish_Step8.xlsx' - Articoli NON in inglese rimossi\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. 'Total_EnglishOnly_BA_Step8.xlsx' è il tuo dataset FINALE!\n")
cat("2. Rivedi 'Removed_NonEnglish_Step8.xlsx' per verificare se ci sono falsi positivi\n")
if (exists("accents_only") && nrow(accents_only) > 0) {
  cat("3. ⚠️  Controlla manualmente gli articoli con accenti se necessario\n")
}
cat("4. Procedi con le analisi (PCA, bibliometrics, word frequency, etc.)\n")

cat("\n✅ Identificazione e rimozione articoli NON in inglese completata!\n")