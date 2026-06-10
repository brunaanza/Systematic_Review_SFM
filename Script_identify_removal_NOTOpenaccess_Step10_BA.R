# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_AfterHumanRemoval_BA_Step9.xlsx")

cat("=== IDENTIFICAZIONE E RIMOZIONE ARTICOLI NON OPEN ACCESS ===\n\n")
cat(paste0("Totale articoli iniziali: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== VERIFICARE SE ESISTE LA COLONNA OPEN_ACCESS =====

if (!"Open_Access" %in% names(df)) {
  cat("❌ ERRORE: La colonna 'Open_Access' non esiste nel file!\n\n")
  cat("Colonne disponibili nel file:\n")
  print(names(df))
  stop("Colonna 'Open_Access' non trovata. Verifica il nome della colonna.")
}

cat("✓ Colonna 'Open_Access' trovata!\n\n")

# ===== ANALIZZARE I VALORI NELLA COLONNA OPEN_ACCESS =====

cat("=== VALORI NELLA COLONNA 'OPEN_ACCESS' ===\n")

# Contare tutti i valori unici (inclusi NA)
open_access_summary <- df %>%
  group_by(Open_Access) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(open_access_summary)

# Contare anche i NA separatamente
na_count <- sum(is.na(df$Open_Access))
cat(paste0("\nValori NA (mancanti): ", na_count, "\n\n"))

# ===== IDENTIFICARE ARTICOLI DA RIMUOVERE E DA MANTENERE =====

# Considerare come "Yes" anche varianti maiuscole/minuscole
df$is_open_access <- tolower(df$Open_Access) == "yes" | is.na(df$Open_Access)

# Articoli Open Access (Yes o NA - assumiamo che NA = potenzialmente accessibile)
articles_open_access <- df %>% filter(is_open_access == TRUE)

# Articoli NON Open Access (No)
articles_not_open_access <- df %>% filter(is_open_access == FALSE)

# ===== STATISTICHE =====
cat("=== STATISTICHE ===\n")
cat(paste0("Articoli Open Access ('Yes' o NA): ", nrow(articles_open_access), "\n"))
cat(paste0("Articoli NON Open Access ('No'): ", nrow(articles_not_open_access), "\n"))
cat(paste0("Percentuale Open Access: ", round((nrow(articles_open_access) / nrow(df)) * 100, 2), "%\n"))
cat(paste0("Percentuale NON Open Access: ", round((nrow(articles_not_open_access) / nrow(df)) * 100, 2), "%\n\n"))

# ===== BREAKDOWN DETTAGLIATO =====
cat("=== BREAKDOWN DETTAGLIATO ===\n")

yes_count <- sum(tolower(df$Open_Access) == "yes", na.rm = TRUE)
no_count <- sum(tolower(df$Open_Access) == "no", na.rm = TRUE)

cat(paste0("  'Yes' (Open Access): ", yes_count, "\n"))
cat(paste0("  'No' (NON Open Access): ", no_count, "\n"))
cat(paste0("  NA/Missing: ", na_count, "\n"))
cat(paste0("  TOTALE: ", yes_count + no_count + na_count, " = ", nrow(df), "\n\n"))

# ===== ESEMPI ARTICOLI NON OPEN ACCESS =====
if (nrow(articles_not_open_access) > 0) {
  cat("=== ESEMPI ARTICOLI NON OPEN ACCESS (primi 30) ===\n\n")
  examples_no <- articles_not_open_access %>%
    select(Title, Open_Access) %>%
    head(30)
  
  for (i in 1:nrow(examples_no)) {
    cat(paste0(i, ". [", examples_no$Open_Access[i], "] ", examples_no$Title[i], "\n\n"))
  }
}

# ===== ESEMPI ARTICOLI OPEN ACCESS =====
cat("\n=== ESEMPI ARTICOLI OPEN ACCESS (primi 20) ===\n\n")
examples_yes <- articles_open_access %>%
  select(Title, Open_Access) %>%
  head(20)

for (i in 1:nrow(examples_yes)) {
  oa_status <- ifelse(is.na(examples_yes$Open_Access[i]), "NA", examples_yes$Open_Access[i])
  cat(paste0(i, ". [", oa_status, "] ", examples_yes$Title[i], "\n\n"))
}

# ===== SALVARE I 2 FILE - MANTENENDO TUTTE LE COLONNE ORIGINALI =====

# FILE 1: SOLO articoli Open Access (senza "No")
articles_open_access_clean <- articles_open_access %>%
  select(-is_open_access)  # Rimuovo solo il flag temporaneo

write_xlsx(articles_open_access_clean, "Total_OpenAccessOnly_BA_Step10.xlsx")
cat("\n✓ FILE 1 salvato: 'Total_OpenAccessOnly_BA_Step10.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_open_access_clean), " articoli Open Access\n"))
cat(paste0("  (Include 'Yes' e NA - esclusi i 'No')\n"))
cat(paste0("  Colonne: ", ncol(articles_open_access_clean), " (tutte le originali)\n\n"))

# FILE 2: SOLO articoli NON Open Access (solo "No")
articles_not_open_access_clean <- articles_not_open_access %>%
  select(-is_open_access)  # Rimuovo solo il flag temporaneo

write_xlsx(articles_not_open_access_clean, "Removed_NotOpenAccess_BA_Step10.xlsx")
cat("✓ FILE 2 salvato: 'Removed_NotOpenAccess_BA_Step10.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_not_open_access_clean), " articoli NON Open Access\n"))
cat(paste0("  (Solo articoli con 'No')\n"))
cat(paste0("  Colonne: ", ncol(articles_not_open_access_clean), " (tutte le originali)\n\n"))

# ===== VERIFICA FINALE RIGOROSA =====
cat("=== VERIFICA FINALE (RIGOROSA) ===\n")
cat(paste0("Articoli iniziali (Step 9): ", nrow(df), "\n"))
cat(paste0("Articoli Open Access: ", nrow(articles_open_access_clean), "\n"))
cat(paste0("Articoli NON Open Access: ", nrow(articles_not_open_access_clean), "\n"))
cat(paste0("Verifica somma: ", nrow(articles_open_access_clean) + nrow(articles_not_open_access_clean), 
           " = ", nrow(df), 
           ifelse(nrow(articles_open_access_clean) + nrow(articles_not_open_access_clean) == nrow(df), " ✓", " ✗"), "\n\n"))

# Verificare colonne
cat(paste0("Colonne originali: ", ncol(df), "\n"))
cat(paste0("Colonne file Open Access: ", ncol(articles_open_access_clean), "\n"))
cat(paste0("Colonne file NON Open Access: ", ncol(articles_not_open_access_clean), "\n"))

if (ncol(articles_open_access_clean) == ncol(df)) {
  cat("✅ Tutte le colonne originali mantenute!\n\n")
} else {
  cat("⚠️  Differenza nel numero di colonne!\n\n")
}

# Test finale: verificare che non ci siano "No" rimasti nel file Open Access
remaining_no <- sum(tolower(articles_open_access_clean$Open_Access) == "no", na.rm = TRUE)

cat("Test finale sul file Open Access:\n")
cat(paste0("  Articoli con Open_Access = 'No' rimasti: ", remaining_no, "\n"))

if (remaining_no > 0) {
  cat("  ⚠️  ATTENZIONE: Ci sono ancora articoli con 'No'!\n\n")
  
  leftover <- articles_open_access_clean %>%
    filter(tolower(Open_Access) == "no")
  
  cat("Esempi (primi 10):\n")
  for (i in 1:min(10, nrow(leftover))) {
    cat(paste0(i, ". ", leftover$Title[i], "\n"))
  }
  
  write_xlsx(leftover, "DEBUG_RemainingNoOpenAccess.xlsx")
  cat("\n✓ Salvati per debug: 'DEBUG_RemainingNoOpenAccess.xlsx'\n\n")
  
} else {
  cat("  ✅ Nessun articolo con Open_Access = 'No' rimasto!\n\n")
}

# Verificare distribuzione Open_Access nel file finale
cat("Distribuzione Open_Access nel file finale:\n")
final_oa_dist <- articles_open_access_clean %>%
  group_by(Open_Access) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
print(final_oa_dist)

# Mostrare nomi delle colonne
cat("\n=== COLONNE NEL FILE FINALE (OPEN ACCESS) ===\n")
cat(paste(names(articles_open_access_clean), collapse = "\n"))
cat("\n")

# ===== RIEPILOGO FINALE =====
cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli iniziali (Step 9): ", nrow(df), "\n"))
cat(paste0("Articoli NON Open Access rimossi: ", nrow(articles_not_open_access_clean), "\n"))
cat(paste0("Articoli finali Open Access (Step 10): ", nrow(articles_open_access_clean), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(articles_not_open_access_clean) / nrow(df)) * 100, 2), "%\n"))
cat(paste0("Percentuale mantenuta: ", round((nrow(articles_open_access_clean) / nrow(df)) * 100, 2), "%\n"))

cat("\n=== FILE CREATI ===\n")
cat("1. 'Total_OpenAccessOnly_BA_Step10.xlsx' - Dataset FINALE solo Open Access ✨\n")
cat("   (Include articoli con 'Yes' e NA, esclusi solo i 'No')\n")
cat("2. 'Removed_NotOpenAccess_BA_Step10.xlsx' - Solo articoli NON Open Access\n")
cat("   (Solo articoli con 'No')\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. 'Total_OpenAccessOnly_BA_Step10.xlsx' è il tuo dataset FINALE! ✨\n")
cat("2. Rivedi 'Removed_NotOpenAccess_BA_Step10.xlsx' se necessario\n")
cat("3. Se ci sono articoli NA (mancanti) che vuoi rimuovere, posso creare uno script apposito\n")
cat("4. Procedi con le analisi quantitative (PCA, bibliometrics, word frequency, etc.)\n")

cat("\n✅ Rimozione articoli NON Open Access completata!\n")