# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_WithoutDiet_BA_Step5.xlsx")

cat("=== IDENTIFICAZIONE E SEPARAZIONE ARTICOLI PLANT-BASED (TUTTE LE FORME) ===\n\n")
cat(paste0("Totale articoli nel file: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== KEYWORDS PLANT-BASED (case-insensitive) =====
keywords_plant_based <- c(
  # Base terms
  "plant-based", "plant based", "plantbased",
  
  # Specific products
  "plant-based meat", "plant-based protein",
  "plant-based alternative", "plant-based food",
  "plant-based product", "plant-based diet",
  
  # Related terms
  "plant protein", "plant proteins",
  "vegetal protein", "vegetable protein",
  "meat alternative", "meat substitute",
  "meat analog", "meat analogue",
  
  # Specific categories
  "vegan meat", "vegan burger", "vegan product",
  "vegetarian meat", "vegetarian protein",
  
  # Technology-specific
  "texturized vegetable protein", "textured vegetable protein",
  "tvp", "soy protein isolate", "pea protein",
  "mycoprotein",
  
  # Product types
  "burger patty", "plant burger",
  "meatless", "meat-free", "meat free"
)

# Identificare articoli plant-based - CASE INSENSITIVE
df$is_plant_based <- FALSE
df$found_plant_based_keywords <- ""

cat("Processando articoli (case-insensitive per tutte le forme)...\n")

for (i in 1:nrow(df)) {
  if (!is.na(df$Title[i]) && df$Title[i] != "") {
    title_lower <- tolower(df$Title[i])
    found <- c()
    
    for (keyword in keywords_plant_based) {
      # Cercare in modo case-insensitive
      keyword_lower <- tolower(keyword)
      if (grepl(keyword_lower, title_lower, fixed = TRUE)) {
        found <- c(found, keyword)
        df$is_plant_based[i] <- TRUE
      }
    }
    
    if (length(found) > 0) {
      df$found_plant_based_keywords[i] <- paste(found, collapse = "; ")
    }
  }
  
  # Progress indicator
  if (i %% 100 == 0) {
    cat(paste0("Processati ", i, " articoli su ", nrow(df), "\n"))
  }
}

cat("\nCompletato!\n\n")

# ===== SEPARARE ARTICOLI - MANTENENDO TUTTE LE COLONNE =====
# File 1: SOLO articoli plant-based
articles_plant_based <- df %>% filter(is_plant_based == TRUE)

# File 2: Articoli senza plant-based (plant-based rimossi)
articles_without_plant_based <- df %>% filter(is_plant_based == FALSE)

# ===== VERIFICA VARIANTI SPECIFICHE =====
cat("=== VERIFICA VARIANTI TROVATE ===\n\n")

# Contare forme diverse
plant_based_hyphen <- sum(grepl("plant-based", df$Title, ignore.case = TRUE))
plant_based_space <- sum(grepl("plant based", df$Title, ignore.case = TRUE))
plant_based_nospace <- sum(grepl("plantbased", df$Title, ignore.case = TRUE))
plant_protein_var <- sum(grepl("plant protein", df$Title, ignore.case = TRUE))
meat_alternative_var <- sum(grepl("meat alternative", df$Title, ignore.case = TRUE))
vegan_var <- sum(grepl("vegan", df$Title, ignore.case = TRUE))
meatless_var <- sum(grepl("meatless", df$Title, ignore.case = TRUE))

cat("Varianti trovate:\n")
cat(paste0("  'plant-based' (con trattino): ", plant_based_hyphen, "\n"))
cat(paste0("  'plant based' (con spazio): ", plant_based_space, "\n"))
cat(paste0("  'plantbased' (senza spazio): ", plant_based_nospace, "\n"))
cat(paste0("  'plant protein': ", plant_protein_var, "\n"))
cat(paste0("  'meat alternative': ", meat_alternative_var, "\n"))
cat(paste0("  'vegan': ", vegan_var, "\n"))
cat(paste0("  'meatless': ", meatless_var, "\n\n"))

# ===== STATISTICHE =====
cat("=== STATISTICHE ===\n")
cat(paste0("Articoli plant-based identificati: ", nrow(articles_plant_based), "\n"))
cat(paste0("Articoli senza plant-based: ", nrow(articles_without_plant_based), "\n"))
cat(paste0("Percentuale plant-based: ", round((nrow(articles_plant_based) / nrow(df)) * 100, 2), "%\n\n"))

# Keywords più frequenti negli articoli plant-based
cat("=== KEYWORDS PLANT-BASED PIÙ FREQUENTI ===\n")
all_keywords_found <- unlist(strsplit(articles_plant_based$found_plant_based_keywords[articles_plant_based$found_plant_based_keywords != ""], "; "))
if (length(all_keywords_found) > 0) {
  keyword_freq <- sort(table(all_keywords_found), decreasing = TRUE)
  print(keyword_freq)
}

# ===== ESEMPI =====
cat("\n=== ESEMPI ARTICOLI PLANT-BASED (primi 20) ===\n\n")
examples_pb <- articles_plant_based %>%
  select(Title, found_plant_based_keywords) %>%
  head(20)

for (i in 1:nrow(examples_pb)) {
  cat(paste0(i, ". ", examples_pb$Title[i], "\n"))
  cat(paste0("   Keywords: ", examples_pb$found_plant_based_keywords[i], "\n\n"))
}

cat("\n=== ESEMPI ARTICOLI SENZA PLANT-BASED (primi 20) ===\n\n")
examples_other <- articles_without_plant_based %>%
  select(Title) %>%
  head(20)

for (i in 1:nrow(examples_other)) {
  cat(paste0(i, ". ", examples_other$Title[i], "\n\n"))
}

# ===== SALVARE I 2 FILE PRINCIPALI - MANTENENDO TUTTE LE COLONNE =====

# FILE 1: SOLO articoli plant-based
articles_plant_based_clean <- articles_plant_based %>%
  select(-is_plant_based)  # Rimuovo solo il flag, tengo found_keywords per riferimento

write_xlsx(articles_plant_based_clean, "PlantBased_Only_BA_Step6.xlsx")
cat("\n✓ FILE 1 salvato: 'PlantBased_Only_BA_Step6.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_plant_based_clean), " articoli SOLO plant-based\n"))
cat(paste0("  Colonne: ", ncol(articles_plant_based_clean), " (originali + found_plant_based_keywords)\n"))

# FILE 2: Articoli SENZA plant-based (dataset pulito)
articles_without_plant_based_clean <- articles_without_plant_based %>%
  select(-is_plant_based, -found_plant_based_keywords)  # Rimuovo colonne aggiunte

write_xlsx(articles_without_plant_based_clean, "Total_WithoutPlantBased_BA_Step6.xlsx")
cat("✓ FILE 2 salvato: 'Total_WithoutPlantBased_BA_Step6.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_without_plant_based_clean), " articoli SENZA plant-based\n"))
cat(paste0("  Colonne: ", ncol(articles_without_plant_based_clean), " (tutte le originali)\n"))

# ===== ANALISI PER TIPO DI KEYWORD =====
cat("\n=== ANALISI DETTAGLIATA ARTICOLI PLANT-BASED ===\n")

articles_plant_based_temp <- articles_plant_based %>%
  mutate(
    has_plant_based = grepl("plant-based|plant based|plantbased", tolower(found_plant_based_keywords)),
    has_plant_protein = grepl("plant protein", tolower(found_plant_based_keywords)),
    has_meat_alternative = grepl("meat alternative|meat substitute|meat analog", tolower(found_plant_based_keywords)),
    has_vegan = grepl("vegan", tolower(found_plant_based_keywords)),
    has_specific_protein = grepl("pea protein|soy protein|mycoprotein", tolower(found_plant_based_keywords)),
    has_meatless = grepl("meatless|meat-free|meat free", tolower(found_plant_based_keywords))
  )

cat(paste0("Con 'plant-based/plant based': ", sum(articles_plant_based_temp$has_plant_based), "\n"))
cat(paste0("Con 'plant protein': ", sum(articles_plant_based_temp$has_plant_protein), "\n"))
cat(paste0("Con 'meat alternative/substitute/analog': ", sum(articles_plant_based_temp$has_meat_alternative), "\n"))
cat(paste0("Con 'vegan': ", sum(articles_plant_based_temp$has_vegan), "\n"))
cat(paste0("Con proteine specifiche (pea/soy/myco): ", sum(articles_plant_based_temp$has_specific_protein), "\n"))
cat(paste0("Con 'meatless/meat-free': ", sum(articles_plant_based_temp$has_meatless), "\n"))

# ===== VERIFICA FINALE RIGOROSA =====
cat("\n=== VERIFICA FINALE (RIGOROSA) ===\n")
cat(paste0("Articoli iniziali (Step 5): ", nrow(df), "\n"))
cat(paste0("Articoli plant-based: ", nrow(articles_plant_based_clean), "\n"))
cat(paste0("Articoli senza plant-based: ", nrow(articles_without_plant_based_clean), "\n"))
cat(paste0("Verifica somma: ", nrow(articles_plant_based_clean) + nrow(articles_without_plant_based_clean), 
           " = ", nrow(df), 
           ifelse(nrow(articles_plant_based_clean) + nrow(articles_without_plant_based_clean) == nrow(df), " ✓", " ✗"), "\n\n"))

# Verificare colonne
cat(paste0("Colonne originali: ", ncol(df), "\n"))
cat(paste0("Colonne file SENZA plant-based: ", ncol(articles_without_plant_based_clean), "\n"))
cat(paste0("Colonne file SOLO plant-based: ", ncol(articles_plant_based_clean), " (originali + keywords)\n"))

if (ncol(articles_without_plant_based_clean) == ncol(df)) {
  cat("✅ Tutte le colonne originali mantenute nel file pulito!\n\n")
} else {
  cat("⚠️  Differenza nel numero di colonne!\n\n")
}

# Test keywords rimaste nel file SENZA plant-based (case-insensitive)
remaining_plant_based <- sum(grepl("plant-based|plant based|plantbased", 
                                   tolower(articles_without_plant_based_clean$Title), perl = TRUE))
remaining_plant_protein <- sum(grepl("plant protein", 
                                     tolower(articles_without_plant_based_clean$Title), perl = TRUE))
remaining_vegan <- sum(grepl("vegan", 
                             tolower(articles_without_plant_based_clean$Title), perl = TRUE))

cat("Articoli rimasti con keywords nel file SENZA plant-based:\n")
cat(paste0("  'plant-based/plant based': ", remaining_plant_based, "\n"))
cat(paste0("  'plant protein': ", remaining_plant_protein, "\n"))
cat(paste0("  'vegan': ", remaining_vegan, "\n"))

if (remaining_plant_based > 0 || remaining_plant_protein > 0) {
  cat("\n⚠️  ATTENZIONE: Ci sono ancora articoli plant-based rimasti!\n")
  leftover <- articles_without_plant_based_clean %>%
    filter(grepl("plant-based|plant based|plantbased|plant protein", tolower(Title), perl = TRUE))
  
  cat(paste0("Totale: ", nrow(leftover), "\n"))
  cat("Esempi (primi 10):\n")
  for (i in 1:min(10, nrow(leftover))) {
    cat(paste0(i, ". ", leftover$Title[i], "\n"))
  }
  
  write_xlsx(leftover, "DEBUG_RemainingPlantBasedArticles.xlsx")
  cat("\n✓ Salvati per debug: 'DEBUG_RemainingPlantBasedArticles.xlsx'\n")
} else {
  cat("\n✅ Nessun articolo plant-based rimasto nel file pulito!\n")
}

# Mostrare nomi delle colonne
cat("\n=== COLONNE NEL FILE SENZA PLANT-BASED ===\n")
cat(paste(names(articles_without_plant_based_clean), collapse = "\n"))
cat("\n")

cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli iniziali (Step 5): ", nrow(df), "\n"))
cat(paste0("Articoli plant-based identificati: ", nrow(articles_plant_based_clean), "\n"))
cat(paste0("Articoli NON plant-based: ", nrow(articles_without_plant_based_clean), "\n"))
cat(paste0("Percentuale plant-based: ", round((nrow(articles_plant_based_clean) / nrow(df)) * 100, 2), "%\n"))

cat("\n=== FILE CREATI ===\n")
cat("1. 'PlantBased_Only_BA_Step6.xlsx' - SOLO articoli plant-based (con keywords)\n")
cat("2. 'Total_WithoutPlantBased_BA_Step6.xlsx' - Dataset SENZA articoli plant-based (pulito)\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. Usa 'Total_WithoutPlantBased_BA_Step6.xlsx' per continuare con step successivi\n")
cat("2. 'PlantBased_Only_BA_Step6.xlsx' è disponibile per analisi separata\n")
cat("3. Procedi con rimozione social/food security\n")

cat("\n✅ Separazione completata! Tutte le forme (maiuscole/minuscole) sono state cercate.\n")