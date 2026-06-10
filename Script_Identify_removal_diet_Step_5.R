# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_AfterConsumerRemoval_BA_Step4_AllForms.xlsx")

cat("=== RIMOZIONE ARTICOLI DIET-RELATED (TUTTE LE FORME) ===\n\n")
cat(paste0("Totale articoli iniziali: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== KEYWORDS COMPLETE (tutte le forme saranno cercate case-insensitive) =====
keywords_diet_complete <- c(
  # Diet base (tutte le forme: diet/diets/Diet/Diets/DIET/DIETS/dietary/Dietary/DIETARY)
  "diet", "dietary", "diets",
  
  # Eating behavior
  "eating behavior", "eating behaviour", "eating habit",
  "eating pattern", "food choice", "food selection",
  "dietary behavior", "dietary behaviour",
  
  # Nutrition (nutrition/Nutrition/NUTRITION/nutritional/Nutritional/NUTRITIONAL)
  "nutrition", "nutritional", "nutrient intake",
  "dietary intake", "food intake", "dietary pattern",
  "nutritional status", "nutritional value",
  
  # Dietary interventions/changes
  "dietary intervention", "dietary change", "diet quality",
  "healthy diet", "healthy eating", "dietary guideline",
  "dietary recommendation",
  
  # Consumption patterns
  "food consumption", "meat consumption", "protein consumption",
  "dietary shift", "dietary transition", "food habit",
  
  # Diet types
  "vegetarian diet", "vegan diet", "plant-based diet",
  "flexitarian diet", "mediterranean diet",
  
  # Other related
  "meal pattern", "meal frequency", "feeding behavior",
  "feeding behaviour", "food frequency"
)

# Identificare articoli da rimuovere - CASE INSENSITIVE
df$to_remove_diet <- FALSE
df$found_diet_keywords <- ""

cat("Processando articoli (case-insensitive per tutte le forme)...\n")

for (i in 1:nrow(df)) {
  if (!is.na(df$Title[i]) && df$Title[i] != "") {
    title_lower <- tolower(df$Title[i])
    found <- c()
    
    for (keyword in keywords_diet_complete) {
      # Cercare in modo case-insensitive
      keyword_lower <- tolower(keyword)
      if (grepl(keyword_lower, title_lower, fixed = TRUE)) {
        found <- c(found, keyword)
        df$to_remove_diet[i] <- TRUE
      }
    }
    
    if (length(found) > 0) {
      df$found_diet_keywords[i] <- paste(found, collapse = "; ")
    }
  }
  
  # Progress indicator
  if (i %% 100 == 0) {
    cat(paste0("Processati ", i, " articoli su ", nrow(df), "\n"))
  }
}

cat("\nCompletato!\n\n")

# Articoli da rimuovere e mantenere - MANTENENDO TUTTE LE COLONNE
articles_to_remove <- df %>% filter(to_remove_diet == TRUE)
articles_to_keep <- df %>% filter(to_remove_diet == FALSE)

# ===== VERIFICA VARIANTI SPECIFICHE =====
cat("=== VERIFICA VARIANTI TROVATE ===\n\n")

# Contare forme diverse delle parole chiave principali
diet_lower <- sum(grepl("\\bdiet\\b", df$Title, perl = TRUE, ignore.case = FALSE))
diet_upper <- sum(grepl("\\bDiet\\b", df$Title, perl = TRUE, ignore.case = FALSE))
diet_allcaps <- sum(grepl("\\bDIET\\b", df$Title, perl = TRUE, ignore.case = FALSE))

diets_lower <- sum(grepl("\\bdiets\\b", df$Title, perl = TRUE, ignore.case = FALSE))
diets_upper <- sum(grepl("\\bDiets\\b", df$Title, perl = TRUE, ignore.case = FALSE))

dietary_lower <- sum(grepl("\\bdietary\\b", df$Title, perl = TRUE, ignore.case = FALSE))
dietary_upper <- sum(grepl("\\bDietary\\b", df$Title, perl = TRUE, ignore.case = FALSE))

nutrition_lower <- sum(grepl("\\bnutrition\\b", df$Title, perl = TRUE, ignore.case = FALSE))
nutrition_upper <- sum(grepl("\\bNutrition\\b", df$Title, perl = TRUE, ignore.case = FALSE))
nutrition_allcaps <- sum(grepl("\\bNUTRITION\\b", df$Title, perl = TRUE, ignore.case = FALSE))

nutritional_lower <- sum(grepl("\\bnutritional\\b", df$Title, perl = TRUE, ignore.case = FALSE))
nutritional_upper <- sum(grepl("\\bNutritional\\b", df$Title, perl = TRUE, ignore.case = FALSE))

cat("Forme di 'diet':\n")
cat(paste0("  'diet' (minuscolo): ", diet_lower, "\n"))
cat(paste0("  'Diet' (maiuscolo): ", diet_upper, "\n"))
cat(paste0("  'DIET' (tutto maiuscolo): ", diet_allcaps, "\n"))
cat(paste0("  'diets' (minuscolo): ", diets_lower, "\n"))
cat(paste0("  'Diets' (maiuscolo): ", diets_upper, "\n"))
cat(paste0("  'dietary' (minuscolo): ", dietary_lower, "\n"))
cat(paste0("  'Dietary' (maiuscolo): ", dietary_upper, "\n\n"))

cat("Forme di 'nutrition':\n")
cat(paste0("  'nutrition' (minuscolo): ", nutrition_lower, "\n"))
cat(paste0("  'Nutrition' (maiuscolo): ", nutrition_upper, "\n"))
cat(paste0("  'NUTRITION' (tutto maiuscolo): ", nutrition_allcaps, "\n"))
cat(paste0("  'nutritional' (minuscolo): ", nutritional_lower, "\n"))
cat(paste0("  'Nutritional' (maiuscolo): ", nutritional_upper, "\n\n"))

# ===== STATISTICHE =====
cat("=== STATISTICHE RIMOZIONE ===\n")
cat(paste0("Articoli da rimuovere (diet-related): ", nrow(articles_to_remove), "\n"))
cat(paste0("Articoli rimanenti: ", nrow(articles_to_keep), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(articles_to_remove) / nrow(df)) * 100, 2), "%\n\n"))

# Keywords più frequenti
cat("=== KEYWORDS PIÙ FREQUENTI TROVATE ===\n")
all_keywords_found <- unlist(strsplit(articles_to_remove$found_diet_keywords[articles_to_remove$found_diet_keywords != ""], "; "))
if (length(all_keywords_found) > 0) {
  keyword_freq <- sort(table(all_keywords_found), decreasing = TRUE)
  print(keyword_freq)
}

# ===== ESEMPI DI ARTICOLI DA RIMUOVERE =====
cat("\n=== ESEMPI ARTICOLI DA RIMUOVERE (primi 30) ===\n\n")
examples <- articles_to_remove %>%
  select(Title, found_diet_keywords) %>%
  head(30)

for (i in 1:nrow(examples)) {
  cat(paste0(i, ". ", examples$Title[i], "\n"))
  cat(paste0("   Keywords: ", examples$found_diet_keywords[i], "\n\n"))
}

# ===== SALVARE I 2 FILE PRINCIPALI =====

# FILE 1: Articoli SENZA diet-related (articoli rimossi dal dataset)
articles_to_keep_clean <- articles_to_keep %>%
  select(-to_remove_diet, -found_diet_keywords)

write_xlsx(articles_to_keep_clean, "Total_WithoutDiet_BA_Step5.xlsx")
cat("\n✓ FILE 1 salvato: 'Total_WithoutDiet_BA_Step5.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_to_keep_clean), " articoli SENZA diet-related\n"))
cat(paste0("  Colonne: ", ncol(articles_to_keep_clean), " (tutte le originali)\n"))

# FILE 2: SOLO articoli diet-related (articoli selezionati/rimossi)
articles_diet_only <- articles_to_remove %>%
  select(-to_remove_diet)  # Rimuovo solo il flag, tengo found_diet_keywords

write_xlsx(articles_diet_only, "DietRelated_Only_BA_Step5.xlsx")
cat("✓ FILE 2 salvato: 'DietRelated_Only_BA_Step5.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_diet_only), " articoli SOLO diet-related\n"))
cat(paste0("  Colonne: ", ncol(articles_diet_only), " (originali + found_diet_keywords)\n"))

# ===== FILE AGGIUNTIVI PER REVISIONE =====

# File con solo articoli con "nutrition/nutritional" per revisione specifica
articles_with_nutrition <- articles_to_remove %>%
  filter(grepl("nutrition", found_diet_keywords, fixed = TRUE, ignore.case = TRUE)) %>%
  arrange(Title)

if (nrow(articles_with_nutrition) > 0) {
  write_xlsx(articles_with_nutrition, "DietRelated_OnlyNutrition_ForReview.xlsx")
  cat("✓ File aggiuntivo salvato: 'DietRelated_OnlyNutrition_ForReview.xlsx'\n")
  cat(paste0("  Contiene: ", nrow(articles_with_nutrition), " articoli con nutrition/nutritional (da rivedere per falsi positivi)\n"))
}

# ===== BREAKDOWN PER CATEGORIA =====
cat("\n=== BREAKDOWN PER CATEGORIA DI KEYWORD ===\n")

# Contare articoli per tipo di keyword
df_temp <- articles_to_remove %>%
  mutate(
    has_diet_word = grepl("\\bdiet\\b|\\bdietary\\b|\\bdiets\\b", tolower(found_diet_keywords)),
    has_nutrition = grepl("nutrition", tolower(found_diet_keywords)),
    has_consumption = grepl("consumption", tolower(found_diet_keywords)),
    has_eating = grepl("eating", tolower(found_diet_keywords)),
    has_food_choice = grepl("food choice|food selection", tolower(found_diet_keywords))
  )

cat(paste0("Articoli con 'diet/dietary/diets': ", sum(df_temp$has_diet_word), "\n"))
cat(paste0("Articoli con 'nutrition/nutritional': ", sum(df_temp$has_nutrition), "\n"))
cat(paste0("Articoli con 'consumption': ", sum(df_temp$has_consumption), "\n"))
cat(paste0("Articoli con 'eating': ", sum(df_temp$has_eating), "\n"))
cat(paste0("Articoli con 'food choice/selection': ", sum(df_temp$has_food_choice), "\n"))

# ===== VERIFICA FINALE RIGOROSA =====
cat("\n=== VERIFICA FINALE (RIGOROSA) ===\n")
cat(paste0("Articoli nel file SENZA diet: ", nrow(articles_to_keep_clean), "\n"))
cat(paste0("Articoli nel file SOLO diet: ", nrow(articles_diet_only), "\n"))
cat(paste0("TOTALE: ", nrow(articles_to_keep_clean) + nrow(articles_diet_only), 
           " = ", nrow(df), 
           ifelse(nrow(articles_to_keep_clean) + nrow(articles_diet_only) == nrow(df), " ✓", " ✗"), "\n\n"))

# Verificare che le colonne siano corrette
cat(paste0("Colonne originali: ", ncol(df), "\n"))
cat(paste0("Colonne file SENZA diet: ", ncol(articles_to_keep_clean), "\n"))
cat(paste0("Colonne file SOLO diet: ", ncol(articles_diet_only), " (originali + keywords)\n"))

if (ncol(articles_to_keep_clean) == ncol(df)) {
  cat("✅ Tutte le colonne originali mantenute nel file pulito!\n\n")
} else {
  cat("⚠️  Differenza nel numero di colonne!\n\n")
}

# Test keywords principali rimaste nel file SENZA diet (case-insensitive)
remaining_diet <- sum(grepl("\\bdiet\\b|\\bdiets\\b|\\bdietary\\b", 
                            tolower(articles_to_keep_clean$Title), perl = TRUE))
remaining_nutrition <- sum(grepl("\\bnutrition\\b|\\bnutritional\\b", 
                                 tolower(articles_to_keep_clean$Title), perl = TRUE))
remaining_consumption <- sum(grepl("consumption", 
                                   tolower(articles_to_keep_clean$Title), perl = TRUE))

cat("Articoli rimasti con keywords nel file SENZA diet:\n")
cat(paste0("  'diet/diets/dietary': ", remaining_diet, "\n"))
cat(paste0("  'nutrition/nutritional': ", remaining_nutrition, "\n"))
cat(paste0("  'consumption': ", remaining_consumption, "\n"))

if (remaining_diet > 0) {
  cat("\n⚠️  ATTENZIONE: Ci sono ancora articoli con 'diet' rimasti!\n")
  leftover_diet <- articles_to_keep_clean %>%
    filter(grepl("\\bdiet\\b|\\bdiets\\b|\\bdietary\\b", tolower(Title), perl = TRUE))
  
  cat(paste0("Totale: ", nrow(leftover_diet), "\n"))
  cat("Esempi (primi 10):\n")
  for (i in 1:min(10, nrow(leftover_diet))) {
    cat(paste0(i, ". ", leftover_diet$Title[i], "\n"))
  }
  
  write_xlsx(leftover_diet, "DEBUG_RemainingDietArticles.xlsx")
  cat("\n✓ Salvati per debug: 'DEBUG_RemainingDietArticles.xlsx'\n")
} else {
  cat("\n✅ Nessun articolo con 'diet/dietary/diets' rimasto nel file pulito!\n")
}

# Mostrare nomi delle colonne mantenute
cat("\n=== COLONNE NEL FILE SENZA DIET ===\n")
cat(paste(names(articles_to_keep_clean), collapse = "\n"))
cat("\n")

cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli iniziali (Step 4): ", nrow(df), "\n"))
cat(paste0("Articoli diet-related identificati: ", nrow(articles_diet_only), "\n"))
cat(paste0("Articoli NON diet-related: ", nrow(articles_to_keep_clean), "\n"))
cat(paste0("Percentuale diet-related: ", round((nrow(articles_diet_only) / nrow(df)) * 100, 2), "%\n"))

cat("\n=== FILE CREATI ===\n")
cat("1. 'Total_WithoutDiet_BA_Step5.xlsx' - Dataset SENZA articoli diet-related\n")
cat("2. 'DietRelated_Only_BA_Step5.xlsx' - SOLO articoli diet-related\n")
cat("3. 'DietRelated_OnlyNutrition_ForReview.xlsx' - Articoli con nutrition/nutritional (per revisione)\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. Usa 'Total_WithoutDiet_BA_Step5.xlsx' per continuare l'analisi\n")
cat("2. Rivedi 'DietRelated_OnlyNutrition_ForReview.xlsx' per verificare falsi positivi\n")
cat("3. Se trovi falsi positivi, recuperali e aggiungili al file principale\n")
cat("4. Procedi con gli step successivi (plant-based, social, etc.)\n")

cat("\n✅ Separazione completata! Tutte le forme (maiuscole/minuscole) sono state cercate.\n")