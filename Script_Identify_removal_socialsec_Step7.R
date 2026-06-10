# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel CORRETTO
df <- read_excel("Total_AfterPlantBasedRemoval_BA_Step6.xlsx")

cat("=== RIMOZIONE ARTICOLI SOCIAL IMPACT & FOOD SECURITY (TUTTE LE FORME) ===\n\n")
cat(paste0("Totale articoli nel file: ", nrow(df), "\n"))
cat(paste0("Totale colonne originali: ", ncol(df), "\n\n"))

# ===== KEYWORDS SOCIAL IMPACT & FOOD SECURITY (case-insensitive) =====
keywords_social_food_security <- c(
  # Food security (singolare/plurale)
  "food security", "food insecurity", "food secure",
  "nutritional security", "nutrition security",
  "food access", "food availability", "food affordability",
  "food poverty", "hunger",
  
  # Social impact (singolare/plurale)
  "social impact", "social impacts", "social implication", "social implications",
  "social acceptance", "social aspect", "social aspects", "social dimension", "social dimensions",
  "social sustainability", "socio-economic", "socioeconomic",
  "social equity", "social justice", "food justice",
  "societal impact", "societal impacts",
  
  # Employment/labor (singolare/plurale)
  "employment", "job creation", "labor", "labour",
  "workforce", "livelihood", "livelihoods", "income",
  "job security", "employment opportunity", "employment opportunities",
  
  # Community/society
  "community impact", "community impacts", "societal impact", "societal change",
  "social change", "rural development", "farmer", "farmers",
  "smallholder", "smallholders", "developing country", "developing countries",
  "community development",
  
  # Policy/governance
  "food policy", "food policies", "food governance",
  "policy implication", "policy implications", "public policy",
  "food system", "food systems",
  
  # Inequality/access (singolare/plurale)
  "inequality", "inequalities", "food inequality",
  "access to food", "vulnerable population", "vulnerable populations",
  "food distribution", "social disparity", "social disparities",
  
  # Cultural/social norms
  "cultural acceptance", "cultural aspect", "cultural aspects",
  "social norm", "social norms", "tradition", "traditions",
  "cultural identity", "cultural barrier", "cultural barriers",
  
  # Welfare/wellbeing
  "social welfare", "social wellbeing", "social well-being",
  "community welfare", "human welfare",
  
  # Economic/social development
  "socioeconomic development", "socio-economic development",
  "economic development", "social development",
  "poverty", "poverty alleviation", "poverty reduction",
  
  # Ethics/values
  "social value", "social values", "ethical issue", "ethical issues",
  "moral concern", "moral concerns", "social responsibility",
  
  # Additional specific terms
  "food desert", "food deserts", "food sovereignty",
  "right to food", "global food security"
)

# Identificare articoli social/food security - CASE INSENSITIVE
df$is_social_food_sec <- FALSE
df$found_social_keywords <- ""

cat("Processando articoli (case-insensitive per tutte le forme)...\n")

for (i in 1:nrow(df)) {
  if (!is.na(df$Title[i]) && df$Title[i] != "") {
    title_lower <- tolower(df$Title[i])
    found <- c()
    
    for (keyword in keywords_social_food_security) {
      # Cercare in modo case-insensitive
      keyword_lower <- tolower(keyword)
      if (grepl(keyword_lower, title_lower, fixed = TRUE)) {
        found <- c(found, keyword)
        df$is_social_food_sec[i] <- TRUE
      }
    }
    
    if (length(found) > 0) {
      df$found_social_keywords[i] <- paste(found, collapse = "; ")
    }
  }
  
  if (i %% 100 == 0) {
    cat(paste0("Processati ", i, " articoli su ", nrow(df), "\n"))
  }
}

cat("\nCompletato!\n\n")

# Separare MANTENENDO TUTTE LE COLONNE
articles_social <- df %>% filter(is_social_food_sec == TRUE)
articles_other <- df %>% filter(is_social_food_sec == FALSE)

# ===== VERIFICA VARIANTI SPECIFICHE =====
cat("=== VERIFICA VARIANTI TROVATE ===\n\n")

# Contare forme diverse delle keywords principali
food_security_var <- sum(grepl("food security", df$Title, ignore.case = TRUE))
social_impact_singular <- sum(grepl("social impact\\b", df$Title, ignore.case = TRUE, perl = TRUE))
social_impacts_plural <- sum(grepl("social impacts", df$Title, ignore.case = TRUE))
farmer_singular <- sum(grepl("\\bfarmer\\b", df$Title, ignore.case = TRUE, perl = TRUE))
farmers_plural <- sum(grepl("\\bfarmers\\b", df$Title, ignore.case = TRUE, perl = TRUE))
employment_var <- sum(grepl("employment", df$Title, ignore.case = TRUE))
inequality_singular <- sum(grepl("inequality\\b", df$Title, ignore.case = TRUE, perl = TRUE))
inequalities_plural <- sum(grepl("inequalities", df$Title, ignore.case = TRUE))
policy_singular <- sum(grepl("\\bpolicy\\b", df$Title, ignore.case = TRUE, perl = TRUE))
policies_plural <- sum(grepl("policies", df$Title, ignore.case = TRUE))
livelihood_singular <- sum(grepl("livelihood\\b", df$Title, ignore.case = TRUE, perl = TRUE))
livelihoods_plural <- sum(grepl("livelihoods", df$Title, ignore.case = TRUE))

cat("Varianti trovate:\n")
cat(paste0("  'food security': ", food_security_var, "\n"))
cat(paste0("  'social impact' (singolare): ", social_impact_singular, "\n"))
cat(paste0("  'social impacts' (plurale): ", social_impacts_plural, "\n"))
cat(paste0("  'farmer' (singolare): ", farmer_singular, "\n"))
cat(paste0("  'farmers' (plurale): ", farmers_plural, "\n"))
cat(paste0("  'employment': ", employment_var, "\n"))
cat(paste0("  'inequality' (singolare): ", inequality_singular, "\n"))
cat(paste0("  'inequalities' (plurale): ", inequalities_plural, "\n"))
cat(paste0("  'policy' (singolare): ", policy_singular, "\n"))
cat(paste0("  'policies' (plurale): ", policies_plural, "\n"))
cat(paste0("  'livelihood' (singolare): ", livelihood_singular, "\n"))
cat(paste0("  'livelihoods' (plurale): ", livelihoods_plural, "\n\n"))

# ===== STATISTICHE =====
cat("=== STATISTICHE ===\n")
cat(paste0("Articoli social/food security identificati: ", nrow(articles_social), "\n"))
cat(paste0("Articoli senza social/food security: ", nrow(articles_other), "\n"))
cat(paste0("Percentuale social/food security: ", round((nrow(articles_social) / nrow(df)) * 100, 2), "%\n\n"))

# Keywords più frequenti
cat("=== KEYWORDS PIÙ FREQUENTI ===\n")
all_keywords <- unlist(strsplit(articles_social$found_social_keywords[articles_social$found_social_keywords != ""], "; "))
if (length(all_keywords) > 0) {
  keyword_freq <- sort(table(all_keywords), decreasing = TRUE)
  print(head(keyword_freq, 20))
}

# ===== ESEMPI =====
cat("\n=== ESEMPI ARTICOLI DA RIMUOVERE (primi 30) ===\n\n")
examples <- articles_social %>%
  select(Title, found_social_keywords) %>%
  head(30)

for (i in 1:nrow(examples)) {
  cat(paste0(i, ". ", examples$Title[i], "\n"))
  cat(paste0("   Keywords: ", examples$found_social_keywords[i], "\n\n"))
}

# ===== SALVARE I 2 FILE PRINCIPALI - MANTENENDO TUTTE LE COLONNE =====

# FILE 1: SOLO articoli social/food security
articles_social_clean <- articles_social %>%
  select(-is_social_food_sec)  # Rimuovo solo il flag, tengo found_keywords

write_xlsx(articles_social_clean, "SocialFoodSecurity_Only_BA_Step7.xlsx")
cat("\n✓ FILE 1 salvato: 'SocialFoodSecurity_Only_BA_Step7.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_social_clean), " articoli SOLO social/food security\n"))
cat(paste0("  Colonne: ", ncol(articles_social_clean), " (originali + found_social_keywords)\n"))

# FILE 2: Articoli SENZA social/food security (dataset pulito)
articles_other_clean <- articles_other %>%
  select(-is_social_food_sec, -found_social_keywords)  # Rimuovo colonne aggiunte

write_xlsx(articles_other_clean, "Total_AfterSocialRemoval_BA_Step7.xlsx")
cat("✓ FILE 2 salvato: 'Total_AfterSocialRemoval_BA_Step7.xlsx'\n")
cat(paste0("  Contiene: ", nrow(articles_other_clean), " articoli SENZA social/food security\n"))
cat(paste0("  Colonne: ", ncol(articles_other_clean), " (tutte le originali)\n"))

# ===== BREAKDOWN PER CATEGORIA =====
cat("\n=== BREAKDOWN PER CATEGORIA ===\n")

if (nrow(articles_social) > 0) {
  articles_temp <- articles_social %>%
    mutate(
      has_food_security = grepl("food security|food insecurity|nutritional security", 
                                tolower(found_social_keywords)),
      has_social_impact = grepl("social impact|social implication|societal impact", 
                                tolower(found_social_keywords)),
      has_employment = grepl("employment|job|labor|labour|livelihood", 
                             tolower(found_social_keywords)),
      has_policy = grepl("policy|governance", 
                         tolower(found_social_keywords)),
      has_farmer = grepl("farmer|smallholder|rural", 
                         tolower(found_social_keywords)),
      has_inequality = grepl("inequality|equity|justice|vulnerable", 
                             tolower(found_social_keywords)),
      has_cultural = grepl("cultural|tradition|social norm", 
                           tolower(found_social_keywords)),
      has_food_system = grepl("food system", 
                              tolower(found_social_keywords))
    )
  
  cat(paste0("Food security: ", sum(articles_temp$has_food_security), "\n"))
  cat(paste0("Social impacts: ", sum(articles_temp$has_social_impact), "\n"))
  cat(paste0("Employment/livelihood: ", sum(articles_temp$has_employment), "\n"))
  cat(paste0("Policy/governance: ", sum(articles_temp$has_policy), "\n"))
  cat(paste0("Farmer/rural: ", sum(articles_temp$has_farmer), "\n"))
  cat(paste0("Inequality/equity: ", sum(articles_temp$has_inequality), "\n"))
  cat(paste0("Cultural aspects: ", sum(articles_temp$has_cultural), "\n"))
  cat(paste0("Food system: ", sum(articles_temp$has_food_system), "\n"))
}

# ===== VERIFICA FINALE RIGOROSA =====
cat("\n=== VERIFICA FINALE (RIGOROSA) ===\n")
cat(paste0("Articoli iniziali (Step 6): ", nrow(df), "\n"))
cat(paste0("Articoli social/food security: ", nrow(articles_social_clean), "\n"))
cat(paste0("Articoli senza social/food security: ", nrow(articles_other_clean), "\n"))
cat(paste0("Verifica somma: ", nrow(articles_social_clean) + nrow(articles_other_clean), 
           " = ", nrow(df), 
           ifelse(nrow(articles_social_clean) + nrow(articles_other_clean) == nrow(df), " ✓", " ✗"), "\n\n"))

# Verificare colonne
cat(paste0("Colonne originali: ", ncol(df), "\n"))
cat(paste0("Colonne file SENZA social: ", ncol(articles_other_clean), "\n"))
cat(paste0("Colonne file SOLO social: ", ncol(articles_social_clean), " (originali + keywords)\n"))

if (ncol(articles_other_clean) == ncol(df)) {
  cat("✅ Tutte le colonne originali mantenute nel file pulito!\n\n")
} else {
  cat("⚠️  Differenza nel numero di colonne!\n\n")
}

# Test keywords principali rimaste (case-insensitive)
remaining_food_security <- sum(grepl("food security|food insecurity", 
                                     tolower(articles_other_clean$Title), perl = TRUE))
remaining_social_impact <- sum(grepl("social impact", 
                                     tolower(articles_other_clean$Title), perl = TRUE))
remaining_farmer <- sum(grepl("\\bfarmers?\\b", 
                              tolower(articles_other_clean$Title), perl = TRUE))
remaining_employment <- sum(grepl("employment", 
                                  tolower(articles_other_clean$Title), perl = TRUE))

cat("Articoli rimasti con keywords nel file pulito:\n")
cat(paste0("  'food security/insecurity': ", remaining_food_security, "\n"))
cat(paste0("  'social impact': ", remaining_social_impact, "\n"))
cat(paste0("  'farmer/farmers': ", remaining_farmer, "\n"))
cat(paste0("  'employment': ", remaining_employment, "\n"))

if (remaining_food_security > 0 || remaining_social_impact > 0) {
  cat("\n⚠️  ATTENZIONE: Ci sono ancora articoli social/food security rimasti!\n")
  leftover <- articles_other_clean %>%
    filter(grepl("food security|social impact|farmers?|employment", 
                 tolower(Title), perl = TRUE))
  
  cat(paste0("Totale: ", nrow(leftover), "\n"))
  cat("Esempi (primi 10):\n")
  for (i in 1:min(10, nrow(leftover))) {
    cat(paste0(i, ". ", leftover$Title[i], "\n"))
  }
  
  write_xlsx(leftover, "DEBUG_RemainingSocialArticles.xlsx")
  cat("\n✓ Salvati per debug: 'DEBUG_RemainingSocialArticles.xlsx'\n")
} else {
  cat("\n✅ Nessun articolo social/food security rimasto nel file pulito!\n")
}

# Mostrare nomi delle colonne
cat("\n=== COLONNE NEL FILE PULITO ===\n")
cat(paste(names(articles_other_clean), collapse = "\n"))
cat("\n")

cat("\n=== RIEPILOGO FINALE ===\n")
cat(paste0("Articoli iniziali (Step 6): ", nrow(df), "\n"))
cat(paste0("Articoli social/food security rimossi: ", nrow(articles_social_clean), "\n"))
cat(paste0("Articoli finali (Step 7): ", nrow(articles_other_clean), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(articles_social_clean) / nrow(df)) * 100, 2), "%\n"))
cat(paste0("Colonne mantenute: ", ncol(articles_other_clean), " (tutte le originali)\n"))

cat("\n=== FILE CREATI ===\n")
cat("1. 'SocialFoodSecurity_Only_BA_Step7.xlsx' - SOLO articoli social/food security\n")
cat("2. 'Total_AfterSocialRemoval_BA_Step7.xlsx' - Dataset SENZA social/food security (FINALE)\n")

cat("\n=== PROSSIMI PASSI ===\n")
cat("1. 'Total_AfterSocialRemoval_BA_Step7.xlsx' è il tuo dataset FINALE pulito!\n")
cat("2. Rivedi gli articoli rimossi se necessario\n")
cat("3. Procedi con le analisi quantitative (PCA, bibliometrics, etc.)\n")

cat("\n✅ Rimozione completata! Tutte le forme (singolare/plurale/maiuscole/minuscole) sono state cercate.\n")