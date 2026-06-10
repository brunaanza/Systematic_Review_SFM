# Caricare le librerie
library(readxl)
library(writexl)
library(dplyr)

# Leggere il file Excel
df <- read_excel("Total_AfterRDuplicatesRemoval_BA_Step2.xlsx")

cat("=== IDENTIFICAZIONE E RIMOZIONE NON-ARTICLE ===\n\n")

# Statistiche iniziali
cat(paste0("Totale articoli iniziali: ", nrow(df), "\n\n"))

# Verificare se la colonna Articletype esiste
if (!"Articletype" %in% names(df)) {
  cat("ERRORE: La colonna 'Articletype' non esiste nel file!\n")
  cat("Colonne disponibili:\n")
  print(names(df))
  stop("Colonna 'Articletype' non trovata")
}

# Mostrare tutti i tipi di articolo presenti
cat("=== TIPI DI ARTICOLO PRESENTI ===\n")
articletype_summary <- df %>%
  group_by(Articletype) %>%
  summarise(Numero = n()) %>%
  arrange(desc(Numero))
print(articletype_summary)

# Identificare i NON "article"
# Considera come "article" anche valori con maiuscole/minuscole diverse
non_articles <- df %>%
  filter(tolower(Articletype) != "article" | is.na(Articletype))

cat("\n=== ARTICOLI NON-ARTICLE IDENTIFICATI ===\n")
cat(paste0("Totale NON-article da rimuovere: ", nrow(non_articles), "\n\n"))

# Mostrare sommario dei tipi NON-article
cat("=== DETTAGLIO TIPI NON-ARTICLE ===\n")
non_article_types <- non_articles %>%
  group_by(Articletype) %>%
  summarise(Numero = n()) %>%
  arrange(desc(Numero))
print(non_article_types)

# Mostrare alcuni esempi di NON-article
cat("\n=== ESEMPI DI NON-ARTICLE (prime 20 righe) ===\n")
examples <- non_articles %>%
  select(DOI, Title, Authors, Articletype) %>%
  head(20)
print(examples)

# Filtrare per mantenere SOLO gli "article"
articles_only <- df %>%
  filter(tolower(Articletype) == "article")

# Statistiche finali
cat("\n=== STATISTICHE FINALI ===\n")
cat(paste0("Articoli iniziali: ", nrow(df), "\n"))
cat(paste0("NON-article rimossi: ", nrow(non_articles), "\n"))
cat(paste0("Articoli rimanenti: ", nrow(articles_only), "\n"))
cat(paste0("Percentuale rimossa: ", round((nrow(non_articles) / nrow(df)) * 100, 2), "%\n"))

# Salvare il file con solo gli "article"
write_xlsx(articles_only, "Total_AfterRArticleTypeRemoval_BA_Step3.xlsx")

cat("\n✓ File salvato con successo come 'Total_AfterRArticleTypeRemoval_BA_Step3.xlsx'\n")

# Opzionale: salvare anche un file con i NON-article rimossi per riferimento
write_xlsx(non_articles, "Removed_NonArticles_Step3.xlsx")
cat("✓ File con articoli rimossi salvato come 'Removed_NonArticles_Step3.xlsx' (per riferimento)\n")

# Mostrare le colonne nel file finale
cat("\n=== COLONNE NEL FILE FINALE ===\n")
cat(paste(names(articles_only), collapse = "\n"))
cat("\n")

# Verifica finale
cat("\n=== VERIFICA FINALE ===\n")
cat("Tutti gli articoli nel nuovo file sono 'article'?\n")
verification <- articles_only %>%
  group_by(Articletype) %>%
  summarise(Numero = n())
print(verification)
