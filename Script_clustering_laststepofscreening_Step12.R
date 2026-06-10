# ============================================================================
# R Script for Scientific Paper Clustering - FULL ENGLISH VERSION
# ============================================================================

# Install and load required packages
packages <- c("readxl", "tm", "SnowballC", "factoextra", "cluster", 
              "ggplot2", "dplyr", "tidytext", "writexl", 
              "Rtsne", "umap")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# 1. DATA LOADING
# ============================================================================

cat("=== LOADING FILE ===\n")

# File name (without extension - script will try .xlsx and .xls)
file_name <- "Total_WithAbstracts_BA_Step11"

# Show current working directory
cat("Current working directory:", getwd(), "\n")

# Try to find the file with different extensions
possible_files <- c(
  paste0(file_name, ".xlsx"),
  paste0(file_name, ".xls")
)

file_path <- NULL
for (f in possible_files) {
  if (file.exists(f)) {
    file_path <- f
    break
  }
}

# If not found, search in current directory
if (is.null(file_path)) {
  cat("\n⚠️ File not found in current directory.\n")
  cat("Files searched:\n")
  for (f in possible_files) {
    cat("  -", normalizePath(f, mustWork = FALSE), "\n")
  }
  
  cat("\nExcel files available in current directory:\n")
  excel_files <- list.files(pattern = "\\.xlsx$|\\.xls$", ignore.case = TRUE)
  if (length(excel_files) > 0) {
    for (ef in excel_files) {
      cat("  -", ef, "\n")
    }
  } else {
    cat("  (no Excel files found)\n")
  }
  
  stop("\n❌ ERROR: File 'Total_WithAbstracts_BA_Step11.xlsx' not found!\nMake sure the file is in the working directory.")
}

cat("✅ File found:", basename(file_path), "\n")
cat("Full path:", normalizePath(file_path), "\n\n")

# Read the file
papers <- read_excel(file_path)

cat("Dataset dimensions:", dim(papers), "\n")
cat("Available columns:\n")
print(names(papers))

# ============================================================================
# 2. TEXT PREPROCESSING
# ============================================================================

# Text columns to use for clustering
text_columns <- c("Title", "Abstract")
available_cols <- intersect(text_columns, names(papers))

if (length(available_cols) == 0) {
  stop("No text columns found. Check column names!")
}

# Data completeness analysis
cat("\n=== DATA COMPLETENESS ANALYSIS ===\n")
has_title <- !is.na(papers$Title) & nchar(papers$Title) > 0
has_abstract <- !is.na(papers$Abstract) & nchar(papers$Abstract) > 0

cat("Papers with Title + Abstract:", sum(has_title & has_abstract), "\n")
cat("Papers with Title only:", sum(has_title & !has_abstract), "\n")
cat("Papers without Title:", sum(!has_title), "\n")
cat("Papers completely empty:", sum(!has_title & !has_abstract), "\n")

# Combine text columns
papers$combined_text <- apply(papers[, available_cols, drop = FALSE], 1, 
                              function(x) paste(na.omit(x), collapse = " "))

# Filter completely empty papers
empty_papers <- which(!has_title & !has_abstract)
if (length(empty_papers) > 0) {
  cat("\n⚠️ Removing", length(empty_papers), "completely empty papers\n")
  papers <- papers[-empty_papers, ]
}

cat("\nPapers used for clustering:", nrow(papers), "\n\n")

# Create corpus
corpus <- Corpus(VectorSource(papers$combined_text))

# Text cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language = "english")

# ============================================================================
# 3. TF-IDF MATRIX CREATION
# ============================================================================

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Remove too common or too rare terms
# Keep only terms present in 5-95% of documents
dtm <- removeSparseTerms(dtm, 0.95)

# Remove also too frequent terms (present in >90% documents)
term_freq <- colSums(as.matrix(dtm)) / nrow(dtm)
dtm <- dtm[, term_freq < 0.9 & term_freq > 0.05]

# Convert to TF-IDF
dtm_tfidf <- weightTfIdf(dtm)

# Identify and handle empty documents
empty_docs <- which(rowSums(as.matrix(dtm_tfidf)) == 0)
if (length(empty_docs) > 0) {
  cat("\n⚠️ WARNING: Found", length(empty_docs), "empty documents after preprocessing:\n")
  for (doc_id in empty_docs) {
    cat("\nPaper", doc_id, ":\n")
    cat("Title:", substr(papers$Title[doc_id], 1, 80), "...\n")
    if (!is.na(papers$Abstract[doc_id])) {
      cat("Abstract length:", nchar(papers$Abstract[doc_id]), "characters\n")
    } else {
      cat("Abstract: MISSING\n")
    }
  }
  
  # Remove empty documents to avoid clustering problems
  cat("\n→ Removing these documents from clustering...\n")
  papers_backup <- papers
  papers <- papers[-empty_docs, ]
  tfidf_matrix_temp <- as.matrix(dtm_tfidf)
  tfidf_matrix_temp <- tfidf_matrix_temp[-empty_docs, ]
  
  cat("Documents after removal:", nrow(papers), "\n\n")
} else {
  tfidf_matrix_temp <- as.matrix(dtm_tfidf)
}

# Convert to matrix and normalize by document length
tfidf_matrix <- tfidf_matrix_temp

# L2 normalization (each document has norm 1)
tfidf_matrix <- tfidf_matrix / sqrt(rowSums(tfidf_matrix^2))
tfidf_matrix[is.nan(tfidf_matrix)] <- 0

cat("\nTF-IDF matrix dimensions:", dim(tfidf_matrix), "\n")
cat("Number of terms used:", ncol(tfidf_matrix), "\n")

# ============================================================================
# 4. DETERMINATION OF OPTIMAL NUMBER OF CLUSTERS
# ============================================================================

# Elbow Method
set.seed(123)
fviz_nbclust(tfidf_matrix, kmeans, method = "wss", k.max = 10) +
  ggtitle("Elbow Method for Optimal K") +
  xlab("Number of clusters k") +
  ylab("Total Within Sum of Squares") +
  theme_minimal()
ggsave("elbow_plot_final_english.png", width = 10, height = 6, dpi = 300)

# Silhouette Method
fviz_nbclust(tfidf_matrix, kmeans, method = "silhouette", k.max = 10) +
  ggtitle("Silhouette Method for Optimal K") +
  xlab("Number of clusters k") +
  ylab("Average Silhouette Width") +
  theme_minimal()
ggsave("silhouette_plot_final_english.png", width = 10, height = 6, dpi = 300)

# ============================================================================
# 5. K-MEANS CLUSTERING
# ============================================================================

# Optimal number of clusters based on Elbow and Silhouette analysis
k <- 2

# Run k-means with more attempts and iterations
set.seed(123)
kmeans_result <- kmeans(tfidf_matrix, centers = k, nstart = 50, iter.max = 300, algorithm = "Lloyd")

# Add cluster to original dataset
papers$cluster <- kmeans_result$cluster

cat("\n=== CLUSTERING RESULTS ===\n")
cat("Number of clusters:", k, "\n")
cat("Distribution of papers per cluster:\n")
print(table(papers$cluster))

# ============================================================================
# 6. HIERARCHICAL CLUSTERING
# ============================================================================

# Calculate distances
dist_matrix <- dist(tfidf_matrix, method = "euclidean")

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Dendrogram
png("dendrogram_final_english.png", width = 1200, height = 800, res = 100)
plot(hc, main = "Hierarchical Clustering Dendrogram", 
     xlab = "", sub = "", cex = 0.5, ylab = "Height")
rect.hclust(hc, k = k, border = 2:6)
dev.off()

papers$cluster_hierarchical <- cutree(hc, k = k)

# ============================================================================
# 7. CLUSTER ANALYSIS
# ============================================================================

# Find most representative terms for each cluster
top_terms_per_cluster <- function(cluster_id, n_terms = 10) {
  cluster_docs <- which(papers$cluster == cluster_id)
  cluster_tfidf <- colMeans(tfidf_matrix[cluster_docs, , drop = FALSE])
  top_terms <- names(sort(cluster_tfidf, decreasing = TRUE)[1:n_terms])
  return(top_terms)
}

cat("\n=== TOP TERMS PER CLUSTER ===\n")
for (i in 1:k) {
  cat("\nCluster", i, "(", sum(papers$cluster == i), "papers ):\n")
  terms <- top_terms_per_cluster(i, n_terms = 15)
  cat(paste(terms, collapse = ", "), "\n")
}

# ============================================================================
# 8. MULTIPLE VISUALIZATIONS
# ============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

## 8.1 PCA
cat("Creating PCA visualization...\n")
pca <- prcomp(tfidf_matrix, scale. = TRUE)
pca_data <- data.frame(
  Dim1 = pca$x[, 1],
  Dim2 = pca$x[, 2],
  Cluster = as.factor(papers$cluster)
)

pca_plot <- ggplot(pca_data, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  stat_ellipse(level = 0.95, linetype = 2) +
  theme_minimal() +
  labs(title = "PCA Visualization",
       x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)")) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(pca_plot)
ggsave("cluster_PCA_final_english.png", pca_plot, width = 12, height = 8, dpi = 300)

## 8.2 t-SNE
cat("Creating t-SNE visualization (may take a few minutes)...\n")

# Identify duplicates in TF-IDF matrix (after preprocessing)
cat("\n=== TF-IDF MATRIX DUPLICATE ANALYSIS ===\n")
cat("(These are papers that become identical AFTER preprocessing)\n\n")

unique_rows <- !duplicated(tfidf_matrix)
duplicate_rows <- which(!unique_rows)
n_duplicates <- length(duplicate_rows)

if (n_duplicates > 0) {
  cat("⚠️ Found", n_duplicates, "documents that become identical after preprocessing:\n\n")
  
  # For each duplicate, find the original
  for (dup_idx in duplicate_rows) {
    for (i in 1:(dup_idx-1)) {
      if (all(tfidf_matrix[i,] == tfidf_matrix[dup_idx,])) {
        cat("Duplicate pair #", which(duplicate_rows == dup_idx), ":\n", sep="")
        cat("  Paper", dup_idx, ":\n")
        cat("    Title:", substr(papers$Title[dup_idx], 1, 80), "\n")
        if (!is.na(papers$Abstract[dup_idx]) && nchar(papers$Abstract[dup_idx]) > 0) {
          cat("    Abstract:", substr(papers$Abstract[dup_idx], 1, 100), "...\n")
        } else {
          cat("    Abstract: MISSING\n")
        }
        cat("  Paper", i, "(identical after preprocessing):\n")
        cat("    Title:", substr(papers$Title[i], 1, 80), "\n")
        if (!is.na(papers$Abstract[i]) && nchar(papers$Abstract[i]) > 0) {
          cat("    Abstract:", substr(papers$Abstract[i], 1, 100), "...\n")
        } else {
          cat("    Abstract: MISSING\n")
        }
        cat("  → Probably same content with minor variations\n\n")
        break
      }
    }
  }
  
  cat("NOTE: These papers are DIFFERENT in the original file, but become identical after:\n")
  cat("  - Stopword removal (the, and, is, etc.)\n")
  cat("  - Stemming (cultivation → cultiv)\n")
  cat("  - Rare/common term filtering\n")
  cat("→ They will be temporarily removed only for t-SNE (which requires unique documents).\n")
  cat("→ They remain in the final dataset and clustering.\n\n")
  
  tfidf_unique <- tfidf_matrix[unique_rows, ]
  cluster_unique <- papers$cluster[unique_rows]
} else {
  cat("✅ No duplicates found after preprocessing!\n\n")
  tfidf_unique <- tfidf_matrix
  cluster_unique <- papers$cluster
}

# Calculate appropriate perplexity (must be < n_samples/3)
max_perplexity <- floor(nrow(tfidf_unique) / 3) - 1
perplexity_value <- min(30, max_perplexity)

cat("Documents used for t-SNE:", nrow(tfidf_unique), "\n")
cat("Perplexity used:", perplexity_value, "\n\n")

set.seed(123)
tsne_result <- Rtsne(tfidf_unique, dims = 2, perplexity = perplexity_value, 
                     verbose = FALSE, max_iter = 1000, check_duplicates = FALSE)

tsne_data <- data.frame(
  Dim1 = tsne_result$Y[, 1],
  Dim2 = tsne_result$Y[, 2],
  Cluster = as.factor(cluster_unique)
)

tsne_plot <- ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  stat_ellipse(level = 0.95, linetype = 2) +
  theme_minimal() +
  labs(title = "t-SNE Visualization",
       subtitle = paste0("(", nrow(tsne_data), " unique documents after preprocessing)"),
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

print(tsne_plot)
ggsave("cluster_tSNE_final_english.png", tsne_plot, width = 12, height = 8, dpi = 300)

## 8.3 UMAP
cat("Creating UMAP visualization...\n")
set.seed(123)
umap_config <- umap.defaults
umap_config$n_neighbors <- 15
umap_config$min_dist <- 0.1
umap_result <- umap(tfidf_matrix, config = umap_config)

umap_data <- data.frame(
  Dim1 = umap_result$layout[, 1],
  Dim2 = umap_result$layout[, 2],
  Cluster = as.factor(papers$cluster)
)

umap_plot <- ggplot(umap_data, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  stat_ellipse(level = 0.95, linetype = 2) +
  theme_minimal() +
  labs(title = "UMAP Visualization",
       x = "UMAP Dimension 1",
       y = "UMAP Dimension 2") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(umap_plot)
ggsave("cluster_UMAP_final_english.png", umap_plot, width = 12, height = 8, dpi = 300)

## 8.4 Side-by-side comparison
cat("Creating comparative visualization...\n")
library(gridExtra)

pca_plot_small <- pca_plot + theme(legend.position = "none")
tsne_plot_small <- tsne_plot + theme(legend.position = "none")
umap_plot_small <- umap_plot + theme(legend.position = "none")

combined_plot <- grid.arrange(pca_plot_small, tsne_plot_small, umap_plot_small, 
                              ncol = 3, top = "Comparison of Visualization Methods")

ggsave("cluster_comparison_final_english.png", combined_plot, width = 18, height = 6, dpi = 300)
cat("\n✅ All visualizations have been created and displayed!\n")

# ============================================================================
# 9. RESULTS EXPORT
# ============================================================================

# Save complete results
write_xlsx(papers, "papers_with_clusters_final_english.xlsx")

# Save cluster summary
cluster_summary <- papers %>%
  group_by(cluster) %>%
  summarise(
    n_papers = n(),
    percentage = round(n() / nrow(papers) * 100, 2)
  )
write_xlsx(cluster_summary, "cluster_summary_final_english.xlsx")

cat("\n=== SCRIPT COMPLETED ===\n")
cat("Files created:\n")
cat("- papers_with_clusters_final_english.xlsx\n")
cat("- cluster_summary_final_english.xlsx\n")
cat("- elbow_plot_final_english.png\n")
cat("- silhouette_plot_final_english.png\n")
cat("- dendrogram_final_english.png\n")
cat("- cluster_PCA_final_english.png\n")
cat("- cluster_tSNE_final_english.png\n")
cat("- cluster_UMAP_final_english.png\n")
cat("- cluster_comparison_final_english.png (side-by-side comparison)\n")
cat("\nRECOMMENDATION: Use t-SNE visualization for better cluster separation!\n")
