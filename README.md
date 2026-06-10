# Serum-free media development strategies for cultivated meat: a machine learning-assisted systematic scoping review

This repository contains the R and Python scripts used for the automated literature screening and machine learning-based classification pipeline described in:

> Anzà, B., Fraterrigo Garofalo, S., Massarelli, E., Massai, D.N.C., Fino, D. *Serum-free media development strategies for cultivated meat: a machine learning-assisted systematic scoping review.* Food Reviews International (under review).

---

## Repository Structure

```
├── README.md
├── R_scripts/
│   ├── Script_Add_Columns_Articletype_Language_OpenAccess_BA.R   # Step 3a
│   ├── Script_IdentifyArticleType__Step3_BA.R                    # Step 3b
│   ├── Script_IdentifyConsumerPaper_Step4_BA.R                   # Step 4
│   ├── Script_Identify_removal_diet_Step_5.R                     # Step 5
│   ├── Script_Identify_removal_plantbased_Step6.R                # Step 6
│   ├── Script_Identify_removal_socialsec_Step7.R                 # Step 7
│   ├── Script_Identify_removal_nonenglish_Step8_BA.R             # Step 8
│   ├── Script_Identify_removal_human_Step9_BA.R                  # Step 9
│   ├── Script_identify_removal_NOTOpenaccess_Step10_BA.R         # Step 10
│   ├── SCript_identiy_addAbstract_BA.R                           # Step 11
│   └── Script_clustering_laststepofscreening_Step12.R            # Step 12
└── Python_scripts/
    └── Bert_final_before_topic_reduction_BA.py                   # BERTopic classification
```

---

## Pipeline Overview

The full screening and classification pipeline consists of two parts: an R-based automated screening pipeline (Steps 1–12) and a Python-based machine learning classification pipeline (BERTopic).

### Part 1 — R Screening Pipeline

> **Note:** Steps 1 and 2 (database search and duplicate removal) were performed manually in Microsoft Excel and are not included in this repository. The pipeline starts from Step 3, which expects a deduplicated Excel file as input.

| Step | Script | Description | Input file | Output file |
|------|--------|-------------|------------|-------------|
| 3a | `Script_Add_Columns_Articletype_Language_OpenAccess_BA.R` | Retrieves article type, language, and open access status via OpenAlex API | `Total_AfterRDuplicatesRemoval_BA_Step2.xlsx` | `Total_AfterRDuplicatesRemoval_BA_Step2_WithArticleType.xlsx` |
| 3b | `Script_IdentifyArticleType__Step3_BA.R` | Removes non-article records (reviews, book chapters, notes, commentary) | `Total_AfterRDuplicatesRemoval_BA_Step2_WithArticleType.xlsx` | `Total_AfterRArticleTypeRemoval_BA_Step3.xlsx` |
| 4 | `Script_IdentifyConsumerPaper_Step4_BA.R` | Removes consumer behaviour and sensory studies | `Total_AfterRArticleTypeRemoval_BA_Step3.xlsx` | `Total_AfterConsumerRemoval_BA_Step4_AllForms.xlsx` |
| 5 | `Script_Identify_removal_diet_Step_5.R` | Removes diet and dietary behaviour studies | `Total_AfterConsumerRemoval_BA_Step4_AllForms.xlsx` | `Total_WithoutDiet_BA_Step5.xlsx` |
| 6 | `Script_Identify_removal_plantbased_Step6.R` | Removes plant-based food product studies | `Total_WithoutDiet_BA_Step5.xlsx` | `Total_AfterPlantBasedRemoval_BA_Step6.xlsx` |
| 7 | `Script_Identify_removal_socialsec_Step7.R` | Removes social impact and food security studies | `Total_AfterPlantBasedRemoval_BA_Step6.xlsx` | `Total_AfterSocialRemoval_BA_Step7.xlsx` |
| 8 | `Script_Identify_removal_nonenglish_Step8_BA.R` | Removes non-English records | `Total_AfterSocialRemoval_BA_Step7.xlsx` | `Total_AfterNonEnglishRemoval_Step8.xlsx` |
| 9 | `Script_Identify_removal_human_Step9_BA.R` | Removes human cell line studies | `Total_AfterNonEnglishRemoval_Step8.xlsx` | `Total_AfterHumanRemoval_BA_Step9.xlsx` |
| 10 | `Script_identify_removal_NOTOpenaccess_Step10_BA.R` | Removes non-open access records | `Total_AfterHumanRemoval_BA_Step9.xlsx` | `Total_OpenAccessOnly_BA_Step10.xlsx` |
| 11 | `SCript_identiy_addAbstract_BA.R` | Retrieves missing abstracts via OpenAlex API | `Total_OpenAccessOnly_BA_Step10.xlsx` | `Total_WithAbstracts_BA_Step11.xlsx` |
| 12 | `Script_clustering_laststepofscreening_Step12.R` | K-means and hierarchical clustering to stratify records by technical content | `Total_WithAbstracts_BA_Step11.xlsx` | `papers_with_clusters_final_english.xlsx` |

#### Clustering parameters (Step 12)

K-means clustering was performed using the following parameters:
- `k = 2` (determined by Silhouette and Elbow analysis)
- `algorithm = "Lloyd"`
- `nstart = 50`
- `iter.max = 300`

Hierarchical clustering was performed using Ward's minimum variance method (`method = "ward.D2"`) with Euclidean distance.

Prior to clustering, text (title + abstract) was preprocessed via TF-IDF vectorization with stemming and English stopword removal using the `tm` package in R.

---

### Part 2 — Python BERTopic Classification Pipeline

The BERTopic script takes as input the 98 papers retained after human-supervised full-text eligibility screening of Cluster 1 (n=113) from Step 12, and classifies them into eight predefined research categories.

#### Classification pipeline:
1. Title and abstract concatenation and encoding using **SciBERT** (`allenai/scibert_scivocab_uncased`)
2. Dimensionality reduction using **UMAP** (`n_neighbors=15`, `n_components=5`, `min_dist=0.0`)
3. Topic clustering using **HDBSCAN** (`min_cluster_size=2`)
4. Topic-to-category mapping using a keyword scoring algorithm
5. Hierarchical rule-based refinement (3 priority levels)
6. Iterative manual validation with whitelist system

#### Research categories:
1. Plant-based and Circular Approaches
2. Microbial and Recombinant Proteins
3. Animal-based Ingredients
4. Bioprocess Optimization
5. Cell Line Development
6. Computational Modelling
7. Life Cycle Assessment
8. Other

The full keyword lexicons used for each category are reported in Supplementary Table S3 of the manuscript.

---

## Requirements

### R (version 4.4.2)

Install required packages by running:

```r
install.packages(c(
  "readxl",
  "writexl",
  "dplyr",
  "httr",
  "jsonlite",
  "tm",
  "SnowballC",
  "factoextra",
  "cluster",
  "ggplot2",
  "tidytext",
  "Rtsne",
  "umap",
  "gridExtra"
))
```

### Python (version 3.14)

Install required packages by running:

```bash
pip install bertopic
pip install sentence-transformers
pip install pandas
pip install plotly
pip install matplotlib
pip install numpy
pip install scikit-learn
```

---

## How to Run

1. **Prepare your input file:** Export your deduplicated records from Scopus, Web of Science, and PubMed as an Excel file. Rename it to `Total_AfterRDuplicatesRemoval_BA_Step2.xlsx`. Ensure it contains at minimum a `Title` column and a `DOI` column.

2. **Run the R scripts in order:** Execute each script sequentially from Step 3a to Step 12. Each script reads the output of the previous step. Set your working directory to the folder containing both the scripts and the Excel files.

3. **Human-supervised eligibility screening:** After Step 12, manually review Cluster 1 papers (n=113 in the original study) against the inclusion and exclusion criteria defined in the manuscript. Retain papers meeting all criteria (n=98 in the original study).

4. **Run the BERTopic script:** Provide the retained papers as input to `Bert_final_before_topic_reduction_BA.py` and run the classification pipeline.

---

## Notes

- Steps 1 and 2 (database search and deduplication) were performed manually and are not scripted.
- The exclusion of non-open access records (Step 10) was operationally driven by the requirement for full-text access to verify medium formulation disclosure. This represents a potential source of selection bias, as discussed in the manuscript.
- The OpenAlex API (Steps 3a and 11) is free and does not require an API key. However, rate limiting may apply for large datasets.
- All keyword lexicons used for screening (Steps 4–10) are reported in Supplementary Table S2 of the manuscript.
- All keyword lexicons used for BERTopic classification are reported in Supplementary Table S3 of the manuscript.

---

## License

This repository is shared for reproducibility purposes in accordance with open science principles. If you use these scripts, please cite the original manuscript.

---

## Contact

For questions regarding the scripts or pipeline, please contact:
**Bruna Anzà** — bruna.anza@polito.it  
**Silvia Fraterrigo Garofalo** — silvia.fraterrigo@polito.it  
Department of Applied Science and Technology (DISAT), Politecnico di Torino, Italy
