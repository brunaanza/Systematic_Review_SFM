import pandas as pd
from bertopic import BERTopic
from sentence_transformers import SentenceTransformer
import plotly.io as pio
import matplotlib.pyplot as plt
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer

# =============================================================================
# BERT TOPIC MODELING + KEYWORD MATCHING - VERSION 11 (WHITELIST SYSTEM)
# =============================================================================
# MAJOR CHANGES IN V11:
# 
# 🎯 NEW FEATURE: WHITELIST SYSTEM (Learns from your corrections!)
# 
# HOW IT WORKS:
# - Reads your previous classification Excel file (e.g., BERTopic_V10_Papers_CLASSIFIED.xlsx)
# - Papers with EMPTY "Correct_category" = Already classified CORRECTLY ✅
#   → These go into WHITELIST and keep their current classification
# - Papers with FILLED "Correct_category" = Need RECLASSIFICATION ❌
#   → These are reclassified using your manual correction
# - NEW papers (not in previous file) = Get classified with BERT + Enhanced Rules
# 
# BENEFITS:
# ✅ Papers you verified as correct NEVER change category (100% accuracy maintained)
# ✅ Papers you corrected get the right category immediately
# ✅ Only NEW papers or uncorrected ones go through full classification
# ✅ Accuracy improves with each iteration as whitelist grows
# 
# 1. WHITELIST (ABSOLUTE PRIORITY - Checked FIRST!)
#    - Loads correctly classified papers from previous classification file
#    - Papers in whitelist keep their verified category forever
#    - Also loads manual corrections and applies them immediately
# 
# 2. ENHANCED RULES (Based on error analysis)
#    - "protein expression and purification" → Microbial & Recombinant
#    - "kluyveromyces" + "leghemoglobin" → Microbial (yeast-specific)
#    - "spontaneous immortalization" → Cell Line Development
#    - "insect fat" OR "insect cultivation" → Animal-based Ingredients
#    - "life cycle assessment" in TITLE → Life Cycle Assessment
#    - "food by-product" + "screening" → Animal-based Ingredients
#    - "food-derived plant extract" → Plant-based (specific context)
# 
# 3. All previous V10 rules maintained
# 
# Expected: 100% accuracy on whitelisted + corrected papers, improved on new papers
# =============================================================================

# =============================================================================
# INITIAL CONFIGURATION
# =============================================================================
DATA_FILE = "Total_papers_title_screening_Step14_BA.csv"
OUTPUT_PREFIX = "BERTopic_V11"

# =============================================================================
# V11: WHITELIST CONFIGURATION
# =============================================================================
# File containing manually corrected papers (with Correct_category column)
WHITELIST_FILE = "BERTopic_V10_Papers_CLASSIFIED.xlsx"  # Change this to your corrected file
USE_WHITELIST = True  # Set to False to disable whitelist

# =============================================================================
# DATA LOADING
# =============================================================================
print("Loading dataset...")
# Flexible dataset loading
import os

if DATA_FILE.endswith(".xlsx"):
    df = pd.read_excel(DATA_FILE)
else:
    try:
        df = pd.read_csv(DATA_FILE, encoding="utf-8", sep=None, engine="python")
    except Exception:
        df = pd.read_csv(DATA_FILE, encoding="latin-1", sep=None, engine="python")

print(f"File loaded successfully ({len(df)} rows). Columns found:")
print(df.columns.tolist())

# =============================================================================
# V11: LOAD WHITELIST (Manually Corrected Papers)
# =============================================================================
# IMPORTANT: Papers with EMPTY Correct_category are CORRECTLY classified
#            Papers with FILLED Correct_category need to be reclassified
whitelist_dict = {}  # Title -> Verified correct category
papers_to_reclassify = {}  # Title -> New correct category

if USE_WHITELIST and os.path.exists(WHITELIST_FILE):
    print(f"\n{'='*80}")
    print("LOADING WHITELIST (Already Correct Classifications)")
    print(f"{'='*80}")
    try:
        df_whitelist = pd.read_excel(WHITELIST_FILE)
        
        # Find columns
        correct_col = None
        final_col = None
        for col in df_whitelist.columns:
            if 'correct' in col.lower() and 'category' in col.lower():
                correct_col = col
            if 'final' in col.lower() and 'category' in col.lower():
                final_col = col
        
        if correct_col and final_col:
            # Build dictionaries
            for idx, row in df_whitelist.iterrows():
                title = row['Title'].strip()
                correct_cat = row[correct_col]
                final_cat = row[final_col]
                
                # EMPTY Correct_category = Already correct, add to whitelist
                if pd.isna(correct_cat) or str(correct_cat).strip() == '':
                    whitelist_dict[title] = final_cat
                
                # FILLED Correct_category = Wrong, need reclassification
                else:
                    papers_to_reclassify[title] = str(correct_cat).strip()
            
            print(f"✓ Loaded {len(whitelist_dict)} CORRECTLY classified papers (whitelist)")
            print(f"  These papers will KEEP their current classification")
            print(f"✓ Loaded {len(papers_to_reclassify)} papers that need RECLASSIFICATION")
            print(f"  These papers will use the corrected category")
        else:
            print("⚠ Warning: Required columns not found in whitelist file")
            print(f"  Looking for: 'Correct_category' and 'Final_Category'")
    except Exception as e:
        print(f"⚠ Warning: Could not load whitelist file: {e}")
        print("  Continuing without whitelist...")
else:
    if not USE_WHITELIST:
        print("\nWhitelist disabled (USE_WHITELIST = False)")
    else:
        print(f"\n⚠ Whitelist file not found: {WHITELIST_FILE}")
        print("  Continuing without whitelist...")
print()

# =============================================================================
# TEXT PREPARATION
# =============================================================================
print("\nPreparing texts...")
# Find title and abstract columns
title_col = [col for col in df.columns if 'title' in col.lower() or 'titolo' in col.lower()][0]
abstract_col = [col for col in df.columns if 'abstract' in col.lower() or 'riassunto' in col.lower()][0]

print(f"Using columns: Title='{title_col}', Abstract='{abstract_col}'")

# Combine title + abstract
df['combined_text'] = df[title_col].astype(str) + ". " + df[abstract_col].astype(str)

# Remove duplicates and filter too short texts
df_clean = df[df['combined_text'].str.len() > 100].drop_duplicates(subset='combined_text')
docs = df_clean['combined_text'].tolist()

print(f"Documents to analyze: {len(docs)}")

# =============================================================================
# MODELS: EMBEDDING + BERTopic
# =============================================================================
print("\nCreating embedding model (SciBERT)...")
embedding_model = SentenceTransformer("allenai/scibert_scivocab_uncased")

# STOPWORDS - Remove useless words from topics
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS
custom_stopwords = list(ENGLISH_STOP_WORDS) + [
    'study', 'studies', 'result', 'results', 'method', 'methods',
    'using', 'used', 'based', 'paper', 'research', 'approach',
    'show', 'showed', 'found', 'also', 'however', 'therefore',
    'can', 'may', 'could', 'would', 'will', 'within', 'across'
]

# Vectorizer with stopwords
vectorizer_model = CountVectorizer(
    stop_words=custom_stopwords,
    min_df=2,
    ngram_range=(1, 2)  # Include bigrams like "serum free"
)

print("Running BERTopic (with stopwords removal)...")
print("(This may take 5-10 minutes...)")
topic_model = BERTopic(
    embedding_model=embedding_model,
    vectorizer_model=vectorizer_model,  # Use custom vectorizer
    verbose=True,
    min_topic_size=2,  # Reduced to 2 to capture more papers
    nr_topics=11  # FIXED at 11 for consistency with final analysis
)
topics, probs = topic_model.fit_transform(docs)

# =============================================================================
# MAPPING BERT TOPICS TO CUSTOM CATEGORIES
# =============================================================================
print("\n" + "="*80)
print("MAPPING BERT TOPICS TO CUSTOM CATEGORIES")
print("="*80)
print()

# Define your 7 custom categories with keywords (ENGLISH)
custom_categories = {
    'Plant-based & Circular Approaches': {
        'keywords': [
            # Plant-based proteins (V10: Added soy, soybean, plant extract)
            'plant', 'soy', 'soybean', 'wheat', 'rice', 'pea', 'corn', 'cereal',
            'legume', 'protein hydrolysate', 'hydrolysate', 'plant extract', 
            'plant derived', 'plant protein', 'plant-based',
            
            # Plant-based media (V10: Added Essential 8)
            'essential 8', 'essential8',
            
            # Circular economy & waste (V9: ADDED agro-industrial waste)
            'side stream', 'byproduct', 'by-product', 'waste', 'circular', 
            'circular economy', 'valorization', 'valorisation', 'upcycling',
            'spent grain', 'brewery', 'pomace', 'press cake',
            'agro industrial waste', 'agro-industrial waste', 'agroindustrial waste',
            'agro industrial by-product', 'agro-industrial by-product',
            'agricultural waste', 'agricultural by-product',
            
            # Microalgae (photosynthetic)
            'microalgae', 'microalga', 'algae', 'spirulina', 'chlorella', 
            'seaweed', 'cyanobacteria', 'nitrogen-fixing', 'nitrogen fixing',
            
            # Sustainability
            'sustainable', 'renewable', 'alternative protein'
        ]
    },
    
    'Animal-based Ingredients': {
        'keywords': [
            # Egg-derived (V10: Broadened to include general egg/eggs)
            'egg', 'eggs', 'egg extract', 'egg white', 'egg yolk', 'chicken egg',
            'egg protein', 'egg-derived', 'ovalbumin',
            
            # Dairy/Milk-derived
            'milk', 'whey', 'milk whey', 'dairy', 'milk-derived',
            'casein', 'lactose', 'milk protein', 'whey protein',
            
            # Insect-derived
            'insect', 'insect protein', 'insect hydrolysate', 'insect cell',
            'insect fat', 'insect-derived', 'insect muscle',
            
            # Marine animal-derived
            'marine invertebrate', 'marine protein', 'fish hydrolysate',
            'marine hydrolysate', 'shellfish', 'fish protein',
            
            # Other animal derivatives (ADDED BSA!)
            'bsa', 'bovine serum albumin', 'bovine albumin',
            'collagen', 'gelatin', 'animal-derived', 'heme protein',
            'serum substitute', 'fbs substitute', 'serum replacement'
        ]
    },
    
    'Microbial & Recombinant Proteins': {
        'keywords': [
            # Recombinant proteins
            'recombinant', 'recombinant protein', 'recombinant albumin',
            'recombinant growth factor', 'albumin', 'expressed', 
            'heterologous', 'engineered protein',
            
            # Yeast
            'yeast', 'yeast extract', 'yeast lysate', 'saccharomyces', 
            'pichia', 'pichia pastoris', 'kluyveromyces',
            
            # Microbial (V9: ADDED methyl cyclodextrin)
            'microbial', 'bacterial', 'bacteria', 'escherichia', 'e. coli',
            'fungal', 'fungi', 'fermentation', 'fermented',
            'microbial protein', 'lysate', 'microbial lysate',
            'bacterial lysate', 'cell lysate', 'biomass',
            'methyl cyclodextrin', 'methyl-cyclodextrin', 'methylcyclodextrin'
        ]
    },
    
    'Bioprocess Optimization': {
        'keywords': [
            # V7: ONLY technical equipment and specific processes
            # Removed: "production", "manufacturing", "yield", "productivity" (too generic!)
            'bioprocess', 'bioreactor', 'microcarrier', 'perfusion', 
            'fed batch', 'fed-batch', 'hollow fiber', 'spinner flask',
            'stirred tank', 'suspension culture',
            'scale up', 'scale-up', 'upscaling', 'scalability',
            'process optimization', 'industrial scale',
            # Keep some specific terms
            'seeding density', 'expansion system'
        ]
    },
    
    'Cell Line Development': {
        'keywords': [
            # Genetic/immortalization (V10: Added DNA and spontaneous)
            'cell line', 'immortalized', 'immortalization', 'engineered',
            'genetic', 'transfection', 'myod', 'pax7', 'cdkn2a',
            'gene expression', 'overexpression', 'stable line',
            'selection', 'screening', 'clone', 'adaptation',
            'dna', 'dna identification', 'dna analysis',
            'spontaneous', 'spontaneously', 'spontaneous immortalization',
            
            # Cell culture and development
            'satellite cells', 'satellite cell', 'proliferation',
            'differentiation', 'myogenic', 'myoblast', 'myotube',
            'characterization', 'phenotype', 'marker',
            'progenitor cell', 'stem cell', 'primary cell',
            
            # Media development (BROAD - catch-all)
            'media development', 'medium development', 'culture medium',
            'serum-free', 'chemically defined', 'cell culture',
            'cell expansion', 'cell maintenance'
        ]
    },
    
    'Computational Modelling': {
        'keywords': [
            'model', 'modeling', 'modelling', 'computational',
            'simulation', 'algorithm', 'bayesian', 'bayesian optimization',
            'machine learning', 'artificial intelligence', 'neural network',
            'deep learning', 'optimization algorithm', 'genetic algorithm',
            'design experiment', 'design of experiment', 'doe',
            'response surface', 'predictive', 'mathematical',
            'kinetic', 'metabolic', 'silico', 'in silico'
        ]
    },
    
    'Life Cycle Assessment': {
        'keywords': [
            'life cycle', 'lca', 'environmental impact', 'environmental assessment',
            'sustainability assessment', 'carbon footprint', 'greenhouse gas',
            'emissions', 'climate', 'water consumption', 'water footprint',
            'energy consumption', 'resource', 'eutrophication',
            'eco efficiency', 'impact assessment', 'cradle', 'gate'
        ]
    }
}

def map_bert_topic_to_category(topic_id, topic_model, custom_categories):
    """Map a BERT topic to custom category with improved weighted scoring"""
    # Get topic words
    topic_words = topic_model.get_topic(topic_id)
    if not topic_words or topic_id == -1:
        return "Other", {}, 0
    
    topic_terms = [word for word, _ in topic_words[:30]]  # Top 30 terms
    topic_str = ' '.join(topic_terms).lower()
    
    # HIGH PRIORITY KEYWORDS (weight: 3) - V4 LOGIC
    # These categories get HIGHEST priority and override others
    high_priority = {
        # PRIORITY 1: Computational (VERY specific - overrides everything)
        'Computational Modelling': [
            'bayesian optimization', 'bayesian algorithm',
            'machine learning', 'artificial intelligence', 'neural network',
            'deep learning', 'optimization algorithm', 'genetic algorithm'
        ],
        # PRIORITY 2: Specific ingredient categories
        'Animal-based Ingredients': [
            'bsa', 'bovine serum albumin', 'bovine albumin',
            'egg extract', 'egg white', 'egg yolk', 
            'milk whey', 'whey protein', 'milk-derived',
            'insect protein', 'insect cell', 'insect hydrolysate',
            'marine invertebrate', 'marine protein', 'fish hydrolysate',
            'serum substitute', 'fbs substitute'
        ],
        'Plant-based & Circular Approaches': [
            'plant protein', 'plant extract', 'plant hydrolysate', 'plant-based',
            'soy protein', 'soybean', 'wheat protein', 'rice protein',
            'microalgae', 'spirulina', 'chlorella',
            'cyanobacteria', 'nitrogen-fixing', 'nitrogen fixing',
            'byproduct', 'by-product', 'side stream', 'circular economy'
        ],
        'Microbial & Recombinant Proteins': [
            'recombinant protein', 'recombinant albumin', 'recombinant growth factor',
            'yeast extract', 'yeast lysate', 'microbial lysate', 'bacterial lysate',
            'pichia pastoris', 'kluyveromyces', 'e. coli'
        ],
        # PRIORITY 3: Technical bioprocess terms
        'Bioprocess Optimization': [
            'bioreactor', 'microcarrier', 'hollow fiber',
            'scale-up', 'scale up', 'perfusion', 'fed-batch',
            'stirred tank', 'spinner flask'
        ],
        # PRIORITY 4: LCA (very specific)
        'Life Cycle Assessment': [
            'lca', 'life cycle assessment', 'environmental impact assessment',
            'carbon footprint'
        ],
        # NOTE: Cell Line Development has NO high priority keywords
        # It will be the DEFAULT/CATCH-ALL category
        'Cell Line Development': []
    }
    
    # MEDIUM PRIORITY KEYWORDS (weight: 2)
    medium_priority = {
        'Plant-based & Circular Approaches': [
            'plant', 'soy', 'wheat', 'rice', 'pea',
            'algae', 'sustainable', 'renewable'
        ],
        'Animal-based Ingredients': [
            'egg', 'milk', 'whey', 'dairy', 'insect', 'marine',
            'collagen', 'gelatin', 'heme protein'
        ],
        'Microbial & Recombinant Proteins': [
            'yeast', 'microbial', 'bacterial', 'recombinant',
            'fermentation', 'lysate', 'expressed', 'heterologous'
        ],
        'Bioprocess Optimization': [
            # V7: Only keep "bioprocess", "scale", "scalability" 
            # Removed: "manufacturing", "production", "yield", "productivity"
            'bioprocess', 'scale', 'scalability', 'upscaling'
        ],
        'Cell Line Development': [
            # BROAD keywords for catch-all
            'satellite cell', 'satellite cells', 'progenitor cell',
            'stem cell', 'proliferation', 'differentiation',
            'myogenic', 'myoblast', 'cell culture',
            'serum-free', 'chemically defined', 'medium', 'media'
        ],
        'Computational Modelling': [
            'algorithm', 'computational', 'simulation', 
            'optimization', 'model', 'predictive'
        ],
        'Life Cycle Assessment': [
            'life cycle', 'sustainability assessment', 'environmental',
            'emissions', 'impact assessment'
        ]
    }
    
    # Calculate weighted scores
    scores = {}
    matches = {}
    
    for cat_name in custom_categories.keys():
        score = 0
        matched = []
        
        # High priority (weight: 3)
        if cat_name in high_priority:
            for kw in high_priority[cat_name]:
                if kw in topic_str:
                    score += 3
                    matched.append(f"{kw}(H)")
        
        # Medium priority (weight: 2)
        if cat_name in medium_priority:
            for kw in medium_priority[cat_name]:
                if kw in topic_str:
                    score += 2
                    matched.append(f"{kw}(M)")
        
        # Regular keywords (weight: 1)
        for kw in custom_categories[cat_name]['keywords']:
            if kw in topic_str and kw not in str(matched):
                score += 1
                matched.append(f"{kw}(L)")
        
        scores[cat_name] = score
        matches[cat_name] = matched
    
    # Get best category with minimum threshold
    max_score = max(scores.values())
    
    if max_score >= 3:  # Strong match
        best_category = max(scores, key=scores.get)
        return best_category, matches[best_category], max_score
    elif max_score >= 1:  # Weak match - will be corrected by rules
        best_category = max(scores, key=scores.get)
        return best_category, matches[best_category], max_score
    else:  # No match - will be handled by rules
        return "NEEDS_RULES", matches, 0

# Get topic info
topic_info = topic_model.get_topic_info()

# Map each BERT topic to custom category
topic_mapping = {}
topic_scores = {}
print("BERT mapping topics to your 6 custom categories:\n")

for topic_id in topic_info['Topic']:
    if topic_id == -1:
        topic_mapping[topic_id] = "Other"
        topic_scores[topic_id] = 0
        continue
    
    category, matched_keywords, score = map_bert_topic_to_category(topic_id, topic_model, custom_categories)
    topic_mapping[topic_id] = category
    topic_scores[topic_id] = score
    
    # Get topic terms
    topic_words = topic_model.get_topic(topic_id)
    top_terms = [word for word, _ in topic_words[:10]] if topic_words else []
    
    print(f"BERT Topic {topic_id} -> {category} (score: {score})")
    print(f"  Top terms: {', '.join(top_terms)}")
    if matched_keywords:
        # Convert matched_keywords to list if needed
        if isinstance(matched_keywords, dict):
            kw_list = []
            for cat_kws in matched_keywords.values():
                if cat_kws:
                    kw_list.extend(cat_kws)
            keywords_str = ', '.join(str(kw) for kw in kw_list[:8])
        elif isinstance(matched_keywords, list):
            keywords_str = ', '.join(str(kw) for kw in matched_keywords[:8])
        else:
            keywords_str = str(matched_keywords)
        
        if keywords_str:
            print(f"  Matched keywords: {keywords_str}")
    print()

# Apply mapping to papers
print("Applying mapping to papers...")
df_clean['BERT_Topic'] = topics
df_clean['BERT_Probability'] = probs
df_clean['Custom_Category'] = df_clean['BERT_Topic'].map(topic_mapping)
df_clean['Topic_Score'] = df_clean['BERT_Topic'].map(topic_scores)

# POST-PROCESSING: Rule-based classification with V4 PRIORITY LOGIC
print("\nApplying rule-based classification (V4 priority logic)...")

corrected = 0
rules_applied = {}

for idx, row in df_clean.iterrows():
    title = str(row.get('Title', '')).lower()
    title_original = str(row.get('Title', '')).strip()  # Keep original for whitelist matching
    abstract = str(row.get('Abstract', '')).lower()
    combined = title + ' ' + abstract
    current_category = df_clean.at[idx, 'Custom_Category']
    score = df_clean.at[idx, 'Topic_Score']
    
    # ========================================================================
    # V11: WHITELIST CHECK (ABSOLUTE PRIORITY - Checked BEFORE ANYTHING ELSE!)
    # ========================================================================
    
    # Case 1: Paper is CORRECTLY classified (empty Correct_category in Excel)
    # → Keep current classification, SKIP all rules
    if title_original in whitelist_dict:
        verified_category = whitelist_dict[title_original]
        df_clean.at[idx, 'Custom_Category'] = verified_category
        # Track that this came from whitelist
        if 'whitelist_correct' not in rules_applied:
            rules_applied['whitelist_correct'] = 0
        rules_applied['whitelist_correct'] += 1
        continue  # SKIP all other processing for this paper
    
    # Case 2: Paper needs RECLASSIFICATION (filled Correct_category in Excel)
    # → Use the corrected category from manual correction
    if title_original in papers_to_reclassify:
        corrected_category = papers_to_reclassify[title_original]
        df_clean.at[idx, 'Custom_Category'] = corrected_category
        # Track that this came from manual correction
        if 'manual_correction' not in rules_applied:
            rules_applied['manual_correction'] = 0
        rules_applied['manual_correction'] += 1
        continue  # SKIP all other processing for this paper
    
    # Case 3: Paper is NEW (not in previous classification file)
    # → Apply all rules normally
    
    # Apply rules to ALL papers with weak scores OR "NEEDS_RULES"
    should_check_rules = (score < 3) or (current_category == "NEEDS_RULES")
    
    new_category = None
    rule_used = None
    
    # ========================================================================
    # SUPER HIGH PRIORITY V8: TITLE-BASED RULES (checked FIRST!)
    # These are ABSOLUTE - if in title, classification is certain
    # ========================================================================
    
    # V8 FIX 1: SATELLITE CELL / MYOSATELLITE in TITLE → ALWAYS Cell Line
    # Even if paper mentions plant/heme protein, if title has satellite cell = Cell Line paper
    if 'satellite cell' in title or 'myosatellite' in title:
        new_category = 'Cell Line Development'
        rule_used = 'SUPER PRIORITY V8: Satellite cell in TITLE'
    
    # ========================================================================
    # SUPER HIGH PRIORITY: EXPLICIT INGREDIENT KEYWORDS
    # V10: Added Essential 8, plant extract, soy, egg rules
    # These override EVERYTHING - if ingredient is mentioned, that's the category
    # ========================================================================
    
    # V10 NEW: EGG / EGGS → ALWAYS Animal-based (broader than V8)
    # Captures all egg-related ingredients
    elif ' egg ' in combined or ' eggs ' in combined or combined.startswith('egg ') or combined.endswith(' egg'):
        new_category = 'Animal-based Ingredients'
        rule_used = 'SUPER PRIORITY V10: Egg/eggs (ingredient)'
    
    # V8 FIX 2: EGG EXTRACTION/PROTEIN → ALWAYS Animal-based (kept for specificity)
    # More specific than just "egg" which can be in unrelated words
    elif 'egg extraction' in combined or 'egg protein' in combined or 'egg white protein' in combined:
        new_category = 'Animal-based Ingredients'
        rule_used = 'SUPER PRIORITY V8: Egg extraction/protein (ingredient)'
    
    # V10 NEW: ESSENTIAL 8 / PLANT EXTRACT / SOY → Plant-based
    elif 'essential 8' in combined or 'essential8' in combined or 'plant extract' in combined or ' soy ' in combined or 'soybean' in combined:
        new_category = 'Plant-based & Circular Approaches'
        rule_used = 'SUPER PRIORITY V10: Essential 8/Plant extract/Soy (ingredient)'
    
    # V10 NEW: DNA / SPONTANEOUS → Cell Line Development
    elif 'dna identification' in combined or 'dna analysis' in combined:
        new_category = 'Cell Line Development'
        rule_used = 'SUPER PRIORITY V10: DNA identification (cell line)'
    
    # V11 ENHANCED: SPONTANEOUS IMMORTALIZATION → Cell Line (more specific)
    elif 'spontaneous' in combined and 'immortal' in combined:
        new_category = 'Cell Line Development'
        rule_used = 'V11 ENHANCED: Spontaneous immortalization'
    
    # ========================================================================
    # V11: ENHANCED RULES (Based on error analysis of V10)
    # ========================================================================
    
    # ENHANCED 1: PROTEIN EXPRESSION AND PURIFICATION → Microbial & Recombinant
    elif 'protein expression and purification' in combined or 'expression and purification' in combined:
        new_category = 'Microbial & Recombinant Proteins'
        rule_used = 'V11 ENHANCED: Protein expression & purification'
    
    # ENHANCED 2: KLUYVEROMYCES + LEGHEMOGLOBIN → Microbial (yeast-specific recombinant)
    elif 'kluyveromyces' in combined and 'leghemoglobin' in combined:
        new_category = 'Microbial & Recombinant Proteins'
        rule_used = 'V11 ENHANCED: Kluyveromyces leghemoglobin production'
    
    # ENHANCED 3: INSECT FAT/CULTIVATION → Animal-based
    elif ('insect' in combined and ('fat' in combined or 'cultivation' in combined)) or 'insect fat' in combined:
        new_category = 'Animal-based Ingredients'
        rule_used = 'V11 ENHANCED: Insect fat/cultivation'
    
    # ENHANCED 4: LIFE CYCLE ASSESSMENT in TITLE → LCA category
    elif 'life cycle assessment' in title or 'lca' in title:
        new_category = 'Life Cycle Assessment'
        rule_used = 'V11 ENHANCED: LCA in title'
    
    # ENHANCED 5: SCREENING + FOOD BY-PRODUCT → Animal-based (context-specific)
    elif ('screening' in combined or 'evaluation' in combined) and ('food by-product' in combined or 'food industry' in combined):
        new_category = 'Animal-based Ingredients'
        rule_used = 'V11 ENHANCED: Screening food by-products'
    
    # ENHANCED 6: FOOD-DERIVED PLANT EXTRACT → Plant-based (specific context)
    elif 'food-derived plant' in combined or 'food derived plant' in combined:
        new_category = 'Plant-based & Circular Approaches'
        rule_used = 'V11 ENHANCED: Food-derived plant extract'
    
    # RAPESEED PROTEIN → ALWAYS Plant-based (even if has "recombinant" or "albumin")
    elif 'rapeseed protein' in combined or 'rapeseed isolate' in combined:
        new_category = 'Plant-based & Circular Approaches'
        rule_used = 'SUPER PRIORITY: Rapeseed protein (ingredient)'
    
    # YEAST SPECIES → ALWAYS Microbial (never Animal)
    elif 'pichia pastoris' in combined or 'kluyveromyces' in combined or 'saccharomyces cerevisiae' in combined:
        new_category = 'Microbial & Recombinant Proteins'
        rule_used = 'SUPER PRIORITY: Yeast species'
    
    # MICROBIAL LYSATE (explicit) → ALWAYS Microbial
    elif 'microbial lysate' in combined or 'bacterial lysate' in combined or 'yeast lysate' in combined:
        new_category = 'Microbial & Recombinant Proteins'
        rule_used = 'SUPER PRIORITY: Microbial/bacterial lysate (ingredient)'
    
    # IMMORTALIZATION → ALWAYS Cell Line Development
    elif 'immortalization' in combined or 'immortalized cell line' in combined:
        new_category = 'Cell Line Development'
        rule_used = 'SUPER PRIORITY: Immortalization'
    
    # EXPLICIT SOY PROTEIN (as ingredient, not just mention) → Plant-based
    elif 'soy protein hydrolysate' in combined or 'soybean protein isolate' in combined:
        new_category = 'Plant-based & Circular Approaches'
        rule_used = 'SUPER PRIORITY: Soy protein (ingredient)'
    
    # PLANT PROTEIN HYDROLYSATE (explicit) → Plant-based
    elif 'plant protein hydrolysate' in combined or 'plant-derived protein' in combined:
        new_category = 'Plant-based & Circular Approaches'
        rule_used = 'SUPER PRIORITY: Plant protein hydrolysate (ingredient)'
    
    # MICROALGAE (explicit species) → ALWAYS Plant-based
    elif 'spirulina' in combined or 'chlorella' in combined or 'microalgae extract' in combined:
        new_category = 'Plant-based & Circular Approaches'
        rule_used = 'SUPER PRIORITY: Microalgae species (ingredient)'
    
    # ========================================================================
    # PRIORITY 1: Computational Modelling (technical methods)
    # ========================================================================
    elif any(kw in combined for kw in ['bayesian optimization', 'bayesian algorithm',
                                       'machine learning', 'artificial intelligence',
                                       'neural network', 'deep learning']):
        new_category = 'Computational Modelling'
        rule_used = 'Computational (HIGH PRIORITY)'
    
    # ========================================================================
    # PRIORITY 2: Life Cycle Assessment (very specific)
    # ========================================================================
    elif 'life cycle assessment' in combined or 'lca' in title or 'environmental impact assessment' in combined:
        new_category = 'Life Cycle Assessment'
        rule_used = 'LCA keywords'
    
    # ========================================================================
    # PRIORITY 3: Specific Ingredients (Animal-based)
    # BUT: Exclude if paper is clearly about plant or microbial alternatives
    # ========================================================================
    elif any(kw in combined for kw in ['bsa', 'bovine serum albumin', 'bovine albumin',
                                         'egg extract', 'egg white', 'egg yolk', 'egg protein',
                                         'milk whey', 'whey protein', 'milk-derived', 'dairy',
                                         'insect protein', 'insect cell', 'insect hydrolysate',
                                         'marine invertebrate', 'marine protein', 'fish hydrolysate',
                                         'serum substitute', 'fbs substitute', 'serum replacement']):
        # EXCEPTION: If paper is about PLANT or MICROBIAL alternatives to animal ingredients
        # Check if it's a comparison/testing paper
        if any(kw in combined for kw in ['plant-derived', 'plant hydrolysate', 'soy hydrolysate',
                                          'microbial-derived', 'yeast-derived', 'alternative to']):
            # This is likely a paper COMPARING/TESTING alternatives, not using animal ingredients
            # Will be caught by Plant/Microbial rules later
            pass
        else:
            new_category = 'Animal-based Ingredients'
            rule_used = 'Animal-based (specific keywords)'
    
    # ========================================================================
    # PRIORITY 4: Specific Ingredients (Plant-based)
    # V9: Added agro-industrial waste keywords
    # ========================================================================
    elif any(kw in combined for kw in ['plant extract', 'plant protein', 'plant hydrolysate',
                                         'soy protein', 'soybean protein', 'wheat protein', 'rice protein',
                                         'pea protein', 'rapeseed', 'microalgae', 'microalga',
                                         'cyanobacteria', 'nitrogen-fixing',
                                         'side stream', 'byproduct', 'by-product', 'circular economy',
                                         'agro industrial waste', 'agro-industrial waste', 'agroindustrial waste',
                                         'agricultural waste', 'agricultural by-product']):
        # Exception: if recombinant production using plants
        if 'recombinant' in title and 'plant' in title:
            new_category = 'Microbial & Recombinant Proteins'
            rule_used = 'Recombinant (plant-based production)'
        else:
            new_category = 'Plant-based & Circular Approaches'
            rule_used = 'Plant-based (ingredient keywords)'
    
    # V6 NEW: Generic "plant-based" only if refers to INGREDIENT (not scaffold/matrix)
    elif 'plant-based' in combined:
        # Check context - is it about ingredient or material?
        if any(context in combined for context in ['plant-based protein', 'plant-based medium',
                                                     'plant-based ingredient', 'plant-based supplement']):
            new_category = 'Plant-based & Circular Approaches'
            rule_used = 'Plant-based (ingredient context)'
        elif any(context in combined for context in ['plant-based scaffold', 'plant-based matrix',
                                                      'plant-based material', 'plant-based structure']):
            # This is about MATERIALS, not ingredients - skip to next rule
            pass
        else:
            # Generic plant-based, likely ingredient-related
            new_category = 'Plant-based & Circular Approaches'
            rule_used = 'Plant-based (generic)'
    
    # ========================================================================
    # PRIORITY 5: Microbial & Recombinant Proteins
    # V9: Added methyl cyclodextrin
    # ========================================================================
    elif any(kw in combined for kw in ['yeast extract', 'yeast lysate', 'yeast-based',
                                         'microbial lysate', 'bacterial lysate', 'microbial protein',
                                         'recombinant protein', 'recombinant albumin', 
                                         'recombinant growth factor',
                                         'pichia pastoris', 'kluyveromyces', 'e. coli',
                                         'lactococcus', 'escherichia',
                                         'fermentation', 'heterologous expression',
                                         'methyl cyclodextrin', 'methyl-cyclodextrin', 'methylcyclodextrin']):
        new_category = 'Microbial & Recombinant Proteins'
        rule_used = 'Microbial/Recombinant keywords'
    
    # ========================================================================
    # PRIORITY 6: Bioprocess Optimization (specific technical terms)
    # ========================================================================
    elif any(kw in combined for kw in ['bioreactor', 'microcarrier', 'hollow fiber',
                                         'scale-up', 'scale up', 'scalability',
                                         'perfusion culture', 'fed-batch', 'fed batch',
                                         'stirred tank', 'suspension culture', 'spinner flask']):
        new_category = 'Bioprocess Optimization'
        rule_used = 'Bioprocess (technical keywords)'
    
    # ========================================================================
    # PRIORITY 7 (DEFAULT): Cell Line Development
    # Papers about cells/culture without specific other keywords → Cell Line
    # BUT: Must have SPECIFIC cell keywords, not just generic "media" or "serum-free"
    # ========================================================================
    elif any(kw in combined for kw in ['satellite cell', 'satellite cells',
                                         'progenitor cell', 'muscle precursor',
                                         'myogenic differentiation', 'myoblast', 'myotube',
                                         'cell line development', 'immortalized cell',
                                         'stem cell culture', 'primary cell culture']):
        new_category = 'Cell Line Development'
        rule_used = 'Cell Line (specific cell keywords)'
    
    # ========================================================================
    # FALLBACK: Generic cell culture terms (lower confidence)
    # Only if no other category matched and score is very low
    # ========================================================================
    elif score < 2 and any(kw in combined for kw in ['cell culture', 'serum-free medium',
                                                       'chemically defined medium',
                                                       'proliferation', 'differentiation']):
        # These are TOO generic - only use as last resort
        new_category = 'Cell Line Development'
        rule_used = 'Cell Line (generic fallback)'
    
    # Apply new category if found
    if new_category and (should_check_rules or new_category != current_category):
        df_clean.at[idx, 'Custom_Category'] = new_category
        corrected += 1
        rules_applied[rule_used] = rules_applied.get(rule_used, 0) + 1
    
    # If still NEEDS_RULES and no rule matched, mark as Other
    if df_clean.at[idx, 'Custom_Category'] == "NEEDS_RULES":
        df_clean.at[idx, 'Custom_Category'] = "Other"

print(f"✓ Applied {corrected} rule-based corrections")
print(f"\nRules breakdown:")

# V11: Separate whitelist and manual correction stats
whitelist_count = rules_applied.get('whitelist_correct', 0)
manual_count = rules_applied.get('manual_correction', 0)

if whitelist_count > 0:
    print(f"\n  ✅ WHITELIST (Already Correct): {whitelist_count} papers")
    print(f"     These papers kept their existing correct classification")

if manual_count > 0:
    print(f"\n  📝 MANUAL CORRECTIONS: {manual_count} papers")
    print(f"     These papers were reclassified based on your corrections")

# Remove whitelist/manual from rules_applied for cleaner report
other_rules = {k: v for k, v in rules_applied.items() 
               if k not in ['whitelist_correct', 'manual_correction']}

if other_rules:
    print(f"\n  Other rules applied to NEW papers:")
    for rule, count in sorted(other_rules.items(), key=lambda x: x[1], reverse=True):
        print(f"    - {rule}: {count} papers")
elif whitelist_count == 0 and manual_count == 0:
    # No whitelist used at all
    for rule, count in sorted(rules_applied.items(), key=lambda x: x[1], reverse=True):
        print(f"  - {rule}: {count} papers")

# Summary statistics
print("\n" + "="*80)
print("CLASSIFICATION SUMMARY")
print("="*80)
print()

category_counts = df_clean['Custom_Category'].value_counts()
print("Papers per custom category:")
for cat, count in category_counts.items():
    pct = 100 * count / len(df_clean)
    bert_topics = df_clean[df_clean['Custom_Category'] == cat]['BERT_Topic'].unique()
    bert_topics_str = ', '.join([f"T{t}" for t in sorted(bert_topics)])
    print(f"  {cat:45s}: {count:3d} papers ({pct:5.1f}%) [BERT topics: {bert_topics_str}]")
print()

# =============================================================================
# RESULTS
# =============================================================================
print("\n" + "="*80)
print("SAVING RESULTS")
print("="*80)
print()

# Save CSV with SEMICOLON for Italian Excel
df_clean.to_csv(f"{OUTPUT_PREFIX}_papers_results_FULL.csv", 
                index=False, 
                sep=';',
                encoding='utf-8-sig')

# CREATE MAIN EXCEL FILE with Custom Categories
print("Creating main Excel file...")
excel_output = df_clean[['Custom_Category', 'BERT_Topic', 'Topic_Score', 'BERT_Probability', 
                          'Authors', 'Title', 'Year', 'Abstract']].copy()
excel_output = excel_output.sort_values(['Custom_Category', 'Topic_Score', 'BERT_Probability'], 
                                        ascending=[True, False, False])

# Rename columns to English
excel_output.columns = ['Final_Category', 'BERT_Topic', 'Mapping_Score', 'Probability_%', 
                        'Authors', 'Title', 'Year', 'Abstract']
excel_output['Probability_%'] = (excel_output['Probability_%'] * 100).round(1)

# Save to EXCEL (.xlsx)
excel_output.to_excel(f"{OUTPUT_PREFIX}_Papers_CLASSIFIED.xlsx", 
                     index=False, 
                     sheet_name='Papers_by_Category')
print(f"✓ Main Excel file saved: {OUTPUT_PREFIX}_Papers_CLASSIFIED.xlsx")

# Save also simplified CSV
excel_output.to_csv(f"{OUTPUT_PREFIX}_Papers_CLASSIFIED.csv",
                   index=False,
                   sep=';',
                   encoding='utf-8-sig')
print(f"✓ Classified CSV saved: {OUTPUT_PREFIX}_Papers_CLASSIFIED.csv")

# Save topic mapping table
mapping_df = []
for topic_id, category in topic_mapping.items():
    if topic_id == -1:
        continue
    topic_words = topic_model.get_topic(topic_id)
    top_terms = [word for word, _ in topic_words[:10]] if topic_words else []
    count = len(df_clean[df_clean['BERT_Topic'] == topic_id])
    mapping_df.append({
        'BERT_Topic': topic_id,
        'Custom_Category': category,
        'Papers_Count': count,
        'Top_Terms': ', '.join(top_terms)
    })

mapping_table = pd.DataFrame(mapping_df)
mapping_table.to_csv(f"{OUTPUT_PREFIX}_Topic_Mapping.csv", 
                    index=False, 
                    encoding='utf-8-sig')
print(f"✓ Mapping saved: {OUTPUT_PREFIX}_Topic_Mapping.csv")

# Summary dei topic
topic_info = topic_model.get_topic_info()
topic_info.to_csv(f"{OUTPUT_PREFIX}_topics_summary.csv", index=False, encoding='utf-8-sig')
print(f"✓ Topic summary saved: {OUTPUT_PREFIX}_topics_summary.csv")

print("\nTopic Summary:")
print(topic_info.head(20))

# =============================================================================
# VISUALIZZAZIONI (HTML + PNG)
# =============================================================================
print("\nGenerating visualizations (HTML + PNG)...")

try:
    # Configura kaleido per PNG
    pio.kaleido.scope.default_format = "png"
    pio.kaleido.scope.default_width = 1600
    pio.kaleido.scope.default_height = 1000
    pio.kaleido.scope.default_scale = 2
    png_available = True
except:
    print("Kaleido not available, generating HTML only")
    png_available = False

# Overview Map
print("  - Overview Map...")
try:
    fig_overview = topic_model.visualize_topics()
    fig_overview.write_html(f"{OUTPUT_PREFIX}_Overview_Map.html")
    if png_available:
        pio.write_image(fig_overview, f"{OUTPUT_PREFIX}_Overview_Map.png", scale=2)
    print("    OK")
except Exception as e:
    print(f"    Errore: {e}")

# Distribution dei Topic
print("  - Topic Distribution...")
try:
    fig_barchart = topic_model.visualize_barchart(top_n_topics=20)
    fig_barchart.write_html(f"{OUTPUT_PREFIX}_Topic_Distribution.html")
    if png_available:
        pio.write_image(fig_barchart, f"{OUTPUT_PREFIX}_Topic_Distribution.png", scale=2)
    print("    OK")
except Exception as e:
    print(f"    Errore: {e}")

# Similarity tra Topic
print("  - Topic Similarity...")
try:
    fig_heatmap = topic_model.visualize_heatmap()
    fig_heatmap.write_html(f"{OUTPUT_PREFIX}_Topic_Similarity.html")
    if png_available:
        pio.write_image(fig_heatmap, f"{OUTPUT_PREFIX}_Topic_Similarity.png", scale=2)
    print("    OK")
except Exception as e:
    print(f"    Errore: {e}")

# =============================================================================
# GAP ANALYSIS (topic deboli, emergenti, outlier)
# =============================================================================
print("\nGAP analysis between topics...")
topic_sizes = topic_info[topic_info["Topic"] >= 0][["Topic", "Count"]].copy()
topic_sizes["Normalized"] = topic_sizes["Count"] / topic_sizes["Count"].sum()

weak_topics = topic_sizes[topic_sizes["Normalized"] < 0.02]
emerging_topics = topic_sizes[(topic_sizes["Normalized"] >= 0.02) & (topic_sizes["Normalized"] < 0.05)]
dominant_topics = topic_sizes[topic_sizes["Normalized"] >= 0.05]

print(f"\nWeak topics (<2%): {len(weak_topics)}")
print(f"Emerging topics (2-5%): {len(emerging_topics)}")
print(f"Dominant topics (>5%): {len(dominant_topics)}")

fig, ax = plt.subplots(figsize=(14, 6))
plt.bar(topic_sizes["Topic"], topic_sizes["Normalized"], color="lightgray", label="All Topics")
if len(weak_topics) > 0:
    plt.bar(weak_topics["Topic"], weak_topics["Normalized"], color="red", label="Weak Topics (<2%)")
if len(emerging_topics) > 0:
    plt.bar(emerging_topics["Topic"], emerging_topics["Normalized"], color="orange", label="Emerging Topics (2-5%)")
if len(dominant_topics) > 0:
    plt.bar(dominant_topics["Topic"], dominant_topics["Normalized"], color="green", label="Dominant Topics (>5%)")
plt.xlabel("Topic ID", fontsize=12, fontweight='bold')
plt.ylabel("Normalized Frequency", fontsize=12, fontweight='bold')
plt.title("GAP Analysis - Relative Strength of Topics", fontsize=14, fontweight='bold')
plt.legend()
plt.grid(True, alpha=0.3, axis='y')
plt.tight_layout()
plt.savefig(f"{OUTPUT_PREFIX}_GAP_Representation.png", dpi=300, bbox_inches='tight')
plt.close()

# =============================================================================
# RIEPILOGO FINALE
# =============================================================================
print("\n" + "="*80)
print("ANALYSIS COMPLETED!")
print("="*80)
print("\nGenerated files:")
print(f"  ★★★ {OUTPUT_PREFIX}_Papers_CLASSIFIED.xlsx ★★★  ← MAIN FILE!")
print(f"  1. {OUTPUT_PREFIX}_Papers_CLASSIFIED.csv - CSV version")
print(f"  2. {OUTPUT_PREFIX}_Topic_Mapping.csv - BERT mapping → Custom")
print(f"  3. {OUTPUT_PREFIX}_papers_results_FULL.csv - Complete data")
print(f"  4. {OUTPUT_PREFIX}_topics_summary.csv - BERT topics summary")
print(f"  5. {OUTPUT_PREFIX}_Overview_Map.html - Interactive map")
print(f"  6. {OUTPUT_PREFIX}_Topic_Distribution.html - Distribution")
print(f"  7. {OUTPUT_PREFIX}_Topic_Similarity.html - Similarity")
print(f"  8. {OUTPUT_PREFIX}_GAP_Representation.png - Gap analysis")

if png_available:
    print(f"  9. {OUTPUT_PREFIX}_Overview_Map.png")
    print(f" 10. {OUTPUT_PREFIX}_Topic_Distribution.png")
    print(f" 11. {OUTPUT_PREFIX}_Topic_Similarity.png")

print("\n" + "="*80)
print(f"Total documents analyzed: {len(docs)}")
print(f"BERT topics found: {len(topic_info[topic_info['Topic'] >= 0])}")
print(f"Final custom categories: {len(category_counts)}")
print(f"Outlier documents (Topic -1): {len(df_clean[df_clean['BERT_Topic'] == -1])}")
print("="*80)
print("\n★★★ OPEN THE CLASSIFIED EXCEL FILE! ★★★")
print(f"   → {OUTPUT_PREFIX}_Papers_CLASSIFIED.xlsx")
print("\nThis file contains:")
print("  - Final_Category: Le TUE 5 categorie + Other/Outlier")
print("  - BERT_Topic: Original topic found by BERT")
print("  - Probability_%: How confident is BERT")
print("="*80)
