# ==================================================
# 0. SETUP: Load Required Packages and Data ----
# ==================================================

library(text)       # Primary package for advanced text-based NLP.
library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization
library(tidyr)      # Data restructuring
library(stringr)    # String operations
library(tidytext)   # Text processing
library(viridis)    # Color palettes
library(EGAnet)  # EGA package for network analysis

# ==================================================
# 1. CREATE THE COMPLETE TFI DATASET (ALL 25 ITEMS) ----
# ==================================================

tfi_data <- data.frame(
  QuestionID = 1:25,
  QuestionText = c(
    # Intrusive (I)
    "What percentage of your time awake were you consciously aware of your tinnitus?",
    "How strong or loud was your tinnitus?",
    "What percentage of your time awake were you annoyed by your tinnitus?",
    
    # Sense of Control (SC)
    "Did you feel in control in regard to your tinnitus?",
    "How easy was it for you to cope with your tinnitus?",
    "How easy was it for you to ignore your tinnitus?",
    
    # Cognitive (C)
    "How much did your tinnitus interfere with your ability to concentrate?",
    "How much did your tinnitus interfere with your ability to think clearly?",
    "How much did your tinnitus interfere with your ability to focus attention on other things besides your tinnitus?",
    
    # Sleep (SL)
    "How often did your tinnitus make it difficult to fall asleep or stay asleep?",
    "How often did your tinnitus cause you difficulty in getting as much sleep as you needed?",
    "How much of the time did your tinnitus keep you from sleeping as deeply or as peacefully as you would have liked?",
    
    # Auditory (A)
    "How much has your tinnitus interfered with your ability to hear clearly?",
    "How much has your tinnitus interfered with your ability to understand people who are talking?",
    "How much has your tinnitus interfered with your ability to follow conversations in a group or at meetings?",
    
    # Relaxation (R)
    "How much has your tinnitus interfered with your quiet resting activities?",
    "How much has your tinnitus interfered with your ability to relax?",
    "How much has your tinnitus interfered with your ability to enjoy peace and quiet?",
    
    # Quality of Life (Q)
    "How much has your tinnitus interfered with your enjoyment of social activities?",
    "How much has your tinnitus interfered with your enjoyment of life?",
    "How much has your tinnitus interfered with your relationships with family, friends, and other people?",
    "How often did your tinnitus cause you difficulty performing your work or other tasks, such as home maintenance, schoolwork, or caring for children or others?",
    
    # Emotional (E)
    "How anxious or worried has your tinnitus made you feel?",
    "How bothered or upset have you been because of your tinnitus?",
    "How depressed were you because of your tinnitus?"
  ),
  IntendedDomain = c(
    rep("Intrusive", 3),
    rep("Sense of Control", 3),
    rep("Cognitive", 3),
    rep("Sleep", 3),
    rep("Auditory", 3),
    rep("Relaxation", 3),
    rep("Quality of Life", 4),
    rep("Emotional", 3)
  ),
  stringsAsFactors = FALSE
)

candidate_domains <- c(
  "Intrusive", "Sense of Control", "Cognitive", "Sleep", 
  "Auditory", "Relaxation", "Quality of Life", "Emotional"
)

tfi_domains <- tibble(
  Domain = c("Auditory", "Cognitive", "Emotional", "Intrusive", 
             "Quality of Life", "Relaxation", "Sense of Control", "Sleep"),
  Definition = c(
    "Evaluates problems hearing clearly due to tinnitus.",
    "Captures difficulties in concentration and cognitive tasks due to tinnitus.",
    "Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression).",
    "Measures how much the tinnitus intrudes on consciousness and daily life.",
    "Measures the effect of tinnitus on overall enjoyment and engagement in life activities.",
    "Examines the impact of tinnitus on relaxation and quiet activities.",
    "Determines how much control the patient feels they have over their tinnitus.",
    "Assesses the extent to which tinnitus interferes with sleep."
  )
)

# ==================================================
# 2a. ZERO-SHOT CLASSIFICATION OF TFI ITEMS ACCORDING TO DOMAIN LABELS (e.g. "Intrusive") (multi_label = F) ----
# ==================================================

# classification_results <- data.frame(
#   Item = character(),
#   Predicted_Domain = character(),
#   Confidence = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# for (i in 1:length(tfi_data$QuestionText)) {
#   test_result <- textZeroShot(tfi_data$QuestionText[i], candidate_domains)
#   test_result$Item <- tfi_data$QuestionText[i]
#   classification_results <- rbind(classification_results, test_result)
# }

# ==================================================
# 2b. DATA TRANSFORMATION: RESTRUCTURING OUTPUT ----
# ==================================================

# # Pivot labels into long format, keeping Item for context
# classification_results_long <- classification_results %>%
#   pivot_longer(
#     cols = starts_with("labels_x"), 
#     names_to = "Label_Index",
#     values_to = "Predicted_Domain"
#   ) %>%
#   mutate(Score_Index = gsub("labels", "scores", Label_Index)) %>%
#   select(sequence, Item, Label_Index, Predicted_Domain, Score_Index) # Retain Score_Index for merging
# 
# # Pivot scores separately
# classification_scores_long <- classification_results %>%
#   pivot_longer(
#     cols = starts_with("scores_x_"), 
#     names_to = "Score_Index",
#     values_to = "Confidence"
#   ) %>%
#   select(sequence, Score_Index, Confidence) # Keep only necessary columns
# 
# 
# # Perform the join using both sequence and Score_Index
# classification_results_long <- classification_results_long %>%
#   left_join(classification_scores_long, by = c("sequence", "Score_Index")) %>%
#   arrange(sequence, -Confidence) %>%
#   select(Item, Predicted_Domain, Confidence)
# 
# # Using the tfi_data for reference, add question ID to classification_results_long and arrange by QuestionID
# classification_results_long <- classification_results_long %>%
#   left_join(tfi_data, by = c("Item" = "QuestionText")) %>%
#   select(QuestionID, Item, Predicted_Domain, Confidence) %>%
#   arrange(QuestionID)
# 
# # Using the tfi_data for reference, add predicted domain to classification_results_long
# classification_results_long <- classification_results_long %>%
#   left_join(tfi_data, by = c("Item" = "QuestionText")) 
# 
# #drop rename to QuestionID.x to QuestionID, drop QuestionID.y, and select item, questionID, item, intended domain, predicted domain, and confidence
# classification_results_long <- classification_results_long %>%
#   rename(QuestionID = QuestionID.x) %>%
#   select(QuestionID, Item, IntendedDomain, Predicted_Domain, Confidence)

#write.csv(classification_results_long, "TFI_ZeroShot_Classifications_Long.csv", row.names = FALSE)

#read saved results
classification_results_long <- read.csv("TFI_ZeroShot_Classifications_Long.csv")


# ==================================================
# 2c. VISUALIZATION: INDIVIDUAL ITEM PLOTS ----
# ==================================================

for (item in unique(classification_results_long$Item)) {
  
  item_data <- classification_results_long %>% filter(Item == item)
  item_number <- unique(item_data$QuestionID)
  intended_domain <- unique(item_data$IntendedDomain)
  
  p <- ggplot(item_data, aes(
    x = reorder(Predicted_Domain, -Confidence),  
    y = Confidence,
    fill = Predicted_Domain
  )) +
    geom_bar(stat = "identity", width = 0.6) +
    theme_minimal() +
    labs(
      title = paste("Item", item_number, ":", item),
      subtitle = paste("Intended Domain:", intended_domain),
      x = "Predicted Domain",
      y = "Confidence Score",
      fill = "Predicted Domain"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
  
  print(p)
}

# Using classification_results_long, summarise how many items were correctly classified (i.e., the largest confidence score was also it's predicted domain)
correctly_classified <- classification_results_long %>%
  group_by(QuestionID) %>%
  filter(Confidence == max(Confidence)) %>%
  filter(Predicted_Domain == IntendedDomain) %>%
  summarise(Correctly_Classified = n())

# ================================================================
# 3a. ZERO-SHOT CLASSIFICATION OF TFI ITEMS ACCORDING TO DOMAIN LABELS (e.g. "Intrusive") (multi_label = T) ----
# ================================================================

# classification_results_multi <- data.frame(
#   Item = character(),
#   Predicted_Domain = character(),
#   Confidence = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# for (i in 1:length(tfi_data$QuestionText)) {
#   test_result <- textZeroShot(tfi_data$QuestionText[i], candidate_domains, multi_label = TRUE)
#   test_result$Item <- tfi_data$QuestionText[i]
#   classification_results_multi <- rbind(classification_results_multi, test_result)
# }

# ==================================================
# 3b. DATA TRANSFORMATION: RESTRUCTURING OUTPUT ----
# ==================================================

# # Pivot labels into long format, keeping Item for context
# classification_results_multi_long <- classification_results_multi %>%
#   pivot_longer(
#     cols = starts_with("labels_x"), 
#     names_to = "Label_Index",
#     values_to = "Predicted_Domain"
#   ) %>%
#   mutate(Score_Index = gsub("labels", "scores", Label_Index)) %>%
#   select(sequence, Item, Label_Index, Predicted_Domain, Score_Index) # Retain Score_Index for merging
# 
# # Pivot scores separately
# classification_scores_multi_long <- classification_results_multi %>%
#   pivot_longer(
#     cols = starts_with("scores_x_"), 
#     names_to = "Score_Index",
#     values_to = "Confidence"
#   ) %>%
#   select(sequence, Score_Index, Confidence) # Keep only necessary columns
# 
# 
# # Perform the join using both sequence and Score_Index
# classification_results_multi_long <- classification_results_multi_long %>%
#   left_join(classification_scores_multi_long, by = c("sequence", "Score_Index")) %>%
#   arrange(sequence, -Confidence) %>%
#   select(Item, Predicted_Domain, Confidence)
# 
# # Using the tfi_data for reference, add question ID to classification_results_long and arrange by QuestionID
# classification_results_multi_long <- classification_results_multi_long %>%
#   left_join(tfi_data, by = c("Item" = "QuestionText")) %>%
#   select(QuestionID, Item, Predicted_Domain, Confidence) %>%
#   arrange(QuestionID)
# 
# # Using the tfi_data for reference, add predicted domain to classification_results_long
# classification_results_multi_long <- classification_results_multi_long %>%
#   left_join(tfi_data, by = c("Item" = "QuestionText")) 
# 
# #drop rename to QuestionID.x to QuestionID, drop QuestionID.y, and select item, questionID, item, intended domain, predicted domain, and confidence
# classification_results_multi_long <- classification_results_multi_long %>%
#   rename(QuestionID = QuestionID.x) %>%
#   select(QuestionID, Item, IntendedDomain, Predicted_Domain, Confidence)
# 
# #write.csv(classification_results_multi_long, "TFI_ZeroShot_Classifications_Multi_Long.csv", row.names = FALSE)

classification_results_multi_long <- read.csv("TFI_ZeroShot_Classifications_Multi_Long.csv")

# ==================================================
# 3c. VISUALIZATION: INDIVIDUAL ITEM PLOTS ----
# ==================================================

for (item in unique(classification_results_multi_long$Item)) {
  
  item_data <- classification_results_multi_long %>% filter(Item == item)
  item_number <- unique(item_data$QuestionID)
  intended_domain <- unique(item_data$IntendedDomain)
  
  p <- ggplot(item_data, aes(
    x = reorder(Predicted_Domain, -Confidence),  
    y = Confidence,
    fill = Predicted_Domain
  )) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "darkgrey", linewidth = 1) +  # Dark grey threshold line
    annotate("text", 
             x = length(unique(item_data$Predicted_Domain)) + 0.2,  # Further right
             y = 0.64,  # Lowered text position
             label = "High Confidence\nThreshold (0.7)",  # Line break added
             color = "darkgrey", hjust = 1, size = 3.2) +  # Reduced text size
    theme_minimal() +
    labs(
      title = paste("Item", item_number, ":", item),
      subtitle = paste("Intended Domain:", intended_domain),
      x = "Predicted Domain",
      y = "Confidence Score",
      fill = "Predicted Domain"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
  
  print(p)
}


# Using classification_results_multi_long, summarise how many items were correctly classified (i.e., the largest confidence score was also it's predicted domain)
correctly_classified_multi <- classification_results_multi_long %>%
  group_by(QuestionID) %>%
  filter(Confidence == max(Confidence)) %>%
  filter(Predicted_Domain == IntendedDomain) %>%
  summarise(Correctly_Classified = n())

# Using classification_results_multi_long, summarise how many items had confidence scores >0.7
high_confidence_multi <- classification_results_multi_long %>%
  group_by(QuestionID) %>%
  filter(Confidence > 0.7) %>%
  summarise(High_Confidence = n())

# Using classification_results_multi_long, filter to all rows where confidence scores >0.7 and where the predicted domain does not match the intended domain
high_confidence_incorrect_multi <- classification_results_multi_long %>%
  filter(Confidence > 0.7) %>%
  filter(Predicted_Domain != IntendedDomain)


# ==================================================
# 4. Inter-item Semantic Similarity ----
# ==================================================

# Compute similarity matrix using cosine similarity
# textSimilarity(
#   x = tfi_embeddings_texts_df[1,],  # Text embeddings
#   y = tfi_embeddings_texts_df[3,],  # Compare all to all
#   method = "cosine"  # Default is cosine similarity
# )

# ✅ Load Precomputed TFI Item Embeddings
tfi_embeddings_texts_df <- readRDS("TFI_Embeddings_Sentences.rds")
tfi_embeddings_texts_df <- as.data.frame(tfi_embeddings_texts_df$texts)

# Initialize an empty dataframe
similarity_results <- data.frame(
  Question1 = character(),
  Domain1 = character(),
  Question2 = character(),
  Domain2 = character(),
  Similarity = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each question to compute similarity
for (i in 1:(nrow(tfi_embeddings_texts_df) - 1)) {
  for (j in (i + 1):nrow(tfi_embeddings_texts_df)) {
    
    # Compute similarity between Question i and Question j
    similarity_score <- textSimilarity(
      x = tfi_embeddings_texts_df[i, , drop = FALSE],  # Question i
      y = tfi_embeddings_texts_df[j, , drop = FALSE],  # Question j
      method = "cosine"
    )
    
    # Store results in dataframe with domain information
    similarity_results <- rbind(similarity_results, data.frame(
      Question1 = tfi_data$QuestionText[i],
      Domain1 = tfi_data$IntendedDomain[i],
      Question2 = tfi_data$QuestionText[j],
      Domain2 = tfi_data$IntendedDomain[j],
      Similarity = similarity_score
    ))
  }
}


# Add a column indicating if the domains match
similarity_results <- similarity_results %>%
  mutate(SameDomain = ifelse(Domain1 == Domain2, "Within-Domain", "Cross-Domain"))

# Compute pairwise theta transformations
similarity_results <- similarity_results %>%
  mutate(
    Theta = acos(pmin(1, abs(Similarity))) * (180 / pi)  # Convert to degrees
  )

# Create reversed pairs to ensure every question has a full comparison
similarity_results_expanded <- similarity_results %>%
  rename(Original_Question1 = Question1, Original_Domain1 = Domain1,
         Original_Question2 = Question2, Original_Domain2 = Domain2) %>%
  bind_rows(
    similarity_results %>%
      rename(Original_Question1 = Question2, Original_Domain1 = Domain2,
             Original_Question2 = Question1, Original_Domain2 = Domain1)
  ) %>%
  rename(Question1 = Original_Question1, Domain1 = Original_Domain1,
         Question2 = Original_Question2, Domain2 = Original_Domain2)

# Ensure Question2 is treated as a factor with full levels
similarity_results_expanded$Question2 <- factor(similarity_results_expanded$Question2, levels = unique(similarity_results_expanded$Question2))



# Merge TFI item numbers for both Question1 and Question2
similarity_results_expanded <- similarity_results_expanded %>%
  left_join(tfi_data %>% select(QuestionID, QuestionText), by = c("Question1" = "QuestionText")) %>%
  rename(QuestionID1 = QuestionID) %>%
  left_join(tfi_data %>% select(QuestionID, QuestionText), by = c("Question2" = "QuestionText")) %>%
  rename(QuestionID2 = QuestionID)



# Ensure Question2 is a factor with item numbers in labels
similarity_results_expanded$Question2 <- factor(
  paste0("(", similarity_results_expanded$QuestionID2, ") ", similarity_results_expanded$Question2), 
  levels = unique(paste0("(", similarity_results_expanded$QuestionID2, ") ", similarity_results_expanded$Question2))
)

for (target_item in unique(similarity_results_expanded$Question1)) {
  
  # Get the item number for the focal item
  target_item_id <- similarity_results_expanded$QuestionID1[similarity_results_expanded$Question1 == target_item][1]
  
  # Wrap the title for readability
  wrapped_title <- str_wrap(paste0("Semantic Similarity for Item ", target_item_id, ": ", target_item), width = 60)
  
  # Filter dataset for the current TFI item and order by ascending Theta (smallest at bottom)
  similarity_results_item <- similarity_results_expanded %>%
    filter(Question1 == target_item) %>%
    mutate(Comparator_Domain = Domain2) %>%
    arrange(Theta)  # Ensures smallest Theta is at the bottom
  
  # Compute mean within-domain and cross-domain theta values
  mean_within_theta <- mean(similarity_results_item$Theta[similarity_results_item$SameDomain == "Within-Domain"], na.rm = TRUE)
  mean_cross_theta <- mean(similarity_results_item$Theta[similarity_results_item$SameDomain == "Cross-Domain"], na.rm = TRUE)
  
  # Extract the domain of the target item
  target_item_domain <- unique(similarity_results_item$Domain1)[1]
  
  # Ensure Question2 is treated as a factor in the correct order
  similarity_results_item$Question2 <- factor(similarity_results_item$Question2, levels = similarity_results_item$Question2)
  
  # Manually structured subtitle (avoids overwriting line breaks)
  subtitle_text <- paste(
    "Item Domain:", target_item_domain, "\n",
    "Dashed Lines: Black = Mean Within-Domain Theta, Grey = Mean Cross-Domain Theta", "\n",
    "0 = Perfectly Similar, 90 = Perfectly Dissimilar"
  )
  
  # Generate the plot
  plot <- ggplot(similarity_results_item, aes(y = Question2, x = Theta, color = Comparator_Domain)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_vline(xintercept = mean_within_theta, linetype = "dashed", color = "black", size = 1.2, alpha = 0.7) +
    geom_vline(xintercept = mean_cross_theta, linetype = "dashed", color = "grey", size = 1.2, alpha = 0.6) +
    scale_color_viridis_d(option = "H") +  
    scale_x_continuous(breaks = seq(0, 90, by = 5)) +
    theme_minimal() +
    labs(title = wrapped_title,
         subtitle = subtitle_text,  # Manually structured with line breaks
         x = "Theta (Degrees)",
         y = "Comparator TFI Item",
         color = "Comparator Domain") +
    theme(axis.text.y = element_text(size = 8), legend.position = "bottom") +
    coord_cartesian(xlim = c(0, 90))
  
  # Display plot
  print(plot)
}


# ==================================================
# 5. Construct Semantic Mapping ----
# ==================================================

# ✅ Load Precomputed TFI Item Embeddings
tfi_embeddings_texts_df <- readRDS("TFI_Embeddings_Sentences.rds")
tfi_embeddings_texts_df <- as.data.frame(tfi_embeddings_texts_df$texts)

# ✅ Define theoretical domain descriptions (Alphabetically Ordered)
tfi_domains <- tibble(
  Domain = c("Auditory", "Cognitive", "Emotional", "Intrusive", 
             "Quality of Life", "Relaxation", "Sense of Control", "Sleep"),
  Definition = c(
    "Evaluates problems hearing clearly due to tinnitus.",
    "Captures difficulties in concentration and cognitive tasks due to tinnitus.",
    "Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression).",
    "Measures how much the tinnitus intrudes on consciousness and daily life.",
    "Measures the effect of tinnitus on overall enjoyment and engagement in life activities.",
    "Examines the impact of tinnitus on relaxation and quiet activities.",
    "Determines how much control the patient feels they have over their tinnitus.",
    "Assesses the extent to which tinnitus interferes with sleep."
  )
)

# # ✅ Compute Embeddings for Domain Definitions (Precompute once)
# domain_embeddings <- textEmbed(
#   texts = tfi_domains$Definition, 
#   model = "bert-base-uncased",
#   aggregation_from_layers_to_tokens = "mean",
#   aggregation_from_tokens_to_texts = "mean",
#   keep_token_embeddings = FALSE
# )

# Save precomputed domain definition embeddings as an RDS file
# saveRDS(domain_embeddings, "TFI_Domain_Definition_Embeddings.rds")

# Load precomputed domain definition embeddings
domain_embeddings <- readRDS("TFI_Domain_Definition_Embeddings.rds")

# ✅ Convert Domain Embeddings into DataFrame
domain_embeddings_df <- as.data.frame(domain_embeddings$texts$texts)

# # ✅ Extract metadata for Item 1
# question_id <- tfi_data$QuestionID[1]  # ID of item 1
# question_text <- tfi_data$QuestionText[1]  # Full question text
# intended_domain <- tfi_data$IntendedDomain[1]  # Theoretical domain of item 1
# 
# # ✅ Initialize results dataframe
# semantic_mapping_results <- data.frame(
#   QuestionID = integer(),
#   QuestionText = character(),
#   IntendedDomain = character(),
#   ComparedDomain = character(),
#   DomainDefinition = character(),
#   SimilarityScore = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# # ✅ Loop through each domain and compute similarity (NO nested loops)
# for (j in 1:nrow(tfi_domains)) {
#   
#   # Extract domain name, definition, and corresponding embedding
#   compared_domain_name <- tfi_domains$Domain[j]  
#   compared_domain_definition <- tfi_domains$Definition[j]
#   
#   # Ensure the domain embedding is in the correct format (dataframe row)
#   domain_embedding <- domain_embeddings_df[j, , drop = FALSE]  
#   
#   # Compute similarity score
#   similarity_score <- textSimilarity(
#     x = tfi_embeddings_texts_df[1, , drop = FALSE],  # Ensure 1-row format
#     y = domain_embedding,  # Ensure 1-row format
#     method = "cosine"
#   )
#   
#   # Store results
#   semantic_mapping_results <- rbind(semantic_mapping_results, data.frame(
#     QuestionID = question_id,
#     QuestionText = question_text,
#     IntendedDomain = intended_domain,
#     ComparedDomain = compared_domain_name,
#     DomainDefinition = compared_domain_definition,  # Include definition
#     SimilarityScore = similarity_score
#   ))
# }


# Expanded to a nested loop

# ✅ Initialize results dataframe
semantic_mapping_results <- data.frame(
  QuestionID = integer(),
  QuestionText = character(),
  IntendedDomain = character(),
  ComparedDomain = character(),
  DomainDefinition = character(),
  SimilarityScore = numeric(),
  stringsAsFactors = FALSE
)

# ✅ Loop through each TFI item
for (i in 1:nrow(tfi_data)) {
  
  # Extract question text and metadata
  question_id <- tfi_data$QuestionID[i]
  question_text <- tfi_data$QuestionText[i]
  intended_domain <- tfi_data$IntendedDomain[i]
  
  # Extract precomputed embedding for the question
  question_embedding <- tfi_embeddings_texts_df[i, , drop = FALSE]
  
  # ✅ Loop through each domain
  for (j in 1:nrow(tfi_domains)) {
    
    # Extract domain name and corresponding embedding
    compared_domain_name <- tfi_domains$Domain[j]  
    domain_definition <- tfi_domains$Definition[j]
    domain_embedding <- domain_embeddings_df[j, , drop = FALSE] 
    
    # ✅ Compute similarity score
    similarity_score <- textSimilarity(
      x = question_embedding,  
      y = domain_embedding,  
      method = "cosine"
    )
    
    # ✅ Store results
    semantic_mapping_results <- rbind(semantic_mapping_results, data.frame(
      QuestionID = question_id,
      QuestionText = question_text,
      IntendedDomain = intended_domain,
      ComparedDomain = compared_domain_name,
      DomainDefinition = domain_definition,
      SimilarityScore = similarity_score
    ))
  }
}

# Mutate theta coefficient using Similarity result to semantic_mapping_results

# Compute pairwise theta transformations
semantic_mapping_results <- semantic_mapping_results %>%
  mutate(
    Theta = acos(pmin(1, abs(SimilarityScore))) * (180 / pi)  # Convert to degrees
  )



# ✅ Preprocess to include Domain Definitions in Labels
semantic_mapping_results <- semantic_mapping_results %>%
  mutate(ComparedDomainLabel = paste0(ComparedDomain, " - ", DomainDefinition))  # Combine domain name + definition

# ✅ Loop through all TFI items
for (target_item_id in unique(semantic_mapping_results$QuestionID)) {
  
  # Extract question content for title
  target_question_text <- semantic_mapping_results %>%
    filter(QuestionID == target_item_id) %>%
    pull(QuestionText) %>%
    unique()
  
  # Extract intended domain for subtitle
  target_item_domain <- semantic_mapping_results %>%
    filter(QuestionID == target_item_id) %>%
    pull(IntendedDomain) %>%
    unique()
  
  # Compute mean similarity score for other items within the same domain (excluding the focal item)
  mean_other_items_similarity <- semantic_mapping_results %>%
    filter(IntendedDomain == target_item_domain & QuestionID != target_item_id) %>%
    summarise(mean_similarity = mean(SimilarityScore, na.rm = TRUE)) %>%
    pull(mean_similarity)
  
  # Format question text for title (wrapped)
  wrapped_title <- str_wrap(paste0("Semantic Construct Similarity for Item ", target_item_id, ": ", target_question_text), width = 80)
  
  # ✅ Generate plot (Flipped Axes & Highest Similarity at Bottom)
  plot <- semantic_mapping_results %>% 
    filter(QuestionID == target_item_id) %>%
    arrange(desc(SimilarityScore)) %>%  # ✅ Highest similarity at bottom
    ggplot(aes(y = reorder(ComparedDomainLabel, -SimilarityScore), x = SimilarityScore)) +  
    geom_point(size = 4, alpha = 0.8, color = "blue") +
    geom_vline(xintercept = mean_other_items_similarity, linetype = "dashed", color = "black", size = 1) +  
    theme_minimal() +
    scale_x_reverse(limits = c(1, 0), breaks = seq(1, 0, by = -0.05)) +  # ✅ Reverse X-axis (1 to 0, left to right)
    labs(
      title = wrapped_title,  
      subtitle = paste("Intended Domain:", target_item_domain, 
                       "\nDashed Black Line = Mean Similarity Score for Other Items in the Same Domain"),
      x = "Similarity Score (Higher = Left, Lower = Right)",  
      y = "Compared Domain Definition"  
    ) +
    theme(axis.text.x = element_text(size = 10),  
          axis.text.y = element_text(size = 10),  
          legend.position = "bottom")  
  
  # ✅ Display each plot
  print(plot)  
}

# Use semantic_mapping_results to summarise the highest similarity domain for each item
highest_similarity_domain <- semantic_mapping_results %>%
  group_by(QuestionID) %>%
  filter(SimilarityScore == max(SimilarityScore)) %>%
  summarise(Highest_Similarity_Domain = first(ComparedDomain))

# Use highest_similarity_domain to summarise domain frequency
domain_frequency <- highest_similarity_domain %>%
  group_by(Highest_Similarity_Domain) %>%
  summarise(Frequency = n())



# # ================================================================
# # 6a. ZERO-SHOT CLASSIFICATION OF TFI ITEMS ACCORDING TO DOMAIN DESCRIPTION in MEIKLE et al. 2012) (multi_label = T) ----
# # ================================================================
# 
# # zeroshot_results <- data.frame(
# #   Item = character(),
# #   Predicted_Domain = character(),
# #   Confidence = numeric(),
# #   stringsAsFactors = FALSE
# # )
# # 
# # for (i in 1:length(tfi_data$QuestionText)) {
# #   test_result <- textZeroShot(tfi_data$QuestionText[i], tfi_domains$Definition, multi_label = TRUE)
# #   test_result$Item <- tfi_data$QuestionText[i]
# #   zeroshot_results <- rbind(zeroshot_results, test_result)
# # }
# 
# # ==================================================
# # 6b. DATA TRANSFORMATION: RESTRUCTURING OUTPUT ----
# # ==================================================
# 
# 
# # Pivot labels into long format, keeping Item for context
# zeroshot_results_long <- zeroshot_results %>%
#   pivot_longer(
#     cols = starts_with("labels_x"), 
#     names_to = "Label_Index",
#     values_to = "Predicted_Domain"
#   ) %>%
#   mutate(Score_Index = gsub("labels", "scores", Label_Index)) %>%
#   select(sequence, Item, Label_Index, Predicted_Domain, Score_Index) # Retain Score_Index for merging
# 
# # Pivot scores separately
# zeroshot_scores_multi_long <- zeroshot_results %>%
#   pivot_longer(
#     cols = starts_with("scores_x_"), 
#     names_to = "Score_Index",
#     values_to = "Confidence"
#   ) %>%
#   select(sequence, Score_Index, Confidence) # Keep only necessary columns
# 
# 
# # Perform the join using both sequence and Score_Index
# zeroshot_results_long <- zeroshot_results_long %>%
#   left_join(zeroshot_scores_multi_long, by = c("sequence", "Score_Index")) %>%
#   arrange(sequence, -Confidence) %>%
#   select(Item, Predicted_Domain, Confidence)
# 
# # Using the tfi_data for reference, add question ID to classification_results_long and arrange by QuestionID
# zeroshot_results_long <- zeroshot_results_long %>%
#   left_join(tfi_data, by = c("Item" = "QuestionText")) %>%
#   select(QuestionID, Item, Predicted_Domain, Confidence) %>%
#   arrange(QuestionID)
# 
# # Using the tfi_data for reference, add predicted domain to classification_results_long
# zeroshot_results_long <- zeroshot_results_long %>%
#   left_join(tfi_data, by = c("Item" = "QuestionText")) 
# 
# #drop rename to QuestionID.x to QuestionID, drop QuestionID.y, and select item, questionID, item, intended domain, predicted domain, and confidence
# zeroshot_results_long <- zeroshot_results_long %>%
#   rename(QuestionID = QuestionID.x) %>%
#   select(QuestionID, Item, IntendedDomain, Predicted_Domain, Confidence)
# 
# # ✅ Add Intended and Predicted Domain Definitions (Label + Definition)
# zeroshot_results_long <- zeroshot_results_long %>%
#   mutate(
#     # Intended Domain Definitions
#     Intended_Domain_Definition = case_when(
#       IntendedDomain == "Auditory" ~ "Auditory: Evaluates problems hearing clearly due to tinnitus.",
#       IntendedDomain == "Cognitive" ~ "Cognitive: Captures difficulties in concentration and cognitive tasks due to tinnitus.",
#       IntendedDomain == "Emotional" ~ "Emotional: Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression).",
#       IntendedDomain == "Intrusive" ~ "Intrusive: Measures how much the tinnitus intrudes on consciousness and daily life.",
#       IntendedDomain == "Quality of Life" ~ "Quality of Life: Measures the effect of tinnitus on overall enjoyment and engagement in life activities.",
#       IntendedDomain == "Relaxation" ~ "Relaxation: Examines the impact of tinnitus on relaxation and quiet activities.",
#       IntendedDomain == "Sense of Control" ~ "Sense of Control: Determines how much control the patient feels they have over their tinnitus.",
#       IntendedDomain == "Sleep" ~ "Sleep: Assesses the extent to which tinnitus interferes with sleep.",
#       TRUE ~ NA_character_
#     ),
#     
#     # Predicted Domain Definitions
#     Predicted_Domain_Definition = case_when(
#       Predicted_Domain == "Evaluates problems hearing clearly due to tinnitus." ~ "Auditory: Evaluates problems hearing clearly due to tinnitus.",
#       Predicted_Domain == "Captures difficulties in concentration and cognitive tasks due to tinnitus." ~ "Cognitive: Captures difficulties in concentration and cognitive tasks due to tinnitus.",
#       Predicted_Domain == "Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression)." ~ "Emotional: Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression).",
#       Predicted_Domain == "Measures how much the tinnitus intrudes on consciousness and daily life." ~ "Intrusive: Measures how much the tinnitus intrudes on consciousness and daily life.",
#       Predicted_Domain == "Measures the effect of tinnitus on overall enjoyment and engagement in life activities." ~ "Quality of Life: Measures the effect of tinnitus on overall enjoyment and engagement in life activities.",
#       Predicted_Domain == "Examines the impact of tinnitus on relaxation and quiet activities." ~ "Relaxation: Examines the impact of tinnitus on relaxation and quiet activities.",
#       Predicted_Domain == "Determines how much control the patient feels they have over their tinnitus." ~ "Sense of Control: Determines how much control the patient feels they have over their tinnitus.",
#       Predicted_Domain == "Assesses the extent to which tinnitus interferes with sleep." ~ "Sleep: Assesses the extent to which tinnitus interferes with sleep.",
#       TRUE ~ NA_character_
#     )
#   )


#write.csv(zeroshot_results_long, "TFI_zeroshot_domains_results_multilabel.csv", row.names = FALSE)


# ==================================================
# 6c. VISUALIZATION: INDIVIDUAL ITEM PLOTS (FLIPPED & REFINED) ----
# ==================================================

zeroshot_results_long <- read.csv("TFI_zeroshot_domains_results_multilabel.csv")

for (item in unique(zeroshot_results_long$Item)) {
  
  # ✅ Filter item data
  item_data <- zeroshot_results_long %>% filter(Item == item)
  item_number <- unique(item_data$QuestionID)  # ✅ Corrected
  intended_domain <- unique(item_data$IntendedDomain)  # ✅ Corrected
  
  # ✅ Wrap the title for better readability
  wrapped_title <- str_wrap(paste("Zero-Shot Classification (multi-label) for Item", item_number, ":", item), width = 80)
  
  # ✅ Generate flipped plot
  p <- ggplot(item_data, aes(
    y = reorder(Predicted_Domain_Definition, Confidence),  # ✅ Flip to y-axis
    x = Confidence
  )) +
    geom_bar(stat = "identity", width = 0.6) +
    
    # ✅ Convert high-confidence threshold into a vertical reference line
    geom_vline(xintercept = 0.7, linetype = "dashed", color = "darkgrey", linewidth = 1) +  
    
    # ✅ Position threshold label in the bottom right of each plot
    annotate("text", 
             y = 0.8,   # ✅ Near the bottom of the y-axis
             x = 0.72,   # ✅ Slightly past the reference line
             label = "High Confidence\nThreshold (0.7)",  
             color = "darkgrey", hjust = 0, size = 3.2) +  
    
    theme_minimal() +
    
    # ✅ Adjust confidence score scale with 0.05 increments
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.05)) +
    
    labs(
      title = wrapped_title,  # ✅ Wrapped title for better readability
      subtitle = paste("Intended Domain:", intended_domain),
      x = "Confidence Score",
      y = "Predicted Domain Definition"  # ✅ Adjusted for flipped axis
    ) +
    theme(
      axis.text.y = element_text(size = 8),  # ✅ Increase readability for long domain text
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # ✅ Rotate x-axis labels
      legend.position = "none"  # ✅ Removed unnecessary legend
    )
  
  print(p)
}

# ================================================================
# 7a. ZERO-SHOT CLASSIFICATION OF TFI ITEMS ACCORDING TO DOMAIN DESCRIPTION in MEIKLE et al. 2012) (multi_label = F) ----
# ================================================================

# zeroshot_results <- data.frame(
#   Item = character(),
#   Predicted_Domain = character(),
#   Confidence = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# for (i in 1:length(tfi_data$QuestionText)) {
#   test_result <- textZeroShot(tfi_data$QuestionText[i], tfi_domains$Definition, multi_label = FALSE)
#   test_result$Item <- tfi_data$QuestionText[i]
#   zeroshot_results <- rbind(zeroshot_results, test_result)
# }

# ==================================================
# 7b. DATA TRANSFORMATION: RESTRUCTURING OUTPUT ----
# ==================================================


# Pivot labels into long format, keeping Item for context
zeroshot_results_long <- zeroshot_results %>%
  pivot_longer(
    cols = starts_with("labels_x"), 
    names_to = "Label_Index",
    values_to = "Predicted_Domain"
  ) %>%
  mutate(Score_Index = gsub("labels", "scores", Label_Index)) %>%
  select(sequence, Item, Label_Index, Predicted_Domain, Score_Index) # Retain Score_Index for merging

# Pivot scores separately
zeroshot_scores_multi_long <- zeroshot_results %>%
  pivot_longer(
    cols = starts_with("scores_x_"), 
    names_to = "Score_Index",
    values_to = "Confidence"
  ) %>%
  select(sequence, Score_Index, Confidence) # Keep only necessary columns


# Perform the join using both sequence and Score_Index
zeroshot_results_long <- zeroshot_results_long %>%
  left_join(zeroshot_scores_multi_long, by = c("sequence", "Score_Index")) %>%
  arrange(sequence, -Confidence) %>%
  select(Item, Predicted_Domain, Confidence)

# Using the tfi_data for reference, add question ID to classification_results_long and arrange by QuestionID
zeroshot_results_long <- zeroshot_results_long %>%
  left_join(tfi_data, by = c("Item" = "QuestionText")) %>%
  select(QuestionID, Item, Predicted_Domain, Confidence) %>%
  arrange(QuestionID)

# Using the tfi_data for reference, add predicted domain to classification_results_long
zeroshot_results_long <- zeroshot_results_long %>%
  left_join(tfi_data, by = c("Item" = "QuestionText")) 

#drop rename to QuestionID.x to QuestionID, drop QuestionID.y, and select item, questionID, item, intended domain, predicted domain, and confidence
zeroshot_results_long <- zeroshot_results_long %>%
  rename(QuestionID = QuestionID.x) %>%
  select(QuestionID, Item, IntendedDomain, Predicted_Domain, Confidence)

# ✅ Add Intended and Predicted Domain Definitions (Label + Definition)
zeroshot_results_long <- zeroshot_results_long %>%
  mutate(
    # Intended Domain Definitions
    Intended_Domain_Definition = case_when(
      IntendedDomain == "Auditory" ~ "Auditory: Evaluates problems hearing clearly due to tinnitus.",
      IntendedDomain == "Cognitive" ~ "Cognitive: Captures difficulties in concentration and cognitive tasks due to tinnitus.",
      IntendedDomain == "Emotional" ~ "Emotional: Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression).",
      IntendedDomain == "Intrusive" ~ "Intrusive: Measures how much the tinnitus intrudes on consciousness and daily life.",
      IntendedDomain == "Quality of Life" ~ "Quality of Life: Measures the effect of tinnitus on overall enjoyment and engagement in life activities.",
      IntendedDomain == "Relaxation" ~ "Relaxation: Examines the impact of tinnitus on relaxation and quiet activities.",
      IntendedDomain == "Sense of Control" ~ "Sense of Control: Determines how much control the patient feels they have over their tinnitus.",
      IntendedDomain == "Sleep" ~ "Sleep: Assesses the extent to which tinnitus interferes with sleep.",
      TRUE ~ NA_character_
    ),
    
    # Predicted Domain Definitions
    Predicted_Domain_Definition = case_when(
      Predicted_Domain == "Evaluates problems hearing clearly due to tinnitus." ~ "Auditory: Evaluates problems hearing clearly due to tinnitus.",
      Predicted_Domain == "Captures difficulties in concentration and cognitive tasks due to tinnitus." ~ "Cognitive: Captures difficulties in concentration and cognitive tasks due to tinnitus.",
      Predicted_Domain == "Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression)." ~ "Emotional: Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression).",
      Predicted_Domain == "Measures how much the tinnitus intrudes on consciousness and daily life." ~ "Intrusive: Measures how much the tinnitus intrudes on consciousness and daily life.",
      Predicted_Domain == "Measures the effect of tinnitus on overall enjoyment and engagement in life activities." ~ "Quality of Life: Measures the effect of tinnitus on overall enjoyment and engagement in life activities.",
      Predicted_Domain == "Examines the impact of tinnitus on relaxation and quiet activities." ~ "Relaxation: Examines the impact of tinnitus on relaxation and quiet activities.",
      Predicted_Domain == "Determines how much control the patient feels they have over their tinnitus." ~ "Sense of Control: Determines how much control the patient feels they have over their tinnitus.",
      Predicted_Domain == "Assesses the extent to which tinnitus interferes with sleep." ~ "Sleep: Assesses the extent to which tinnitus interferes with sleep.",
      TRUE ~ NA_character_
    )
  )


#write.csv(zeroshot_results_long, "TFI_zeroshot_domains_results_singlelabel.csv", row.names = FALSE)


# ==================================================
# 7c. VISUALIZATION: INDIVIDUAL ITEM PLOTS (FLIPPED & REFINED) ----
# ==================================================

zeroshot_results_long <- read.csv("TFI_zeroshot_domains_results_singlelabel.csv")

for (item in unique(zeroshot_results_long$Item)) {
  
  # ✅ Filter item data
  item_data <- zeroshot_results_long %>% filter(Item == item)
  item_number <- unique(item_data$QuestionID)  # ✅ Corrected
  intended_domain <- unique(item_data$IntendedDomain)  # ✅ Corrected
  
  # ✅ Wrap the title for better readability
  wrapped_title <- str_wrap(paste("Zero-Shot Classification (single-label) for Item", item_number, ":", item), width = 80)
  
  # ✅ Generate flipped plot
  p <- ggplot(item_data, aes(
    y = reorder(Predicted_Domain_Definition, Confidence),  # ✅ Flip to y-axis
    x = Confidence
  )) +
    geom_bar(stat = "identity", width = 0.6) +
    
    # ✅ Convert high-confidence threshold into a vertical reference line
    geom_vline(xintercept = 0.7, linetype = "dashed", color = "darkgrey", linewidth = 1) +  
    
    # ✅ Position threshold label in the bottom right of each plot
    annotate("text", 
             y = 0.8,   # ✅ Near the bottom of the y-axis
             x = 0.72,   # ✅ Slightly past the reference line
             label = "High Confidence\nThreshold (0.7)",  
             color = "darkgrey", hjust = 0, size = 3.2) +  
    
    theme_minimal() +
    
    # ✅ Adjust confidence score scale with 0.05 increments
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.05)) +
    
    labs(
      title = wrapped_title,  # ✅ Wrapped title for better readability
      subtitle = paste("Intended Domain:", intended_domain),
      x = "Confidence Score",
      y = "Predicted Domain Definition"  # ✅ Adjusted for flipped axis
    ) +
    theme(
      axis.text.y = element_text(size = 8),  # ✅ Increase readability for long domain text
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # ✅ Rotate x-axis labels
      legend.position = "none"  # ✅ Removed unnecessary legend
    )
  
  print(p)
}

zeroshot_results_long <- zeroshot_results_long %>%
  mutate(
    # Predicted Domain Definitions
    predicted_domain_label = case_when(
      Predicted_Domain == "Evaluates problems hearing clearly due to tinnitus." ~ "Auditory",
      Predicted_Domain == "Captures difficulties in concentration and cognitive tasks due to tinnitus." ~ "Cognitive",
      Predicted_Domain == "Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression)." ~ "Emotional",
      Predicted_Domain == "Measures how much the tinnitus intrudes on consciousness and daily life." ~ "Intrusive",
      Predicted_Domain == "Measures the effect of tinnitus on overall enjoyment and engagement in life activities." ~ "Quality of Life",
      Predicted_Domain == "Examines the impact of tinnitus on relaxation and quiet activities." ~ "Relaxation",
      Predicted_Domain == "Determines how much control the patient feels they have over their tinnitus." ~ "Sense of Control",
      Predicted_Domain == "Assesses the extent to which tinnitus interferes with sleep." ~ "Sleep",
      TRUE ~ NA_character_
    )
  )

# select the largest confidence score for each item as a new dataframe
largest_confidence <- zeroshot_results_long %>%
  group_by(QuestionID) %>%
  filter(Confidence == max(Confidence)) %>%
  select(QuestionID, predicted_domain_label, Confidence)

# summarise the each predicted_domain_label frequency and arrange by descending frequency
domain_frequency <- largest_confidence %>%
  group_by(predicted_domain_label) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# ==================================================
# 8a. Embedding Dimensionality: PCA ----
# ==================================================

# tfi_embeddings <- textEmbed(
#   texts = tfi_data$QuestionText,  # Full TFI question text
#   aggregation_from_layers_to_tokens = "mean",  # Aggregate across layers
#   aggregation_from_tokens_to_texts = "mean",  # Aggregate all token embeddings to one per question
#   keep_token_embeddings = FALSE  # Only store full sentence embeddings
# )

# saveRDS(tfi_embeddings$texts, "TFI_Embeddings_Sentences.rds")

tfi_embeddings_texts_df <- readRDS("TFI_Embeddings_Sentences.rds")

tfi_embeddings_texts_df <- as.data.frame(tfi_embeddings_texts_df$texts)

items <- as.data.frame(t(tfi_embeddings_texts_df))  # Transpose: Dimensions → Rows, Items → Columns

# Perform PCA on transposed data
pca_result <- prcomp(items, center = TRUE, scale. = TRUE)

# Extract the eigenvalues from the PCA object
eigenvalues <- pca_result$sdev^2

# Determine y-axis limits based on max eigenvalue (rounded up)
y_max <- ceiling(max(eigenvalues))

# Create a scree plot
plot(eigenvalues, type = "b", pch = 19, col = "blue",
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     main = "Scree Plot of PCA Eigenvalues",
     xaxt = "n", yaxt = "n",  # Remove default axis ticks
     ylim = c(0, y_max))  # Set y-axis limits

# Manually add x-axis ticks for all 25 principal components
axis(1, at = 1:25, labels = 1:25, las = 2)  # `las = 2` rotates labels vertically

# Manually add y-axis ticks at every whole number eigenvalue
axis(2, at = seq(0, y_max, by = 1), labels = seq(0, y_max, by = 1)) 

# Add a horizontal line at y = 1
abline(h = 1, col = "red", lty = 2, lwd = 2)  # Dashed red line

# ================================================================
# 8b. Embedding Dimensionality: EGA ----
# ================================================================


# ✅ Load zero-shot classification results
classification_results_multi_long <- read.csv("TFI_zeroshot_domains_results_multilabel.csv")

classification_results_multi_long <- classification_results_multi_long %>%
  mutate(
    # Predicted Domain Definitions
    Predicted_Domain_Definition = case_when(
      Predicted_Domain == "Evaluates problems hearing clearly due to tinnitus." ~ "Auditory",
      Predicted_Domain == "Captures difficulties in concentration and cognitive tasks due to tinnitus." ~ "Cognitive",
      Predicted_Domain == "Evaluates the emotional distress caused by tinnitus (e.g., anxiety, frustration, depression)." ~ "Emotional",
      Predicted_Domain == "Measures how much the tinnitus intrudes on consciousness and daily life." ~ "Intrusive",
      Predicted_Domain == "Measures the effect of tinnitus on overall enjoyment and engagement in life activities." ~ "Quality of Life",
      Predicted_Domain == "Examines the impact of tinnitus on relaxation and quiet activities." ~ "Relaxation",
      Predicted_Domain == "Determines how much control the patient feels they have over their tinnitus." ~ "Sense of Control",
      Predicted_Domain == "Assesses the extent to which tinnitus interferes with sleep." ~ "Sleep",
      TRUE ~ NA_character_
    )
  )


# # ✅ Pivot the dataset into wide format
# classification_results_wide <- classification_results_multi_long %>%
#   select(QuestionID, Confidence, Predicted_Domain_Definition) %>% 
#   pivot_wider(names_from = QuestionID, values_from = Confidence) # Ensure consistent order
# 
# # create new variable from predicted domain definition that takes all text before":"
# classification_results_wide$Predicted_Domain_Definition <- gsub(":.*", "", classification_results_wide$Predicted_Domain_Definition)
# 
# #rownames(classification_results_wide) <- classification_results_wide$Predicted_Domain_Definition # Assign rownames as QuestionID
# 
# # ✅ Convert to matrix format for EGA()
# ega_input<- classification_results_wide %>%
#   select(-c(Predicted_Domain_Definition))


# Transpose the embeddings to ensure columns represent items
ega_matrix <- t(as.matrix(tfi_embeddings_texts_df))

# ✅ Run Exploratory Graph Analysis (EGA)
ega_result <- EGA(ega_matrix, model = "glasso", seed = 99, type = "parametric",
                  algorithm = "walktrap", na.data = "pairwise", uni.method = "louvain")  # Graphical LASSO model

# ✅ Print Summary
summary(ega_result)

# ✅ Visualize the EGA Network
plot(ega_result, title = "Latent Semantic Graph Analysis for TFI")


# ✅ Run Exploratory Graph Analysis (EGA)
ega_result <- EGA(ega_matrix, model = "glasso")  # Graphical LASSO model

bootstrapped_ega <- bootEGA(ega_matrix, model = "glasso", iter = 500, seed = 99, type = "parametric",
                            algorithm = "walktrap", na.data = "pairwise", uni.method = "louvain")


