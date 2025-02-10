# ==================================================
# 0. SETUP: Load Required Packages and Data ----
# ==================================================

library(text)       # Primary package for advanced text-based NLP.
library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization
library(tidyr)      # Data restructuring
library(stringr)    # String operations
library(tidytext)   # Text processing

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

# ==================================================
# 2a. ZERO-SHOT CLASSIFICATION OF TFI ITEMS (multi_label = F) ----
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
# 3a. DATA TRANSFORMATION: RESTRUCTURING OUTPUT ----
# ==================================================

# Pivot labels into long format, keeping Item for context
classification_results_long <- classification_results %>%
  pivot_longer(
    cols = starts_with("labels_x"), 
    names_to = "Label_Index",
    values_to = "Predicted_Domain"
  ) %>%
  mutate(Score_Index = gsub("labels", "scores", Label_Index)) %>%
  select(sequence, Item, Label_Index, Predicted_Domain, Score_Index) # Retain Score_Index for merging

# Pivot scores separately
classification_scores_long <- classification_results %>%
  pivot_longer(
    cols = starts_with("scores_x_"), 
    names_to = "Score_Index",
    values_to = "Confidence"
  ) %>%
  select(sequence, Score_Index, Confidence) # Keep only necessary columns


# Perform the join using both sequence and Score_Index
classification_results_long <- classification_results_long %>%
  left_join(classification_scores_long, by = c("sequence", "Score_Index")) %>%
  arrange(sequence, -Confidence) %>%
  select(Item, Predicted_Domain, Confidence)

# Using the tfi_data for reference, add question ID to classification_results_long and arrange by QuestionID
classification_results_long <- classification_results_long %>%
  left_join(tfi_data, by = c("Item" = "QuestionText")) %>%
  select(QuestionID, Item, Predicted_Domain, Confidence) %>%
  arrange(QuestionID)

# Using the tfi_data for reference, add predicted domain to classification_results_long
classification_results_long <- classification_results_long %>%
  left_join(tfi_data, by = c("Item" = "QuestionText")) 

#drop rename to QuestionID.x to QuestionID, drop QuestionID.y, and select item, questionID, item, intended domain, predicted domain, and confidence
classification_results_long <- classification_results_long %>%
  rename(QuestionID = QuestionID.x) %>%
  select(QuestionID, Item, IntendedDomain, Predicted_Domain, Confidence)

#write.csv(classification_results_long, "TFI_ZeroShot_Classifications_Long.csv", row.names = FALSE)

#read saved results
classification_results_long <- read.csv("TFI_ZeroShot_Classifications_Long.csv")

# ==================================================
# 4a. VISUALIZATION: STACKED BAR CHARTS ----
# ==================================================

ggplot(classification_results_long, aes(
  x = reorder(Item, -Confidence, median), 
  y = Confidence, 
  fill = Predicted_Domain
)) +
  geom_bar(stat = "identity", position = "stack") +  
  facet_wrap(~ IntendedDomain, scales = "free_y", ncol = 1) +  
  coord_flip() +  
  theme_minimal() +
  labs(
    title = "Confidence Scores for Predicted Domains per TFI Item",
    x = "TFI Item",
    y = "Confidence Score",
    fill = "Predicted Domain"
  ) +
  theme(axis.text.y = element_text(size = 8), legend.position = "bottom")

# ==================================================
# 5a. VISUALIZATION: INDIVIDUAL ITEM PLOTS ----
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
# 2b. ZERO-SHOT CLASSIFICATION OF TFI ITEMS (multi_label = T) ----
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

# Pivot labels into long format, keeping Item for context
classification_results_multi_long <- classification_results_multi %>%
  pivot_longer(
    cols = starts_with("labels_x"), 
    names_to = "Label_Index",
    values_to = "Predicted_Domain"
  ) %>%
  mutate(Score_Index = gsub("labels", "scores", Label_Index)) %>%
  select(sequence, Item, Label_Index, Predicted_Domain, Score_Index) # Retain Score_Index for merging

# Pivot scores separately
classification_scores_multi_long <- classification_results_multi %>%
  pivot_longer(
    cols = starts_with("scores_x_"), 
    names_to = "Score_Index",
    values_to = "Confidence"
  ) %>%
  select(sequence, Score_Index, Confidence) # Keep only necessary columns


# Perform the join using both sequence and Score_Index
classification_results_multi_long <- classification_results_multi_long %>%
  left_join(classification_scores_multi_long, by = c("sequence", "Score_Index")) %>%
  arrange(sequence, -Confidence) %>%
  select(Item, Predicted_Domain, Confidence)

# Using the tfi_data for reference, add question ID to classification_results_long and arrange by QuestionID
classification_results_multi_long <- classification_results_multi_long %>%
  left_join(tfi_data, by = c("Item" = "QuestionText")) %>%
  select(QuestionID, Item, Predicted_Domain, Confidence) %>%
  arrange(QuestionID)

# Using the tfi_data for reference, add predicted domain to classification_results_long
classification_results_multi_long <- classification_results_multi_long %>%
  left_join(tfi_data, by = c("Item" = "QuestionText")) 

#drop rename to QuestionID.x to QuestionID, drop QuestionID.y, and select item, questionID, item, intended domain, predicted domain, and confidence
classification_results_multi_long <- classification_results_multi_long %>%
  rename(QuestionID = QuestionID.x) %>%
  select(QuestionID, Item, IntendedDomain, Predicted_Domain, Confidence)

#write.csv(classification_results_multi_long, "TFI_ZeroShot_Classifications_Multi_Long.csv", row.names = FALSE)

classification_results_multi_long <- read.csv("TFI_ZeroShot_Classifications_Multi_Long.csv")

# ==================================================
# 4b. VISUALIZATION: INDIVIDUAL ITEM PLOTS ----
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
# 5. LEXICAL EMBEDDING PSYCHOMETRICS (LEP) ----
# ==================================================

# tfi_embeddings <- textEmbed(
#   texts = tfi_data$QuestionText,  # Full TFI question text
#   aggregation_from_layers_to_tokens = "mean",  # Aggregate across layers
#   aggregation_from_tokens_to_texts = "mean",  # Aggregate all token embeddings to one per question
#   keep_token_embeddings = FALSE  # Only store full sentence embeddings
# )

# saveRDS(tfi_embeddings$texts, "TFI_Embeddings_Sentences.rds")

tfi_embeddings_texts_df <- readRDS("TFI_Embeddings_Sentences.rds")


