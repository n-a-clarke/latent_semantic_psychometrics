# =============================================
# 0. SETUP: Load Required Packages and Data
# =============================================

# Load the main package for text analysis.
library(text)            # Primary package for advanced text-based NLP.
# Additional packages for data manipulation and visualization.
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidytext)
# =============================================
# 1. CREATE THE COMPLETE TFI DATASET (ALL 25 ITEMS)
# =============================================

# Data frame with all 25 TFI items and their respective domains
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



# Define the TFI domain labels
candidate_domains <- c(
  "Intrusive", 
  "Sense of Control", 
  "Cognitive", 
  "Sleep", 
  "Auditory", 
  "Relaxation", 
  "Quality of Life", 
  "Emotional"
)

# ==============================================
### 2. ZERO-SHOT CLASSIFICATION OF TFI ITEMS ###
# ==============================================


# Define the TFI domain labels
candidate_domains <- c(
  "Intrusive", 
  "Sense of Control", 
  "Cognitive", 
  "Sleep", 
  "Auditory", 
  "Relaxation", 
  "Quality of Life", 
  "Emotional"
)

# Assume `tfi_data$QuestionText` contains 25 TFI items
# Assume `candidate_domains` contains domain labels

# Initialize empty dataframe to store results
classification_results <- data.frame(
  Item = character(),
  Predicted_Domain = character(),
  Confidence = numeric(),
  stringsAsFactors = FALSE
)

# Iterate through all TFI items
for (i in 1:length(tfi_data$QuestionText)) {
  
  # Apply zero-shot classification (returns multiple probabilities)
  test_result <- textZeroShot(tfi_data$QuestionText[i], candidate_domains)
  
  # Add item column for tracking
  test_result$Item <- tfi_data$QuestionText[i]
  
  # Append results to dataframe
  classification_results <- rbind(classification_results, test_result)
}


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

ggplot(classification_results_long, aes(
  x = reorder(Item, -Confidence, median), 
  y = Confidence, 
  fill = Predicted_Domain
)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  facet_wrap(~ IntendedDomain, scales = "free_y", ncol = 1) +  # Facet to show Intended Domain
  coord_flip() +  # Flip for readability
  theme_minimal() +
  labs(
    title = "Confidence Scores for Predicted Domains per TFI Item",
    x = "TFI Item",
    y = "Confidence Score",
    fill = "Predicted Domain"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),  # Highlight intended domain in facet labels
    legend.position = "bottom"
  )


ggplot(classification_results_long, aes(
  x = reorder_within(Predicted_Domain, -Confidence, Item),  # Orders per item
  y = Confidence,
  fill = Predicted_Domain  # Different colors for domains
)) +
  geom_bar(stat = "identity", width = 0.6) +  # Regular bars
  facet_wrap(~ Item, scales = "free_y", ncol = 2, labeller = label_wrap_gen(width = 40)) +  # Auto-wrap long titles, wider layout
  scale_x_reordered() +  # Ensures reordering works within facets
  theme_minimal() +
  labs(
    title = "Predicted Domains per TFI Item (Ordered by Confidence)",
    x = "Predicted Domain",
    y = "Confidence Score",
    fill = "Predicted Domain"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Tilt X-axis for readability
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 9, face = "bold"),  # Keep readable facet titles
    legend.position = "bottom"
  )


for (item in unique(classification_results_long$Item)) {
  
  # Filter data for the specific item
  item_data <- classification_results_long %>% filter(Item == item)
  
  # Extract item number (QuestionID) and intended domain
  item_number <- unique(item_data$QuestionID)
  intended_domain <- unique(item_data$IntendedDomain)  # Extract intended domain
  
  # Create plot
  p <- ggplot(item_data, aes(
    x = reorder(Predicted_Domain, -Confidence),  # Order by confidence
    y = Confidence,
    fill = Predicted_Domain
  )) +
    geom_bar(stat = "identity", width = 0.6) +  # Regular bars
    theme_minimal() +
    labs(
      title = paste("Item", item_number, ":", item),  # Includes QuestionID in title
      subtitle = paste("Intended Domain:", intended_domain),  # Shows intended domain
      x = "Predicted Domain",
      y = "Confidence Score",
      fill = "Predicted Domain"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
  
  # Display the plot
  print(p)
}




