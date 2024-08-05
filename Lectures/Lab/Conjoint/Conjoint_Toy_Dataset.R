cat("\014")
rm(list=ls())

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

p_load(dplyr,tibble,cregg)

# Define the attributes and their levels for political candidates
attributes <- list(
  Policy = c("Healthcare Reform", "Tax Cuts", "Climate Change Action"),
  Experience = c("Newcomer", "Experienced", "Veteran"),
  Party_Affiliation = c("Party A", "Party B"),
  Gender = c("Male", "Female")
)

# Create the full factorial design for one candidate
profiles <- expand.grid(attributes)

# Function to create a dataset with pairwise comparisons
create_conjoint_dataset <- function(n_respondents, n_choices) {
  data <- tibble()
  
  for (i in 1:n_respondents) {
    for (j in 1:n_choices) {
      # Randomly select two different profiles
      choice_set <- profiles[sample(nrow(profiles), 2), ]
      
      # Add respondent ID and choice ID
      choice_set <- choice_set %>% mutate(
        Respondent = i,
        Choice = j,
        Option = c("A", "B")
      )
      
      # Bind the choice set to the data
      data <- bind_rows(data, choice_set)
    }
  }
  
  return(data)
}

# Set parameters
n_respondents <- 100
n_choices <- 10

# Generate the conjoint dataset
conjoint_data <- create_conjoint_dataset(n_respondents, n_choices)

# Introduce probabilistic choice logic with more pronounced differences
set.seed(123)
conjoint_data <- conjoint_data %>%
  group_by(Respondent, Choice) %>%
  mutate(ChoiceProb = case_when(
    (Gender == "Male" & Experience == "Experienced") ~ 0.95,
    (Gender == "Female" & Policy == "Healthcare Reform") ~ 0.95,
    TRUE ~ 0.5
  ),
  Chosen = sample(Option, 1, prob = if_else(Option == "A", ChoiceProb, 1 - ChoiceProb))
  ) %>%
  ungroup()

# Format data for cregg analysis
dat <- conjoint_data %>%
  mutate(ChoiceMade = as.integer(Chosen == Option)) %>%
  select(Respondent, Choice, Option, ChoiceMade, everything())


# Analysis

p_load(cregg,dplyr,ggplot2)

analysis.1 <- cj(dat, ChoiceMade ~ Policy + Experience + Party_Affiliation, 
                                             id = ~ Respondent, 
                                             estimate = "amce")

plot(analysis.1)



analysis.2 <- cj(dat, ChoiceMade ~ Policy + Experience + Party_Affiliation, 
                                              id = ~ Respondent, 
                                              estimate = "mm", 
                                              by = ~Gender)


ggplot(analysis.2,
                           aes(factor(level),
                               y=estimate,
                               ymin=lower,
                               ymax=upper,
                               color=factor(Gender))) + 
  geom_hline(yintercept = 0.5, colour = "black", lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size=0.25)+
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    axis.text.y = element_text(size=14), 
    axis.text.x = element_text(size=14), 
    axis.title.y = element_text(size=14), 
    axis.title.x = element_text(size=14), 
    legend.text=element_text(size=14), 
    legend.title=element_text(size=14),
    plot.title = element_text(size=14),
    strip.text.x = element_text(size = 14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "", y = "")
