########## Conversion rate by yards to go plot


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales) # For percentage formatting

# Create bins and calculate conversion rates
plot_data_1 <- pbp_final_analysis %>%
  # Create bins for ydstogo
  mutate(
    ydstogo_group = cut(ydstogo,
                        breaks = c(0, 1, 2, 3, 6, 9, 50),
                        labels = c("1 yd", "2 yds", "3 yds", "4-6 yds", "7-9 yds", "10+ yds"),
                        right = TRUE
    )
  ) %>%
  # Group by the new bins and calculate the mean conversion rate
  group_by(ydstogo_group) %>%
  summarise(conversion_rate = mean(fourth_down_converted, na.rm = TRUE))

# Create the bar plot
ggplot(plot_data_1, aes(x = ydstogo_group, y = conversion_rate)) +
  geom_bar(stat = "identity", fill = "#4c72b0") +
  # Add percentage labels on top of the bars
  geom_text(aes(label = scales::percent(conversion_rate, accuracy = 0.1)), 
            vjust = -0.5, 
            fontface = "bold") +
  # Format titles and axes
  labs(
    title = "4th Down Conversion Rate by Yards-to-Go",
    x = "Yards-to-Go (Grouped)",
    y = "Conversion Rate"
  ) +
  # Format the Y-axis as percentages
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16))






############## Odds Ratio Plot

library(broom) # For cleaning up model output

# Run the logistic regression model (glm)
# We use the significant variables for a clean plot
logit_model <- glm(fourth_down_converted ~ ydstogo + is_pass + no_huddle + shotgun + is_outside, 
                   data = pbp_final_analysis, 
                   family = binomial(link = "logit"))

# Use broom::tidy() to get a clean data frame of coefficients
# conf.int = TRUE gives us confidence intervals
# exponentiate = TRUE automatically converts log-odds to odds ratios (Exp(B))
plot_data_2 <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE)

# Filter out the (Intercept)
plot_data_2_significant <- plot_data_2 %>%
  filter(term != "(Intercept)")

# Create the forest plot
ggplot(plot_data_2_significant, aes(x = estimate, y = term)) +
  # Add the points (Odds Ratios) and the error bars (95% CI)
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  # Add the vertical "no effect" line at 1.0
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "red", linewidth = 1) +
  # Format titles and axes
  labs(
    title = "Odds Ratios for Predictors",
    x = "Odds Ratio (Exp(B)) with 95% Confidence Interval",
    y = "Predictor Variable",
    caption = "Variables are significant if their 95% CI does not cross the red line (Odds Ratio = 1.0)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16))







########### Pass/Run Decision vs yards to go

library(tidyr) # For pivoting data

# 1. Create the data for the plot
plot_data_3 <- pbp_final_analysis %>%
  
  # Create the same bins for ydstogo
  mutate(
    ydstogo_group = cut(ydstogo,
                        breaks = c(0, 1, 2, 3, 6, 9, 50),
                        labels = c("1 yd", "2 yds", "3 yds", "4-6 yds", "7-9 yds", "10+ yds"),
                        right = TRUE
    )
  ) %>%
  
  # Group by the new bins and calculate pass and run rates
  group_by(ydstogo_group) %>%
  summarise(
    Pass = mean(is_pass, na.rm = TRUE),
    Run = 1 - mean(is_pass, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Pivot to long format for ggplot stacking
  pivot_longer(
    cols = c("Pass", "Run"),
    names_to = "play_type",
    values_to = "percentage"
  ) %>%
  
  # drop_na is important if any group had 0 plays
  tidyr::drop_na(ydstogo_group) 

# 2. Create the stacked bar plot
ggplot(plot_data_3, aes(x = ydstogo_group, y = percentage, fill = play_type)) +
  # position = "stack" (or "fill") creates the 100% stacked bar
  geom_bar(stat = "identity", position = "stack") + 
  
  # Add percentage labels inside the bars
  geom_text(
    aes(label = scales::percent(percentage, accuracy = 0.1)), 
    position = position_stack(vjust = 0.5), # Stacks labels in the middle
    fontface = "bold",
    size = 3.5,
    color = "white" # White text for better contrast
  ) +
  
  # Format titles and axes
  labs(
    title = "Coaching Tendency: Play Type by Yards-to-Go",
    x = "Yards-to-Go (Grouped)",
    y = "Percentage of Plays",
    fill = "Play Type"
  ) +
  
  # Format the Y-axis as percentages
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.0)) +
  # Manually set colors for consistency
  scale_fill_manual(values = c("Pass" = "#4c72b0", "Run" = "green4")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16))








########## Conversion rate by play type and yards to go

# 1. Create the data for the plot, but add 'count = n()'
plot_data_4_with_counts <- pbp_final_analysis %>%
  
  mutate(
    ydstogo_group = cut(ydstogo,
                        breaks = c(0, 1, 2, 3, 6, 9, 50),
                        labels = c("1 yd", "2 yds", "3 yds", "4-6 yds", "7-9 yds", "10+ yds"),
                        right = TRUE
    ),
    play_type = factor(ifelse(is_pass == 1, "Pass", "Run"))
  ) %>%
  
  group_by(ydstogo_group, play_type) %>%
  
  # --- ADD 'count = n()' HERE ---
  summarise(
    count = n(),  # This will count the number of plays in each group
    conversion_rate = mean(fourth_down_converted, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  tidyr::drop_na(ydstogo_group)

# 2. Print the table to the console to check
print(plot_data_4_with_counts)

# 2. Create the grouped bar plot
ggplot(plot_data_4_with_counts, aes(x = ydstogo_group, y = conversion_rate, fill = play_type)) +
  # position = "dodge" creates the grouped (side-by-side) bars
  geom_bar(stat = "identity", position = "dodge") +
  
  geom_text(
    # Create a new label with two lines: percent and count
    aes(label = paste0(scales::percent(conversion_rate, accuracy = 0.1), "\n(n=", count, ")")), 
    vjust = -0.4, # Adjust vertical position to show both lines
    fontface = "bold",
    size = 3.0,   # Make text slightly smaller
    position = position_dodge(width = 0.9)
  ) +
  
  # Format titles and axes
  labs(
    title = "Conversion Rate: Pass vs. Run by Yards-to-Go",
    x = "Yards-to-Go (Grouped)",
    y = "Conversion Rate",
    fill = "Play Type" # This titles the legend
  ) +
  
  # Format the Y-axis as percentages
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.90)) +
  # Use the same colors as Plot 1 for consistency
  scale_fill_manual(values = c("Pass" = "#4c72b0", "Run" = "green4")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16))
