
shaps <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("select *
 from model.shap
 where run_id = '2025-01-28-priceless-nicole'"
  ))

data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("select meta_pin, floor
 from model.assessment_card
 where run_id = '2025-01-28-priceless-nicole'"
  ))

shaps <- shaps %>%
  rename(floor_shap = floor) %>%
  select(meta_pin, floor_shap) %>%
  mutate(floor_shap = as.numeric(floor_shap))


data <- data %>%
  select(meta_pin, floor)

data <- data %>%
  left_join(shaps, by = c("meta_pin" = "meta_pin"))

# Ensure `floor` is numeric
data <- data %>%
  mutate(floor = as.numeric(floor),
         floor_shap = as.numeric(floor_shap))

# Group `floor` into the specified bins
data <- data %>%
  mutate(
    floor_group = case_when(
      floor == 0 ~ "0",
      floor == 1 ~ "1",
      floor == 2 ~ "2",
      floor == 3 ~ "3",
      floor == 4 ~ "4",
      floor >= 5 & floor <= 10 ~ "5-10",
      floor >= 11 & floor <= 20 ~ "11-20",
      floor >= 21 & floor <= 30 ~ "21-30",
      floor > 30 ~ ">30",
      is.na(floor) ~ "NA", # Handle missing values
      TRUE ~ "Other" # Optional, handles unexpected cases
    ),
    floor_group = factor(
      floor_group,
      levels = c("0", "1", "2", "3", "4", "5-10", "11-20", "21-30", ">30", "NA", "Other")
    )
  )

# Create the violin plot with correct order
ggplot(data, aes(x = floor_group, y = floor_shap, fill = floor_group)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Violin Plot of SHAP Values by Floor Groups",
    x = "Floor Group",
    y = "Floor SHAP Values",
    fill = "Floor Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend if not needed
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for clarity
  )
