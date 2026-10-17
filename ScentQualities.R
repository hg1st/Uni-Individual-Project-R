library(tidyr)
library(dplyr)
library(ggplot2)

scent <- matrix(c(

), nrow = 11, byrow = TRUE)

intensity <- matrix(c(

), nrow = 11, byrow = TRUE)

pleasantness <- matrix(c(

), nrow = 11, byrow = TRUE)

familiarity <- matrix(c(
  
), nrow = 11, byrow = TRUE)

df <- data.frame(
  Participant = rep(1:nrow(scent), each = ncol(scent)),
  Trial = rep(1:ncol(scent), times = nrow(scent)),
  Scent = as.vector(scent),
  Intensity = as.vector(intensity),
  Pleasantness = as.vector(pleasantness),
  Familiarity = as.vector(current)
)

df_long <- df %>%
  pivot_longer(
    cols = c(Intensity, Pleasantness, Familiarity),
    names_to = "Measure",
    values_to = "Value"
  )

summary_df <- df_long %>%
  group_by(Scent, Measure) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    se = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(summary_df, aes(x = Scent, y = mean, fill = Scent)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.25
  ) +
  facet_wrap(~ Measure, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Scent",
    y = "Mean Rating",
    title = "Scent Familiarity, Intensity and Pleasantness"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )