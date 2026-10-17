library(lme4)
library(ggplot2)
library(emmeans)

scent <- matrix(c(
  
), nrow = 11, byrow = TRUE)

modality <- matrix(c(

), nrow = 11, byrow = TRUE)

df <- data.frame(
  Participant = rep(1:nrow(scent), each = ncol(scent)),
  Trial = rep(1:ncol(scent), times = nrow(scent)),
  Scent = as.vector(scent),
  Modality = as.vector(modality)
)

df$Scent <- factor(df$Scent, levels = c("Control", "Strawberry", "Vanilla", "Citrus", "Wood", "Garlic"))

df$SmellDominant <- ifelse(df$Modality == "Smell", 1, 0)

df_sub <- subset(df, Modality != "Neither")

model <- lmer(
  SmellDominant ~ Scent + (1 | Participant),
  data = df_sub
)

posthoc <- emmeans(model, trt.vs.ctrl ~ Scent, ref = "Control")

posthoc

summary(model)

set.seed(1)
plot <- ggplot(df_sub, aes(x = Scent, fill = Modality)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(
    x = "Scent",
    y = "Proportion",
    title = "Modality Dominance by Scent"
  )

plot