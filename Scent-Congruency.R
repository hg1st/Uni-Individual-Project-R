library(lme4)
library(ggplot2)
library(emmeans)

scent <- matrix(c(

), nrow = 11, byrow = TRUE)

congruency <- matrix(c(
  
), nrow = 11, byrow = TRUE)

df <- data.frame(
  Participant = rep(1:nrow(scent), each = ncol(scent)),
  Trial = rep(1:ncol(scent), times = nrow(scent)),
  Scent = as.vector(scent),
  Congruency = as.vector(congruency)
)

df$Scent <- factor(df$Scent, levels = c("Control", "Strawberry", "Vanilla", "Citrus", "Wood", "Garlic"))

model <- lmer(Congruency ~ Scent + (1 | Participant), data = df)

posthoc <- emmeans(model, trt.vs.ctrl ~ Scent, ref = "Control")

posthoc

summary(model)

set.seed(1)
plot <- ggplot(df, aes(x = Scent, y = Congruency)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.3) +
  theme_bw() +
  labs(
    x = "Scent",
    y = "Perceived Congruency",
    title = "Effect of Scent on Perceived Congruency"
  )

plot