library(lme4)
library(ggplot2)

current <- matrix(c(

), nrow = 11, byrow = TRUE)

pleasantness <- matrix(c(
  
), nrow = 11, byrow = TRUE)

df <- data.frame(
  Participant = rep(1:nrow(current), each = ncol(current)),
  Trial = rep(1:ncol(current), times = nrow(current)),
  Current = as.vector(current),
  Pleasantness = as.vector(pleasantness)
)

model <- lmer(Pleasantness ~ Current + (1 | Participant), data = df)

corr <- rmcorr(participant = Participant,
               measure1 = Current,
               measure2 = Pleasantness,
               dataset = df)

corr

summary(model)

set.seed(1)
plot <- ggplot(df, aes(x = Current, y = Pleasantness)) +
  geom_jitter(width = 0.02, height = 0, alpha = 0.25) +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  geom_smooth(method = "loess", color = "red", size = 1) +
  theme_bw() +
  labs(
    x = "Fraction of Current Based On Maximum Tolerance",
    y = "Perceived Taste Pleasantness",
    title = "Effect of Current on Perceived Taste Pleasantness"
  )

plot