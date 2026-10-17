library(lme4)
library(ggplot2)

current <- matrix(c(

), nrow = 11, byrow = TRUE)

metallicness <- matrix(c(
  
), nrow = 11, byrow = TRUE)

df <- data.frame(
  Participant = rep(1:nrow(current), each = ncol(current)),
  Trial = rep(1:ncol(current), times = nrow(current)),
  Current = as.vector(current),
  Metallicness = as.vector(metallicness)
)

model <- lmer(Metallicness ~ Current + (1 | Participant), data = df)

corr <- rmcorr(participant = Participant,
               measure1 = Current,
               measure2 = Metallicness,
               dataset = df)

corr

summary(model)

set.seed(1)
plot <- ggplot(df, aes(x = Current, y = Metallicness)) +
  geom_jitter(width = 0.02, height = 0, alpha = 0.25) +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  geom_smooth(method = "loess", color = "red", size = 1) +
  theme_bw() +
  labs(
    x = "Fraction of Current Based On Maximum Tolerance",
    y = "Perceived Taste Metallicness",
    title = "Effect of Current on Perceived Taste Metallicness"
  )

plot