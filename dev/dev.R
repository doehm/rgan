
library(myPalettes)

choose_palette()
load_font("Open Sans")

y_train <- get_training_data()
plot_curves(y_train) +
  theme_minimal() +
  scale_fill_gd()


plot_curves(y_train) +
  labs(
    title = "Sine waves used for input into the GAN"
    ) +
  theme(
    plot.title = element_text(family = "userfont", hjust = 0.5, size = 48)
    ) +
  ggsave("./sine-wave-input-data.png", height = 6, width = 12, dpi = 150)



get_curves(y_train) %>%
  mutate(
    cluster = as.factor(cluster)
    ) %>%
  ggplot(aes(x = x, y = y, colour = cluster)) +
  geom_line(size = 0.5) +
  facet_wrap(~series, nrow = 1) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_blank(),
    legend.position = "none",
    plot.title = element_text(family = "userfont", hjust = 0.5, size = 24),
    plot.subtitle = element_text(family = "userfont", hjust = 0.5, size = 12)
  ) +
  scale_colour_gd() +
  labs(
    title = "Sine waves used for input into the GAN",
    subtitle = expression(paste("Functions:  3", sin, "(10", pi, theta, ") +", epsilon, "  and  ", sin, "(2", pi, theta, ") +", epsilon, "  where  ", epsilon, " = N(0, 0.05)"))
  ) +
  ggsave("./sine-wave-input-data.png", height = 6, width = 12, dpi = 100)


