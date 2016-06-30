# Clear
rm(list = ls())
cat("\014")

# Include Libraries
source("includes.R")

# Initialize Libraries
init.libraries()

# ggplot Theme
plot.theme <- get.theme()

###############################################################################
## Data Set Initialization
###############################################################################
dataset <- get.data("AllData.csv", header = T)

###############################################################################
## Plotting
###############################################################################

#####################
### Scatter Plots
#####################

#### Prepare Plotting Data Set
plot.dataset <- dataset

rating.key <- c("UserRating")
rating.label <- c("User Rating")

plot.source <- data.frame()
for(name in colnames(plot.dataset)){
  if(name %in% names(METRICS)){
    plot.source <- rbind(
      plot.source,
      data.frame(
        "rating" = plot.dataset[[rating.key]],
        "metric.label" = METRICS[name], "metric.value" = plot.dataset[[name]],
        row.names = NULL
      )
    )
  }
}

# Export Resolution: 1440 x 560
ggplot(plot.source, aes(x = rating, y = metric.value)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~ metric.label, nrow = 1, scales = "free") +
  scale_x_continuous(breaks = seq(0.0, 5.0, by = 0.5)) +
  labs(
    title = "Scatter Plot of Avg. User Rating Versus Security Metrics",
    x = "Avg. User Rating", y = "Metric Value"
  ) +
  plot.theme

#####################
### Box Plots
#####################

#### Prepare Plotting Data Set
components <- split.dataset(dataset, metric = "UserRating")
plot.dataset <- rbind(
  data.frame(
    "type" = "l", "metric.key" = "dl", "metric.label" = "# Downloads",
    "value" = components$lower$LowerDownloads
  ),
  data.frame(
    "type" = "u", "metric.key" = "dl", "metric.label" = "# Downloads",
    "value" = components$upper$LowerDownloads
  ),
  data.frame(
    "type" = "l", "metric.key" = "ur", "metric.label" = "Avg. User Ratings",
    "value" = components$lower$UserRating
  ),
  data.frame(
    "type" = "u", "metric.key" = "ur", "metric.label" = "Avg. User Ratings",
    "value" = components$upper$UserRating
  )
)

#### Prepare Individual Plots
box.downloads <- ggplotGrob(
  ggplot(plot.dataset[plot.dataset$metric.key == "dl",], aes(type, value)) +
    geom_boxplot(aes(fill = type)) +
    facet_wrap(~ metric.label, scales = "free") +
    scale_y_log10() +
    scale_x_discrete(
      breaks = c("l", "u"), labels = c("Low Rated", "High Rated")
    ) +
    scale_fill_manual(values = c("u" = "#636363", "l" = "#f0f0f0")) +
    labs(title = NULL, x = NULL, y = "Metric Value (Log Scale)") +
    plot.theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)
    )
)

box.ratings <- ggplotGrob(
  ggplot(plot.dataset[plot.dataset$metric.key == "ur",], aes(type, value)) +
    geom_boxplot(aes(fill = type)) +
    facet_wrap(~ metric.label, scales = "free") +
    scale_x_discrete(
      breaks = c("l", "u"), labels = c("Low Rated", "High Rated")
    ) +
    scale_fill_manual(values = c("u" = "#636363", "l" = "#f0f0f0")) +
    labs(
      title = NULL, x = NULL, y = "Metric Value"
    ) +
    plot.theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)
    )
)

#### Arrange Plots
##### Export Resolution: 560 x 320
plot.grid <- grid.arrange(box.downloads, box.ratings, nrow = 1)
