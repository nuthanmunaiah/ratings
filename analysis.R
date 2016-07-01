# Clear
rm(list = ls())
cat("\014")

# Include Libraries
source("includes.R")

# Initialize Libraries
init.libraries()

###############################################################################
## Data Set Initialization
###############################################################################
dataset <- get.data("AllData.csv", header = T)

###############################################################################
## Association
###############################################################################

#####################
### MWW
#####################

#### Overall

##### Split Data Set
components <- split.dataset(dataset, metric = "UserRating")
lower <- components$lower
upper <- components$upper

##### Test
test.outcome <- data.frame()
for(metric in names(METRICS)){
  result <- run.wilcox(population.one = lower, population.two = upper, metric)
  test.outcome <- rbind(
    test.outcome,
    data.frame(
      "genre" = "-", "metric" = metric, "n" = nrow(dataset),
      "p" = sprintf("%.4e", result$wilcox.test.out$p.value),
      "is.significant" = result$is.significant,
      "lower.median" = result$one.median,
      "compare.median" = result$median.ieq,
      "upper.median" = result$two.median,
      "lower.mean" = result$one.mean,
      "compare.mean" = result$mean.ieq,
      "upper.mean" = result$two.mean
    )
  )
}

#### Genre
for(genre in GENRES){
  ##### Filter Data Set
  genre.dataset <- dataset[dataset$Genre == genre,]

  ##### Split Data Set
  components <- split.dataset(genre.dataset, metric = "UserRating")
  lower <- components$lower
  upper <- components$upper
  ##### Test
  for(metric in names(METRICS)){
    result <- run.wilcox(population.one = lower, population.two = upper, metric)
    test.outcome <- rbind(
      test.outcome,
      data.frame(
        "genre" = genre, "metric" = metric, "n" = nrow(genre.dataset),
        "p" = sprintf("%.4e", result$wilcox.test.out$p.value),
        "is.significant" = result$is.significant,
        "lower.median" = result$one.median,
        "compare.median" = result$median.ieq,
        "upper.median" = result$two.median,
        "lower.mean" = result$one.mean,
        "compare.mean" = result$mean.ieq,
        "upper.mean" = result$two.mean
      )
    )
  }
}
print(test.outcome)
