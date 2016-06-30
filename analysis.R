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
      "genre" = "-", "metric" = metric,
      "p" = result$wilcox.test.out$p.value,
      "lower.median" = result$one.median,
      "compare.median" = (
        if(result$one.median == result$two.median) "="
        else if(result$one.median > result$two.median)
          ">"
        else
          "<"
      ),
      "upper.median" = result$two.median,
      "lower.mean" = result$one.mean,
      "compare.mean" = (
        if(result$one.mean == result$two.mean) "="
        else if(result$one.mean > result$two.mean)
          ">"
        else
          "<"
      ),
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
        "genre" = genre, "metric" = metric,
        "p" = result$wilcox.test.out$p.value,
        "lower.median" = result$one.median,
        "compare.median" = (
          if(result$one.median == result$two.median) "="
          else if(result$one.median > result$two.median)
            ">"
          else
            "<"
        ),
        "upper.median" = result$two.median,
        "lower.mean" = result$one.mean,
        "compare.mean" = (
          if(result$one.mean == result$two.mean) "="
          else if(result$one.mean > result$two.mean)
            ">"
          else
            "<"
        ),
        "upper.mean" = result$two.mean
      )
    )
  }
}
print(test.outcome)
