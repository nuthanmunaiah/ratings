# Constants Initialization
METRICS <- c(
  "PermissionCount" = "# Permissions",
  "OPrivCount" = "# Over Permissions",
  "UPrivCount" = "# Under Permissions",
  "AndroRisk" = "Androrisk Score"
)

GENRES <- c(
  "Tools", "Entertainment", "Education", "Personalization", "Puzzle"
)

init.libraries <- function(){
  suppressPackageStartupMessages(library("dplyr"))
  suppressPackageStartupMessages(library("ggplot2"))
  suppressPackageStartupMessages(library("grid"))
  suppressPackageStartupMessages(library("gridExtra"))
  suppressPackageStartupMessages(library("hexbin"))
  suppressPackageStartupMessages(library("knitr"))
  suppressPackageStartupMessages(library("stringr"))
}

get.theme <- function(){
  plot.theme <-
    theme_bw() +
    theme(
      plot.title = element_text(
        size = 14, face = "bold", margin = margin(5,0,25,0)
      ),
      axis.text.x = element_text(size = 10, angle = 50, vjust = 1, hjust = 1),
      axis.title.x = element_text(face = "bold", margin = margin(15,0,5,0)),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(face = "bold", margin = margin(0,15,0,5)),
      strip.text.x = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 9)
    )
  return(plot.theme)
}

get.data <- function(file, header=T){
  if(!file.exists(file)){
    stop(sprintf(paste(file, "not found.")))
  }
  return(read.csv(file, header = header))
}

get.spearmansrho <- function(dataset, column.one, column.two, p.value = 0.05){
  correlation <- cor.test(
    dataset[[column.one]], dataset[[column.two]],
    method = "spearman", exact = F
  )
  if(correlation$p.value > p.value){
    warning(paste("Spearman's insignificant with p =", correlation$p.value))
  }
  return(round(correlation$estimate, 4))
}

split.dataset <- function(dataset, metric, grouping.variable, threshold = 0.1){
  dataset <- dataset[order(dataset[[metric]]),]
  n <- nrow(dataset)
  threshold.number <- ceiling(n * threshold)

  lower <- dataset[1:threshold.number,]
  upper <- dataset[(n - threshold.number + 1):n,]

  return(list("upper" = upper, "lower" = lower))
}

run.wilcox <- function(population.one, population.two, metric, alpha = 0.05){
  population.one.metric <- population.one[[metric]]
  population.two.metric <- population.two[[metric]]

  result <- vector(mode = "list", length = 8)
  names(result) <- c(
    "wilcox.test.out", "is.significant",
    "one.median", "med.ieq", "two.median",
    "one.mean", "mean.ieq", "two.mean"
  )
  result$wilcox.test.out <- wilcox.test(
    population.one.metric, population.two.metric
  )
  result$is.significant <- "No"
  if(result$wilcox.test.out$p.value <= alpha){
    result$is.significant <- "Yes"
  }

  result$one.median = round(median(population.one.metric, na.rm=TRUE), 4)
  result$two.median = round(median(population.two.metric, na.rm=TRUE), 4)
  result$median.ieq = get.inequality(result$one.median, result$two.median)

  result$one.mean = round(mean(population.one.metric, na.rm=TRUE), 4)
  result$two.mean = round(mean(population.two.metric, na.rm=TRUE), 4)
  result$mean.ieq = get.inequality(result$one.mean, result$two.mean)

  return(result)
}

get.inequality <- function(one, two){
  if(one == two)
    return("=")
  else if(one > two)
    return(">")
  else
    return("<")
}
