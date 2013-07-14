library('ggplot2')
source('kmeans.R')

data.sample <- read.csv('sample-data.csv')
names(data.sample) <- c('X', 'Y')

k <- 15
data <- as.matrix(data.sample)

my.kmeans(k, as.matrix(data.sample), graph = TRUE)

