library('ggplot2')
source('kmeans.R')

data.sample <- read.csv('sample-data.csv')
names(data.sample) <- c('X', 'Y')

k <- 15
data <- as.matrix(data.sample)

my.kmeans(k, as.matrix(data.sample), graph = TRUE)



data.circle <-  function (n, c, sd, p = 0) {
    data <- data.frame(R = runif(n, min = p, max = p + sd), angle = runif(n, max = 2 * pi))
    cbind(with(data, c[1] + R * cos(angle)),
          with(data, c[2] + R * sin(angle)))
}

data.fail <- rbind(data.circle(5000, c(0,0), 1),
                   data.circle(3000, c(1,1),  0.35),
                   data.circle(3000, c(1,-1), 0.35)
                   )
ggplot(as.data.frame(data.fail), aes(x = V1, y = V2)) + geom_point()

my.kmeans(3, data.fail, graph = TRUE, filename = 'fail')


data.fail2 <- rbind(data.circle(3000, c(0,0), 1),
                    data.circle(3000, c(0,0), 0.2, p = 1.4))
ggplot(as.data.frame(data.fail2), aes(x = V1, y = V2)) + geom_point()

my.kmeans(2, data.fail2, graph = TRUE, filename = 'fail2')

