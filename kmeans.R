fun.centers <- function (k, data, index) {
    do.call(rbind, lapply(1:k, function (i) {
        cluster <- data[index == i, ]
        colSums(cluster) / nrow(cluster)
    }))
}

fun.neighbor <- function (data, centers) {
    apply(data, 1, function (v) {
        which.min(apply(centers, 1, function (center) {
            sum((v - center)^2)
        }))
    })
}

fun.clusters.init <- function (k, data) {
    index <- sample(1:nrow(data), k)
    data[index, ]
}

fun.clusters.draw <- function (i, data, clusters, centers) {
    data.clustered <- data.frame(X = data[,1], Y = data[,2], Cluster = factor(clusters))

    centers.df <- as.data.frame(centers)
    names(centers.df) <- c('X', 'Y')

    p <- ggplot(data.clustered, aes(x = X, y = Y)) +
        geom_point(aes(color = Cluster), size = 1) + 
            geom_point(aes(x = X, y = Y), data = centers.df, color = 'red', size = 2)
    ggsave(p, file = sprintf('images/graph-%02d.png', i), width = 6, height = 6)
}    

fun.is.converge <- function (c1, c2) {
    centers <- rbind(c1$Centers, c2$Centers)
    range <- apply(centers, 2, function (v) { (max(v) - min(v)) })

    sqrt(sum((c1$Centers - c2$Centers)^2) / sum(range^2)) < 0.005
}

my.kmeans.init <- function (k, data) {
    centers <- fun.clusters.init(k, data)
    clusters <- fun.neighbor(data, centers)

    list(Clusters = clusters, Centers = centers)
}
my.kmeans.step <- function (k, data, clusters, centers) {
    centers <- fun.centers(k, data, clusters)
    clusters <- fun.neighbor(data, centers)

    list(Clusters = clusters, Centers = centers)
}

my.kmeans <- function (k, data, max_trial = 20, graph = FALSE) {

    cluster <- my.kmeans.init(k, data)
    if (graph) { with(cluster, { fun.clusters.draw(0, data, Clusters, Centers) }) }

    i <- 1
    repeat {

        cluster.old <- cluster
        cluster <- with(cluster, my.kmeans.step(k, data, Clusters, Centers))

        if (graph) { with(cluster, { fun.clusters.draw(i, data, Clusters, Centers) }) }

        if (i == max_trial || fun.is.converge(cluster.old, cluster)) { break }

        i <- i + 1
    }

    cluster
}
