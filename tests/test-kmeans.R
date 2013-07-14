test_that("centers", {
    sample <- rbind(c(1,1),
                    c(1,2),
                    c(2,1),
                    c(2,2),
                    c(3,3),
                    c(4,5))
    index <- c(1,1,1,1,2,2)

    centers <- fun.centers(2, sample[,c(1,2)], index)
    expect_equal(nrow(centers), 2)
    expect_equal(centers[1,], c(1.5, 1.5))
})

test_that("neighbor", {
    sample <- cbind(rnorm(10, mean = 0),
                    rnorm(10, mean = 0))
    centers <- rbind(c( 1,  1),
                     c(-1,  1),
                     c( 1, -1),
                     c(-1, -1))

    neighbor <- fun.neighbor(sample, centers)

    
    cluster.1 <- sample[neighbor == 1,]
    x0 <- cluster.1[1,1]
    y0 <- cluster.1[1,2]
    expect_true(all(x0 * cluster.1[,1] >= 0) && all(y0 * cluster.1[,2] >= 0)) #
    expect_equal(length(unique(neighbor)), nrow(centers))
})

test_that("kmeans", {
    sample <- rbind(cbind(rnorm(10, mean = 1, sd = 0.05),
                          rnorm(10, mean = 1, sd = 0.05)),
                    cbind(rnorm(10, mean = 0, sd = 0.05),
                          rnorm(10, mean = 0, sd = 0.05)))

    cluster <- my.kmeans(2, sample)

    expect_equal(length(unique(cluster$Clusters[1:10])), 1)
})
