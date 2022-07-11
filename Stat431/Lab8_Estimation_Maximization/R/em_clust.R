#' Clusters observations into k clusters using the EM algorithm
#'
#' @param dat A dataframe
#' @param k Number of clusters
#'
#' @return A vector of cluster assignments
#'
#' @import dplyr
#' @import emdbook
#'
#' @export
em_clust <- function(dat, k) {

    means <- sample_n(dat, k)

    probs <- data.frame()

    for (i in 1:k) {
        prob_i <- dmvnorm(as.matrix(dat), as.numeric(means[i,]), cov(dat))
        probs <- rbind(probs, prob_i)
    }

    probs <- as.data.frame(probs)
    last_probs <- probs
    iter <- 0

    while (sum((as.matrix(last_probs) - as.matrix(probs))^2) >= 0.01
           | sum((as.matrix(last_probs) - as.matrix(probs))^2) == 0) {
        iter <- iter + 1

        for (i in 1:ncol(dat)) {

            sum <- 0
            denom <- 0

            for (j in 1:ncol(probs)) {
                sum <- sum + probs[, j] * dat[j, i]
                denom <- denom + probs[, j]
            }

            mean_from_prob <- sum/denom
            means[, i] <- mean_from_prob
        }

        last_probs <- probs

        probs <- data.frame()
        for (n in 1:k) {
            M_mean <- matrix(data = 1, nrow = nrow(dat)) %>%
                cbind(means[n,])
            M_mean <- M_mean[, -1]
            D <- as.matrix(as.matrix(dat) - M_mean)
            C <- t(D) %*% D * (nrow(dat) - 1)^-1

            prob_i <- dmvnorm(as.matrix(dat), as.numeric(means[n,]), C)
            probs <- rbind(probs, prob_i)
        }

        probs <- as.data.frame(probs)

    }

    high <- c()
    for (i in 1:ncol(probs)) {
        high[i] <- which.max(probs[, i])
    }

    all_cov <- data.frame()
    for (n in 1:k) {
        M_mean <- matrix(data = 1, nrow = nrow(dat)) %>%
            cbind(means[n,])
        M_mean <- M_mean[, -1]
        D <- as.matrix(as.matrix(dat) - M_mean)
        C <- t(D) %*% D * (nrow(dat) - 1)^-1
        C <- as.data.frame(C) %>%
            cbind(cluster = n)
        all_cov <- rbind(all_cov, C)
    }

    prob <- data.matrix(probs)

    return(list(cluster = high, iterations = iter, cluster_means = means, covariances = all_cov, probabilities = t(prob)))

}
