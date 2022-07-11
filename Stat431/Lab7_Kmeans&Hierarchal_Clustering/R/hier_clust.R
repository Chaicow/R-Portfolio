#' Clusters observations into groups using agglomerative hierarchical clustering
#'
#' @param dat A Data Frame
#' @k Number of Clusters to Return the Assignments at
#'
#' @return A Vector of Cluster Assignments
#'
#' @import dplyr
#'
#'
#' @export
hier_clust <- function(dat, k) {
    names <- rownames(dat)

    merges <- data.frame(one = as.numeric(),
                         two = as.numeric())
    data <- dat

    for (i in 1:(nrow(data) - 1)){
        dist <- dist(data)
        dist <- dist %>%
            as.matrix() %>%
            as.data.frame()
        dist[dist == 0] <- NA
        closest <- which(as.matrix(dist) == min(dist, na.rm = TRUE), arr.ind = TRUE)[1,]
        merges[i,] <- c(-closest[2], -closest[1])
        recent_merge <- data[c(closest[1], closest[2]),] %>%
            summarize_all(mean)
        data[closest[1],] <- recent_merge
        data[closest[2],] <- NA
    }

    n <- nrow(dat)
    clean_df <- clean_merges(merges, n)

    res <- list()
    res$merge <- clean_df %>%
        as.matrix()
    result <- cutree(res, k)

    names(result) <- names

    return(result)
}



clean_merges <- function(merges, n) {

    merges_clean <- data.frame(one = as.numeric(),
                               two = as.numeric())

    merges_clean[1,] <- merges[1,]

    for (i in 2:(n - 1)) {
        if (merges$one[i] %in% merges$one[1:i-1] | merges$one[i] %in% merges$two[1:i-1]){
            rows <- which(merges$one[i] == merges$one[1:i-1])
            rows <- c(rows, which(merges$one[i] == merges$two[1:i-1]))
            rows <- max(rows)
            merges_clean[i,1] <- rows
            if (merges$two[i] %in% merges$one[1:i-1] | merges$two[i] %in% merges$two[1:i-1]){
                rows <- which(merges$two[i] == merges$one[1:i-1])
                rows <- c(rows, which(merges$two[i] == merges$two[1:i-1]))
                rows <- max(rows)
                merges_clean[i,2] <- rows
            } else {
                merges_clean[i,2] <- merges[i,2]
            }
        } else if (merges$two[i] %in% merges$one[1:i-1] | merges$two[i] %in% merges$two[1:i-1]) {
            rows <- which(merges$two[i] == merges$one[1:i-1])
            rows <- c(rows, which(merges$two[i] == merges$two[1:i-1]))
            rows <- max(rows)
            merges_clean[i,2] <- rows
            if (merges$one[i] %in% merges$one[1:i-1] | merges$one[i] %in% merges$two[1:i-1]){
                rows <- which(merges$one[i] == merges$one[1:i-1])
                rows <- c(rows, which(merges$one[i] == merges$two[1:i-1]))
                rows <- max(rows)
                merges_clean[i,1] <- rows
            } else {
                merges_clean[i,1] <- merges[i,1]
            }
        } else {
            merges_clean[i,] <- merges[i,]
        }

    }

    return(merges_clean)

}


