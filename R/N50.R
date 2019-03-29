N50 <- function(len) {
    # sort scaffold or chromosome lenghts in descending order
    len.sorted <- rev(sort(len))
    # compute N50 over all scaffold or chromosome lenghts in Mbp
    N50 <- len.sorted[cumsum(as.numeric(len.sorted)) >= sum(len.sorted)*0.5][1] / 1000000
    return(N50)
}
