complete.vectorized <- function(directory, id=1:332) {
    csvfiles <- sprintf("%s/%03d.csv", directory, id)
    nrows <- sapply(csvfiles, function(f) nrow(read.csv(f)))
#     length(count.fields(f, sep="\n"))
    sum(nrows)
}
