# Importing Required Libraries
library(microbenchmark)
library(ggplot2)

# result Dataframe will contain runtime for different 
result <- data.frame(
    N=integer(),
    Algorithm=character(),
    Time=numeric(),
    stringsAsFactors=TRUE
)

TimeDelay <- function() {
    Sys.sleep(0.000001)
}

Quadratic_Function <- function(N) {
    for (i in seq(1,N^2)) {
        TimeDelay()
    }
}

Exponential_Function <- function(N) {
    i = 0
    temp = 2^(N)
    while (i < temp) {
        i = i + 1
        TimeDelay()
    }
}

# Run Microbench For different Data Sizes
for (i in seq(5,15)) {

    # Print i to see code progress
    print(i)

    # run Microbench Test
    res <- microbenchmark(
        Quadratic_Function(i),
        Exponential_Function(i),
        times = 10L,
        unit = "ns"
    )

    # adding Data to result dataframe
    temp <- summary(res)$min
    df = data.frame(N=i, Algorithm="Quad", Time=temp[1])
    result = rbind(result, df)
    df = data.frame(N=i, Algorithm="Expo", Time=temp[2])
    result = rbind(result, df)
}

logresult <- result

# scale datapoints to their log
logresult[,1] <- log(result[,1])
logresult[,3] <- log(result[,3])

plot <- ggplot(logresult, aes(x = N, y = Time, group=Algorithm)) +
        geom_point() +
        geom_line(aes(color=Algorithm)) +
        xlab("log(N)") +
        ylab("log(Runtime (ns))")
print(plot)

# Segregating Result in different Databases for Classification
Quad <- result[ which(result$Algorithm == "Quad"),]
Expo <- result[ which(result$Algorithm == "Expo"),]