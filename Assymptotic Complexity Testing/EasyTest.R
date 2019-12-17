# Importing Required Libraries
library(microbenchmark)
library(PeakSegOptimal)
library(PeakSegDP)
library(ggplot2)

# result Dataframe will contain runtime for different 
result <- data.frame(
    N=integer(),
    Algorithm=character(),
    Time=numeric(),
    stringsAsFactors=TRUE
)

# Run Microbench For different Data Sizes
for (i in c(10,100,1000)) {

    # Print i to see code progress
    print(i)

    # Specifying Data Lengths
    count = rpois(i, 5)
    weight = rep(1, length(count))
    maxSegments = 3L

    # run Microbench Test
    res <- microbenchmark(
        cDPA(count, weight, maxSegments),
        PeakSegPDPA(count, weight, maxSegments),
        times = 25L,
        unit = "ns"
    )

    # adding Data to result dataframe
    temp <- summary(res)$min
    df = data.frame(N=i, Algorithm="cDPA", Time=temp[1])
    result = rbind(result, df)
    df = data.frame(N=i, Algorithm="PDPA", Time=temp[2])
    result = rbind(result, df)
}

# scale datapoints to their log
logresult <- result
logresult[,1] <- log(result[,1])   
logresult[,3] <- log(result[,3])

# PLotting log-log graph
plot <- ggplot(logresult, aes(x = N, y = Time, group=Algorithm)) + 
        geom_point() + 
        geom_line(aes(color=Algorithm)) +
        xlab("log(N)") +
        ylab("log(Runtime (ns))")
print(plot)

# Segregating Data for Classification
cDPA <- result[ which(result$Algorithm == "cDPA"),]
PDPA <- result[ which(result$Algorithm == "PDPA"),]