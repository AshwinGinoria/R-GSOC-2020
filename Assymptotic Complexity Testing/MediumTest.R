# Function to Classify Pieces of code as quadratic or log-linear 
# based on the runtime using curve fitting.
Classify <- function (data) {
    
    # Scale the data to log-log data
    Time <- data$Time
    logTime <- log(Time)
    N <- data$N
    N2 <- N^2

    # Get to Number of datapoints
    data_size <- length(Time)
    
    # fitting quadratic model to data
    quadratic.model <-lm(Time ~ N + N2)

    # Calculate error in Quadratic
    rms_quad <- sqrt(sum((Time - predict(quadratic.model, list(N=N, N2=N2)))^2)/data_size)

    cat("RMS-Quadratic : ", rms_quad, "\n")
    
    Plot_Quad <- function() {
        # Generating Data Points to plot the predicted Quadratic Model
        Nvalues <- seq(min(N), max(N), length=100)
        predictedTime <- predict(quadratic.model,list(N=Nvalues, N2=Nvalues^2))

        # Plotting Predicted Quadratic
        plot(N, Time, pch=16, xlab = "N", ylab = "Time", cex.lab = 1.3, col = "blue")
        lines(Nvalues, predictedTime, col = "darkgreen", lwd = 3)
    }

    # fitting log-linear model to data
    log_linear.model <- lm(logTime ~ N)
    
    rms_log <- sqrt(sum((Time - exp(predict(log_linear.model, list(N=N))))^2)/data_size)

    cat("RMS-LogLinear : ", rms_log, "\n")

    Plot_Log <- function() {
        # Generating Data Points to plot the predicted log-linear Model
        Nvalues <- (seq(min(N), max(N), length=200))
        predictedTime <- exp(predict(log_linear.model, list(N=Nvalues)))

        # Plotting Log-Linear Model
        plot(N, Time, pch=16, xlab = "N", ylab = "Time", cex.lab = 1.3, col = "blue")
        lines(Nvalues, predictedTime, col = "darkgreen", lwd = 3)
    }

    if (rms_log < rms_quad) {
        print("Log-Linear")
        Plot_Log()
    }
    else {
       print("Quadratic")
       Plot_Quad()
    }
}