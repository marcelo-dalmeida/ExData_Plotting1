plot3 <- function()
{
    # Forum tip to put weekdays like plot example
    Sys.setlocale("LC_ALL","C")
    
    ## Read the data and convert both Date and Time to DateTime
    data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
    data[,1] <- as.Date(data[,1], "%d/%m/%Y")
    data <- data[(data$Date >= as.Date("2007-02-01")) & (data$Date <= as.Date("2007-02-02")),]
    data[,2] <- as.POSIXct(strptime(paste(data[,1], data[,2]), "%Y-%m-%d %H:%M:%S"))
    data <- data[-c(1)]
    names(data)[names(data) == "Time"] <- "DateTime"
    
    ##convert submetering variable to double
    data[6:8] <- sapply(data[6:8], factorToNumeric <- function(f) as.double(as.character(f)))
    
    ## PLOT 3
    png("plot3.png", width = 480, height = 480)
    
    with(data,
         plot(DateTime, Sub_metering_1, main = "", xlab = "", 
              ylab = "Energy sub metering",
              type = "n"))
    
    with(data, lines(DateTime, Sub_metering_1, col = "black"))
    with(data, lines(DateTime, Sub_metering_2, col = "red"))
    with(data, lines(DateTime, Sub_metering_3, col = "blue"))
    
    legend("topright", lwd = 2, col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    dev.off()
}