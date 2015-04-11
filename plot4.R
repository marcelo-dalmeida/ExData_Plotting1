plot4 <- function()
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
    
    ##convert  variables to double
    data[2:8] <- sapply(data[2:8], factorToNumeric <- function(f) as.double(as.character(f)))
    
    ## PLOT 4
    png("plot4.png", width = 480, height = 480)
    
    par(mfrow = c(2, 2))
    with(data2, {
        
        #PLOT 4.1
        plot(DateTime, Global_active_power, main = "", 
             xlab = "", ylab = "Global Active Power", type = "l")
        
        #PLOT 4.2
        plot(DateTime, Voltage, main = "", 
            xlab = "datetime", ylab = "Voltage", type = "l")
        
        #PLOT 4.3
        plot(DateTime, Sub_metering_1, main = "", xlab = "", 
             ylab = "Energy sub metering",
             type = "n")
    
        lines(DateTime, Sub_metering_1, col = "black")
        lines(DateTime, Sub_metering_2, col = "red")
        lines(DateTime, Sub_metering_3, col = "blue")
    
        legend("topright", lwd = 2, col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        #PLOT 4.4
        plot(DateTime, Global_reactive_power, main = "", 
             xlab = "datetime", type = "l")
    })
    
    dev.off()
}