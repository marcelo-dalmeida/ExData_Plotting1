plot1 <- function()
{
    ## Read the data and convert both Date and Time to DateTime
    data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
    data[,1] <- as.Date(data[,1], "%d/%m/%Y")
    data <- data[(data$Date >= as.Date("2007-02-01")) & (data$Date <= as.Date("2007-02-02")),]
    data[,2] <- as.POSIXct(strptime(paste(data[,1], data[,2]), "%Y-%m-%d %H:%M:%S"))
    data <- data[-c(1)]
    names(data)[names(data) == "Time"] <- "DateTime"
    
    ##convert Global Active Power variable to double
    data[,2] <- as.double(as.character(data[,2]))
    
    ## PLOT 1
    png("plot1.png", width = 480, height = 480)
    hist(data$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "RED")
    dev.off()
}