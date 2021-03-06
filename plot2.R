## The readData function obtains data for the Course Project 1.
## It downloads a zip file from the given URL, unzips it and reads the
## data from the resulting text file, storing it in a data frame.
readData <- function (override = FALSE) {
    # Download zip file if (i) it does not exist yet or (ii) it should be overriden.
    if (!file.exists("electric_power_consuption.zip") || override == TRUE) {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = "electric_power_consuption.zip")
        
        print("Zip file successfully downloaded.")
    }
    else {
        print("Zip file already downloaded.")
    }
    
    # Unzip file and create text file if (i) it does not exist yet or (ii) it should be overriden.
    if (!file.exists("household_power_consumption.txt") || override == TRUE) {
        unzip("electric_power_consuption.zip")
        
        print("File successfully unziped.")
    }
    else {
        print("File already unziped.")
    }
    
    # Read data and store it on a data frame
    data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", skip=66637, nrows=2880, col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    # Transform Date and Time columns on an only DateTime column, in the correct format.
    data$DateTime <- paste(data$Date, data$Time)
    data$DateTime <- strptime(data$DateTime, format = "%d/%m/%Y %H:%M:%S")
    
    # Return the resulting data frame.
    data
}

## The plot2 function draws a graph corresponding to the Plot 2 graph of the project.
plot2 <- function() {
    # Read data into a data frame using readData function.
    data <- readData()

    # Create the graph.
    with (data, plot(DateTime, Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
}

## The png2 function uses the plot2 function to create a png with the Plot 2 graph of the project.
png2 <- function() {
    # Open png device.
    png(filename = "plot2.png", width = 480, height = 480, units = "px")
    
    # Create the graph using plot2 function.
    plot2()
    
    # Close the png device.
    dev.off()
}