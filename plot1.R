# STEP 0 - Initial set-up
# download and unzip data from the web
file_url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zip_file <- ".\\exdata_data_NEI_data.zip"
if (!file.exists(zip_file)) { 
        download.file(file_url, destfile = zip_file, mode= 'wb')
        unzip(zip_file, exdir = getwd())
        date_download <- date()
}

# read the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get a sense of the data
head(NEI)
head(SCC[, c("SCC", "Short.Name")])

# years provided in the data set
yearsNEI <- table(NEI$year)

# sum the emissions grouped by year
library(dplyr)
total.emissions.peryear <- NEI %>% 
                           group_by(year) %>%
                           summarize(Emissions = sum(Emissions))

# barplot
png("plot1.png", width = 640, height = 480)
emission.plot <- with(total.emissions.peryear,
                    barplot(height = total.emissions.peryear$Emissions/1000, 
                            names.arg = total.emissions.peryear$year,
                            col = 3: (length(yearsNEI)+2),
                            xlab = "Year",
                            ylab = expression('PM'[2.5]*' emission [Kilotons]'),
                            main = expression('Total PM'[2.5]*' emission from all sources for each year'),
                            ylim = c(0, 8000)
                            )
                      )

# add number of emissions at the top of each year-bar
emission.value <- round(total.emissions.peryear$Emissions/1000, 2)
text(x = emission.plot, 
     y = emission.value, 
     label = emission.value,
     pos = 3,
     cex= 0.8,
     font = 3 # italic font
     )

print(emission.plot)
dev.off()





