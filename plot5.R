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
table(SCC$EI.Sector)

vehicles.SCC.data <- SCC[grepl("[Vv]ehicles", SCC$EI.Sector), "SCC"]

# years provided in the data set
yearsNEI <- table(NEI$year)
countyNEI <- table(NEI$fips)

# sum the emissions related to coal combustion grouped by year
library(dplyr)
baltimore.emissions.peryear <- NEI %>%
        filter(fips == "24510" & SCC %in% vehicles.SCC.data) %>% 
        group_by(year) %>%
        summarize(Emissions = sum(Emissions))

# ggplot
library(ggplot2)
result.plot <- ggplot(data = baltimore.emissions.peryear,
               aes(x = factor(year), y = round(Emissions/1000, 2), fill = year, label = round(Emissions/1000,2))) +    
               geom_bar(stat = "identity") + 
               xlab("Year") + 
               ylab(expression('PM'[2.5]*' emission [Kilotons]')) +
               ggtitle(label = expression('Total PM'[2.5]*' vehicle emission in Baltimore city'), subtitle = 'For each year') +
               theme(plot.title = element_text(hjust = 0.5)) +
               theme(plot.subtitle = element_text(hjust = 0.5)) +
               geom_label(aes(fill = year), colour = "white", fontface = "bold", cex = 2.8)
ggsave("plot5.png")