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

# years provided in the data set
yearsNEI <- table(NEI$year)
countyNEI <- table(NEI$fips)


# sum the emissions related to coal combustion grouped by year
library(dplyr)
baltimore.emissions.peryear <- NEI %>%
                               filter(fips == "24510") %>% 
                               group_by(year, type) %>%
                               summarize(Emissions = sum(Emissions))

# ggplot
library(ggplot2)
ggplot(data = baltimore.emissions.peryear,
       aes(x = factor(year), y = Emissions, fill = type, label = round(Emissions,2))) + 
                  geom_bar(stat = "identity") + 
                  facet_grid(. ~ type) +
                  xlab("Year") + 
                  ylab(expression('PM'[2.5]*' emission [tons]')) +
                  ggtitle(label = expression('Total PM'[2.5]*' emission in Baltimore city'), subtitle = 'From all sources for each year') +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(plot.subtitle = element_text(hjust = 0.5)) +
                  geom_label(aes(fill = type), colour = "black", fontface = "bold", cex = 1.5)

ggsave("plot3.png")
