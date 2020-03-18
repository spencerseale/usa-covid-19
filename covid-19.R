library(usmap)
library(ggplot2)

#download csv from same source CDC tracks data
covid_data <- read.csv('http://cdc.gov/coronavirus/2019-ncov/map-data-cases.csv', header = TRUE, stringsAsFactors = FALSE)

covid_data$Cases.Reported
#class(covid_data)

covid_data <- covid_data[covid_data$Name != "District of Columbia" 
                         & covid_data$Name != "Guam" 
                         & covid_data$Name != "Marshall Islands" 
                         & covid_data$Name != "American Samoa" 
                         & covid_data$Name != "Palau"
                         & covid_data$Name != "Northern Marianas"
                         & covid_data$Name != "Puerto Rico"
                         & covid_data$Name != "Micronesia"
                         & covid_data$Name != "Virgin Islands"
                         ,]
#nrow(covid_data)
#ncol(covid_data)

covid_data$fips <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
                     "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                     "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
                     "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
                     "WI", "WY")

#check code and name align
covid_data[,c(1,6)]

for (x in 1:nrow(covid_data)) {
  if ("1 to 5" %in% covid_data[x,3]) {
    covid_data[x,3] <- "1"
  } else if ("None" %in% covid_data[x,3]) {
    covid_data[x,3] <- "0"
  }
}

#setting column from character to numeric
covid_data$Cases.Reported <- as.numeric(covid_data$Cases.Reported)

#verifying class
#class(covid_data$Cases.Reported)

#initializing a new column with NA 
covid_data$Severity <- NA

covid_data

#categorizing severity of case numbers for each state
for (i in 1:nrow(covid_data)) {
  if ((covid_data[i,3] / sum(covid_data$Cases.Reported)) > 0.08) {
    covid_data[i,7] <- "Severe, > 8% caseload"
  } else if ((covid_data[i,3] / sum(covid_data$Cases.Reported)) > 0.04) {
    covid_data[i,7] <- "Moderate, > 4% caseload"
  } else if ((covid_data[i,3] / sum(covid_data$Cases.Reported)) > 0.01) {
    covid_data[i,7] <- "Mild, > 1% caseload"
  } else {
    covid_data[i,7] <- "Less than 1% caseload"
  }
}

#getting just WA data and appending today's date to the data
WA_today <- cbind(Sys.Date(), subset(covid_data, Name == 'Washington'))
colnames(WA_today)[1] <- 'Date'

#running this only for the first time to intialize the WA data, ran on 2020-03-18
#WA_data_log <- WA_today

#run this once each morning to build time series data as updated by CDC, CDC must update each day to be accurate
WA_data_log <- rbind(WA_data_log, WA_today)

#scatter plot
# ggplot(data=covid_data, mapping=aes(y=Cases.Reported, x=fips, color=Severity)) +
#   geom_point() +
#   theme(axis.text.x = element_text(hjust = 1, size=10, angle=45))


ggplot(data=covid_data, aes(fips, Cases.Reported)) +
  geom_bar(stat = "identity", aes(fill=Severity)) +
  labs(x = "State", y = "Reported Cases", title = "U.S. COVID-19 Caseload", fill = "Severity") +
  theme(axis.text.x = element_text(hjust = 1, size=10, angle=45))

#barplot using base R
#barplot(Cases.Reported ~ fips, data = covid_data, las = 2, cex.names = 0.55, ylab = "Reported Cases", xlab = "States", main = "Current Number of Reported COVID-19 Cases")


(plot_usmap(regions = "states", data = covid_data[,c(6, 3)], 
            values = "Cases.Reported", color = 'black') +
  scale_fill_continuous(name = "Cases.Reported") +
  theme(legend.position = "right"))

covid_data[,c(6, 3)]

plot_usmap(data = covid_data[,c(6, 3)], values = "Cases.Reported")


barplot(Cases.Reported ~ fips, data = covid_data, las = 2, cex.names = 0.55, ylab = "Reported Cases", xlab = "States", main = "Current Number of Reported COVID-19 Cases")


