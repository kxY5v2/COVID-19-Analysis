install.packages("dplyr")
install.packages("lubridate")
install.packages("readr")
install.packages("usmap")
install.packages("ggplot2")

# Function to ensure packages are downloaded
EnsurePackage<-function(x) {
  x <- as.character(x)
  
  if (!require(x,character.only=TRUE)) {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# Packages to ensure
EnsurePackage("dplyr")
EnsurePackage("lubridate")
EnsurePackage("readr")
EnsurePackage("usmap")
EnsurePackage("ggplot2")

# Read csv file 
o_data <- read.csv("/home/zero/Desktop/Intro to Data Science/Midterm/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
# Make copy of data
myData <- o_data

# Munging the Data
myData=myData[,1:8]
myData=myData[,-4:-7]
myData=myData[myData$state != "RMI",]
myData=myData[myData$state != "NYC",]
myData=myData[myData$state != "PR",]
myData=myData[myData$state != "AS",]
myData=myData[myData$state != "GU",]
myData=myData[myData$state != "FSM",]
myData=myData[myData$state != "PW",]
myData=myData[myData$state != "MP",]

myData=myData[order(myData$state),]

# Find Monthly Totals
myData$submission_date = as.Date(myData$submission_date, "%m/%d/%y")
myData_monthly = myData %>%
  select(submission_date, state, tot_cases, tot_death) %>%
  filter(submission_date == as.Date("2020-01-31") 
         | submission_date == as.Date("2020-02-29")
         | submission_date == as.Date("2020-03-31") 
         | submission_date == as.Date("2020-04-30")
         | submission_date == as.Date("2020-05-31") 
         | submission_date == as.Date("2020-06-30") 
         | submission_date == as.Date("2020-07-31") 
         | submission_date == as.Date("2020-08-31")
         | submission_date == as.Date("2020-09-30") 
         | submission_date == as.Date("2020-10-31") )

# Get information for last day of each month
january <- myData_monthly[myData_monthly$submission_date == "2020-01-31",]
february <- myData_monthly[myData_monthly$submission_date == "2020-02-29",]
march <- myData_monthly[myData_monthly$submission_date == "2020-03-31",]
april <- myData_monthly[myData_monthly$submission_date == "2020-04-30",]
may <- myData_monthly[myData_monthly$submission_date == "2020-05-31",]
june <- myData_monthly[myData_monthly$submission_date == "2020-06-30",]
july <- myData_monthly[myData_monthly$submission_date == "2020-07-31",]
august <- myData_monthly[myData_monthly$submission_date == "2020-08-31",]
september <- myData_monthly[myData_monthly$submission_date == "2020-09-30",]

################################################################################

# Plot January Heatmap of cases
plot_usmap(data = january, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma, limits = c(0,12)) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot Feb Heatmap of cases
plot_usmap(data = february, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma, limits = c(0, 12)) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot March Heatmap of cases
plot_usmap(data = march, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot April Heatmap of cases
plot_usmap(data = april, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma, limits = c(0, 200000)) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# # Plot May Heatmap of cases
plot_usmap(data = may, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma, limits = c(0, 200000)) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot June Heatmap of cases
plot_usmap(data = june, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot July Heatmap of cases
plot_usmap(data = july, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma, limits = c(0, 500000)) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot August Heatmap of cases
plot_usmap(data = august, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot Semptember Heatmap of cases
plot_usmap(data = september, values="tot_cases", color="black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Total Cases", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

################################################################################

# Plot January Heatmap of deaths
plot_usmap(data = january, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot Feb heatmap of deaths
plot_usmap(data = february, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot March heatmap of deaths
plot_usmap(data = march, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot April heatmap of deaths
plot_usmap(data = april, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot May heatmap of deaths
plot_usmap(data = may, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot June heatmap of deaths
plot_usmap(data = june, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot July heatmap of deaths
plot_usmap(data = july, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot August heatmap of deaths
plot_usmap(data = august, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot Sept heatmap of deaths
plot_usmap(data = september, values="tot_death", color="black") +
  scale_fill_gradient2(low = "white", high = "black", name = "Total Deaths", label = scales::comma) +
  theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"))

################################################################################

# Create another copy of the data ordered by submission date
myData_date=myData[order(myData$submission_date),]

# Munge the data
myData_date=myData_date[,-2]
myData_date=aggregate(. ~ submission_date, myData_date, sum)

# Create a line graph
lp = ggplot(data = myData_date, aes(x = submission_date)) +
  geom_line(aes(y = tot_cases), color = "darkred", size=1.5) +
  geom_line(aes(y = tot_death), color = "black", size=1.5)

lp = lp + ggtitle("Cases and Deaths due to Covid-19 in the US")
lp = lp + labs(x="Date", y="Number of Persons")
lp

# Create a bar chart of most recent month information
g <- ggplot(september, aes(x=reorder(state, tot_cases), y= tot_cases, group=1))
g <- g + geom_col()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g
