#Chapter 14 R Code
# Load packages
library(cluster)
library(animation)
library(readxl)

# Set working directory
setwd("C:/Users/joruongo/Documents/ITM 466/Week 11/")

# Load the Cities data
myData <- read_excel("jaggia_ba_2e_ch14_data.xlsx", sheet = "Subscribers")
km <- kmeans.ani(myData, centers=5)
myData <- data.frame(Cluster = km$cluster, myData)

# Review the three clusters to see if there is evidence for clusters.
# Distribution for Salary: Histogram and boxplot
ggplot(myData, aes(Salary)) + geom_histogram() + ggtitle("Distribution of Salary")
ggplot(myData, aes(Salary)) + geom_boxplot() + ggtitle("Distribution of Salary")

# Check the distribution of Crime by Cluster
ggplot(myData, aes(Salary, fill = Cluster)) + geom_histogram() + 
  facet_grid(Cluster~.) + ggtitle("Distribution of Salary by Cluster")
ggplot(myData, aes(Salary, group = Cluster, fill = Cluster)) + geom_boxplot() + 
  ggtitle("Distribution of Salary by Cluster")


# Association Rules
#Example 14.5
library(arules)
library(arulesViz)
#Insert the path to your .csv file in the statement below.
myData <- read.transactions("Transaction.csv", format = "basket", sep = ",")

# We can look at the first five transactions.
inspect(myData[1:5])

# This calculates the frequency of every value in each transaction
itemFrequency(myData)

# Then we can plot the distribution of the different values in each transaction.
itemFrequencyPlot(myData)

# Conduct the association rules analysis. This can be done using the apriori function as shown below.
rules <- apriori(myData, parameter = list(minlen = 2, supp = 0.1, conf = 0.5))

# Sort the rules derived above by lift. That is, by the most impactful rules.
srules <- sort(rules, by = "lift", decreasing = TRUE)
inspect(srules)

# Finally, we plot the rules to see the distribution. This helps us identify the rules with 
# the highest lift ratios. 
plot(rules)

