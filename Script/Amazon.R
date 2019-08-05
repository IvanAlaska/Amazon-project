library("tidyverse")
library("pacman")
library("doParallel") # for cores
library("caret")      # for models
library("plotly")
library("e1071")      # for SVM model
library("kknn")       # for kknn model


# Reading the data
iphone <- read.csv ("/../Users/ivanpop/Documents/DATA ANALYST/Github/Amazon/Amazon project/Data/data raw/1_iphone_smallmatrix_labeled_8d.csv", 
                    stringsAsFactors = TRUE, sep= ";")

galaxy <- read.csv ("/../Users/ivanpop/Documents/DATA ANALYST/Github/Amazon/Amazon project/Data/data raw/2_galaxy_smallmatrix_labeled_8d.csv", 
                    stringsAsFactors = TRUE, sep= ";")

# Converting to numeric all atributes to plot
iphone_2 <- iphone

# for (x in 1:ncol(iphone_2)){
#   iphone_2[,x] <- as.numeric(iphone_2[,x])
# }


-------------------------------------------------------------------------------------------------------

# Creating categorical value
iphone_2$value <- sapply(iphone_2$iphonesentiment, function(x) 
  if (x == 0) "Very Negative"
  else if (x == 1) "Negative"
  else if (x == 2) "Somewha Negative"
  else if (x == 3) "Somewhat Positive"
  else if (x == 4) "Positive"
  else if (x == 5) "Very Positive")

#Plotting Iphone sentiment
plot.ly_iphone <- iphone_2 %>% plot_ly(., x= ~iphone_2$value, vatype='histogram') %>% 
  layout(title="Sentiment: IPHONE", 
         xaxis = list(title = 'Perception'), 
         yaxis = list(title = 'Count'))

plot.ly_iphone

# Plotting only brands
plot_iphone <-iphone_2[,c(1:5)]
plot_iphone <- gather(plot_iphone, "brand", "values", 1:5)

# Plotting DENSITY of each brand
iphone_density <- plot_iphone %>%
  ggplot(., aes(x = reorder(brand, values), fill = brand)) +
  geom_density(alpha = 0.7) + theme_bw() +
  theme(axis.text.x = element_text (angle = 45, hjust = 1), legend.position = c(0.89, 0.86)) +
  labs(title = "Brands: rating", subtitle = "Density") +
  xlab("RRSSI - Strength") + ylab("Density signals")

iphone_density

# Plotting PIECHART of each brand
iphone_piechart <- plot_iphone %>% plot_ly(., labels = ~brand, values = ~values, type = 'pie') %>%
  layout(title = 'Percentage of each brand',
         xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
         yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE))

iphone_piechart

-------------------------------------------------------------------------------------------------------

# Creating categeorical column
iphone_2 <- iphone

options(max.print=1000000)       # help to plot huge plots

# Correlation matrix
cor.iphone_2 <- cor(iphone_2)
corrplot::corrplot(cor.iphone_2)

rm(x, plot_iphone, cor.iphone_2, iphone_piechart,galaxy)

iphone_2$ios <- NULL       # High correlation variable

# Checking low variance
iphone_2_variance <- nearZeroVar(iphone_2, saveMetrics = TRUE) # SaveMetrics = TRUE returns an object
view(iphone_2_variance)
which(iphone_2_variance$zeroVar == 'TRUE')  # Check how many Variance is 0

# Lets create an index for Near Zero Variance
iphone_2_nearZero <- nearZeroVar(iphone_2, saveMetrics = FALSE)
view(iphone_2_nearZero)

# create a new data set and remove near zero variance features
iphoneNZV <- iphone_2[,-iphone_2_nearZero] 
str(iphoneNZV)

-------------------------------------------------------------------------------------------------------

####################################### FEATURE SELECTION #################################################

# Using doParallel to use the cores
detectCores()                # Find how many cores are on your machine
num_cor <- makeCluster(4)    # Create Cluster with desired number of cores.
registerDoParallel(num_cor)  # Register Cluster
getDoParWorkers()            # Confirm how many cores are now "assigned" to R and RStudio

# Let's sample the data before using RFE
set.seed (123)

iphoneSample <- iphone_2[sample(1:nrow(iphone_2), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)
# Get results
rfeResults

stopCluster(num_cor)         # Stop Cluster. After performing your tasks, stop your cluster. 

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
iphoneRFE <- as.data.frame(iphone_2[,predictors(rfeResults)])

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphone_2$iphonesentiment

# review outcome
str(iphoneRFE)

####################################### PRE PROCESSING #################################################









