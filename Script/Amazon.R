library("tidyverse")
library("pacman")
library("doParallel") # for cores
library("caret")      # for models
library("plotly")


# Reading the data
iphone <- read.csv ("/../Users/ivanpop/Documents/DATA ANALYST/Github/4.0-AWS setiment/Data/raw/1_iphone_smallmatrix_labeled_8d.csv", 
                    stringsAsFactors = TRUE, sep= ";")

# Using doParallel to use the cores
detectCores()                # Find how many cores are on your machine
num_cor <- makeCluster(2)    # Create Cluster with desired number of cores.
registerDoParallel(num_cor)  # Register Cluster
getDoParWorkers()            # Confirm how many cores are now "assigned" to R and RStudio
stopCluster(num_cor)         # Stop Cluster. After performing your tasks, stop your cluster. 

# Correlation matrix
cor.iphone<- cor(iphone)
corrplot::corrplot(cor.iphone)

# Converting to numeric all atributes to plot
iphone_2 <- iphone

for (x in 1:ncol(iphone_2)){
  iphone_2[,x] <- as.numeric(iphone_2[,x])
}

plot_iphone <-iphone_2[,c(1:5)]
plot_iphone <- gather(plot_iphone, "brand", "values", 1:5)

# Plotting DENSITY of each brand
iphone_density <- plot_iphone %>%
  ggplot(., aes(x = reorder(brand, values), fill = brand)) +
  geom_density(alpha = 0.7) + theme_bw() +
  theme(axis.text.x = element_text (angle = 45, hjust = 1), legend.position = c(0.89, 0.86)) +
  labs(title = "Brands: rating", subtitle = "Density") +
  xlab("RRSSI - Strength") + ylab("Density signals")

# Plotting PIECHART of each brand
iphone_piechart <- plot_iphone %>% plot_ly(., labels = ~brand, values = ~values, type = 'pie') %>%
  layout(title = 'Percentage of each brand',
         xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
         yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE))
iphone_piechart


rm(x, num_cor, plot_iphone, iphone_density, iphone_piechart)







iphone_2 <- sapply(iphone, function(x) if (x== 0, "Very Negative" 
                                           else x == 1, "Negative"
                                           else x == 2, "Somewha Negative"
                                           else x == 3, "SOmewhat Positive"
                                           else x == 4, "Positive"
                                           else x == 5, "Very Positive"))




# RUNNING MODEL

set.seed (1234)

KNN_training <- create


