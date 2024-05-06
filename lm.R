
data <- read.csv("C:\\Users\\juanj\\Documents\\Data literacy\\Final\\superbowl_commercials.csv")
data$Youtube.Views <- as.integer(data$Youtube.Views)

model <- lm(Year ~ Funny + Shows.Product.Quickly + Patriotic + Celebrity + Danger + Animals + Uses.Sex,
            data = data)

summary(model)




funny <- data[c("Youtube.Views", "Funny")]

funny <- na.omit (funny)

library(cluster)

kmeans_model <- kmeans (funny, centers = 5, nstart = 10)
print (kmeans_model)

funny$cluster <- kmeans_model$cluster
funn <- kmeans_model$centers[,"Funny"]

print(funn)

centroids <- aggregate(. ~cluster,funny, mean)

View (centroids)

library(plotly)

plot_scatter <- plot_ly(data = funny,
                        x = ~Funny,
                        y = ~Youtube.Views,
                        color = ~Youtube.Views,
                        type = 'scatter'
                        
)%>%
  layout(
    title = "K-means clustering",
    xaxis = list(title="Views"),
    yaxis = list(title="Funny")
  )%>%
  add_trace( data = centroids,
             x = ~ Funny,
             y = ~ Youtube.Views,
             mode = 'markers',
             marker = list(size=10, color= 'black', symbol = 'diamond'))


plot_scatter      

cluster_assignments <- kmeans_model$cluster
c <- data.frame (cluster_assignments)
c$ID <- 1:nrow(c)

funny$ID <- 1:nrow(funny)

funny$cluster <- NULL

merge_clusters <- merge(c,funny, by = "ID")

list_of_dataframes <- split(merge_clusters,merge_clusters$cluster_assignments)



df <- list_of_dataframes[[4]]

correlation1 <- cor(df$Funny,df$Youtube.Views)
correlation1
summary(model_2)
coefficients <- coef(model)
intercept_value <- coefficients[1]
print(coefficients)
print(intercept_value)

df$ID <- 1:nrow(df)
df <- df%>% relocate(ID, .before = Year)




library(ggplot2)
library(dplyr)

