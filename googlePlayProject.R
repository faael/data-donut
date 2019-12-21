View(googleplaystore)
googleplay <- googleplaystore
head(googleplay)
str(googleplay)
summary(googleplay)

library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)


googleplay<- na.omit(googleplay) #omit any missing values

distinct <- nrow(googleplay %>% 
                   distinct()) #scan for duplicates
nrow(googleplay) - distinct

googleplay=googleplay[!duplicated(googleplay), ] #omit duplicates



category = googleplay %>% group_by(Category) %>% select(c(App, Category, Rating))

category = as.data.frame(category)
str(category$Rating)
table(category$Rating)

types<- googleplay %>% group_by(Type) %>% select(c(Rating,Type, Installs, Category, Price))
types=as.data.frame(types)

table(googleplay$Type)

ggplot(types, aes(x=types$Type, y=types$Installs, fill=types$Type))+geom_bar(stat="identity")+labs(x="Type",y="Installs",fill="Types",title="Installation based on Type of Apps")
ggplot(types, aes(x=types$Category, y=types$Installs, fill=types$Category))+ geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Installations based on the Category of Apps",x="Categories",y="Installs",fill="Categories")

# Table containing the sample size, mean, and standard deviation for each category
tapply(googleplay$Rating, googleplay$Category, length)
tapply(googleplay$Rating, googleplay$Category, mean)
tapply(googleplay$Rating, googleplay$Category, sd)


#create a histogram with ratings 

ggplot(googleplay, aes(x = Rating)) + 
  geom_histogram()

googleplay <- googleplay %>% mutate(Price = na.omit(as.double(gsub("\\$", "", Price))))
summary(googleplay$Price)
ggplot(googleplay, aes(x = Price)) + 
  geom_histogram()
#not much variation in price as we can see that the majority of the apps are free from
#our sum of the table of google play type


library(cluster)
top_category = googleplay %>% group_by(Category) %>% 
  summarise (n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate (per = round(cumsum(n)/sum(n),2)) %>%
  filter(per<0.7)
top_category %>% head



contingency_table = googleplay %>% 
  filter(Category == top_category$Category) %>% 
  select(Category, Content.Rating) %>%
  mutate(n=1) %>%
  group_by(Category, Content.Rating) %>%
  summarise(sum = sum(n)) %>%
  spread(Content.Rating, sum)


dplyr_if_else      <- function(x) { mutate_all(x, funs(if_else(is.na(.), 0, .))) }
contingency_table = dplyr_if_else (contingency_table)

contingency_table = data.frame(contingency_table)
rownames(contingency_table) = contingency_table$Category
contingency_table = contingency_table[,-1]
head(contingency_table)




library(factoextra)
set.seed(123)
#k-means clustering
km.res <- kmeans(contingency_table, 3, 25)
#Visualize kmeans clustering
fviz_cluster(km.res, data = contingency_table, palete=c("#2E9FDF","#00AFBB"), 
             ellipse.type="euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal(),
             main="App Category and Content Rating")

#show size of each cluster 
km.res$size
