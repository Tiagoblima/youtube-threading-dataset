
memory.limit (9999999999)
library(tidyr)
library(dplyr)



process_df <- function(df, country_flag){
  
  df <- df %>% filter(video_error_or_removed=="False") %>% group_by(video_id) %>% summarise(
    views = sum(views, na.rm = T),
    likes = sum(likes, na.rm = T),
    dislikes = sum(dislikes, na.rm = T),
    title = unique(title),
    category_id=unique(category_id),
    tags = tags[1],
    comment_count = sum(comment_count, na.rm = T),
    channel_title = channel_title[1]
  ) 
  df$country <-sapply(c(country_flag), function (x) rep(x, nrow(df)))
  return(df)
  
}

DEvideos <- read.csv('datasets/DEvideos.csv')
DEvideos <- process_df(DEvideos, "DE")
nrow(DEvideos)

CAvideos <- read.csv('datasets/CAvideos.csv')
CAvideos <- process_df(CAvideos, "CA")
nrow(CAvideos)


FRvideos <- read.csv('datasets/FRvideos.csv')
FRvideos <- process_df(FRvideos, "FR")
nrow(FRvideos)

GBvideos <- read.csv('datasets/GBvideos.csv')
GBvideos <- process_df(GBvideos, "GB")
nrow(GBvideos)

USvideos <- read.csv('datasets/USvideos.csv')
USvideos <- process_df(USvideos, "US")
nrow(USvideos)



multiFull <- merge(merge(merge(merge(
  DEvideos,
  USvideos, all = TRUE),
  GBvideos, all = TRUE),
  FRvideos, all = TRUE),
  CAvideos, all = TRUE)

write.csv(multiFull,"datasets/youtube_data.csv", row.names = FALSE)
colnames(multiFull)
View(multiFull)
unique(multiFull$country)
write.csv(multiFull,"datasets/youtube_data.csv", row.names = FALSE)


countries <- c("FR", "CA", "GB", "US", "DE")

View(youtube_data)

youtube_data <- read.csv("youtube_data.csv")

youtube_data <- youtube_data[order(youtube_data$views, 
                                   youtube_data$likes,
                                   youtube_data$comment_count, 
                                   decreasing = T),]


head(youtube_data$title)
head(youtube_data$country)
head(youtube_data[, c("title")])

tail(youtube_data$country)

likeprob <- youtube_data$likes/(youtube_data$likes+youtube_data$dislikes)

tail(likeprob)
head(likeprob[order(likeprob, decreasing = T)])

youtube_data <- youtube_data[order(likeprob, decreasing = T),]

head(youtube_data$title)

head(youtube_data$country)

youtube_data[youtube_data$video_id=="FM_7ZyE0ib4", "likes"]

youtube_data %>% group_by(country) %>% summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes)
) %>% filter(likes==max(likes))


youtube_data %>% group_by(country) %>% summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes)
) %>% filter(likes==max(likes))

youtube_data %>% group_by(country) %>% summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes)
) %>% filter(dislikes==max(dislikes))

youtube_data %>% group_by(country) %>% summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes)
) %>% filter(comment_count==max(comment_count))

youtube_data %>% group_by(country) %>% summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes)
) %>% filter(likes==max(likes) &
               comment_count==max(comment_count) &
               dislikes==max(dislikes))

by_category <- youtube_data %>% group_by(category_id) %>% summarise(
  
              views= sum(views),
              comment_count=sum(comment_count),
              dislikes=sum(dislikes),
              likes=sum(likes)
            )


by_category <- by_category[order(by_category$views,
                                 by_category$likes,
                                 by_category$comment_count,
                                 decreasing = T),]
by_category
unique(youtube_data$country)




by_channel <- youtube_data %>%
              group_by(category_id) %>% 
              filter(likes==max(likes)) %>% 
              select(title, channel_title, country)


by_channel <- by_channel[order(by_channel$views,
                                by_channel$likes,
                                by_channel$comment_count,
                                 decreasing = T),]




unique(youtube_data$country)


summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes)
  
)  
by_category <- youtube_data %>% 
              group_by(category_id)%>% filter(likes==max(likes))
              
              
             
by_category <- by_category[, c("views","likes", "dislikes", "title",
                               "category_id",
                               "channel_title")]  
by_category <- by_category[order(by_category$likes, decreasing = T),]



categories <- by_category$category_id

X <- by_category$views/sum(by_category$views)



barplot(X,  
        
        xlab = "Categories",
        ylab="Number of views",
        names.arg=categories)





youtube_data <- read.csv("https://github.com/Tiagoblima/youtube-threading-dataset/raw/master/youtube_data.csv")


youtube_data <- youtube_data %>% slice(1:10000)

write.csv(youtube_data,"youtube_data.csv", row.names = FALSE)


countryInteractions <- youtube_data %>% group_by(country) %>% summarise(
  
  views= sum(views),
  comment_count=sum(comment_count),
  dislikes=sum(dislikes),
  likes=sum(likes),
  inter=sum(likes)+sum(dislikes)+sum(comment_count)+sum(views),
)

per <- round(countryInteractions$inter*100/sum(countryInteractions$inter))

install.packages("rjson")

library(rjson)
install.packages("jsonlite")
library(jsonlite)

result <- fromJSON("US_category_id.json", flatten = TRUE)

write.csv(result$items, "categories.csv")

categories_names <- read.csv("categories.csv")



youtube_data <- youtube_data[youtube_data$country=="GB",]

youtube_data[youtube_data$category_id==29, "title"]


by_category <- youtube_data %>% 
  group_by(category_id) %>%
  
  summarise(
    
    views= sum(views),
   
  )  

by_category <- by_category[order(by_category$likes, decreasing = T),]

categories <- by_category$category_id

by_category$views <- by_category$views/sum(by_category$views)


titles <- c()


for (cat in categories){
  
  titles <- append(titles, categories_names[categories_names$id==cat, "snippet.title"])
  
}
par(mar=c(5,5,5,0)) ##This margin command should do the trick






barplot(X,  
        horiz = TRUE,
        srt = 45,
        xlab = "Number of views",
        names.arg=titles,
        col=rainbow(length(titles)), 
        cex.names=0.5
        ,las=2)



ccf(x = df$C, y = df$P,lag.max = 10, main="foo") 







library(tm)
library(wordcloud)
library(readr)


by_channel <- youtube_data %>%
  group_by(category_id) %>% 
  select(category_id, tags)


text <- paste(strsplit(paste(by_channel[by_channel$category_id==10, ], sep = "|"), "\\|"))


corpus <- Corpus(VectorSource(text))

corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords('portuguese'))



inspect(corpus)


wordcloud(corpus, 
          scale=c(3.3,0.5), 
          max.words=100, 
          min.freq=1, 
          random.order=FALSE, 
          rot.per=0.40, 
          
          colors=brewer.pal(8,"Dark2"))
