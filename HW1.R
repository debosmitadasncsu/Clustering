
library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(data.table)
library(ggrepel)
library(lattice)
library(udpipe)
library(sjmisc)
library(base)

setwd("C:/Users/Debosmita/Documents/ClusteringHW1")


listing <- read_csv("listings.csv")

#######################################################
#Things to look for
#currently available?
#qualities of good review - listing has some scoring too
# local tourist attraction
#contrast number of reviews with tourist attractions
#locate properties with less quality rental sites
#######################################################

# change formatting so that R doesn't exponentiate large values
listing$scrape_id = format(listing$scrape_id, scientific = FALSE)



unique(listing$experiences_offered)
unique(listing$country_code)
unique(listing$country)

# returned 0
listing[listing$host_listings_count < listing$host_total_listings_count,]

# drop unnecessary columns
listing$thumbnail_url = NULL
listing$xl_picture_url = NULL
listing$experiences_offered = NULL
listing$host_thumbnail_url = NULL
listing$medium_url = NULL
listing$picture_url = NULL
listing$listing_url = NULL
listing$host_url = NULL
listing$host_listings_count = NULL # since listing[listing$host_listings_count < listing$host_total_listings_count,] returned 0 rows
listing$host_picture_url = NULL #same as host_has_profile_pic 
listing$neighbourhood = NULL
listing$country_code = NULL
listing$country = NULL
listing$requires_license = NULL
listing$license = NULL
listing$jurisdiction_names = NULL
listing$state = NULL
listing$requires_license = NULL # doesn't affect home investment
listing$last_scraped = NULL # doesn't affect home investment
listing$has_availability = NULL # doesn't affect home investment

#######################################################
#Review Analysis
#######################################################

reviews <- read_csv("reviews.csv")

rv <- reviews %>% group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>% filter( n >= 4) %>% select(-"n") 

#break up the dataset's listing into individual words
#join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
new_reviews <- reviews %>%
  group_by(listing_id) %>%
  unnest_tokens(word, comments)  %>%
  right_join(rv,by="listing_id") %>% filter(!is.na(word))

#####################################################
#find the sentiments that are positive and negative
nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")
#####################################################

reviews_negative <-  new_reviews %>%
  group_by(listing_id) %>%
  inner_join(nrc_negative) %>% # join and count the negative words from the 
  count(word, sort = TRUE) %>% # review DB
  arrange(desc(listing_id)) 

reviews_positive <-  new_reviews %>%
  group_by(listing_id) %>%
  inner_join(nrc_positive) %>% # join and count the positive words from the 
  count(word, sort = TRUE) %>% # review DB
  arrange(desc(listing_id)) 
reviews_negative[,'sentiment'] = as.factor('negative')
reviews_positive[,'sentiment'] = as.factor('positive')

reviews_sentiment = rbind(reviews_negative, reviews_positive)

reviews_sentiment = reviews_sentiment[order(reviews_sentiment$listing_id),]

reviews_sentiment_count=NULL

###############################################################################
# Checking how many negative and positive sentiments are there per listing_id #
###############################################################################

reviews_sentiment_count=NULL
reviews_sentiment_count=aggregate(reviews_sentiment$n, by=list(listing_id=reviews_sentiment$listing_id,sentiment=reviews_sentiment$sentiment), FUN=sum)

reviews_sentiment_count = reviews_sentiment_count[order(reviews_sentiment_count$listing_id),]

for (row in 1: nrow(reviews_sentiment_count))
{
  if(row == 1 & reviews_sentiment_count[row, "listing_id"]==reviews_sentiment_count[row+1, "listing_id"])
  {
    total <- reviews_sentiment_count[row, "x"] + reviews_sentiment_count[row+1, "x"]
    reviews_sentiment_count[row, "output"] <- total
    reviews_sentiment_count[row, "Percentage"] = reviews_sentiment_count[row, "x"]/total
  }
  else
  {
    break
  }
}

for (row in 2: nrow(reviews_sentiment_count))
{
  if(row > 1 & reviews_sentiment_count[row, "listing_id"]==reviews_sentiment_count[row-1, "listing_id"])
  {
    total <- reviews_sentiment_count[row, "x"] + reviews_sentiment_count[row-1, "x"]
    reviews_sentiment_count[row, "output"] <- total
    reviews_sentiment_count[row, "Percentage"] = reviews_sentiment_count[row, "x"]/total
  }
  if(row > 1 & reviews_sentiment_count[row, "listing_id"]==reviews_sentiment_count[row+1, "listing_id"])
  {
    total <- reviews_sentiment_count[row, "x"] + reviews_sentiment_count[row+1, "x"]
    reviews_sentiment_count[row, "output"] <- total
    reviews_sentiment_count[row, "Percentage"] = reviews_sentiment_count[row, "x"]/total
  }
}

for (row in 2: nrow(reviews_sentiment_count))
{
  if(is.na(reviews_sentiment_count[row,"output"]))
  {
    reviews_sentiment_count[row, "output"] = reviews_sentiment_count[row, "x"]
    if(reviews_sentiment_count[row, "sentiment"] == "negative")
      reviews_sentiment_count[row, "Percentage"] = -(reviews_sentiment_count[row, "x"]/reviews_sentiment_count[row, "x"])
    else
    {
      reviews_sentiment_count[row, "Percentage"] = reviews_sentiment_count[row, "x"]/reviews_sentiment_count[row, "x"]
    }
  }
}



#######################################################
#Trying to find local attractions
#######################################################

listing$neighborhood_overview = tolower(listing$neighborhood_overview)

# tokenizing and removing stopwords from neighborhood overview
clean_listing <- listing %>%
  unnest_tokens(word, neighborhood_overview) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word) %>% filter(!is.na(word))



df = select(listing, id, neighborhood_overview, latitude, longitude, property_type, bathrooms, host_id, bedrooms, price)


ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# tokenizing and removing stopwords from  for each listing id
listing_overview <- df %>%
  unnest_tokens(word, neighborhood_overview) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word) %>% filter(!is.na(word))

# picking out top local attractions that most owners has used

listing_overview$word <- as.character(listing_overview$word)
listing_overview$word <- as.character(listing_overview$word)
x <- udpipe_annotate(ud_model, x = listing_overview$word, doc_id = listing_overview$id)
x <- as.data.frame(x)



# using only part of speech - one word at a time
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))

# bar chart shows most occuring nouns
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")



# trying combinations of words

stats_rake <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                            relevant = x$upos %in% c("NOUN", "ADJ"))

#### Graph showing top nouns
stats_rake$key <- factor(stats_rake$keyword, levels = rev(stats_rake$keyword))
barchart(key ~ rake, data = head(subset(stats_rake, freq > 3), 30), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

write.csv(stats_rake, file="local_attraction_keyword.csv")

##################### Find ids which have these top used words associated with them in neighborhood overview##########

####### pulling out top poperties with word combos##############


stats_rake = stats_rake[order(-stats_rake$rake),]

## Choosing top 345 keywords
key_rake = as.vector(stats_rake$key[1:345])

# for cluster 4
colnames(listing)[colnames(listing)=="id"] <- "listing_id"
clu4_details = listing %>%
  inner_join(clu4, by="listing_id")


top_listings_rake4 = clu4_details[FALSE,]

nonna = clu4_details[complete.cases(clu4_details$price,clu4_details$neighborhood_overview,clu4_details$room_type),]
for(row in 1:nrow(clu4_details))
{
  if(any(str_contains(clu4_details[row,"neighborhood_overview"], key_rake)))
  {
    print("match")
    temp=data.frame(clu4_details[row,])
    top_listings_rake4 = rbind(top_listings_rake4, temp)
  }
}


# for cluster 3

clu3_details = listing %>%
  inner_join(clu3, by="listing_id")


top_listings_rake3 = clu3_details[FALSE,]

nonna = clu3_details[complete.cases(clu3_details$price,clu3_details$neighborhood_overview,clu3_details$room_type),]
for(row in 1:nrow(clu3_details))
{
  if(any(str_contains(clu3_details[row,"neighborhood_overview"], key_rake)))
  {
    print("match")
    temp=data.frame(clu3_details[row,])
    top_listings_rake3 = rbind(top_listings_rake3, temp)
  }
}



####################### Finding lat and lon with each street addresses ###################################

register_google(key="AIzaSyDv2lW3sgvYjoAbeJhvqHtpNFjwJ5RbRjw")

# uncomment to run
# retrieves the lat and lon of each listing

strtAddress <- listing$street
count <- 0
lon<- matrix(0,nrow=length(strtAddress))
lat<- matrix(0,nrow=length(strtAddress))
for (ii in 1:length(strtAddress)){
  latLon <- geocode(strtAddress[ii],output="latlon")
  lon[ii] <- as.numeric(latLon[1])
  lat[ii] <- as.numeric(latLon[2])
  count <- count + 1
  print(count)
}
listing_k <-data.frame(listing_id = listing$id, lat = lat, lon = lon)

#########################################
# Get a Map of Boston
#########################################
map <- get_map(location = "Boston", zoom = 12)
map2 <- get_map(location = "Boston", zoom = 11)

#I want to find the Geo-location of these listings :-)
# 
# cluster <- words_and_clusters %>% filter(group == 9) %>%
#   left_join(listing_k,"listing_id") %>%
#   distinct(listing_id,.keep_all = TRUE)
# 
# #notice U of Washington Right in the middle
# ggmap(map, fullpage = TRUE) +
#   geom_point(data = cluster, aes(x = lon, y = lat), color = 'red', size = 2)
# 
# ggmap(map2, fullpage = TRUE) +
#   geom_point(data = cluster, aes(x = lon, y = lat), color = 'red', size = 2)



######################## Finding clusters #############################
nrc_total <- get_sentiments("afinn")

#Load the dataset and group the 
#reviews by the listing id 
#keep all listings having more than 4 reviews 
#reviews <- read_csv("reviews.csv")

rv <- reviews %>% group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>% filter( n >= 4) %>% select(-"n")

#break up the dataset's listing into individual words
#join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
new_reviews <- reviews %>%
  group_by(listing_id) %>%
  unnest_tokens(word, comments)  %>%
  right_join(rv,by="listing_id") %>% filter(!is.na(word)) %>%
  left_join(nrc_total,by="word") %>% filter(!is.na(score))

#Find the number of words scored
score         <- new_reviews %>% group_by(listing_id) %>% mutate(sscore = sum(score)) %>% distinct(listing_id,sscore)
nwords        <- new_reviews %>% group_by(listing_id) %>% count(listing_id) 

complete <- nwords %>% left_join(score,"listing_id") %>% mutate(avg = sscore/n)

complete$avg <- scale(complete$avg) #standardize the score

combined <- complete %>% left_join(listing_k,"listing_id")
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

toC<- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toC),method="complete")
clusters.s <- hclust(dist(toC), method="single")
clusters.s <- hclust(dist(toC), method="average")

plot(clusters.c)
identify(clusters.c)
combined$clus <- cutree(clusters.c,8) #it looks like 7 clusters is reasonable

library(ggmap)
clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)
clu7 <- combined %>% filter(clus == 7)
clu8 <- combined %>% filter(clus == 8)

################################
# plot interesting clusters
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu4, aes(x = lon, y = lat), color = 'red', size = 2)

#mapping top listings
ggmap(map, fullpage = TRUE) +
  geom_point(data = top_listings_rake4, aes(x = top_listings_rake4$lon, y = top_listings_rake4$lat), color = 'red', size = 2)


ggmap(map, fullpage = TRUE) +
  geom_point(data = clu3, aes(x = lon, y = lat), color = 'red', size = 2)


#mapping top listings
ggmap(map, fullpage = TRUE) +
  geom_point(data = top_listings_rake3, aes(x = top_listings_rake3$lon, y = top_listings_rake3$lat), color = 'red', size = 2)


## Summary stats of top listings of 4
print("Mean AirBnB price")
mean(as.numeric(gsub("\\$", "", top_listings_rake4$price)), na.rm = TRUE)
print("Mean AirBnB bedroom number")
mean(top_listings_rake4$bedrooms)
print("Mean AirBnB bathroom number")
mean(top_listings_rake4$bathrooms)
print("Number of top AirBnB's per neighborhood")
as.data.frame(table(top_listings_rake4$neighbourhood_cleansed))

## Summary stats of top listings of 3
print("Mean AirBnB price")
mean(as.numeric(gsub("\\$", "", top_listings_rake3$price)), na.rm = TRUE)
print("Mean AirBnB bedroom number")
mean(top_listings_rake3$bedrooms)
print("Mean AirBnB bathroom number")
mean(top_listings_rake3$bathrooms, na.rm = TRUE)
print("Number of top AirBnB's per neighborhood")
as.data.frame(table(top_listings_rake3$neighbourhood_cleansed))


################### Renter sentiment towards cluster 3 and 4 #########################

sentiment_4 <- reviews_sentiment_count %>%
                inner_join(clu4_details, by="listing_id")

postive_clus4 = sentiment_4[sentiment_4$Percentage>0.8 & sentiment_4$sentiment=="positive",]#30 = 263/865
negative_clus4 = sentiment_4[sentiment_4$Percentage>0.3 & sentiment_4$sentiment=="negative",]


sentiment_3 <- reviews_sentiment_count %>%
  inner_join(clu3_details, by="listing_id")

postive_clus3 = sentiment_3[sentiment_3$Percentage>0.8 & sentiment_3$sentiment=="positive",] #179/445 = 40
negative_clus3 = sentiment_3[sentiment_3$Percentage>0.3 & sentiment_3$sentiment=="negative",]

reviews_sentiment_count_pos <- sentiment_4[sentiment_4$sentiment=="positive",]
reviews_sentiment_count_neg <- sentiment_4[sentiment_4$sentiment=="negative",]

renter_pos = aggregate(reviews_sentiment_count_pos$Percentage, by=list(Category=reviews_sentiment_count_pos$listing_id), FUN=sum)


############################### Word Cloud ################################################
reviews_pos_clus_4 <- reviews_positive %>% inner_join(clu4_details, by="listing_id")
reviews_neg_clus_4 <- reviews_negative %>% inner_join(clu4_details, by="listing_id")

pos_words4 <- sort(unique(reviews_pos_clus_4$word))
neg_words4 <- sort(unique(reviews_neg_clus_4$word))

new_reviews4 <- subset(new_reviews, listing_id %in% clu4_details$listing_id)

negative4 <- data.frame(matrix(0,nrow=length(unique(new_reviews4$listing_id)),ncol = length(unique(reviews_neg_clus_4$word))))
positive4 <- data.frame(matrix(0,nrow=length(unique(new_reviews4$listing_id)),ncol = length(unique(reviews_pos_clus_4$word))))

########################################################
# So we know what each column means we name the column by the word it represents
#########################################################
names(negative4) <- neg_words4
names(positive4) <- pos_words4


#create my bag of words matrices
lsid <- unique(new_reviews4$listing_id)
# asign the number of positive words found
for (ii in 1:nrow(reviews_pos_clus_4)){
  x <- which(pos_words == reviews_pos_clus_4$word[ii])
  y <- which(lsid      == reviews_pos_clus_4$listing_id[ii])
  positive4[y,x] = reviews_pos_clus_4$n.x[ii]
}

# asign the number of negative words found
for (ii in 1:nrow(reviews_neg_clus_4)){
  x <- which(neg_words == reviews_neg_clus_4$word[ii])
  y <- which(lsid      == reviews_neg_clus_4$listing_id[ii])
  negative4[y,x] = reviews_neg_clus_4$n.x[ii]
}


#######################################################
# We have essentially created a "BAG" of words. 
# Each line is the unique listing, and we have a count of the number
# of times a word is used, we have a negative list and a positive list
#######################################################

#COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
library(Matrix)
SP.COS <- 1-sim2(Matrix(as.matrix(positive4)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(positive4)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations


POSITIVE_CLUSTERS_COS4  <- hclust(as.dist(B))  # CLUSTER as.dist(B) convert it to a 'distance'
POSITIVE_CLUSTERS_JAC4  <- hclust(as.dist(C))  # because again hclust is stupid


#####################################################
#Let's just pick 10 clusters and see what happens
#When we build a word cloud
#####################################################
POSITIVE_GROUPS4 <- cutree(POSITIVE_CLUSTERS_COS4,10)
listing_group4 <- as.data.frame(cbind(POSITIVE_GROUPS4,lsid))
names(listing_group4) <- c("group","listing_id")

words_and_clusters4 <- reviews_pos_clus_4 %>% right_join(listing_group4,'listing_id')


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1){
  temp <- words_and_clusters4 %>% group_by('word') %>% 
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(words = temp$word, freq = temp$n, min.freq = 500,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}



#######################################################
# Let's look at the negative words!!
#
#
#######################################################

#COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
library(Matrix)
SP.COS <- 1-sim2(Matrix(as.matrix(negative4)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(negative4)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

NEGATIVE_CLUSTERS_COS4  <- hclust(as.dist(B))  # CLUSTER as.dist(B) convert it to a 'distance'
NEGATIVE_CLUSTERS_JAC4  <- hclust(as.dist(C))  # because again hclust is stupid


#####################################################
#Let's just pick 10 clusters and see what happens
#When we build a word cloud
#####################################################
NEGATIVE_GROUPS4 <- cutree(NEGATIVE_CLUSTERS_COS4,10)
listing_group4 <- as.data.frame(cbind(NEGATIVE_GROUPS4,lsid))
names(listing_group4) <- c("group","listing_id")

words_and_clusters4_neg <- reviews_neg_clus_4 %>% right_join(listing_group4,'listing_id')


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1){
  temp <- words_and_clusters4_neg %>% group_by('word') %>% 
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(words = temp$word, freq = temp$n, min.freq = 500,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}


####################################Cluster 3#######################################


reviews_pos_clus_3 <- reviews_positive %>% inner_join(clu3_details, by="listing_id")
reviews_neg_clus_3 <- reviews_negative %>% inner_join(clu3_details, by="listing_id")

pos_words3 <- sort(unique(reviews_pos_clus_3$word))
neg_words3 <- sort(unique(reviews_neg_clus_3$word))

new_reviews3 <- subset(new_reviews, listing_id %in% clu3_details$listing_id)

negative3 <- data.frame(matrix(0,nrow=length(unique(new_reviews3$listing_id)),ncol = length(unique(reviews_neg_clus_3$word))))
positive3 <- data.frame(matrix(0,nrow=length(unique(new_reviews3$listing_id)),ncol = length(unique(reviews_pos_clus_3$word))))

########################################################
# So we know what each column means we name the column by the word it represents
#########################################################
names(negative3) <- neg_words3
names(positive3) <- pos_words3


#create my bag of words matrices
lsid <- unique(new_reviews3$listing_id)
# asign the number of positive words found
for (ii in 1:nrow(reviews_pos_clus_3)){
  x <- which(pos_words == reviews_pos_clus_3$word[ii])
  y <- which(lsid      == reviews_pos_clus_3$listing_id[ii])
  positive4[y,x] = reviews_pos_clus_3$n.x[ii]
}

# asign the number of negative words found
for (ii in 1:nrow(reviews_neg_clus_3)){
  x <- which(neg_words == reviews_neg_clus_3$word[ii])
  y <- which(lsid      == reviews_neg_clus_3$listing_id[ii])
  negative3[y,x] = reviews_neg_clus_3$n.x[ii]
}


#######################################################
# We have essentially created a "BAG" of words. 
# Each line is the unique listing, and we have a count of the number
# of times a word is used, we have a negative list and a positive list
#######################################################

#COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
library(Matrix)
SP.COS <- 1-sim2(Matrix(as.matrix(positive3)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(positive3)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations


POSITIVE_CLUSTERS_COS3 <- hclust(as.dist(B))  # CLUSTER as.dist(B) convert it to a 'distance'
POSITIVE_CLUSTERS_JAC3  <- hclust(as.dist(C))  # because again hclust is stupid


#####################################################
#Let's just pick 10 clusters and see what happens
#When we build a word cloud
#####################################################
POSITIVE_GROUPS3 <- cutree(POSITIVE_CLUSTERS_COS3,10)
listing_group3 <- as.data.frame(cbind(POSITIVE_GROUPS3,lsid))
names(listing_group3) <- c("group","listing_id")

words_and_clusters3 <- reviews_pos_clus_3 %>% right_join(listing_group3,'listing_id')


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1){
  temp <- words_and_clusters3 %>% group_by('word') %>% 
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(words = temp$word, freq = temp$n, min.freq = 500,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}



#######################################################
# Let's look at the negative words!!
#
#
#######################################################

#COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
library(Matrix)
SP.COS <- 1-sim2(Matrix(as.matrix(negative3)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(negative3)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

NEGATIVE_CLUSTERS_COS3  <- hclust(as.dist(B))  # CLUSTER as.dist(B) convert it to a 'distance'
NEGATIVE_CLUSTERS_JAC3  <- hclust(as.dist(C))  # because again hclust is stupid


#####################################################
#Let's just pick 10 clusters and see what happens
#When we build a word cloud
#####################################################
NEGATIVE_GROUPS3 <- cutree(NEGATIVE_CLUSTERS_COS3,10)
listing_group3 <- as.data.frame(cbind(NEGATIVE_GROUPS3,lsid))
names(listing_group3) <- c("group","listing_id")

words_and_clusters3_neg <- reviews_neg_clus_3 %>% right_join(listing_group3,'listing_id')


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1){
  temp <- words_and_clusters3_neg %>% group_by('word') %>% 
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(words = temp$word, freq = temp$n, min.freq = 500,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}



















####################Reading from Connor's output ################################


clusters <- read_csv("clusters14.csv")


clu1 <- NULL
clu2 <- NULL
clu3 <- NULL
clu4 <- NULL
clu5 <- NULL
clu6 <- NULL
clu7 <- NULL
clu8 <- NULL
clu9 <- NULL
clu10 <- NULL
clu11 <- NULL
clu12 <- NULL
clu13 <- NULL
clu14 <- NULL

clu1 <- clusters[clusters$cluster == 1,]
clu2 <- clusters[clusters$cluster == 2,]
clu3 <- clusters[clusters$cluster == 3,]
clu4 <- clusters[clusters$cluster == 4,]
clu5 <- clusters[clusters$cluster == 5,]
clu6 <- clusters[clusters$cluster == 6,]
clu7 <- clusters[clusters$cluster == 7,]
clu8 <- clusters[clusters$cluster == 8,]
clu9 <- clusters[clusters$cluster == 9,]
clu10 <- clusters[clusters$cluster == 10,]
clu11 <- clusters[clusters$cluster == 11,]
clu12 <- clusters[clusters$cluster == 12,]
clu13 <- clusters[clusters$cluster == 13,]
clu14 <- clusters[clusters$cluster == 14,]



listing_k <-data.frame(listing_id = listings$id, lat = listings$latitude, lon = listings$longitude)

listings[listings$id==54487,]

cluster10 <- clu10 %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)


#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster10, aes(x = lon, y = lat), color = 'red', size = 2)

cluster11 <- clu11 %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)


#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster11, aes(x = lon, y = lat), color = 'red', size = 2)

cluster12 <- clu12 %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)


#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster12, aes(x = lon, y = lat), color = 'red', size = 2)

cluster13 <- clu13 %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)


#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster13, aes(x = lon, y = lat), color = 'red', size = 2)


cluster14 <- clu14 %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)



#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster14, aes(x = lon, y = lat), color = 'red', size = 2)










