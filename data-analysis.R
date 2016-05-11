'
Analyzing crowdsourced disaster data

Kim de Bie
Leiden University College The Hague
Created: 10-04-2016
Last edited: 09-05-2016

'

###################################################################################

#### SET-UP ####

# installing and loading packages
library(gdata)
library(ggplot2)
library(plyr)
library(dplyr)
library(lattice)
library(RColorBrewer)
library(corrplot)
library(arules)
library(cluster)
library(fpc)
library(arulesViz)

# setting the working directory and loading data
setwd("C:/Users/kimde/Documents/GitHub")
tweets <- read.csv("capstone/finaldata.csv", header=TRUE, stringsAsFactors = FALSE)

# transforming the data
tweets$Information.Source <- as.factor(tweets$Information.Source)
tweets$Information.Type <- as.factor(tweets$Information.Type)
tweets$disaster <- as.factor(tweets$disaster)
tweets$Location <- as.factor(tweets$Location)
tweets$Useful.information <- as.factor(tweets$Useful.information)
tweets$econtext <-as.factor(tweets$econtext)
tweets$numcaus <- as.factor(tweets$numcaus)
tweets$awaprep <- as.factor(tweets$awaprep)
tweets$intacc <- as.factor(tweets$intacc)
tweets$twittus <- as.factor(tweets$twittus)

# checking data structure
str(tweets)

# saving the dataset without tweets categorized as Not applicable
# Not-applicable contains the tweets that could not be related to the disaster
categories_tweets <- droplevels(subset(tweets, Information.Type != "Not applicable"))

# useful_tweets contains only the relevant tweets
useful_tweets <- droplevels(subset(categories_tweets, Useful.information == 1))

# duplicate_tweets contains only duplicate tweets
duplicate_tweets <- droplevels(subset(categories_tweets, Duplicate == TRUE))

# checking the levels of this new dataset
levels(categories_tweets$Information.Type)

###################################################################################

#### ANALYZING THE DATA ####

# attaching & detaching different subsets
attach(categories_tweets)
attach(useful_tweets)
attach(duplicate_tweets)
detach(useful_tweets)
detach(categories_tweets)
detach(duplicate_tweets)

###################################################################################

### 1. Creating a stacked bar chart for information types ###

# getting data into the correct format
information_type_per_disaster <- data.frame(table(disaster, Information.Type, dnn=c("disaster", "information type") ))

# alternative format that might be useful later: (uncomment)
#information_type_per_disaster2 <- data.frame(table(disaster, Information.Type, dnn=c("disaster", "information type") )[,])

# converting to relative frequencies
rel_info <- ddply(information_type_per_disaster, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)
rel_info$relative_frequency <-round(rel_info$relative_frequency, digits=5)

# we want to order by "Affected Individuals"
# this bit is rather complicated - the basic logic is as follows: first only the affected-individuals frequencies are selected, then they are sorted, then this sorted order is applied to the whole dataset
sub <- subset(rel_info, information.type == "AI")
detach("package:dplyr", unload=TRUE) # package conflicts with arrange (from plyr package)
ordered_inftype <- arrange(sub, relative_frequency, decreasing=F)
vector_ordered_inftype <- as.vector(ordered_inftype[,1])
sorted_rel_info <- rel_info
total_vector <- vector_ordered_inftype[sort(order(vector_ordered_inftype)[sorted_rel_info$disaster])]
rel_info$disaster<- reorder.factor(rel_info$disaster, new.order=total_vector)
rel_info <- arrange(rel_info, disaster)

# creating the plot

cbPalette<-brewer.pal(7, "Greys")[2:7]

ggplot(rel_info, aes(x=disaster, y=relative_frequency, fill=information.type)) + geom_bar(stat="identity") + coord_flip() + labs(x = "Disaster", y = "Frequency in percentages", fill = "Information Type") + facet_wrap(~information.type, ncol=6) + scale_fill_manual(labels=c("Affected individuals", "Caution and advice", "Donations and volunteering", "Infrastructure and utilities","Other useful information","Sympapthy and emotional support"), values=cbPalette) + theme(legend.position="bottom", strip.text.x = element_text(size=9)) #+ geom_text(aes(label=relative_frequency),colour="black",size=3, hjust = -.5)


###################################################################################

### 2. Creating a stacked bar chart for information sources ###

# getting data into the correct format
information_source_per_disaster <- data.frame(table(disaster, Information.Source, dnn=c("disaster", "information source") ))
# alternative format: (uncomment)
#information_source_per_disaster2 <- data.frame(table(disaster, Information.Source, dnn=c("disaster", "information source") )[,])

# converting to relative frequencies
rel_info <- ddply(information_source_per_disaster, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)

# we want to order by "Eyewitness"
# this bit is rather complicated - the basic logic is as follows: first only the affected-individuals frequencies are selected, then they are sorted, then this sorted order is applied to the whole dataset
sub <- subset(rel_info, information.source == "EW")
detach("package:dplyr", unload=TRUE) # package conflicts with arrange (from plyr package)
ordered_inftype <- arrange(sub, relative_frequency, decreasing=F)
vector_ordered_inftype <- as.vector(ordered_inftype[,1])
sorted_rel_info <- rel_info
total_vector <- vector_ordered_inftype[sort(order(vector_ordered_inftype)[sorted_rel_info$disaster])]
rel_info$disaster<- reorder.factor(rel_info$disaster, new.order=total_vector)
rel_info <- arrange(rel_info, disaster)

# making the graph

cbPalette<-brewer.pal(7, "Greys")[2:7]

ggplot(rel_info, aes(x=disaster, y=relative_frequency, fill=information.source)) + geom_bar(stat="identity") + coord_flip() + labs(x = "Disaster", y = "Frequency in percentages", fill = "Information Source") + facet_wrap(~information.source, ncol=6) + scale_fill_manual(labels=c("Eyewitness", "Government", "Media and news outlets", "NGOs", "Other", "Outsider"), values=cbPalette) + theme(legend.position="bottom", strip.text.x = element_text(size=9))


###################################################################################

### 3. Creating a stacked bar chart for usefulness vs. non-usefulness ###

# getting data into the correct format
usefulness_per_disaster <- data.frame(table(disaster, Useful.information, dnn=c("disaster", "useful information") ))

# converting to relative frequencies
rel_info <- ddply(usefulness_per_disaster, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)

# we want to order by "Relevance"
# this bit is rather complicated - the basic logic is as follows: first only the affected-individuals frequencies are selected, then they are sorted, then this sorted order is applied to the whole dataset
sub <- subset(rel_info, useful.information == 1)
detach("package:dplyr", unload=TRUE) # package conflicts with arrange (from plyr package)
ordered_inftype <- arrange(sub, relative_frequency, decreasing=F)
vector_ordered_inftype <- as.vector(ordered_inftype[,1])
sorted_rel_info <- rel_info
total_vector <- vector_ordered_inftype[sort(order(vector_ordered_inftype)[sorted_rel_info$disaster])]
rel_info$disaster<- reorder.factor(rel_info$disaster, new.order=total_vector)
rel_info <- arrange(rel_info, disaster)

# making the graph
cbPalette<-brewer.pal(3, "Greys")[2:3]

ggplot(rel_info, aes(x=disaster, y=relative_frequency, fill=useful.information)) + geom_bar(stat="identity") + coord_flip() + labs(x = "Disaster", y = "Frequency in percentages", fill= "") + scale_fill_manual(labels=c("Not related", "Related"), values = cbPalette) 


###################################################################################

# 4. Creating a stacked bar chart for duplicate vs. non-duplicate
## also: who are the duplicate tweeters? what do they tweet about?
## perhaps mosaic plot (like in step 6) for just the duplicate tweets

duplicates_per_disaster <- data.frame(table(disaster, Duplicate, dnn=c("disaster", "duplicate") ))

# converting to relative frequencies
rel_info <- ddply(duplicates_per_disaster, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)

# we want to order by "Relevance"
# this bit is rather complicated - the basic logic is as follows: first only the affected-individuals frequencies are selected, then they are sorted, then this sorted order is applied to the whole dataset
sub <- subset(rel_info, duplicate == TRUE)
detach("package:dplyr", unload=TRUE) # package conflicts with arrange (from plyr package)
ordered_inftype <- arrange(sub, relative_frequency, decreasing=F)
vector_ordered_inftype <- as.vector(ordered_inftype[,1])
sorted_rel_info <- rel_info
total_vector <- vector_ordered_inftype[sort(order(vector_ordered_inftype)[sorted_rel_info$disaster])]
rel_info$disaster<- reorder.factor(rel_info$disaster, new.order=total_vector)
rel_info <- arrange(rel_info, disaster)

# making the graph

cbPalette<-brewer.pal(3, "Greys")[2:3]

ggplot(rel_info, aes(x=disaster, y=relative_frequency, fill=duplicate)) + geom_bar(stat="identity") + coord_flip() + labs(x = "Disaster", y = "Frequency in percentages", fill = "") + scale_fill_manual(labels=c("Non-duplicates", "Duplicates"), values = cbPalette) 


###################################################################################

# 5. Creating a stacked bar chart for location vs. non-location

# this is ugly but I couldn't figure it out 
locations_per_disaster <- data.frame(table(disaster, Location, dnn=c("disaster", "location") ))
locations_per_disaster <-droplevels(subset(locations_per_disaster, disaster != "2013 Sardinia floods"))
locations_per_disaster <-droplevels(subset(locations_per_disaster, disaster != "2012 Italy earthquakes"))
locations_per_disaster <-droplevels(subset(locations_per_disaster, disaster != "2012 Guatemala earthquake"))
locations_per_disaster <-droplevels(subset(locations_per_disaster, disaster != "2012 Costa Rica earthquake"))

# converting to relative frequencies
rel_info <- ddply(locations_per_disaster, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)

# we want to order by "location"
# this bit is rather complicated - the basic logic is as follows: first only the affected-individuals frequencies are selected, then they are sorted, then this sorted order is applied to the whole dataset
sub <- subset(rel_info, location == 0)
detach("package:dplyr", unload=TRUE) # package conflicts with arrange (from plyr package)
ordered_inftype <- arrange(sub, relative_frequency, decreasing=F)
vector_ordered_inftype <- as.vector(ordered_inftype[,1])
sorted_rel_info <- rel_info
total_vector <- vector_ordered_inftype[sort(order(vector_ordered_inftype)[sorted_rel_info$disaster])]
rel_info$disaster<- reorder.factor(rel_info$disaster, new.order=total_vector)
rel_info <- arrange(rel_info, disaster)

# making the graph

cbPalette<-brewer.pal(4, "Greys")[2:4]

ggplot(rel_info, aes(x=disaster, y=relative_frequency, fill=location)) + geom_bar(stat="identity") + coord_flip() + labs(x = "Disaster", y = "Frequency in percentages", fill = "") + scale_fill_manual(labels=c("No location", "Manual location", "Geolocation"), values = cbPalette)


###################################################################################

# 6. Creating a mosaic plot for information type vs. information source

# getting data into the right format

# ... for including all tweets
#information_type_per_author <- data.frame(table(categories_tweets$Information.Source, categories_tweets$Information.Type, dnn=c("source", "information type") )[,]) / nrow(categories_tweets) * 100

# ... for including all duplicate tweets
information_type_per_author <- data.frame(table(duplicate_tweets$Information.Source, duplicate_tweets$Information.Type, dnn=c("source", "information type") )[,]) / nrow(duplicate_tweets) * 100

# ... for including only useful tweets
information_type_per_author <- data.frame(table(useful_tweets$Information.Source, useful_tweets$Information.Type, dnn=c("source", "information type") )[,]) / nrow(useful_tweets) * 100

information_type_per_author <- as.matrix(information_type_per_author)

rownames(information_type_per_author) <- c("Eyewitness", "Government", "Media", "NGOs", "Other", "Outsiders")

colnames(information_type_per_author) <- c("Affected Individuals", "Caution and Advice", "Donations", "Infrastructure", "Other useful", "Sympathy")

# picking a color palette

col1<-colorRampPalette(brewer.pal(9, "Greys"))

# making the plot
corrplot(information_type_per_author, 
         method="circle", 
         is.corr=FALSE, 
         bg = brewer.pal(9, "Greys")[2],
         outline=T, 
         order="FPC", 
         col = col1(100), 
         cl.lim = c(0, 25), # should be >23 for duplicates
         cl.pos = "b",
         cl.length = 6,
         #addCoef.col = "red",
         tl.col = "black",
         tl.srt=35,
         tl.cex = 0.8)

###################################################################################


# 7. Creating a time-plot (for disasters where time data is available)

# TODO 

###################################################################################

# 8. Perform an association rules analysis

# making a subset of the data with the appropriate variables
asrulesdata <- subset(categories_tweets, , -c(X.ID,Tweet.ID, time, author, author.id, lon, lat, Tweet.Text, Duplicate.of, disaster))

# running the apriori algorithm
# support: the minimum frequency that a pair occurs in the data. I read somewhere that around 500 observations is an appropriate treshold?! that's around 0.025 for this dataset
# conf: minimum confidence level
rules <- apriori(asrulesdata, parameter = list(maxlen = 2, supp = 0.025, conf = 0.7))
inspect(rules)

quality(rules) <- round(quality(rules), digits=3)

# rules sorted by lift: confidence / expected confidence. How many more times did we observe this phenomena more/less than we expected?
# e.g. lift of 1.6: 60% more than expected
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# remove redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
rules.pruned <- rules.sorted[!redundant]

# this is the final set of rules
inspect(rules.pruned)

# attempt at visualization - not very successful
plot(rules.pruned, method="grouped")

###################################################################################


# 9. Perform a cluster analysis

# getting all data in one data frame

# formatting information type
information_type <- data.frame(table(disaster, Information.Type, dnn=c("disaster", "information type") ))
rel_info_type <- ddply(information_type, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)
rel_info_type <- subset(rel_info_type, , -c(Freq))
info_type <- reshape(rel_info_type, timevar = "information.type", idvar = c("disaster"), direction = "wide")


# formatting information source
information_source <- data.frame(table(disaster, Information.Source, dnn=c("disaster", "information source") ))
rel_info_source <- ddply(information_source, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)
rel_info_source <- subset(rel_info_source, , -c(Freq))
info_source <- reshape(rel_info_source, timevar = "information.source", idvar = c("disaster"), direction = "wide")

# formatting location proportions
location <- data.frame(table(disaster, Location, dnn=c("disaster", "location") ))
rel_location <- ddply(location, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)
rel_location <- subset(rel_location, , -c(Freq))
loc <- reshape(rel_location, timevar = "location", idvar = c("disaster"), direction = "wide")

# formatting useful information
useful <- data.frame(table(disaster, Useful.information, dnn=c("disaster", "Useful.information") ))
rel_useful <- ddply(useful, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)
rel_useful <- subset(rel_useful, , -c(Freq))
usef <- reshape(rel_useful, timevar = "Useful.information", idvar = c("disaster"), direction = "wide")

# formatting duplicate
duplicate <- data.frame(table(disaster, Duplicate, dnn=c("disaster", "Duplicate") ))
rel_dupl <- ddply(duplicate, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)
rel_dupl <- subset(rel_dupl, , -c(Freq))
dupl <- reshape(rel_dupl, timevar = "Duplicate", idvar = c("disaster"), direction = "wide")

# bind data frames 
all <- join_all(list(info_type,info_source,loc, usef, dupl), by = 'disaster', type = 'full')

# apply the clustering method to information type
d <- dist(info_type)
hc <- hclust(d)
plot(hc, labels=info_type$disaster,main="Cluster Dendogram for Information Type")#, which.plots = 2)

# apply the clustering method to information source
d <- dist(info_source)
hc <- hclust(d)
plot(hc, labels=info_source$disaster,main="Cluster Dendogram for Information Source")

# apply the clustering method to everything
d <- dist(all)
hc <- hclust(d)
plot(hc, labels=all$disaster,main="Cluster Dendogram for All Dimensions")

###################################################################################

# 10. Random calculations (used to support argument throughout thesis - removed and added throughout the writing process)

# proportions of duplicates per information source
duplicates_per_category <- data.frame(table(disaster, Location, dnn=c("disaster", "duplicate") ))
rel_info <- ddply(duplicates_per_category, "disaster", transform, relative_frequency = Freq / sum(Freq) * 100)

save <- subset(rel_info, duplicate==1)
save <-droplevels(subset(save, disaster != "2013 Sardinia floods"))
save <-droplevels(subset(save, disaster != "2012 Italy earthquakes"))
save <-droplevels(subset(save, disaster != "2012 Guatemala earthquake"))
save <-droplevels(subset(save, disaster != "2012 Costa Rica earthquake"))

mean(save$relative_frequency)

# average information sources
avinfsrc <- droplevels(subset(rel_info, information.source=="OTH"))
mean(avinfsrc$relative_frequency)

