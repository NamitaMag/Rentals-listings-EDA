library(moments)
library(dplyr)                          

#Reading csv file
listings <- read.csv("listings.csv")
nrow(listings)  #TOTAL ROWS
ncol(listings)  #TOTAL COLUMNS
dim(listings)  #gives the columns and rows in the dataset
row.names(listings)
str(listings) 
names(listings) #to get the names of all columns
any(is.na(listings)) #to check if there are any NA values, output is logical
head(listings)  #displays first few rows of dataframe with all the columns
tail(listings)  #displays the last few rows of dataframe with all the columns

#data cleaning-removing duplicate tuples
listings <-distinct(listings) #keep rows/colns distinct
colSums(is.na(listings)) #checking how many NA values are present in every column now


#DATA CLEANING - Replacing all NA values in columns with 0 or FALSE 
listings$reviews_per_month <-replace(listings$reviews_per_month, is.na(listings$reviews_per_month),0)
listings$license <-replace(listings$license, is.na(listings$license),FALSE)
listings$review_scores_value <-replace(listings$review_scores_value, is.na(listings$review_scores_value),0)
listings$review_scores_location <-replace(listings$review_scores_location, is.na(listings$review_scores_location),0)
listings$review_scores_communication <-replace(listings$review_scores_communication, is.na(listings$review_scores_communication),0)
listings$review_scores_checkin <-replace(listings$review_scores_checkin, is.na(listings$review_scores_checkin),0)
listings$review_scores_cleanliness <-replace(listings$review_scores_cleanliness, is.na(listings$review_scores_cleanliness),0)
listings$review_scores_accuracy <-replace(listings$review_scores_accuracy, is.na(listings$review_scores_accuracy),0)
listings$review_scores_rating <-replace(listings$review_scores_rating, is.na(listings$review_scores_rating),0)
listings$calendar_updated <-replace(listings$calendar_updated, is.na(listings$calendar_updated),FALSE)
listings$beds <-replace(listings$beds, is.na(listings$beds),0)
listings$bedrooms <-replace(listings$bedrooms, is.na(listings$bedrooms),0)
listings$bathrooms <-replace(listings$bathrooms, is.na(listings$bathrooms),FALSE)
listings$host_total_listings_count <-replace(listings$host_total_listings_count, is.na(listings$host_total_listings_count),0)
listings$host_listings_count  <-replace(listings$host_listings_count , is.na(listings$host_listings_count),0)

colSums(is.na(listings)) #again checking for NA values in columns
any(is.na(listings))  #gives FALSE as all NA values have been replaced with 0

#Statistical analysis
summary(listings) #gives minimum, 1st Quartile, median, mean, 3rd quartile and maximum values in every column
mean(listings$review_scores_cleanliness)
median(listings$review_scores_cleanliness)
min(listings$review_scores_cleanliness)      
max(listings$review_scores_cleanliness)
print(listings$reviews_per_month)
#Pipelining
listings %>% group_by(bedrooms) %>% summarise(review_scores_cleanliness = mean(review_scores_cleanliness))%>% filter(review_scores_cleanliness >= 3.5) %>% arrange(desc(review_scores_cleanliness))

#Visual analysis
library(ggplot2)
#Bar plot
ggplot(listings,aes(x = room_type))+geom_bar()+theme(axis.text.x = element_text(angle = 90, size = 5))

#histogram
hist(listings$review_scores_location, xlab="Reviews for locations", main="Location reviews")  

#line plot
table(listings$accommodates) #This counts how many time a variable is present in the column.
plot(table(listings$accommodates),type="o", col="blue", xlab=" No. of persons per booking", ylab="No.of bookings", main="Accomodations")

#scatterplot matrix
pairs(~availability_30 + availability_60 + availability_90 + availability_365, data = listings, main = "Scatterplot Matrix")

print(min(listings$accommodates))
