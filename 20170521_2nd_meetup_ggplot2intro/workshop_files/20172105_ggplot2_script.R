

install.packages("RCurl")
library(RCurl)
install.packages("RColorBrewer")
library(RColorBrewer)

## A bit of preprocessing -----------------------
berlin_raw<-read.csv("/Users/Pam/Documents/GitHub/rladies_munich/20170521_2nd_meetup_ggplot2intro/workshop_files/Berlin_listings_raw.csv")
summary(berlin_raw)
sum(is.na(berlin_raw)) #4396

berlin_ws <- berlin_raw<-read.csv("/Users/Pam/Documents/GitHub/rladies_munich/20170521_2nd_meetup_ggplot2intro/workshop_files/Berlin_listings_raw.csv", stringsAsFactors = F)
berlin_ws <- berlin_raw[,!colnames(berlin_raw)=="name"]
dim(berlin_ws)
summary(berlin_ws)

berlin_ws[which(berlin_ws$minimum_nights>365),"minimum_nights"] <- NA
sum(is.na(berlin_ws)) #4399

berlin_ws$neighbourhood_group[which(berlin_ws$neighbourhood_group=="Neuk\303\266lln")]<- "Neukoelln"
berlin_ws$neighbourhood_group[which(berlin_ws$neighbourhood_group=="Tempelhof - Sch\303\266neberg")] <- "Tempelhof - Schoeneberg"
berlin_ws$neighbourhood_group[which(berlin_ws$neighbourhood_group=="Treptow - K\303\266penick")] <- "Treptow - Koepenick"
berlin_ws$neighbourhood_group <- as.factor(berlin_ws$neighbourhood_group)
levels(berlin_ws$neighbourhood_group)

summary(berlin_ws)
berlin_ws$id <- as.integer(berlin_ws$id)
berlin_ws$host_id <- as.integer(berlin_ws$host_id)
berlin_ws$host_name <- as.factor(berlin_ws$host_name)
berlin_ws$neighbourhood_group <- as.factor(berlin_ws$neighbourhood_group)
berlin_ws$neighbourhood <- as.factor(berlin_ws$neighbourhood)
berlin_ws$room_type <- as.factor(berlin_ws$room_type)
berlin_ws$number_of_reviews <- as.numeric(berlin_ws$number_of_reviews)
berlin_ws$last_review <- as.Date(berlin_ws$last_review)
berlin_ws$calculated_host_listings_count <- as.numeric(berlin_ws$calculated_host_listings_count)
berlin_ws$availability_365 <- as.numeric(berlin_ws$availability_365)
berlin_ws$minimum_nights <- as.numeric(berlin_ws$minimum_nights)
berlin_ws$reviews_per_month <- as.numeric(berlin_ws$reviews_per_month)
berlin_ws$price <- as.numeric(berlin_ws$price)
summary(berlin_ws)
head(berlin_ws)

write.csv(berlin_ws, "/Users/Pam/Documents/GitHub/rladies_munich/20170521_2nd_meetup_ggplot2intro/workshop_files/Berlin_listings_workshop.csv")

berlin<-read.csv("/Users/Pam/Documents/GitHub/rladies_munich/20170521_2nd_meetup_ggplot2intro/workshop_files/Berlin_listings_workshop.csv")
##---------------------------------------------------------------------

# Import the data
# Option 1: Importing directly from GitHub using RCurl
x <- getURL("https://raw.githubusercontent.com/pamelamatias/RLadies_MUC/master/20170521_2nd_meetup_ggplot2intro/workshop_files/Berlin_listings_workshop.csv")
berlin <- read.csv(text = x)

#Option 2: Download and save (or clone) data, then just type in the corresponding directory and off we go!
berlin<-read.table("/Users/Pam/Documents/GitHub/rladies_munich/20170521_2nd_meetup_ggplot2intro/workshop_files/Berlin_listings_workshop.csv")

# Check nothing funny happened while we were importing it
head(berlin)

#Define type of variables
berlin$id <- as.integer(berlin$id)
berlin$host_id <- as.integer(berlin$host_id)
berlin$host_name <- as.factor(berlin$host_name)
berlin$neighbourhood_group <- as.factor(berlin$neighbourhood_group)
berlin$neighbourhood <- as.factor(berlin$neighbourhood)
berlin$room_type <- as.factor(berlin$room_type)
berlin$number_of_reviews <- as.numeric(berlin$number_of_reviews)
berlin$last_review <- as.Date(berlin$last_review)
berlin$calculated_host_listings_count <- as.numeric(berlin$calculated_host_listings_count)
berlin$availability_365 <- as.numeric(berlin$availability_365)
berlin$minimum_nights <- as.numeric(berlin$minimum_nights)
berlin$reviews_per_month <- as.numeric(berlin$reviews_per_month)
berlin$price <- as.numeric(berlin$price)
berlin <- berlin[,-1]
#And check out its structure
dim(berlin) #20576 x 16
str(berlin)

summary(berlin)

hist(berlin$availability_365)

install.packages("ggplot2")
library(ggplot2)
ggplot(berlin, aes(x = availability_365)) +
geom_histogram()
#scatter 01
#definition of the data set and variables for each axis.
ggplot(berlin, aes(x = minimum_nights, y = price))

#scatter_02
#adding geom_point()
ggplot(berlin, aes(x = minimum_nights, y = price)) +
  geom_point()

#scatter_03
#Identify the neighbourhood group to which each dot belongs by color coding for it
ggplot(berlin, aes(x = minimum_nights, y = price)) +
  geom_point(aes(color = neighbourhood_group))

# line_01
#line plots and alpha (transparency)
ggplot(berlin, aes(x=minimum_nights, y=price, color=neighbourhood_group)) +
geom_line(alpha=0.6)
aes(x = minimum_nights, y = price)

# bar_01
#a simple bar chart showing the distribution of the variable room type
ggplot(berlin, aes(room_type)) + 
geom_bar()

# bar_02
#distinguishing categories in bar charts
ggplot(berlin, aes(room_type, fill=neighbourhood_group)) + 
geom_bar()

# bar_03
#dodging bar charts next to each other
ggplot(berlin, aes(room_type, fill=neighbourhood_group)) + 
geom_bar(position="dodge")


# histogram_01
#simple histogram showing the distribution of pricings for overnight stays in Berlin
ggplot(berlin, aes(price)) +
geom_histogram()

# histogram_01b
berlin_price <- subset(berlin,price<100)
ggplot(berlin_price, aes(price)) +
geom_histogram()


# histogram_02
#definition of number of bins
ggplot(berlin_price, aes(price)) +
geom_histogram(bins = 50)

# histogram_03
#definition of binwidth
ggplot(berlin_price, aes(price, fill = room_type)) +
geom_histogram(binwidth = 5)

# boxplot_01
ggplot(berlin, aes(x=room_type, y=reviews_per_month)) + 
geom_boxplot()


# boxplot_02
#stat_boxplot for defining whisker
ggplot(berlin, aes(x = room_type, y = reviews_per_month))  + 
stat_boxplot(geom ='errorbar', width = 0.65) + 
geom_boxplot() 				# or: stat_boxplot(geom='boxplot')

# boxplot_03
#boxplot grouping prices per color for each cut level, transformation y-axis into log-scale
ggplot(berlin, aes(x = room_type, y = reviews_per_month, fill = neighbourhood_group)) + 
geom_boxplot() 


# multiple_01 -> more examples here!
#splitted by color and arranged in the two rows
ggplot(berlin, aes(x = room_type, y = reviews_per_month)) + 
geom_boxplot(aes(color = room_type)) +
facet_wrap(~neighbourhood_group, nrow = 4)

ggplot(berlin, aes(x = reviews_per_month, y = price)) +
geom_line(aes(color = neighbourhood_group)) +
facet_wrap(~neighbourhood_group, nrow = 2)

ggplot(berlin, aes(x = availability_365)) +
geom_histogram(aes(color = neighbourhood_group),bins=50) +
facet_wrap(~neighbourhood_group, nrow = 4)

ggplot(berlin, aes(x = availability_365, y = number_of_reviews)) +
geom_point(aes(color = neighbourhood_group)) +
facet_wrap(~neighbourhood_group, nrow = 4)

ggplot(berlin, aes(x = availability_365, y = number_of_reviews)) +
geom_point(aes(color = neighbourhood_group)) +
facet_wrap(~neighbourhood_group, nrow = 4)

# multiple_02
ggplot(berlin, aes(availability_365, reviews_per_month, color = room_type)) +
geom_point() +
facet_grid(.~room_type)+                     	#splitted into grid / matrix (x ~ y)
theme(axis.text = element_text(size = 6),       #modifying grid parameter: smaller text sizees
strip.text = element_text(size = 8)) +
scale_color_manual(values = brewer.pal(8, 'Spectral'))+		#changing colors using RColorBrewer
ggtitle('A beautiful ggplot2')	#plot title

ggplot(berlin, aes(availability_365, reviews_per_month, color = room_type)) +
geom_point() +
facet_grid(.~room_type)+                     	#splitted into grid / matrix (x ~ y)
theme(axis.text = element_text(size = 6),       #modifying grid parameter: smaller text sizees
strip.text = element_text(size = 8)) +
scale_color_manual(values = brewer.pal(8, 'Spectral'))+		#changing colors using RColorBrewer
ggtitle('A beautiful ggplot2')	#plot title


######################## Practice time ########################
# Now it's time for you to play a little with our dataset and the little we covered today :) 

##### Practice 1
# data: full dataset 
# type of plot: boxplot
# x-axis: room type 
# y-axis: availability within the next 365 days
ggplot()


ggplot(berlin, aes(x = room_type, y = availability_365))  + 
  geom_boxplot() 				

#### Practice 2
berlin_price <- subset(berlin, price < 100)
ggplot(berlin_price, aes(x = price)) +
  geom_histogram(aes(color = room_type),bins=300) +
  facet_wrap(~room_type, nrow = 3)

#### Practice 3
ggplot(berlin, aes(x = room_type, y = reviews_per_month)) + 
  geom_boxplot(aes(color = room_type)) +
  facet_wrap(~neighbourhood_group, nrow = 4)

####__Practice 4
berlin_neighborhood <- subset(berlin,neighbourhood_group=="Mitte"&minimum_nights>20)
ggplot(berlin_neighborhood, aes(price, number_of_reviews, color = room_type)) +
    geom_point() +
    facet_grid(.~room_type)+                        #splitted into grid / matrix (x ~ y)
    theme(axis.text = element_text(size = 6),       #modifying grid parameter: smaller text sizees
            strip.text = element_text(size = 8)) +
    scale_color_manual(values = brewer.pal(3, 'Spectral'))+     #changing colors using RColorBrewer
    ggtitle('Price and number of reviews from listings in Berlin\'s city center')   #plot title