# Senior-Research
Includes the source code for collecting, analyzing, and mapping of tweets. 

# libraries used
library(data.table)
library(ggmap)
library(maps)
library(streamR)
library(ROAuth)
library(ggplot2)
library(grid)

# declare Twitter API Credentials & Create Handshake

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "RvoZZFd5aHDXNjIg2j9AYgSMS" # From dev.twitter.com
consumerSecret <- "UPvR74cEOeNsI40Jjhv1T9zaMGcDmG37HJTYuBteZxDn3lKKOC" # From dev.twitter.com

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# save the my_oauth data to an .Rdata file
save(my_oauth, file = "my_oauth.Rdata")


load("my_oauth.Rdata") #loading authentication data

filterStream(file.name = "tweets.json", # Save tweets in a json file
             track= c("Nintendo", "Playstation", "Xbox"), # Collect tweets mentioning Nintendo
             language = "en",
             tweets = NULL , #do not specify number
             timeout = 300,
             oauth = my_oauth) # OAuth credentials

nin.df <- parseTweets("tweets.json", simplify = FALSE) #turning json file into dataframe

state_code <- function(x) x[2] #function to return state abbreviations

#splitting unfiltered tweets to plot bar graph

nin_info1 <- nin.df[grep("Nintendo", nin.df$text), ]

playSt_info1 <- nin.df[grep("Playstation", nin.df$text), ]

xbox_info1 <- nin.df[grep("Xbox", nin.df$text), ]

#making bar graph

tweets <- c(nrow(nin_info1),nrow(playSt_info1), nrow(xbox_info1))
barplot(tweets, main="Tweets based on Keywords", xlab="Keywords", ylab="Tweet Count", 
        names.arg=c("Nintendo", "Playstation", "Xbox"),
        border="red")


#sorting dataframe by location 
temp <- nin.df[grep("US", nin.df$country_code), ]
nin.df <- temp

#sorting dataframe by geocode vs NaN

temp2 <- nin.df[grep("-", nin.df$place_lon), ]
nin.df <- temp2

#sorting out states not in the East or West Coast
keywords = c("ME","NH","VT","MA","RI","CT","NY","NJ",
             "PA","MD","DE","VA","NC","SC","GA",
             "FL","DC","CA","OR","WA")

t_str = strsplit(as.character(nin.df$full_name), ",")

states = sapply(t_str, state_code)

keep = states %in% keywords
nin.df[keep,]

#splitting up dataframe by keywords

nin_info <- nin.df[grep("Nintendo", nin.df$text), ]

playSt_info <- nin.df[grep("Playstation", nin.df$text), ]

xbox_info <- nin.df[grep("Xbox", nin.df$text), ]


#mapping 

map.data <- map_data("state")

m = ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) +
  expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), plot.background = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))


# plot map only
plot(m)


#color coding points based on keywords

points_nin <- data.frame(x = as.numeric(nin_info$place_lon), y = as.numeric(nin_info$place_lat))
points_nin <- points_nin[points_nin$y > 25, ]

points_plyst <- data.frame(x = as.numeric(playSt_info$place_lon), y = as.numeric(playSt_info$place_lat))
points_plyst <- points_plyst[points_plyst$y > 25, ]

points_xbx <- data.frame(x = as.numeric(xbox_info$place_lon), y = as.numeric(xbox_info$place_lat))
points_xbx <- points_xbx[points_xbx$y > 25, ]

nin_result = geom_point(data = points_nin,  aes(x = x, y = y), 
                        size = 1, alpha = 1, color = "red")

playst_result = geom_point(data = points_plyst,  aes(x = x, y = y), 
                           size = 1, alpha = 1, color = "purple")

xbx_result = geom_point(data = points_xbx,  aes(x = x, y = y), 
                        size = 1, alpha = 1, color = "green")


#plot map + points
plot(m + nin_result + playst_result + xbx_result)
