# library(devtools)
devtools::install_github("rladies/meetupr")
library(meetupr)

#library("yaml")

# Log in to meetup.com, your key is here: https://secure.meetup.com/meetup_api/key/
api_key <- "YOUR_API_KEY_HERE"

# `urlname` is a human-readable unique id for a meetup, e.g. https://www.meetup.com/rladies-munich/
urlname <- "rladies-munich"  

# Get the upcoming events (an ongoing/live event will still have "upcoming" status)
upcoming_events <- get_events(urlname = urlname, event_status = "upcoming", api_key = api_key)
print(upcoming_events$name[1])

event_id <- upcoming_events$id[1]  # next event (id)
# Get the RSVP list
rsvps <- get_event_rsvps(urlname = urlname, event_id = event_id, api_key = api_key)
# Subset to those who RSVP'd "yes"
attendees <- rsvps[rsvps$response == "yes", ]
head(attendees)

#save atendee list 
class(attendees)
attendee_list <- data.frame(attendees)
head(attendee_list)
attendee_list <- attendee_list[,grep("member_id|member_name|guests",colnames(attendee_list))]

