library("clipr")
library("stringr")

#grab data from clipboard
INPUT<-read_clip()

#find comma, break out 2 vectors based on comma location
comma<-as.numeric(regexec(",", INPUT))
lefty<-substr(INPUT, 1, comma-1)
righty<-substr(INPUT, comma+1, nchar(INPUT))

#same but breaking each apart by the "-", also make numeric
dashL<-as.numeric(regexec("-", lefty))
dashR<-as.numeric(regexec("-", righty))

leftyMin<-as.numeric(substr(lefty, 1, dashL-1))
leftyMax<-as.numeric(substr(lefty, dashL+1, nchar(lefty)))

rightyMin<-as.numeric(substr(righty, 1, dashR-1))
rightyMax<-as.numeric(substr(righty, dashR+1, nchar(righty)))

#check if lefty is a subset of righty (leftyMin>=rightyMin && leftyMax<=rightyMax)
checky1<-leftyMin>=rightyMin & leftyMax<=rightyMax

#check if righty is a subset of lefty
checky2<-rightyMin>=leftyMin & rightyMax<=leftyMax

#combine them with OR logic in case of exact matches and sum
total=sum(checky1|checky2)


### PART TWO ###

#check if leftyMin OR leftyMax is within righty range
checky3<-(leftyMin>=rightyMin & leftyMin<=rightyMax) | (leftyMax>=rightyMin & leftyMax<=rightyMax)

#check if rightyMin or rightyMax is within lefty range
checky4<-(rightyMin>=leftyMin & rightyMin<=leftyMax) | (rightyMax>=leftyMin & rightyMax<=leftyMax)

#combine same as before
total2=sum(checky3|checky4)