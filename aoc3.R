#import data beforehand using IN<-read_clip()

### PART ONE ###
#half-length of each entry
halfL<-nchar(IN)/2
total<-0

#split each string in half
Lsplit<-substr(IN,1,halfL)
Rsplit<-substr(IN,halfL+1,halfL*2)

#make ultra giga letters string w lower and upper case
Letters<-c(letters, LETTERS)

#loop da loop
for (i in 1:length(IN))
{
charVector<-as.integer(str_detect(Lsplit[i],Letters))+as.integer(str_detect(Rsplit[i],Letters))
priority<-which(charVector==2)
total<-total+priority
}
total

### PART TWO ###
#same but checking between 3 rows rather than left/right halves
total2<-0
for (i in seq(from=1,to=length(IN),by=3))
{
charVector2<-as.integer(str_detect(IN[i],Letters))+as.integer(str_detect(IN[i+1],Letters))+as.integer(str_detect(IN[i+2],Letters))
priority<-which(charVector2==3)
total2<-total2+priority
}
total2
