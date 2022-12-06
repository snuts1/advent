#grab initial crates data w crates<-read_clip()

#grab moves data w moves<-read_clip()

#get # of stacks
numStacks<-(nchar(crates[1])+1)/4

#get crate list w split on every char
fkinglist<-strsplit(crates,split="")
df<-data.frame()
df2<-data.frame()

#what is even happening any more. build dataframe by ducking the extra spaces (build it upside down to allow new crates to be new rows)
for (i in c(1:numStacks)){
for (j in c(1:(length(fkinglist)))){
    df[j,i]<-c(fkinglist[[1+length(fkinglist)-j]][(4*(i-1)+2)])
}
}

#get move list
moveList<-strsplit(moves,split=" ")

#build a moves data frame in structure [qty,from,to]
for (i in c(1:length(moves))){
    for (j in c(1:3)){
        df2[i,j]<-as.numeric(moveList[[i]][j*2])
    }
}

#pad up the crates dataframe with space for big stacks
blankz<-matrix(" ",nrow=75,ncol=9)
df<-rbind(df,blankz)

#find height of a stack
stackMeas<-function(stackID){
    stackName<-paste("V",stackID,sep="")
    stackHeight<-dim(df)[1]
    if(df[stackHeight,stackID]==" "){
    stackHeight<-min(which(df[stackName]==" "))-1
    }
    return(stackHeight)
    topCrate<-df[stackHeight,stackID]
    #print(paste("stack number",stackID,"has",stackHeight,"crates. Top crate is",topCrate))
}

#find top crate of a stack
stackRead<-function(stackID){
    stackName<-paste("V",stackID,sep="")
    stackHeight<-dim(df)[1]
    if(df[stackHeight,stackID]==" "){
    stackHeight<-min(which(df[stackName]==" "))-1
    }
    topCrate<-df[stackHeight,stackID]
    return(topCrate)
    #print(paste("stack number",stackID,"has",stackHeight,"crates. Top crate is",topCrate))
}


#move single top crate from one stack to another. df[height,stack] df[row,col]
crateMover<-function(fromStack,toStack){ 
        fromHeight<-stackMeas(fromStack)
        #print(paste("fromHeight is",fromHeight))
        movingCrate<-stackRead(fromStack)
        movingCrate<-df[fromHeight,fromStack]

        #print(paste("movingCrate is",movingCrate))
        df[fromHeight,fromStack]<-" " #empty the spot

        toHeight<-stackMeas(toStack)+1
        #print(paste("toHeight is",toHeight))
        df[toHeight,toStack]<-movingCrate
        return(df)
}

#execute ALL movement commandz
for (j in 1:length(moves)){
qty<-df2[j,1]
for (i in 1:qty){
    df<-crateMover(df2[j,2],df2[j,3])
}
}
ANS<-0
#spit answer
for (i in 1:numStacks){
    ANS<-paste(ANS,stackRead(i),sep="")
}
ANS
testo<-0
funk<-function(qty){
    for (i in 1:qty){
        testo<-c(testo,i)
    }
    return(testo)
}
