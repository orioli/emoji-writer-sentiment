# This is the script for the paper: Emojis people use: An benchmark of authors sentiment
#
# At ASONAM2017, PydatDubai vol 1.0 @ AWOK, PydataBCN2017 @ EASDE we have presented the paper Happiness inside a job? paper... Many peple in the various audiences asked why we avoid using emojis to predict and profile employees. The answer is that  we prefer to use  links of likes because they are more authentic than words or emojis. In the same way that google page rank is more effective when it looks at links between pages rather than content inside the pages. ... Still people keep asking about it. But there is one thing emoji are good at estimating... its author sentiment and that is just possible thanks to the unique characteristics of the dataset at hand

#Previous research has traditionally analyzed emoji sentiment from the point of view of the reader of the content only not the author. Here, we analyze emoji sentiment from the author point of view and present a benchmark that was built from an employee happiness dataset where emoji happen to be annotated with daily happiness of the author of the comment. The data spans over 3 years, and 4k employees of 56 companies based in Barcelona. We then compare sentiment of writer to readers. Results indicate that, there is an 82% agreement between exiting reader sentiment based benchmark and the one presented here. Finally, we report that authors that use emoji are happier than authors that do not use emoji. Emoji use was not found to be correlated with differneces in author moodiness. A list of emoji and wirtter sentiment statistics is provided.

# But the questiosn is... what did we miss? 
#
# (c) Jose Berengueres, UAEU 2017 Sep
# here we load interactions table data for ASONAM PAPER  DATA SET 03 export SEPTEMBER 10TH 2017 by Dani. This is a new larger dataset.
#

library(ggplot2)
library("gridExtra")
library(reshape)
library(scales)

# cleaned data
# cof = read.csv("../input/comments2emoji_frequency_matrix_cleaned.csv")
#write.csv(cof, file = "comments2emoji_frequency_matrix_cleaned.csv")
#write.csv(li, file = "commentInteractions_cleaned.csv")
#write.csv(votes, file = "votes_cleaned.csv")

# change this to your working directory
setwd("/Users/jse/Dropbox/0 - academics/0 - happyforce/1 - ASONAM")

### LOAD LIKES ###
li = read.csv("../input/commentInteractions.csv")
outlier.companies = c("5370af43e4b0cff95558c12a","53a2dd43e4b01cc02f1e9011")
li <- li[!(li$companyAlias %in% outlier.companies),] # remove this outlier

li <- li[li$employee>0,] 
li <- droplevels(li)
#levels(li$companyAlias)
#length(unique(li$companyAlias)) ## 34 companies --> 59 companies with likes interactions
#dim(li) # 599k rows
#summary(li) #table 1 row 3 datum
#is.data.frame(li)
# make unique uid
#li$uid <- paste(li$employee,li$companyAlias)


## Visualize likes
#ggplot(li, aes(disliked)) +  geom_bar(alpha = .75)
#ggplot(li, aes(factor(companyAlias,labels=c(1:59)) ,fill=disliked)) +  geom_bar(alpha = .75)


### LOAD COMMENTS with emojis (emojis as html entities are scraped using a regex) ###
cof = read.csv("../input/comments2emoji_frequency_matrix.csv")
dim(cof)
cof[1,]
summary(cof)
cof$total.emojiCount <- rowSums(cof[,14:356])
dim(cof)

## remove comments with more than 15 emoji, spam etc...
cof <- cof[cof$total.emojiCount<15,] 
dim(cof)

cof = cof[!duplicated(cof$comment), ] 
dim(cof)

cof <- cof[cof$nchar<1000,]
dim(cof) 

cof <- cof[cof$employee>0,]   ## not sure how employees with id <0 can impact stats right now so leave out 
dim(cof)

dim(cof[cof$total.emojiCount<15&cof$total.emojiCount>0,]) # 3991 commets with emojis and 357



# top used emojis in Spain
top.emojis <- as.data.frame(colSums(cof[,14:356]))
top.emojis$emoji.code <-rownames(top.emojis)
colnames(top.emojis)[1] <- c("freq")

# emoji desc by from http://graphemica.com/
top.emojis$meaning <- c("add a desc")
top.emojis["X1f4aa",]$meaning <- c("Flexed biceps")
top.emojis["X1f44f",]$meaning <- c("Clapping hands")
top.emojis["X1f618",]$meaning <- c("Face throwing a kiss")
top.emojis["X1f44d",]$meaning <- c("Thumbs up") 
top.emojis["X1f3fb",]$meaning <- c("fitzpatrick-1 lighter skin tone")

top.emojis["X1f389",]$meaning <- c("Party popper")
top.emojis["X1f64c",]$meaning <- c("Two hands up")
top.emojis["X1f3fc",]$meaning <- c("fitzpatrick-3 light skin tone")

top.emojis["X1f914",]$meaning <- c("Thinking face")
top.emojis["X1f917",]$meaning <- c("Hugging face")
top.emojis["X1f62d",]$meaning <- c("Loud cry face")
top.emojis["X1f44c",]$meaning <- c("OK hand sign")
top.emojis["X1f61e",]$meaning <- c("Disapointed but relieved face")
top.emojis["X1f483",]$meaning <- c("Dancer woman")
top.emojis["X1f625",]$meaning <- c("Disapointed w/ one tear")
top.emojis["X1f3fd",]$meaning <- c("fitzpatrick-4 dark skin tone")
top.emojis["X1f61c",]$meaning <- c("Face stung out tonge and wink")
top.emojis["X1f622",]$meaning <- c("Crying face")
top.emojis["X1f621",]$meaning <- c("Pouting face")
top.emojis["X1f611",]$meaning <- c("Expresionless")
top.emojis["X1f382",]$meaning <- c("B-day cake")
top.emojis["X1f64f",]$meaning <- c("Person w/ folded hands")
top.emojis["X1f613",]$meaning <- c("Face with cold sweat")
top.emojis["X1f631",]$meaning <- c("Face screaming in fear")
top.emojis["X1f619",]$meaning <- c("Kissing face with smiling eyes")
top.emojis["X1f629",]$meaning <- c("Weary face")
top.emojis["X1f614",]$meaning <- c("Pensive face")
top.emojis["X1f381",]$meaning <- c("Wrapped present")
top.emojis["X1f623",]$meaning <- c("Persevering face")
top.emojis["X1f3b6",]$meaning <- c("Multiple musical notes")
top.emojis["X1f643",]$meaning <- c("Upside-down smiling face")
top.emojis["X1f388",]$meaning <- c("Baloon")
top.emojis["X1f525",]$meaning <- c("Fire")

## table 3 ##
top.emojis$freq.as.percent.of.total.emojis <- 100* top.emojis$freq/ sum(top.emojis$freq)
top.emojis <- top.emojis[with(top.emojis, order(-freq)), ]
print("Most used emojis at Happyforce.com")
print(top.emojis[1:30,])
write.csv(top.emojis, file = "top.emojis.csv")


### format table ###
cof$X <- NULL
# rename as in prev version
colnames(cof)[1] <- c("id")
colnames(cof)[2] <- c("coa")

colnames(cof)[4] <- c("txt")
colnames(cof)[5] <- c("like")
colnames(cof)[6] <- c("nolike")
colnames(cof)[7] <- c("date")
dim(cof)
#cof[1,]
#length(unique(cof$coa))
#levels(cof$coa)  # 72
# REMOVE COMPANNIES not present in LIKES table
dim(cof)
cof<-cof[cof$coa %in% unique(li$companyAlias),]
dim(cof)
cof$coa <- droplevels(cof$coa)
#levels(co$coa) <- c(1:length(levels(co$coa)))
length(unique(cof$coa)) ## 34 companies before --> 59 -->57

# parse dates
cof$date<- gsub("UTC", "", cof$date)
cof$date<- gsub("UTC", "", cof$date)
cof$date<- gsub("  ", " ", cof$date)
cof$date<- as.Date(cof$date, format = "%a %b %d %H:%M:%S %Y")
#as.Date(cof$date)
#cof[1,]
cof$uiddate <- paste(cof$uid,cof$date)
min(cof$date)
summary(cof$date)
cof <- na.omit(cof)
dim(cof[cof$total.emojiCount==0,])

## fig 5 ## Relation of emoji and likes received in a comment
c1 <- data.frame(cof$commentId,cof$total.emojiCount)
colnames(c1) <- c("commentId","total.emojiCount")
li$likes <- as.numeric(li$liked) -1

### sum plot
li2 <- aggregate(likes ~ commentId, data=li, sum, na.rm=FALSE)
dim(li2)
colnames(li2)[2] <-c("like.ratio")
m1 <- merge(x = li2, y = c1, by = "commentId", all.x = TRUE,na.rm=TRUE)
m1 <- m1[m1$total.emojiCount>-1&m1$total.emojiCount<7&m1$like.ratio<40&m1$like.ratio>0,]
m1 <- mi[sample(nrow(m1), 23000),]
dim(m1)
p0 <- ggplot(m1, aes(like.ratio , color=as.factor(total.emojiCount))) + geom_density(alpha=.5) + xlab("Sum of likes received by a comment")+ labs(color = "Count of \nemoji")+labs(title="Distribution of comments")
p0

## count of emoji effect on likability
m1 <- merge(x = li2, y = c1, by = "commentId", all.x = TRUE,na.rm=TRUE)
m1$has.emoji <- !m1$total.emojiCount==0
m1[1,]
summary(m1)
aggregate(like.ratio ~ has.emoji, data=m1, mean, na.rm=FALSE)

### ratio plot
li2 <- aggregate(likes ~ commentId, data=li, mean, na.rm=FALSE)
dim(li2)
colnames(li2)[2] <-c("like.ratio")
m1 <- merge(x = li2, y = c1, by = "commentId", all.x = TRUE,na.rm=TRUE)

m1 <- m1[m1$total.emojiCount>-1&m1$total.emojiCount<7&m1$like.ratio<1&m1$like.ratio>0,]

p1 <- ggplot(m1, aes(like.ratio , color=as.factor(total.emojiCount))) + geom_density(alpha=.5) + xlab("likes / (likes + dislikes) ratio of comment")+ labs(color = "Count emoji ") +labs(title="Distribution of comments  ") +theme(legend.position = "none")

grid.arrange(p1, p0,         ncol=2, nrow=1, widths=c(1, 1.3), heights=c(1))
m1 <- NULL
li2 <- NULL
###

  

### LOAD VOTES (this is the happiness table) ##################################################3
votes = read.csv("../input/votes.csv")
is.data.frame(votes)
colnames(votes)
dim(votes) # 409655
hist(votes$employee)
hist(votes$vote)

# REMOVE COMPANNIES WITH NO likes
dim(votes)
votes<-votes[votes$companyAlias %in% unique(li$companyAlias),] # remove this company that has low numbers and no comments
dim(votes)
votes <- droplevels(votes)
## SANITY CHECK
setdiff(unique(votes$companyAlias),unique(li$companyAlias)) # use before changing alias names.
length(unique(votes$companyAlias)) ## 36 companies  --> 59 companies now
votes$uid <- paste(votes$employee,votes$companyAlias)

# parse dates
# votes$dt <- as.POSIXct((votes$voteDate), origin="1970-01-01")
votes$voteDate<- gsub("UTC", "", votes$voteDate)
votes$voteDate<- gsub("UTC", "", votes$voteDate)
votes$voteDate<- gsub("  ", " ", votes$voteDate)
votes$voteDate<- as.Date(votes$voteDate, format = "%a %b %d %H:%M:%S %Y")

# convert 1 2 3 4 scale to -1 +1 usually used in papers of sentiment
c2Polar <- c(-1,-0.5,0,+1)
is.factor(votes$vote)
c2Polar[4]
#idx <- match(c2Polar, names(vt))
#vt2 <- vt[,idx] 
votes$vote.original <- votes$vote
votes$vote <- c2Polar[votes$vote.original]
hist(votes$vote)
## table 2 ##
summary(votes)
summary(aggregate(voteDate ~ uid+voteDate, votes, function(x) length(x)))
summary(aggregate(vote ~ uid, votes, mean))
summary(aggregate(vote ~ uid, votes, sd))
dim(votes)

## fig 1 ## participation histogram by company
ggplot(votes[votes$voteDate>max(votes$voteDate)-1680,], aes(voteDate,fill=factor(companyAlias,labels=c(1:length(unique(votes$companyAlias))))))+   geom_bar(alpha = .95) + theme(legend.position="none")+ labs(title="App usage: Count of votes casted daily", caption="color signifies company")

## fig 2 ##
ggplot(votes, aes(vote,fill=companyAlias)) +  geom_histogram(alpha = 1) + theme(legend.position="none") + labs(title="Count of answers to the question: ‘How happy are you at work today?", caption="there is a bias towards happiness")

## fig 3 ##
vtimeaverage <- aggregate(voteDate ~ uid+companyAlias, votes, function(x) length(x)/ as.double(( difftime(max(x), min(x) , units = "days"))+1))
colnames(vtimeaverage)<- c("uid","ca","votesperday")
vtimeaverage <- vtimeaverage[vtimeaverage$votesperday<1000,]
#remove outlier
vtimeaverage <- vtimeaverage[!vtimeaverage$votesperday>1,] 
ggplot(vtimeaverage, aes(votesperday,fill=ca)) +  geom_histogram(alpha = 1) + theme(legend.position="none")+ labs(title="How often do employees use the app?", caption="About 10% use the app daily including weekends")

## fig 4 ##
ggplot(cof[cof$total.emojiCount>0,], aes(x=total.emojiCount)) +  geom_bar(aes(y = (..count..)/sum(..count..),color=total.emojiCount))+xlim(0,15) + xlab("Number of emojis per comment") + ylab("Percent of comments") + scale_y_continuous(labels = percent)+ theme(legend.position="none") + labs(title="Number of emoji in the comment in comments with emoji", caption="N= 3.8k comments with emoji. Color indicates company")
dim(cof[cof$total.emojiCount>0,])[1]



## Analisys section

# 3. for every comment compute happiness bias = $happines.vote(@ given day) - mean of user
mean.hapiness <-aggregate(votes$vote, by=list(votes$uid),   FUN=mean, na.rm=TRUE)
mean.hapiness[1,]
votes[1,]
colnames(mean.hapiness)[c(1,2)] <- c("uid","mean")
votes$uiddate <- paste(votes$uid,votes$voteDate)
cof$uiddate <- paste(cof$uid,cof$date)
summary(cof$uiddate)
vt <- merge(x = cof, y = votes, by = "uiddate", all.x = TRUE)
find.avg.happiness  <- function(a.uid) {
	return( mean.hapiness[mean.hapiness$uid==a.uid,]$mean)
}
#summary(vt$happines.bias)
vt$avg.hapiness <- unlist(lapply(vt$uid.x, find.avg.happiness))
vt$bias <- vt$vote - vt$avg.hapiness 
summary(vt$bias)
summary(vt$vote)
hist(vt$vote)
hist(vt$vote.original)
vt$vote.original <- NULL
### tracy widom  -- it does not look liek tracy widom
#ggplot(vt, aes(bias)) +  geom_density() 

## fig 5b ## happiness timeline
vt[1,]
vt$comment.has.emoji <- as.factor(vt$total.emojiCount>0)

## table 4 ## about comments
summary(cof$nchar)
aggregate(vote ~ comment.has.emoji,vt, mean)
aggregate(total.emojiCount ~ comment.has.emoji,vt, mean)
aggregate(vote ~ comment.has.emoji,vt, length)
summary(vt[vt$total.emojiCount>0,]$total.emojiCount)
summary(vt[vt$total.emojiCount>0,]$vote)
summary(vt[vt$total.emojiCount==0,]$vote)


## table 4 ## additonal rows
uid2emojicount <- aggregate(total.emojiCount ~ uid,cof, sum)
summary(uid2emojicount)

uid2emojicount <- aggregate(total.emojiCount ~ uid,cof[cof$total.emojiCount>0&cof$total.emojiCount<15,], sum)
summary(uid2emojicount)

uid2emojicount <- aggregate(total.emojiCount ~ uid,cof[cof$total.emojiCount>0&cof$total.emojiCount<15,], length)
summary(uid2emojicount)
used.emoji.atleast.once <- unique(uid2emojicount$uid)

uid2noemojicountforemojiuser <- aggregate(total.emojiCount ~ uid,cof[cof$total.emojiCount==0&cof$total.emojiCount<15&cof$uid %in% used.emoji.atleast.once,], length)
uid2emojicountforemojiuser <- aggregate(total.emojiCount ~ uid,cof[cof$total.emojiCount>0&cof$total.emojiCount<15&cof$uid %in% used.emoji.atleast.once,], length)
ratio.comments.with.emojis.of.emoji.users <- merge(x = uid2emojicountforemojiuser, y = uid2noemojicountforemojiuser, by = "uid", all.x = TRUE)
ratio.comments.with.emojis.of.emoji.users$ratio <- ratio.comments.with.emojis.of.emoji.users$total.emojiCount.x /(ratio.comments.with.emojis.of.emoji.users$total.emojiCount.x + ratio.comments.with.emojis.of.emoji.users$total.emojiCount.y)
summary(ratio.comments.with.emojis.of.emoji.users)
## table 4 end ##


## fig 6 ## 
p0 <- ggplot(vt[sample(nrow(vt), 7000),], aes(voteDate, vote,,color=comment.has.emoji))+  geom_point( alpha = .90, size=.1) + geom_smooth(method="loess") + ylab("Happiness of author")+ xlab("Comment post date")+ xlim(c(as.Date("2016-10-01"),as.Date("2017-9-01")))+ theme(legend.position="none")

compare <- ggplot(vt, aes(vote,color=comment.has.emoji)) +  geom_density() +coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)
better_compare <- aggregate(vote ~ as.factor(vote) + comment.has.emoji, vt, length)
colnames(better_compare)[3] <-c("N") 
sumatory <- aggregate(N ~  comment.has.emoji, better_compare, sum)
better_compare$N.group <- unlist(lapply(better_compare$comment.has.emoji,function(x) {sumatory[sumatory$comment.has.emoji==x,]$N}))
better_compare$freq <- better_compare$N/better_compare$N.group
better_compare$percent <- 100*better_compare$N/better_compare$N.group
better_compare
colnames(better_compare)[1] <-c("happiness") 
compare <- ggplot(data=better_compare, aes(happiness, y=percent, fill=comment.has.emoji)) + geom_bar(stat="identity", position=position_dodge())+coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + labs(fill = "Comment \nhas emoji")
compare
grid.arrange(p0, compare, ncol=2, nrow=1, widths=c(2,2), heights=c(4))

##Kolmogorov.
ks.test(vt[vt$comment.has.emoji,]$vote, vt[vt$comment.has.emoji==FALSE,]$vote, alternative="less")
ks.test(vt[vt$comment.has.emoji,]$vote, vt[vt$comment.has.emoji==FALSE,]$vote, alternative="greater")
#ggplot(vt, aes(vote,color=as.factor(vt$total.emojiCount>0))) +  geom_bar( position=position_dodge())

vt[1,]



## fig 7 ## 
p0 <- ggplot(vt[sample(nrow(vt), 7000),], aes(voteDate, bias))+  geom_point( alpha = .90, size=.1) + ylab("bias: happiness - mean")+ xlab("Comment post date")+ xlim(c(as.Date("2016-10-01"),as.Date("2017-9-01")))+ theme(legend.position="none")

compare <- ggplot(vt, aes(bias,color=comment.has.emoji)) +  geom_density() +coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + labs(color = "Comment \nhas emoji")
compare
grid.arrange(p0, compare, ncol=2, nrow=1, widths=c(3,1.2), heights=c(4))


##Kolmogorov.
#ks.test(vt[vt$comment.has.emoji,]$bias, vt[vt$comment.has.emoji==FALSE,]$bias, alternative="less")
#ks.test(vt[vt$comment.has.emoji,]$bias, vt[vt$comment.has.emoji==FALSE,]$bias, alternative="greater")
## cannot distinguish them
## fig 7 end ##

# todo fix problem of no emojis detected before certain dates....

## table 5 ##  about users
vt$uid <- paste(vt$id,vt$coa)
length(unique(vt$uid))
emoji.users <- unique(vt[vt$comment.has.emoji==TRUE,]$uid)
non.emoji.users <- unique(vt[vt$comment.has.emoji==FALSE,]$uid)
non.emoji.users <-setdiff(unique(vt$uid),emoji.users)
length(emoji.users)
length(non.emoji.users)

vt$emoji.user <- vt$uid %in% emoji.users
aggregate(vote ~ emoji.user,vt, mean)
aggregate(vote ~ emoji.user,vt, sd)
aggregate(vote ~ emoji.user,vt, length)

aggregate(vote ~ comment.has.emoji,vt[vt$uid %in% emoji.users,], mean)
aggregate(vote ~ comment.has.emoji,vt[vt$uid %in% emoji.users,], sd)
aggregate(vote ~ comment.has.emoji,vt[vt$uid %in% emoji.users,], length)


## fig 8 new ##
## Scatter plot x= count emojis an auhtor has posted  y= mean happiness level
#colnames(vt)[363] <- c("uid")
dim(ratio.comments.with.emojis.of.emoji.users)
sfk <- merge(x = ratio.comments.with.emojis.of.emoji.users, y = vt, by = "uid", all.x = TRUE)
sfk <- na.omit(sfk)

p0 <- ggplot(sfk, aes(ratio, avg.hapiness)) +  geom_point( alpha = .90, size=.1) + geom_smooth(method="lm") +ylab("Average happiness") +xlab("Ratio of comments with emoji to all comments")+theme(legend.position="none") 
fit <- lm(avg.hapiness ~ ratio,data=sfk) 
summary(fit)
# Marginal density plot of x (top panel)
xdensity <- ggplot(sfk, aes(ratio)) + 
  geom_density(alpha=.5)  + 
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
xdensity

# Marginal density plot of y (right panel)
ydensity <- ggplot(sfk, aes(avg.hapiness)) + 
  geom_density(alpha=.5) +  
  theme(legend.position = "none") + coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ydensity

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(), 
   panel.border = element_blank(),
   panel.background = element_blank(),
   axis.title.x = element_blank(),
   axis.title.y = element_blank(),
   axis.text.x = element_blank(), 
   axis.text.y = element_blank(),
   axis.ticks = element_blank()
     )

grid.arrange(xdensity, blankPlot, p0, ydensity, 
        ncol=2, nrow=2, widths=c(4,0.7), heights=c(0.7, 4))
## fig 8 end

### fig 9 ###
idx <- match(c(top.emoji.codes,"vote","date","companyAlias"), names(vt))
vt2 <- vt[,idx] 
mdata <- melt(vt2, id=c("vote","date","companyAlias"))
colnames(mdata) <- c("vote","date","companyAlias","emoji","count")
bias.mean.c <- aggregate(vote ~ emoji + companyAlias , mdata[mdata$count>0,], mean)
bias.sd.c <- aggregate(vote ~ emoji + companyAlias , mdata[mdata$count>0,], sd)
bc <- cbind(bias.mean.c,  bias.sd.c)
colnames(bc) <- c("code","coa","bias.mean","emoji2","coa2","sd")
only <- c( "X1f4aa", "X1f621")
bc$emojidesc <-top.emojis[bc$code,]$meaning
letters <- c(LETTERS,paste(LETTERS,LETTERS, sep=""))
bc$cn <- letters[as.numeric(bc$coa)]

ggplot(bc[bc$code%in%only,], aes(sd,bias.mean,color=emojidesc)) +  geom_point( alpha = .90)+geom_text(aes(label=emojidesc),hjust=0, vjust=0) + theme(legend.position="none")

ggplot(bc[bc$code%in%only,], aes(sd,bias.mean,color=emojidesc)) +  geom_point( alpha = .90)+geom_text(aes(label=cn),hjust=0, vjust=0)+ylab("mean happiness of company")

both <- intersect(bc[bc$code%in%c("X1f4aa"),]$coa,bc[bc$code%in%c("X1f621"),]$coa)
ggplot(bc[bc$code%in%only&bc$coa%in%both,], aes(sd,bias.mean,color=emojidesc)) + geom_point( alpha = .90)+geom_text(aes(label=cn),hjust=0, vjust=0) +stat_ellipse(type = "norm", linetype = 3)
## fig 9 end ##


## fig 10 ##
MIN =1
MAX = 22
mylabels <- unique(top.emojis[MIN:MAX,]$meaning)
top.emoji.codes <- rownames(top.emojis[MIN:MAX,])
vt$XALL <- as.integer(as.logical(vt$comment.has.emoji))
idx <- match(c(top.emoji.codes,"bias","date","XALL"), names(vt))
vt[1,]
vt2 <- vt[,idx] 
vt2[1,] ## discard low freq emoji before melt
mdata <- melt(vt2, id=c("bias","date"))
colnames(mdata) <- c("bias","date","emoji","count")

#ggplot(mdata[mdata$count>0,], aes(bias,color=emoji)) +  geom_density(alpha = .30)+scale_color_discrete(labels =mylabels) +facet_grid(emoji~ .) 

show= c("X1f44f","X1f62d","X1f621","XALL")
showlabels = unlist(lapply(show,function(x) mylabels[which(top.emoji.codes==x)]) )
showlabels = c(showlabels,"Top 30 emoji")

ggplot(mdata[mdata$count>0&mdata$emoji%in%show,], aes(bias,color=emoji)) +  geom_density(alpha = .30)+scale_color_discrete(labels = showlabels) + xlab("Happiness bias: happiness - historic average")


### fig 11 ### same for vote
## discard low freq emoji before melt
idx <- match(c(top.emoji.codes,"vote","date","XALL"), names(vt))
vt2 <- vt[,idx] 
vt2[1,] 
mdata <- melt(vt2, id=c("vote","date"))
dim(mdata)
colnames(mdata) <- c("vote","date","emoji","count")

#ggplot(mdata[mdata$count>0&mdata$emoji%in%show,], aes(vote,color=emoji)) +  geom_density(alpha = .30)+scale_color_discrete(labels = showlabels) + xlab("Happiness bias: reported happiness - historic average")

dens <- aggregate(count ~ emoji+vote , mdata[mdata$count>0&mdata$emoji%in%show,], function(x) return(length(x)))
#ggplot(data=dens, aes(x=vote, y=count, fill=emoji)) + geom_bar(stat="identity", position=position_dodge())+facet_grid(emoji~ .) 

#ggplot(data=mdata[mdata$count>0&mdata$emoji%in%show,], aes(x=vote, y=count, fill=emoji)) + geom_bar(stat="identity", position=position_dodge())+scale_fill_discrete(labels =showlabels)

# LOOKS NICER IN THE {1,2,3,4} SCALE OR USE AS FACTORS...
ggplot(mdata[mdata$count>0&mdata$emoji%in%show,], aes(vote,fill=emoji)) +  geom_bar( aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + xlab("Happiness of author ") + ylab("Percent of comments with emoji")+ scale_y_continuous(labels = percent)+facet_grid(~ emoji,drop=T) +scale_fill_discrete(labels =showlabels)

## fig 12 ##
vt$XALL <- as.integer(as.logical(vt$comment.has.emoji))
vt$XALL.no.emoji <- as.integer(!as.logical(vt$comment.has.emoji))
idx <- match(c(top.emoji.codes,"bias","date","XALL","XALL.no.emoji"), names(vt))
vt2 <- vt[,idx] 
vt2[1,] 

mdata <- melt(vt2, id=c("bias","date"))
dim(mdata)
mdata[1,]
colnames(mdata) <- c("bias","date","emoji","count")
bias.mean <- aggregate(bias ~ emoji , mdata[mdata$count>0,], mean)
bias.freq <- aggregate(bias ~ emoji , mdata[mdata$count>0,], function(x) return(length(x)))
bias.sd   <- aggregate(bias ~ emoji , mdata[mdata$count>0,], sd)
bias.mean <- cbind(bias.mean, bias.freq, bias.sd)
summary(bias.mean)

colnames(bias.mean) <- c("code","mean","emoji","count","emoji2","sd")
bias.mean <- bias.mean[with(bias.mean, order(-mean)), ]
bias.mean <- bias.mean[,c(3,2,6,4)]
bias.mean$bias.norm.sd <- bias.mean$mean / bias.mean$sd
bias.mean$desc <- top.emojis[bias.mean$emoji,]$meaning

top.emojis["XALL",]$meaning <- c("All coments with emoji")
top.emojis["XALL",]$emoji.code <- c("XALL")
top.emojis["XALL.no.emoji",]$meaning <- c("All comments with no emoji")
top.emojis["XALL.no.emoji",]$emoji.code <- c("XALL.no.emoji")
top.emoji.codes <- rownames(top.emojis[MIN:MAX,])

bias.mean$desc = unlist(lapply(bias.mean$emoji,function(x) top.emojis[which(rownames(top.emojis)==x),]$meaning) )

#xclude <- c("X1f3fd","X1f3fc","X1f3fb")
#bias.mean <- bias.mean[!bias.mean$emoji %in% xclude,]
bias.mean
print("Table of emoji bias")
print(bias.mean)

bias.mean$emojidesc <-top.emojis[bias.mean$emoji,]$meaning
write.csv(bias.mean[bias.mean$count>30,], file = "bias.mean.csv")
#utf8_print(intToUtf8( as.numeric(paste("0x",(substr(bias.mean$code,2,6)),sep="") ) )) 


ggplot(bias.mean, aes(sd,bias.norm.sd,color=bias.norm.sd,size=log10(count))) +  geom_point( alpha = .90)+geom_text(aes(label=desc),size=3,hjust=0, vjust=	2)+ ylab(" bias / sd") + xlim(0.3,0.7)
+ theme(legend.position="none") 
## fig 12 end


## fig 13 ##
## discard low freq emoji before melt
top.emoji.codes <- rownames(top.emojis[1:50,])
idx <- match(c(top.emoji.codes,"vote","date","XALL","XALL.no.emoji"), names(vt))
vt2 <- vt[,idx] 
mdata <- melt(vt2, id=c("vote","date"))
colnames(mdata) <- c("vote","date","emoji","count")
sd(na.omit(vt2$vote))

vote.mean <- aggregate(vote ~ emoji , mdata[mdata$count>0,], mean)
vote.freq <- aggregate(vote ~ emoji , mdata[mdata$count>0,], function(x) return(length(x)))
vote.sd <- aggregate(vote ~ emoji , mdata[mdata$count>0,], sd)
vote.mean <- cbind(vote.mean, vote.freq, vote.sd)
summary(vote.mean)
colnames(vote.mean) <- c("code","mean","emoji","count","emoji2","sd")
vote.mean <- vote.mean[with(vote.mean, order(-mean)), ]
vote.mean <- vote.mean[,c(3,2,6,4)]
vote.mean$desc = unlist(lapply(vote.mean$emoji,function(x) top.emojis[which(rownames(top.emojis)==x),]$meaning) )

#xclude <- c("X1f3fd","X1f3fc","X1f3fb","XALL")
#vote.mean <- vote.mean[!vote.mean$emoji %in% xclude,]
ggplot(vote.mean, aes(sd,mean,color=mean,size=log10(count))) +  geom_point( alpha = .90)+geom_text(aes(label=desc),size=3,hjust=0, vjust=	2)+ ylab(" mean happiness")
# fig 13 end ##



## table 6 comparo table ##
ijs = read.csv("../input/ijstable.csv",stringsAsFactors=FALSE)
summary(ijs)
colnames(ijs)
ijs$emoji <- gsub("0x1f", "X1f", ijs$Unicode,ignore.case = FALSE)
ijs[1:4,]
ijs[ijs$emoji=="X1f625",]
ijs$Sentiment.score <- as.numeric(paste(ijs$Sentiment.score))
summary(ijs$Sentiment.score)
ijs2 <- as.data.frame(  cbind(ijs$emoji,ijs$Sentiment.score,ijs$Unicode.name,ijs$Char)  )
ijs2[1:3,]
colnames(ijs2) <- c("emoji","s","description","Char")
m <- merge(x=vote.mean,y=ijs2, by="emoji",remove.na=TRUE)
m[1:2,]
m$desc <- NULL
m <- cbind(m[,1:2],m[,5],m[,3:4],m[,6:7])
colnames(m)[3] <- c("s.reader")
colnames(m)[2] <- c("s.writer")
colnames(m)[6] <- c("description")
colnames(m)[7] <- c("Char")
m$s.reader <- as.numeric(paste(m$s.reader))
summary(m)
m$diff <- ( m$s.reader - m$s.writer )
m <- m[with(m, order(-s.writer)), ]
write.csv(m[m$count>25,], file = "readervswriter.csv")
write.xls(m[m$count>25,], file = "readervswriter.csv")

utf8_print(intToUtf8( as.numeric(paste("0x",(substr(m$emoji,2,6)),sep="") ) )) 
utg8_print(m$Char)
ggplot(m[m$count>25,], aes(x=s.reader,y=s.author,color=as.factor(diff>0),size=log10(count)))+ scale_colour_brewer(palette = "Set1") +geom_abline(slope=1, intercept=0, color="grey")+  geom_point( alpha = .90)+geom_text(aes(label=description),size=3,hjust=0, vjust=	2)+ ylab("s writer") + xlab("s reader")+xlim(c(-0.5,1))+annotate("text", x = -0.25, y = 0.78,  label = "s writer > s reader ", parse = FALSE,color="grey")+annotate("text", x = 0.75, y = -0.5,  label = "s writer < s reader ", parse = FALSE,color="grey")



ks.test(rnorm(341, mean = 0.122, sd = 0.6), rnorm(60, mean = -0.27, sd = 0.6), alternative = "less")
fit <- lm(s.writer ~ s.reader,data=m) 
summary(fit)


