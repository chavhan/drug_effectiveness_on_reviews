#install.packages('ape')
library('ape')
data(USArrests)
str(USArrests)
?hclust
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

head(hc)
plot(hc, hang = -1,cex=0.6)
class(hc)
hcd <- as.dendrogram(hc)
plot(hcd, type = "rectangle", ylab = "Height")
plot(hcd, type = "triangle", ylab = "Height")

plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

plot(as.phylo(hc), type = "fan")

###################### Clusters on train dataset ##########################
dim(train)
df <- train
library(dplyr)
library(tidyr)
df <- drop_na(df)
drug_train <- df[1:1000,]
names(drug_train)
drug_train <- drug_train[,c(-1,-4,-6)]
drug_train$drugNameNumeric <- as.numeric(as.factor(drug_train$drugName))
drug_train$conditionNumeric <- as.numeric(as.factor(drug_train$condition))
df1 <- drug_train[,c(-1)]
names(df1)
class(df1)

dim(df1)
df2 <- df1[!duplicated(df1[ , c("condition")]),]
dim(df2)
head(df2)
row.names(df2) <- df2[,1]
df2_dist <- dist(as.matrix(df2),method = 'euclidean')
df1hc <- hclust(df2_dist, method = "ward.D2")
head(df1hc)
class(mydata)
colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(df1hc), type = "fan",tip.color = colors[clus4],
      cex = 0.8)
?plot

class(df2)
head(df2)
drug_labels <- df2[1:100,1]
head(drug_labels)
str(drug_labels)
(length(drug_labels))
drug_labels[100-10]

index <- 1
for(i in seq(1,110,10))
{
  for(j in seq(1,10))
  {
    teststr <- paste(i,index,sep = "_")
    print(teststr)
    if(index > 10)
    {
      myedges$to[index] <- drug_labels[index-10]
    }
    index <- index+1
    
  }
}
dim(myedges)
head(myedges,30)
tail(myedges)
myedges$to[11]

myvertices = data.frame(
  name = unique(c(as.character(myedges$from), as.character(myedges$to))) , 
  value = runif(111)
) 
dim(myvertices)
head(myvertices,30)
# Let's add a column with the group of each name. It will be useful later to color points
myvertices$group = myedges$from[ match( myvertices$name, myedges$to ) ]



#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
myvertices$id=NA
myleaves=which(is.na( match(myvertices$name, myedges$from) ))
head(myleaves)
str(myleaves)
dim(myleaves)
nleaves=length(myleaves)
nleaves
myvertices$id[myleaves] = seq(1:nleaves)
myvertices$id[15]
head(myvertices,20)
tail(myvertices,20)
myvertices$angle= 90 - 360 * myvertices$id / nleaves


# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
myvertices$hjust<-ifelse( myvertices$angle < 90, 1, 0)
head(myvertices$hjust,100)
# flip angle BY to make them readable
myvertices$angle<-ifelse(myvertices$angle < -90, myvertices$angle+180, myvertices$angle)
head(myvertices$angle,100)

head(myvertices,30)