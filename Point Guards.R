## Lines 1-32 are used to read in my data and use just the career statistics
x <- c("Mike Bibby", "Chauncy Billups", "Mookie Blaylock", "Eric Bledsoe", "Muggsy Bogues"
       , "Terrell Brandon", "Sam Cassell", "Mike Conley"
       , "Stephen Curry", "Baron Davis", "Goran Dragic"
       , "Steve Francis", "Anfernee Hardawayan", "Tim Hardawayti"
       , "Ron Harper", "Jrue Holiday", "Kyrie Irving", "Allen Iverson"
       , "Mark Jackson", "Brandon Jennings", "Kevin Johnsonke"
       , "Magic Johnsonma", "Jason Kidd", "Fat Lever", "Damian Lillard"
       , "Kyle Lowry", "Stephon Marbury", "Andre Miller", "Sidney Moncrief"
       , "Steve Nash", "Tony Parker", "Chris Paul", "Gary Payton"
       , "Terry Porter", "Mark Price", "Rajon Rondo", "Derrick Rose"
       , "Scott Skiles", "Kenny Smith", "John Stockton", "Rod Strickland"
       , "Jeff Teague", "Isiah Thomasisi", "Isaiah Thomasisa", "Nick Vanexil"
       , "John Wall", "Spudd Webb", "Russell Westbrook", "Deron Williams")
number <- c("01_per_game.csv")
players <- c("players")
splitnames <- strsplit(x, split=" ")
unlist(splitnames)
lastNameFirstInitial <- unlist(lapply(splitnames, function(v) v[[2]]))
lastname <- tolower(unlist(lapply(splitnames, function(v) v[[2]])))
fn <- tolower(substr(lastNameFirstInitial, 1, 1))
playername <- paste(players,fn,sep="_",lastname,number)
allcsv <- list()
for (i in playername){
  allcsv[[i]] <- read.csv(i, header = TRUE, stringsAsFactors = FALSE)
}
#output <- data.frame(matrix(unlist(allcsv), nrow = 910), stringsAsFactors = FALSE)
#unlist(allcsv)[1:40]

output <- do.call(rbind.data.frame, allcsv)
career<-output[output[,1]=="Career",]
row.names(career) <- x

## Multiple Linear Regression Model
multlin <- lm(career$PTS ~ career$G + career$GS + career$MP + career$FG + career$FGA + 
               career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P + career$X2PA +
               career$X2P. + career$eFG. + career$FT + career$FTA + career$FT. + career$ORB + 
               career$DRB + career$TRB + career$AST + career$STL + career$BLK + career$TOV + 
               career$PF)
summary(multlin)

## Backwards Selection is performed manually (sorry Jeff), FTA taken out first
multlin <- lm(career$PTS ~ career$G + career$GS + career$MP + career$FG + career$FGA + 
                career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P + career$X2PA +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$AST + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## X2PA Taken out
multlin <- lm(career$PTS ~ career$G + career$GS + career$MP + career$FG + career$FGA + 
                career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$AST + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## AST Taken out
multlin <- lm(career$PTS ~ career$G + career$GS + career$MP + career$FG + career$FGA + 
                career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## GS Taken out
multlin <- lm(career$PTS ~ career$G + career$MP + career$FG + career$FGA + 
                career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## MP Taken out
multlin <- lm(career$PTS ~ career$G + career$FG + career$FGA + 
                career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## FGA Taken out 
multlin <- lm(career$PTS ~ career$G + career$FG + 
                career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## X2P Taken out 
multlin <- lm(career$PTS ~ career$G + career$FG + 
                career$FG. + career$X3P + career$X3PA + career$X3P. +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## 3PA Taken out 
multlin <- lm(career$PTS ~ career$G + career$FG + 
                career$FG. + career$X3P + career$X3P. +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$BLK + career$TOV + 
                career$PF)
summary(multlin)
## BLK Taken out
multlin <- lm(career$PTS ~ career$G + career$FG + 
                career$FG. + career$X3P + career$X3P. +
                career$X2P. + career$eFG. + career$FT + career$FT. + career$ORB + 
                career$DRB + career$TRB + career$STL + career$TOV + 
                career$PF)
summary(multlin)
## FT. Taken out 
multlin <- lm(career$PTS ~ career$G + career$FG + 
                career$FG. + career$X3P + career$X3P. +
                career$X2P. + career$eFG. + career$FT + career$ORB + 
                career$DRB + career$TRB + career$STL + career$TOV + 
                career$PF)
summary(multlin)
## STL Taken out 
multlin <- lm(career$PTS ~ career$G + career$FG + 
                career$FG. + career$X3P + career$X3P. +
                career$X2P. + career$eFG. + career$FT + career$ORB + 
                career$DRB + career$TRB + career$TOV + 
                career$PF)
summary(multlin)
## G Taken out 
multlin <- lm(career$PTS ~ career$FG + 
                career$FG. + career$X3P + career$X3P. +
                career$X2P. + career$eFG. + career$FT + career$ORB + 
                career$DRB + career$TRB + career$TOV + 
                career$PF)
summary(multlin)
## 3PT. Taken out 
multlin <- lm(career$PTS ~ career$FG + 
                career$FG. + career$X3P +
                career$X2P. + career$eFG. + career$FT + career$ORB + 
                career$DRB + career$TRB + career$TOV + 
                career$PF)
summary(multlin)
## FG. Taken out
multlin <- lm(career$PTS ~ career$FG + 
                career$X3P +
                career$X2P. + career$eFG. + career$FT + career$ORB + 
                career$DRB + career$TRB + career$TOV + 
                career$PF)
summary(multlin)

## Hierarchical Clustering
## First, scaling is needed 
scale_pointGuards <- scale(career[6:30])
distpg <- dist(scale_pointGuards)
## Then i create single, average, and complete linkage dendrograms
hc.single <- hclust(distpg, method="single")
hc.average <- hclust(distpg, method="average")
hc.complete <- hclust(distpg, method="complete")
## I plot each type of dendrogram 
plot(hc.single)
plot(hc.average)
plot(hc.complete)

## Random Forests/Regression Tree
library(tree)
## First, i create my tree for AST
pg.treeAST <- tree(career$AST~career$G + career$GS + career$MP + career$FG + career$FGA + 
                  career$FG. + career$X3P + career$X3PA + career$X3P. + career$X2P + career$X2PA +
                  career$X2P. + career$eFG. + career$FT + career$FTA + career$FT. + career$ORB + 
                  career$DRB + career$TRB + career$PTS + career$STL + career$BLK + career$TOV + 
                  career$PF)
plot(pg.treeAST)
text(pg.treeAST, pretty=0)
install.packages("randomForest")
library(randomForest)
set.seed(134891)
## I create my randomforest function for AST 
pgrf <- randomForest(AST~G + GS + MP + FG + FGA + 
                       FG. + X3P + X3PA + X3P. + X2P + X2PA +
                       X2P. + eFG. + FT + FTA + FT. + ORB + 
                       DRB + TRB + PTS + STL + BLK + TOV + 
                       PF, data=career, importance=TRUE)
pgrf
## Then i plot the variable importance graph for AST
varImpPlot(pgrf)

## Neural Networks
## First i load in the necessary packages for NN
library(neuralnet)
library(grid)
library(MASS)
## Over here, i create my training and testing set
set.seed(500)
pg.ind <- sample(1:nrow(career), 30)
pg.train <- career[pg.ind,]
pg.test <- career[-pg.ind,]
## Then i apply the neural networks function 
nn.pg <- neuralnet(pg.train$PTS~pg.train$G + pg.train$GS + pg.train$MP + pg.train$FG + pg.train$FGA + 
                     pg.train$FG. + pg.train$X3P + pg.train$X3PA + pg.train$X3P. + pg.train$X2P + pg.train$X2PA +
                     pg.train$X2P. + pg.train$eFG. + pg.train$FT + pg.train$FTA + pg.train$FT. + pg.train$ORB + 
                     pg.train$DRB + pg.train$TRB + pg.train$AST + pg.train$STL + pg.train$BLK + pg.train$TOV + 
                     pg.train$PF, data=pg.train, linear.output = FALSE, hidden=5)
plot(nn.pg)
## I scale my test set
pg.test_Scale <- scale(pg.test[6:30])
pg.dataframe <- as.data.frame(pg.test_Scale)

nnres <- compute(nn.pg, pg.dataframe$PTS + pg.dataframe$G + pg.dataframe$GS + pg.dataframe$MP + pg.dataframe$FG + pg.dataframe$FGA + 
                   pg.dataframe$FG. + pg.dataframe$X3P + pg.dataframe$X3PA + pg.dataframe$X3P. + pg.dataframe$X2P + pg.dataframe$X2PA +
                   pg.dataframe$X2P. + pg.dataframe$eFG. + pg.dataframe$FT + pg.dataframe$FTA + pg.dataframe$FT. + pg.dataframe$ORB + 
                   pg.dataframe$DRB + pg.dataframe$TRB + pg.dataframe$AST + pg.dataframe$STL + pg.dataframe$BLK + pg.dataframe$TOV + 
                   pg.dataframe$PF)
## For the compute function, even after scaling my data, i still had an
## error that said "non-conformable arguments". So i could not use the
## Compute function. Therefore, i could not include a high quality analysis
## In my report.

##KNN
require("class")
## First, I create a normalizing function to refer my data to
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
## Now i actually normalize my data
pg.norm <- as.data.frame(lapply(career[6:30], normalize))
## Then i create a training and testing set
set.seed(200)
pg.ind <- sample(1:nrow(career), 30)
pg.train <- career[pg.ind, ]
pg.test <- career[-pg.ind, ]
## I create the labels needed for knn
pg.train.labels <- pg.train[,1]
pg.test.labels <- pg.test[,1]
## Now i perform KNN
pg_test_pred <- knn(train=pg.train, test=pg.test, cl=pg.train.labels, k=7)
## Unfortunately, i was still not able to properly perform knn because 
## of a "no missing values are allowed" error. Therefore, i could not 
## provide a good quality analysis on K-nearest neighbhors regression.

## K-means clustering
## First, i standardize/scale my data
pg.s <- scale(career[6:30])
## Then i create a scree plot 
wss=numeric(0)
## This calculation collects quantity called "total within-cluster sum of
## squares" for each number of clusters from 2 to 40.
for (i in 2:20){
  wss[i]=sum(kmeans(pg.s,i)$withinss)
}
plot(2:20,wss[2:20],type="b")
grid(col="black")
## The scree plot is not very helpful, but you can see an elbow at about 6 
## clusters. After 6, the plot goes on a very steady, linear decline. 
## We can consider this as the "scree" and ignore those clusters. So our value 
## for k will be 5 (1 less than the elbow). 
pg.km <- kmeans(pg.s, 5)
pg.km$cluster
pg.km$centers
pg.km$withinss
pg.km$size
## Now lets see which players are in which cluster in order to characterize them
split(rownames(career),pg.km$cluster)


## PCA
## First, scaling is needed
pg.pca <- prcomp(career[6:30], scale.=TRUE)
## A summary and the biplot of the scaled data is loaded
summary(pg.pca)
biplot(pg.pca)
## Now we provide ourselves with the loadings
pg.pca$rotation
round(pg.pca$rotation[,1:2], 2)
## We also provide ourselves with a scree plot for factor analysis
plot(pg.pca, type="lines")
