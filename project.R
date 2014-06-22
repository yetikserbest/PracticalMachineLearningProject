library(caret)
# load training data
data<-read.csv("pml-training.csv")
d<-data
# replace empty cells with NA
d[d == ""] <- NA
# find out which columns has NAs
column.has.na <- apply(d, 2, function(x){any(is.na(x))})
sum(column.has.na)
# remove columns with NAs
d.noNA <- d[,!column.has.na]
dim(d)
dim(d.noNA)
# remove first seven column not to be used by the model
d.cleaned <- d.noNA[,-c(1:7)]
# cross validation parameters
tc <- trainControl("cv",10,savePred=T)
# random forest
modfit<-train(classe ~ ., method="rf", trControl=tc, data=d.cleaned)
# load test data and modify it the same way
datatest<-read.csv("pml-testing.csv")
dt <- datatest
dt[dt == ""] <- NA
column.has.na <- apply(dt, 2, function(x){any(is.na(x))})
sum(column.has.na)
dt.noNA <- dt[,!column.has.na]
dim(dt)
dim(dt.noNA)
dt.cleaned <- dt.noNA[,-c(1:7)]
# see results
answers<-predict(modfit,dt.cleaned)
# function to write the asnwers in files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
# writing the answers
pml_write_files(answers)
# variance 
varImp(modfit)
# confusionMatrix
confusionMatrix(modfit)
# plot
plot(modfit)
# pretty cluster plotting function
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
hang = 0.1, ...) {
## modifiction of plclust for plotting hclust objects *in colour*! Copyright
## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
## of labels of the leaves of the tree lab.col: colour for the labels;
## NA=default device foreground colour hang: as in hclust & plclust Side
## effect: A display of hierarchical cluster with coloured leaf labels.
y <- rep(hclust$height, 2)
x <- as.numeric(hclust$merge)
y <- y[which(x < 0)]
x <- x[which(x < 0)]
x <- abs(x)
y <- y[order(x)]
x <- x[order(x)]
plot(hclust, labels = FALSE, hang = hang, ...)
text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}
# cluster plotting
distxy<-dist(d.cleaned[,-ncol(d.cleaned)])
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))
# heatmap plotting
dataMatrix <- as.matrix(dataFrame
heatmap(as.matrix(d.cleaned[,-ncol(d.cleaned)]))

