#'@title Detailed steps useful in survey analysis
#'
#'@description Using the eigen values greater than 1, applying principal component analysis and factor analysis, cluster analysis is carried out.
#'@param data Data frame with only numeric or integer variables without any missing values
#'@param clusters The number of clusters required in further analysis
#'@param threshold The cut off point in carrying out the factor analysis
#'@param seed Enter a certain seed number in order to get a stable result
#'@param method Enter the method of clustering analysis(kmeans,kmodes or hierarchical)
#'@export
#'@return NULL
#'@examples \dontrun{
#'# You dont have to run this
#'}
#' data<-read.csv("train.csv",header=TRUE)
#' suranal(data,clusters=5,threshold=0.3,seed=123,method="hierarchical")
suranal <- function(data, clusters = 4, threshold = 0.25 , seed = 111, method = "kmeans"){

library("klaR")
library("FactoMineR")
#calculate the number of factors that will be applicable in factor analysis
data <- data.frame(sapply(data, function(x) as.numeric(as.character(x))))
pca <- PCA(data)
eigen_values=pca$eig[,1]
E=length(eigen_values[eigen_values>1])

#convert a data frame to a numeric matrix
attach(data)
cols<- data.matrix(data, rownames.force = NA)

#factor analysis on the dataset
fact_data<- factanal(cols,factor =E,rotation = "varimax")
print(fact_data, digits = 3,cutoff=threshold, sort=TRUE)


#converting into a new dataframe after removing the redundant variables
row_out <- row(fact_data$loadings)[which(abs(fact_data$loadings) < threshold)]
row_check <- table(row_out)>E-1
row_no <- names(row_check[row_check])
newdata <- data[-as.numeric(row_no)]


#applying the clustering analysis algorithm
if(method=="kmeans"){
  data_kmeans <- kmeans(newdata,clusters)
  print(data_kmeans)
} else if (method=="kmodes") {
  data_kmodes<- kmodes(newdata,clusters)
  print(data_kmodes)
} else {(method=="hierarchical")
  hclusters <- hclust(dist(newdata))
  set.seed(seed)
  data_hcluster <- cutree(hclusters, clusters)
  aggregate(newdata,list(data_hcluster),mean)
  print(data_hcluster)}
 return(NULL)
}
