data("USArrests")
head(USArrests)
dim(USArrests)
set.seed(123)
clust_dist<-USArrests
### standarization is transforming of variables where your means becomes 0 and sd 1
###scale function can be used for that
scaled.clust<-round((scale(clust_dist)),2)
head(scaled.clust)


###............................
##two compute distances we can use dist() and daisy() in cluster package
dist.euclidean<-dist(scaled.clust,method = "euclidean")
##compute correlation matrix
dist.cor<-cor(t(scaled.clust),method = "pearson")
dist.cor
library(cluster)
daisy.cor<-as.matrix(daisy(scaled.clust))
head(round(daisy.cor),2)

#### lets visualise using corrplot
library(corrplot)
corrplot(as.matrix(scaled.clust[1:10,]),is.corr = F,method = "circle",type="lower")

#########factoextra for visiualisation and cluster for clustering
library(cluster)
library("factoextra")


########################################################################
############K-Means and PAM clustering##################################
####################################################################

fviz_nbclust(scaled.clust,kmeans,method = "gap_stat")
fviz_nbclust(scaled.clust,FUNcluster =kmeans,"wss" )+geom_vline(xintercept = 4,linetype=2)###wss for elbow method
####determine optimal no of cluster
km.res<-kmeans(scaled.clust,4,nstart = 20)
km.res
fviz_cluster(km.res,data=scaled.clust,frame.type ="convex")+theme_minimal()
##### lets do PAM clustering , it is robust to outliers and can handle bigger data sets
pam.res<-pam(scaled.clust,4)
fviz_cluster(pam.res)
#########lets do CLARA clustering, for large data sets, it uses various samples of data and applies PAM clustering on them
#####PAM may need too much memory or computation time so clara must be prefered
###nstart should be >1, because we have to minimise  total within ness
x <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300,50,8), rnorm(300,50,8)))
fviz_nbclust(x,clara,method = "wss")
clara.res<-clara(x,2,sample=50)
fviz_cluster(clara.res,geom="point",pointsize = 1)

######################################################################
##############Hieracrchical Clustering###############################
#####################################################################
hclust_data<-data("USArrests")
hclust_data<-scale(USAccDeaths)
d<-dist(hclust_data,method = "manhattan")
res.hclust<-hclust(d,method ="ward.D2")
####using Ward's method
d1<-dist(hclust_data,method = "euclidean")
res1.hclust<-hclust(d1,method="ward.D2")
#####visualization and adding rectangle
plot(res.hclust,cex=0.5)
rect.hclust(res.hclust,k=4,border=2:5)
plot(res1.hclust,cex=0.6)
rect.hclust(res1.hclust,k=4,border=2:5)

###############
library(factoextra)
res<-hcut(USArrests,k=4,stand = T,hc_func = "diana",hc_method = "ward.D2")
fviz_dend(res,rect = T,color_labels_by_k = T,cex=0.5)

##################
##########checking the cluster tendecy, if data is clusterable(using Hopkins Stastic<0.5)
my_data<-dist(iris[,-5])
get_clust_tendency(my_data,n=50,gradient = list(low="steelblue",high="white"))
fviz_nbclust(my_data,kmeans,method = "gap_stat")
library(NbClust)
####provides metrics for no of clusters
res.nbclust<-NbClust(my_data,distance = "euclidean",min.nc = 2,max.nc = 10,method="complete",index = "all")
res.nbclust

fviz_nbclust(res.nbclust)
####optimal clusters is 3
####Finding the best method for clustering for evaluation of clustering results
clustr_stats<-cluster.stats(d=my_data,km.res$cluster)
clustr_stats$dunn
####for external evaluation Rand index
####finding the appropriate clustering technique
install.packages("clValid")
library(clValid)
intern<-clValid(my_data, nClust = 2:6, 
                clMethods = c("hierarchical","kmeans","pam"),
                validation = "internal")
