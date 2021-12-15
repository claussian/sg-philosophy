library(cluster)
library(factoextra)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(scales)


b <- read.csv('data/beliefs_survey.csv', header=T)

# Extract question columns 
scol <- names(b)[names(b)!='Timestamp'] 

#optimal number of clusters
fviz_nbclust(b[,scol], kmeans, method = "silhouette")

# standardize variables
zb <- (b[,scol[1]] - mean(b[,scol[1]])) / sd(b[,scol[1]])
for (i in 2:length(scol)) {
  zb <- cbind(zb, (b[,scol[i]] - mean(b[,scol[i]])) / sd(b[,scol[i]]))
}
fviz_nbclust(zb, kmeans, method = "silhouette")

# dissimilarity matrix
d <- daisy(b[,scol])
zd <- daisy(zb)

# MDS
m <- cmdscale(d,eig=TRUE, k=2)
zm <- cmdscale(zd,eig=TRUE, k=2)

# Plot
x <- zm$points[,1]
y <- zm$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Metric MDS", bty='n')

# Try n pam clusters
n = 2
zp <- pam(zd, n, diss = T)

# Plot
x <- zm$points[,1]
y <- zm$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Metric MDS", col=factor(zp$clustering), bty='n')

# Try n k-means clusters
zk <- kmeans(zd, n)

# Plot
par(mfrow = c(1,1), col.lab='black', col.axis='black')
par(mar=c(4.5,4.5,2.5,2.5))
x <- zm$points[,1]
y <- zm$points[,2]
pal <- brewer.pal(3,'Set1')
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Multidimensional scaling\n15 questions on 2 dimensions", col=pal[factor(zk$cluster)], bty='n', pch=19)
clustnames <- c("Skeptics (n=40)", "Believers (n=39)")
legend(x="top", legend = clustnames, fill = pal[1:2], bty='n', cex=.8)

# Use k-means clusters and plot distributions for each question using data.table
bdt <- data.table(b)
qv<- data.table(read.csv('data/beliefs_survey_qns.csv')) # actual questions (verbose)
plot_cluster_dist <- function(data, clusters, i) {
  qn <- paste0("Q",i)
  bgrp <- data[, .(.N), keyby = .(cluster=factor(paste0("C",clusters)),score=get(qn))]
  bgrpw <- dcast(bgrp, score ~ cluster, value.var = "N")
  clust <- names(bgrpw)[names(bgrpw)!="score"]
  for (c in clust) {
    p <- quote(paste0(c, "p"))
    bgrpw[,eval(p)] <- bgrpw[,get(c)]/sum(bgrpw[,get(c)], na.rm = T) 
  }
  pclust <- names(bgrpw)[names(bgrpw) %like% "p"]
  bgrpf <- data.frame(bgrpw)
  
  # Plot
  pal <- brewer.pal(3,'Set1')
  bplot <- as.matrix(bgrpf[,pclust])
  rownames(bplot) <- bgrpf$score
  bplot <- t(bplot)
  x <- barplot(bplot*100, beside=T,
          main = paste0(qn,":\n", qv[index==qn,"question"]),
          cex.main=.9, cex.lab=.9,cex.axis = .8,
          ylim = c(0,(max(bplot,na.rm = T)+.05)*100),
          yaxt='n',
          xlab = "Strongly disagree=1; strongly agree=5",
          col = pal[1:nrow(bplot)])
  y <- bplot
  text(x,y*100+3, labels=as.character(paste0(round(y*100,0),"%")), cex = .8)
  result <- list()
  result$bplot <- bplot
  result$bgrpf <- bgrpf
  return(result)
}

# Plot in grid
par(mfrow = c(2,1), col.lab='black', col.axis='black')
par(mar=c(4.5,2.5,2.5,1.5))
labels <- list()
labels[["C1"]] = "Skeptics"
labels[["C2"]] = "Believers"

# Plot questions by category of belief: (1,2), (3,4), (5,6), (7,8,9), (10,11), (12,13), (14,15)
for (i in c(1:2)) {
  result <- plot_cluster_dist(bdt, zk$cluster, i)
  if(i==1) {
    clustnames <- c()
    for (r in rownames(result$bplot)) {
      clustname <- gsub("p","",r)
      csum <- sum(result$bgrpf[,clustname],na.rm=T)
      clustnames <- c(clustnames, paste0(labels[[clustname]]," (n=",csum,")"))
    }
    legend(x="top", legend = clustnames, fill = pal[1:nrow(bplot)], bty='n', cex=.8)
  }
}

# Part II: Subdivide each major cluster

plot_mds <- function(data, n, palcol, major_cluster) {
  # standardize variables
  zb <- (data[,scol[1]] - mean(data[,scol[1]])) / sd(data[,scol[1]])
  for (i in 2:length(scol)) {
    zb <- cbind(zb, (data[,scol[i]] - mean(data[,scol[i]])) / sd(data[,scol[i]]))
  }
  # dissimilarity matrix
  d <- daisy(data[,scol])
  zd <- daisy(zb)
  
  # MDS
  m <- cmdscale(d,eig=TRUE, k=2)
  zm <- cmdscale(zd,eig=TRUE, k=2)
  
  # k-means clusters
  k <- kmeans(d, n)
  zk <- kmeans(zd, n)
  
  # pam clusters
  p <- pam(d, n)
  zp <- pam(zd, n)
  
  # Plot
  par(mfrow = c(1,1), col.lab='black', col.axis='black')
  par(mar=c(4.5,4.5,2.5,2.5))
  x <- zm$points[,1]
  y <- zm$points[,2]
  pal <- rev(brewer.pal(4, palcol))
  clusters <- zk$cluster
  fviz_nbclust(zb, kmeans, method = "silhouette")
  plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
       main=paste0("Multidimensional scaling\n15 questions on 2 dimensions: ",major_cluster), col=pal[factor(clusters)], bty='n', pch=19)
  clustnames <- c("C1", "C2")
  legend(x="top", legend = clustnames, fill = pal[1:2], bty='n', cex=.8)
}

n = 2
colpal = 'Reds'
b$main_cluster <- zk$cluster
plot_mds(b[b$main_cluster==1,], n, colpal, 'Skeptics')

colpal = 'Blues'
plot_mds(b[b$main_cluster==2,], n, colpal, 'Believers')

# Part III: PCA
res.pca <- prcomp(b[,scol], scale = TRUE)

# Scree plot
fviz_eig(res.pca)

# Cumulative explained variance
var <- res.pca$sdev^2
cumvar <- cumsum(var/sum(var))
x <- c(1:length(cumvar))
y <- cumvar*100
par(mar=c(4.5,4.5,3.5,2.5), oma=c(0,0,0,0))
plot(x,y,
     xlim = c(1,15),ylim = c(20,105),
     xlab = 'No. of dimensions', ylab = 'Cumulative explained variance (%)',
     main = 'Cumulative explained variance\nby number of dimensions of belief',
     bty='n',
     col='darkblue',
     lwd=1.5,
     cex=.8,
     pch=19,
     type='o')
text(x,y+4, labels=as.character(paste0(round(y,0),"%")), cex = .8, col='darkblue')
abline(v=8, lty=2)
abline(v=4, lty=2)


# Plot loadings on each dimension from 1 to 8
fviz_pca_var(res.pca, axes=c(1,2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Check the relationship between Q5 and Q15
par(mar=c(4.5,5.5,3.5,2.5), oma=c(0,0,0,0))
plot(jitter(b$Q5), jitter(b$Q15),
     main = 'Response to Q5 vs. Q15',
     xlab = 'Q5. I believe the external world really exists (realism)\nStrongly disagree=1; strongly agree=5',
     ylab = 'Q15. I believe that betterment is a critical life goal\nStrongly disagree=1; strongly agree=5',
     type='p', cex=.8, pch=19, col='darkred', bty='n')
m1 <- lm(Q15 ~ Q5, data = b)
abline(m1, col='black', lty=1, lwd=2)
