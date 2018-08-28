:<<!
c2 <- subset(countries,Year=2009)
c2 <- c2[complete.cases(c2),]
set.seed(201)
c2 <- c2[sample(1:nrow(c2),25),]
rownames(c2) <- c2$Name
c3 <- scale(c2)
hc <- hclust(dist(c3))
plot(hc,hang =-1)

## 243页
!
s-plot hcluster_gg -Q hcluster_gg.col_anno.data  -f hcluster_gg.data  -H TRUE
