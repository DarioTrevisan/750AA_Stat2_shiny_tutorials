data <- data.frame(x=runif(10), y=runif(10))
k=4


hca <- agnes(data, method="complete")


clust <- cutree(hca, k)
dendr    <- dendro_data(hca, type="rectangle") # convert for ggplot
clust.df <- data.frame(label=rownames(data), cluster=factor(clust))
dendr[["labels"]]   <- merge(dendr[["labels"]],clust.df, by="label")
rect <- aggregate(x~cluster,label(dendr),range)
rect <- data.frame(rect$cluster,rect$x)
ymax <- mean(hca$height[length(hca$height)-((k-2):(k-1))])

ggplot() +
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_label(data=label(dendr), aes(x, y, label=label, hjust=0, fill=cluster), 
            size=5, colour="white", fontface = "bold") +
  geom_rect(data=rect, aes(xmin=X1-0.1, xmax=X2+0.1, ymin=0.03, ymax=ymax, color=rect.cluster, fill=rect.cluster), 
             alpha=0.2)+
  coord_flip() +
  scale_y_reverse(expand=c(0.2, 0)) + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")
