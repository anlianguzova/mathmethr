library(DAAG)
library(ggplot2)
library(dplyr)
library(factoextra)
library(dbscan)
data(fossum)

## Создаем "говорящие" названия строк

rownames(fossum) # Было

# Чтобы имена строк были более информативны,
# добавим к ним название популяции
rownames(fossum) <- paste(fossum$Pop,
                          rownames(fossum),
                          sep = "_")
rownames(fossum) # стало

## Отбираем только то, что понадобится
# для кластеризации
fos <- fossum[complete.cases(fossum), 6:14]
f_sc <- scale(fos)


# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(fos, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss,
    linetype = 'dashed',
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )

k <- 5
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(fos, centers = k, nstart = 20)
str(km.out)

my_col <- c("#2E9FDF", "#FF5AD9", "#B5FF0A", "#F37352", "#E7B800")

fviz_cluster(km.out, data = fos,
             palette = my_col,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

fviz_cluster(km.out, data = fos,
             palette = my_col)


data_k <- data.frame(X = c(1.4, 1.8, 2.5, 3, 2.3, 1.8,
                           3, 2.8, 3.5, 4, 4.2,
                           7, 8.2, 7.5, 8.4, 8, 7.7, 9),
                     Y = c(1, 1.5, 3.7, 2.8, 2.5, 3,
                           4.8, 5.2, 7, 6.3, 5.9,
                           5, 5.2, 5.8, 6.7, 6.3, 7, 6.9),
                     Col = c(1, 0, 0, 0, 2, 0,
                             0, 0, 0, 0, 0,
                             0, 0, 0, 0, 3, 0, 0),
                     Size = c(1, 0, 0, 0, 1, 0,
                              0, 0, 0, 0, 0,
                              0, 0, 0, 0, 1, 0, 0))


gg_kmeans <- ggplot(data_k, aes(X, Y)) +
  geom_point(size = 3) + theme_bw()

gg_kmeans +
  geom_point(aes(col = factor(Col), size = Size)) +
  scale_colour_manual(values = c("#000000", "#A1FFA1",
                                 "#A1C7FF", "#FFA1DA")) +
  scale_size(range = c(3, 8)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")


data_k2 <- data.frame(X = c(1.4, 1.8, 2.5, 3, 2.3, 1.8,
                           3, 2.8, 3.5, 4, 4.2,
                           7, 8.2, 7.5, 8.4, 8, 7.7, 9),
                     Y = c(1, 1.5, 3.7, 2.8, 2.5, 3,
                           4.8, 5.2, 7, 6.3, 5.9,
                           5, 5.2, 5.8, 6.7, 6.3, 7, 6.9),
                     Col = c(1, 1, 0, 2, 2, 0,
                             0, 0, 0, 0, 0,
                             0, 0, 0, 3, 3, 0, 0),
                     Size = c(1, 0, 0, 0, 1, 0,
                              0, 0, 0, 0, 0,
                              0, 0, 0, 0, 1, 0, 0))


ggplot(data_k2, aes(X, Y, col = factor(Col), size = Size)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("#000000", "#A1FFA1",
                                "#A1C7FF", "#FFA1DA")) +
  scale_size(range = c(3, 8)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")


data_k3 <- data.frame(X = c(1.4, 1.8, 2.5, 3, 2.3, 1.8,
                            3, 2.8, 3.5, 4, 4.2,
                            7, 8.2, 7.5, 8.4, 8, 7.7, 9),
                      Y = c(1, 1.5, 3.7, 2.8, 2.5, 3,
                            4.8, 5.2, 7, 6.3, 5.9,
                            5, 5.2, 5.8, 6.7, 6.3, 7, 6.9),
                      Col = c(1, 1, 2, 2, 2, 2,
                              2, 2, 3, 3, 3,
                              3, 3, 3, 3, 3, 3, 3),
                      Size = c(1, 0, 0, 0, 1, 0,
                               0, 0, 0, 0, 0,
                               0, 0, 0, 0, 1, 0, 0))

gg_new_centr <- ggplot(data_k3, aes(X, Y, col = factor(Col), size = Size)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("#A1FFA1",
                                 "#A1C7FF", "#FFA1DA")) +
  scale_size(range = c(3, 8)) +
  annotate("text", x = mean(data_k3$X[1:2]), y = mean(data_k3$Y[1:2]), label = "✗") +
  annotate("text", x = mean(data_k3$X[3:8]), y = mean(data_k3$Y[3:8]), label = "✗") +
  annotate("text", x = mean(data_k3$X[9:18]), y = mean(data_k3$Y[9:18]), label = "✗") +
  theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
    legend.position = "none")


gg_dist_new <-  gg_new_centr +
  geom_segment(aes(X[1], y = Y[1], xend = mean(X[1:2]), yend = mean(Y[1:2])),
               linewidth = 0.5, colour = "black") +
  geom_segment(aes(X[4], y = Y[4], xend = mean(X[3:8]), yend = mean(Y[3:8])),
               linewidth = 0.5, colour = "black") +
  geom_segment(aes(X[12], y = Y[12], xend = mean(X[9:18]), yend = mean(Y[9:18])),
               linewidth = 0.5, colour = "black")

gg_dist_new

ggplot(data_k3, aes(X, Y, col = factor(Col), size = Size)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("#A1FFA1",
                                 "#A1C7FF", "#FFA1DA")) +
  scale_size(range = c(3, 8)) +
  annotate("text", x = mean(data_k3$X[1:2]), y = mean(data_k3$Y[1:2]), label = "✗") +
  annotate("text", x = mean(data_k3$X[3:8]), y = mean(data_k3$Y[3:8]), label = "✗") +
  annotate("text", x = mean(data_k3$X[9:18]), y = mean(data_k3$Y[9:18]), label = "✗") +
  geom_segment(aes(X[1], y = Y[1], xend = mean(X[1:2]), yend = mean(Y[1:2])), linewidth = 0.5, colour = "black") +
  geom_segment(aes(X[4], y = Y[4], xend = mean(X[3:8]), yend = mean(Y[3:8])), linewidth = 0.5, colour = "black") +
  geom_segment(aes(X[12], y = Y[12], xend = mean(X[9:18]), yend = mean(Y[9:18])), linewidth = 0.5, colour = "black") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

data_k_final <- data.frame(X = c(1.4, 1.8, 2.5, 3, 2.3, 1.8,
                           3, 2.8, 3.5, 4, 4.2,
                           7, 8.2, 7.5, 8.4, 8, 7.7, 9),
                     Y = c(1, 1.5, 3.7, 2.8, 2.5, 3,
                           4.8, 5.2, 7, 6.3, 5.9,
                           5, 5.2, 5.8, 6.7, 6.3, 7, 6.9),
                     Col = c(1, 1, 1, 1, 1, 1,
                             2, 2, 2, 2, 2,
                             3, 3, 3, 3, 3, 3, 3))

gg_k_final <-  ggplot(data_k_final, aes(X, Y, col = factor(Col))) +
  geom_point(size = 3) +
  theme_bw() +
  scale_colour_manual(values = c("#A1FFA1", "#A1C7FF", "#FFA1DA")) +
  annotate("text", x = mean(data_k_final$X[1:6]), y = mean(data_k_final$Y[1:6]),
           label = "✗") +
  annotate("text", x = mean(data_k_final$X[7:11]), y = mean(data_k_final$Y[7:11]),
           label = "✗") +
  annotate("text", x = mean(data_k_final$X[12:18]), y = mean(data_k_final$Y[12:18]),
           label = "✗") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")
gg_k_final


# (p <- qplot(mpg, cyl, data=mtcars, size=cyl))
# p + scale_size("cylinders")
# p + scale_size("number\nof\ncylinders")
#
# p + scale_size(range = c(0, 10))
# p + scale_size(range = c(1, 2))
#
# # Map area, instead of width/radius
# # Perceptually, this is a little better
# p + scale_size_area()
# p + scale_size_area(max_size = 25)
#
# # Also works with factors, but not a terribly good
# # idea, unless your factor is ordered, as in this example
# qplot(mpg, cyl, data=mtcars, size=factor(cyl))
#
# # To control the size mapping for discrete variable, use
# # scale_size_manual:
# last_plot() + scale_size_manual(values=c(2,4,6))


## dbscan ####
library(dbscan)
library(factoextra)
library(dplyr)

data('multishapes')

multi_circle <- multishapes %>% filter(shape == c(1, 2))
multi <- multi_circle[, 1:2]

ggplot(multi, aes(x, y)) + geom_point()

# k-means does not work
set.seed(123)
circle_kmeans <- kmeans(multi, centers = 2, nstart = 20)
my_col_circle <- c("#2E9FDF", "#E7B800")
fviz_cluster(circle_kmeans, data = multi,
             palette = my_col_circle)



kNNdistplot(multi, k = 5)
abline(h = 0.23, lty = 2)

circle_dbscan <- dbscan(multi, 0.23, 5)
fviz_cluster(circle_dbscan, data = multi,
             palette = my_col_circle)


kNNdistplot(f_sc, k = 5)
abline(h = 2.3, lty = 2) # по этой штуке не оч понятно на самом деле
abline(h = 1.9, lty = 2)

set.seed(123)



# dbscan package
res.db <- dbscan(f_sc, 2.3, 5)
fviz_cluster(res.db, data = f_sc,
             palette = my_col)






# но и dbscan хорош для другого

points_in_circum <- function(radius, num_pts, jitter) {

  x <- mapply(function(n) {return (cos(2*pi/num_pts*n) * radius + rnorm(1, -1 * jitter, jitter))}, 1:num_pts)
  y <- mapply(function(n) {return (sin(2*pi/num_pts*n) * radius + rnorm(1, -1 * jitter, jitter))}, 1:num_pts)

  return (data.frame(x=x, y=y))
}


# Build set of concentric points
df <- points_in_circum(100, 300, 30)
df <- rbind(df, points_in_circum(300, 700, 30))
df <- rbind(df, points_in_circum(500, 1000, 30))

# Add noise points
df <- rbind(df, data.frame(x=runif(300, -600, 600), y=runif(300, -600, 600)))

plot(x ~ y, data=df, pch = 20, cex=0.5)

# dbscan-ом
dbs <- dbscan(as.matrix(df), minPts=5, eps=32)

# Mark noise with big crosses
# shapes <- mapply(function(x) { if (x==0) return(3) else return(20) }, dbs$cluster)
# sizes  <- mapply(function(x) { if (x==0) return(1) else return(0.5)}, dbs$cluster)

palette <- rainbow(length(unique(dbs$cluster)))

# Render noise points (cluster 0) in grey
# palette[1] = rgb(0.8, 0.8, 0.8)

plot( x ~ y, data=df, col=palette[dbs$cluster + 1])
