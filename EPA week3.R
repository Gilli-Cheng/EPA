
set.seed(12345)
par(mar = c(5,5,4,4))

matrix <- matrix(rnorm(400), nrow = 40)
image(1:10,1:40,t(matrix)[ ,nrow(matrix):1])

heatmap(matrix)

set.seed(678910)
matrix2 <- matrix
for (i in 1:40) {
        coinflip <- rbinom(1,1,prob = 0.5)
        if(coinflip){
                matrix2[i,] <- matrix[i,]+rep(c(0,3),each = 5)
        }
        
}
# par(mar = rep(0.2,4))
str(matrix)
image(1:10,1:40,t(matrix)[10:1,])
image(1:10,1:40,t(matrix)[ ,nrow(matrix):1])
image(1:10,1:40,t(matrix2)[ ,nrow(matrix):1])

heatmap(matrix2)

# darker color means larger numbers
# image(matrix(c(-100, 100)))
# m <- matrix(1:12, 4)
# image(t(m)[, nrow(m):1])

cluster <- hclust(dist(matrix2))
plot(cluster)
#names(cluster)
matrixOrder <- matrix2[cluster$order, ]

image(matrixOrder) # cols on the left, from bottom to top, rows read >>>
image(t(matrixOrder)) # cols on the bottom from left to right,rows read ^^^

windows(480,480)
par(mfrow = c(1,3),mar = c(2,2,2,1))

image(t(matrixOrder)[,nrow(matrixOrder):1]) # cols on top, from left to right, rows read downwards
# mean(matrixOrder[1, ])
# mean(matrixOrder[ ,1])

# plot(rowMeans(matrixOrder),40:1, ,xlab = "Row Mean",ylab = "Row",pch = 19)
plot(rowMeans(matrixOrder),40:1, ,xlab = "Row Mean",ylab = "Row",pch = 19)
plot(colMeans(matrixOrder),xlab = "Column",ylab = "Column Mean",pch = 19)

#PCA

windows(960,480)
par(mfrow = c(1,3),mar = c(4,4,2,1))

svd1 <- svd(scale(matrixOrder))
image(t(matrixOrder)[,nrow(matrixOrder):1])
plot(svd1$u[,1],40:1, ,xlab = "Row",ylab = "First left Singular Vector(U)",
     pch = 19)
plot(svd1$v[,1],xlab = "Column",ylab = "First right sungular vector(v)",pch = 19)

dev.cur()
par(mfrow = c(1,2))
graphics.off()
plot(svd1$d,xlab = "Column", ylab = "Singular Value",pch = 19)
plot(svd1$d^2/sum(svd1$d^2),xlab = "Column",ylab = "Prop. of variance explained",
     pch = 19, ylim = c(0,0.4))

#colors()
#install.packages("RColorBrewer")
library(RColorBrewer)
data("volcano")
# pick a palette
cols <- brewer.pal(9,"PuBu")
pal <- colorRampPalette(cols)
image(volcano,col = pal(100))
graphics.off()

#smooth scatter plot
x <- rnorm(1000)
y <- rnorm(1000)
graphics.off()
par(mar = c(2,2,2,1),mfrow = c(2,2))

pal <- colorRamp(c("red","blue"))
pal(1)
# plot(x,y,pch = 19,col = rgb(1,0,1,0.2))
smoothScatter(x,y)
plot(x,y,pch = 19,col = rgb(0,0,0,0.2))

