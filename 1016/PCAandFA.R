food <- read.csv("https://userpage.fu-berlin.de/soga/300/30100_data_sets/food-texture.csv", row.names = "X")
str(food)
#油：糕點中油的百分比
#密度：產品的密度（數字越大，產品越緻密）
#Crispy（脆皮）：脆度測量值，從 7 到 15 分，15 分更脆。
#斷裂：以度為單位的角度，糊狀物在斷裂前可以通過該角度緩慢彎曲。
#硬度：一個尖點用於測量發生斷裂前所需的力的大小。

#### Principal Component Analysis #####
pca.toy <- food[, c('Oil', 'Density')]
plot(pca.toy)


#Scale the data
#The scatter plot above indicates a relationship between the feature Oil and the feature Density. 
#Note that the variables are not on the same scale. In general, the results of PCA depend on the scale of the variables. 
#Therefore, each variable is typically centered and scaled to have a mean of zero and standard deviation of one. 
#In certain settings, however, the variables are measured in the same units, and one may skip the standardization.

#For the sake of comprehensibility we write two simple functions in R and visualize the effects of each pre-processing step. 
#The goal is to center each column to zero mean and then scale it to have unit variance. 
#For further usage we assign a proper variable name (e.g. pca.toy.data) to pre-processed data set.



# Function for centering a vector
center <- function(v){v-mean(v)}
# Function for scaling a vector
scale <- function(v){v/sd(v)}

## save helper functions for later usage
save(center, scale, file = 'helper_functions_30300.RData')

# Apply use defined function on each column of the data set
pca.toy.centered <- apply(pca.toy, MARGIN = 2, FUN = center)
pca.toy.scaled <- apply(pca.toy.centered , MARGIN = 2, FUN = scale)

### Plotting ###
par(mfrow = c(3,2), mar = c(4,4,3,1))
###############
# scatterplot 1
plot(pca.toy, main = 'Raw data')
# calulate mean for visualization
data.mean <- apply(pca.toy, 2, mean)
points(data.mean[1], data.mean[2], col='red', pch=16) # mark mean
# boxplot 1
boxplot(pca.toy, main = 'Raw data')
###############

# scatterplot 2
plot(pca.toy.centered , main = 'Centered data')
# calulate mean for visualization
data.mean <- apply(pca.toy.centered, 2, mean)
points(data.mean[1], data.mean[2], col='red', pch=16) # mark mean
# boxplot 2
boxplot(pca.toy.centered , main = 'Centered data')
###############



# scatterplot 3
plot(pca.toy.scaled, main = 'Scaled and centered data')
# calulate mean for visualization
data.mean <- apply(pca.toy.scaled, 2, mean)
points(data.mean[1], data.mean[2], col='red', pch=16) # mark mean
# boxplot 3
boxplot(pca.toy.scaled, main = 'Scaled and centered data')

# assign propper variable name to pre-processed data set
pca.toy.data <- pca.toy.scaled

# save for later usage
save(pca.toy.data, file = 'pca_food_toy_30300.RData')

library(dplyr)
# center and scale data set 
# useage of the pipe operator %>% provided by the dplyr package
food.pca <- food[,2:ncol(food)] %>%
  apply(MARGIN = 2, FUN = center) %>%
  apply(MARGIN = 2, FUN = scale)
dim(food.pca)

food.pca.eigen <- eigen(cov(food.pca))
food.pca.eigen$values

#Calculation of principal components
#To compute the proportion of variance explained by each principal component, 
#we simply divide the variance explained by each principal component by the total 
#variance explained by all principal components:

food.pca.ve <- food.pca.eigen$values/sum(food.pca.eigen$values)
food.pca.ve

#How many principal components are needed?
#the visual examination of a scree plot
#the variance explained criteria or
#the Kaiser rule.

par(mfrow = c(1,2), mar = c(4,5,3,1))
plot(food.pca.ve,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), 
     type = 'b',
     main = 'Scree plot')

plot(cumsum(food.pca.ve), 
     xlab = "Principal Component", 
     ylab = "Cumulative Proportion of\nVariance Explained", 
     ylim = c(0,1),
     type = 'b',
     main = 'Scree plot')

#The variance explained criteria
cumsum(food.pca.eigen$values/sum(food.pca.eigen$values))

#Kaiser’s rule (Kaiser-Guttman criterion)
food.pca.eigen$values[food.pca.eigen$values >=1] 
#The Kaiser’s rule (Kaiser-Guttman criterion) is a widely used method to evaluate the maximum number of linear combinations 
#to extract from the data set. According to that rule only those principal components are retained, whose variances exceed 1. 
#The idea behind the Kaiser-Guttman criterion is that any principal with variance less than 1 contains less information than 
#one of the original variables and so is not worth retaining (Jolliffe 2002).

food.pca.eigen <- eigen(cov(food.pca))
pca.loading <- food.pca.eigen$vectors[,1:2] # select the first two principal components
colnames(pca.loading) <- c('PC1', 'PC2') # name columns
rownames(pca.loading) <- colnames(food.pca) # name rows
pca.loading

#calculate the scores for our food-texture data set 
Z = food.pca %*% pca.loading
Z
dim(Z)
plot(Z, xlab = 'PC1', ylab = 'PC2')
abline(h = 0, col = "blue")
abline(v = 0, col = "green")
#--------------------------------------#
# load data from previous sections
load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/pca_food_30300.RData"))

food.pca.eigen <- eigen(cov(food.pca))
pca.loading <- food.pca.eigen$vectors[,1:2] # select the first two principal components

pca.scores <- food.pca %*% pca.loading
rownames(pca.scores) <- seq(1, nrow(pca.scores))

# Plot the scores
plot(pca.scores, 
     xlab = expression('Z'[1]), 
     ylab = expression('Z'[2]), 
     main = 'Score plot')
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# Plot the scores as points
text(pca.scores[,1]+0.2,
     pca.scores[,2],
     rownames(pca.scores),
     col="blue", cex=0.6)

#Interpreting loading plots
#The loadings plot is a plot of the direction vectors that define the model. 
#They show how the original variables contribute to creating the principal component.

loading.vector <- food.pca.eigen$vectors
rownames(loading.vector) <- colnames(food.pca)

# Plot the loading vector
plot(loading.vector, 
     xlab = expression('p'[1]), 
     ylab = expression('p'[2]), 
     main = 'Loading plot',
     ylim = c(-1,1),
     xlim = c(-1,1))
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# Plot the loadings as points
text(loading.vector[,1]+0.1,
     loading.vector[,2]+0.1,
     rownames(loading.vector),
     col="blue", cex=1.2)

#Interpreting Biplots
#The biplot is a very popular way for visualization of results from PCA, 
#as it combines both the principal component scores and the loading vectors 
#in a single biplot display.

loading.vector <- food.pca.eigen$vectors
rownames(loading.vector) <- colnames(food.pca)

# Plot the loading vector
plot(loading.vector, 
     xlab = expression('p'[1]), 
     ylab = expression('p'[2]), 
     main = 'Loading plot',
     ylim = c(-1,1),
     xlim = c(-1,1))
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# Plot the loadings as points
text(loading.vector[,1]+0.1,
     loading.vector[,2]+0.1,
     rownames(loading.vector),
     col="blue", cex=1.2)

# Correlation BiPlot
pca.sd <- sqrt(food.pca.eigen$values) # standardize to sd = 1
loading.vector <- food.pca.eigen$vectors
rownames(loading.vector) <- colnames(food.pca)

# Plot
plot(pca.scores,
     xlab = expression('p'[1]), 
     ylab = expression('p'[2]))
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# This is to make the size of the lines more apparent
factor <- 0.5

# Plot the variables as vectors
arrows(0,0,loading.vector[,1]*pca.sd[1]/factor,
       loading.vector[,2]*pca.sd[2]/factor,
       length = 0.1,
       lwd=  2,
       angle = 20,
       col = "red")

# Plot annotations
text(loading.vector[,1]*pca.sd[1]/factor*1.2,
     loading.vector[,2]*pca.sd[2]/factor*1.2,
     rownames(loading.vector),
     col = "red",
     cex = 1.2)

#--------------------------------------#
#### factor Analysis #####
food.fa <- factanal(food, factors = 2)
food.fa
food.fa$uniquenesses
apply(food.fa$loadings^2,1,sum) # communality
1-apply(food.fa$loadings^2,1,sum) # uniqueness

# The residual matrix
Lambda <- food.fa$loadings
Psi <- diag(food.fa$uniquenesses)
S <- food.fa$correlation
Sigma <- Lambda %*% t(Lambda) + Psi
round(S - Sigma, 6)

# Interpretation of the factors

food.fa.none <- factanal(food, factors = 2, rotation = "none")
food.fa.varimax <- factanal(food, factors = 2, rotation = "varimax")
food.fa.promax <- factanal(food, factors = 2, rotation = "promax")

par(mfrow = c(1,3))
plot(food.fa.none$loadings[,1], 
     food.fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)

plot(food.fa.varimax$loadings[,1], 
     food.fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(food.fa.varimax$loadings[,1]-0.08, 
     food.fa.varimax$loadings[,2]+0.08,
     colnames(food),
     col="blue")
abline(h = 0, v = 0)

plot(food.fa.promax$loadings[,1], 
     food.fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)

