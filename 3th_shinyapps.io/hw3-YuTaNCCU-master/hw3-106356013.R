

data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

#install.packages("devtools")
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species , 
              ellipse = TRUE, circle = TRUE , choices=c(4,1)  )
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

#install.packages("shiny")
library(shiny)
setwd("~/Desktop/hw3-106356013/app1")
#install.packages('rsconnect')
rsconnect::setAccountInfo(name='yutanccu', token='0795183F8B4BF0689346C9E5AF2930C8', secret='pk7kmH46F4TnilGC2YzlpJI0mQV4MlJ8e1xA1EOA')
library(rsconnect)
rsconnect::deployApp('~/Desktop/hw3-106356013/app1/') 
