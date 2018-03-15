# hw3

## Example

https://changlab.shinyapps.io/ggvisExample/

![ggvisExample](/images/img1.png)

## Description

* Submit your homework by providing a link, i.e.,  https://yourname.shinyapps.io/hw3/
* Make a shiny interactive web site to show PCA analysis (as the following picture) for the iris data such that users can specify which component to show. Welcome extra visulizations to show more information, i.e., input, PCA result, ... (plus points). 

![pcaExample](/images/img2.png)

* You might start by integrating the following example (pcr.R) into shiny app. Of course, free to find other appropriate package for PCA.

### pca.R

```R
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
```

