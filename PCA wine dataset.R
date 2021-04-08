getwd()

#descarga librerias

library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(factoextra)

#descarga datos

wine_dataset <- read_csv("~/Programación/Python_Scripts/data/wine_dataset.csv")

#quitamos variables cualitativas

wine <- wine_dataset %>% select(- "style", - "quality")

# centramos y calculamos los PC 

wine_pca <- prcomp(wine, center = TRUE, scale. = TRUE)

# sumary de los PC calculados 
summary(wine_pca)

#representamos graficamente cuanto explican de las variables
plot(wine_pca, type = "l")
fviz_eig(wine_pca)

### Representando PCA -------


#representamos los PC como ejes y las posicion de cada individuo respecto a los factores

#representacion de la contribucion de cada var. al PC1 y PC2
fviz_pca_var(wine_pca, col.var = 'blue')

#contribución al PC1 
fviz_contrib(wine_pca, choice = "var", axes = 1
             )
#contribucion Pc2
fviz_contrib(wine_pca, choice = "var", axes = 2
)
#prueba de los 2 
fviz_contrib(wine_pca, choice = "var", axes = 1:2
)

fviz_pca_ind(wine_pca, col.ind = wine_dataset$style)

#representamos 
fviz_pca_biplot(wine_pca,
                col.var = "black",
                col.ind = wine_dataset$style )

winefviz_pca_var(wine_pca,
             col.var = "cos2", # Color en f de contrib a PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             )


