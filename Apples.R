rm(list = ls())     # clear objects  
graphics.off() 
##########################
###### Apples  ###########
##########################


# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","plot3D", "cluster", "readxl", "magrittr",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools")
inst(packages)
theme_set(theme_minimal())




# Dataframe ---------------------------------------------------------------

(data <- read_excel("Desktop/Apples.xlsx", 
                    sheet = "Imputated means", 
                    col_types = c("skip", 
                                  "text", "text", "numeric", "skip", 
                                  "skip", "skip", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric")))




# Data Wrangling ----------------------------------------------------------
## Observations names abbreviation
(rename <- data %>% 
    unite(name, c(Antiox, Cover, Day), sep = "_", remove = TRUE)) 
dim(rename)

## General Overview
pairs.panels(rename[2:17],
             gap = 0,
             bg = c("red", "green", "blue")[rename$name],
             pch = 21)

## Data aggregation by mean
agg<-aggregate(.~name, rename, FUN=mean, na.rm= FALSE, na.action = NULL)

## Indexing
(rownames(agg) <- agg$name)
dim(agg)
df <- agg [ ,2:17]

## Check for missing values
sum(is.na(df))

## Remove missing values
df_na <- na.omit(df)




# PCA analysis ------------------------------------------------------------
## PCA processing
(df.pca<-prcomp(
  df_na, 
  center = TRUE, 
  scale. = TRUE
))
summary(df.pca)
capture.output(df.pca, file = "pca.txt")

## Simple scree plot
plot(
  df.pca, 
  type="lines"
)

## Scree plot
p1 <- fviz_eig(df.pca, 
               return_ggplot = TRUE,
               addlabels = TRUE, 
               barfill = "#798353", 
               barcolor = "#692238",
               linecolor = "#D7B941")
p1 +  ggtitle("Treated Apples Scree plot") +
  labs(x = "Dimensions", y = "percentage of the explained variances") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

## PCA plots
### Individuals
fviz_pca_ind(df.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#D7B941", "#798353", "#692238"),
             repel = TRUE     # Avoid text overlapping
)
### Variables
fviz_pca_var(df.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#D7B941", "#798353", "#692238"),
             repel = TRUE     # Avoid text overlapping
)
### Biplot
fviz_pca_biplot(df.pca, repel = TRUE,
                col.var = "#D7B941", # Variables color
                col.ind = "#692238",  # Individuals color
                max.overlaps = 7     # Maximum number of allowed label overlaps
)





# K-means clustering ------------------------------------------------------
## Data scaling
(dfsc <-df_na %>% 
    scale(.) %>% 
    as_tibble(dfsc))

## Distance matrix
m.distancia <- get_dist(dfsc, method = "euclidean")
fviz_dist (m.distancia, 
           gradient = list (low = "gold3", mid = "white", 
                            high = "darkslategray4"),  
           order = FALSE)

## Elbow, silhouette o gap_stat methods
fviz_nbclust(dfsc, kmeans , method = "wss")
fviz_nbclust (dfsc, kmeans, method = "silhouette")
fviz_nbclust (dfsc, kmeans, method = "gap_stat")

## Clusterization of data at different K-ranges
### 2 Clusters
k2<-kmeansruns(dfsc, krange=2, runs=100)
fviz_cluster(k2, data=dfsc) +
  scale_colour_manual(values = c("#D7B941", "#692238")) +
  scale_fill_manual(values = c("#D7B941", "#692238")) 
### 3 Cluster
k3<-kmeansruns(dfsc, krange=3, runs=100)
fviz_cluster(k3, data=dfsc) +
  scale_colour_manual(values = c("#D7B941", "#798353", "#692238")) +
  scale_fill_manual(values = c("#D7B941", "#798353", "#692238")) +
  annotate(geom = "label", x=1.5, y=-3, label="Extract t>0 cluster", size=4) +
  annotate(geom = "label", x=3, y=2.7, label="Ascorbic t>0 cluster", size=4) +
  annotate(geom = "label", x=-2.5, y=-1, label="Both treatments t=0 cluster", 
           size=4) +
  theme_minimal()




# Plotting of the 3rd component -------------------------------------------
## 3 Principal components extraction
df.pca3 <- as.data.frame(df.pca$x[,1:3])
capture.output(df.pca3, file = "pca3.txt")

## Join of dataframes
df_pca3 <- cbind(df.pca3, df_na)

## 3D graph
scatter3D(
  x = df_pca3$PC1, 
  y = df_pca3$PC2, 
  z = df_pca3$PC3, 
  colvar = NULL, 
  col = df_pca3$Fungos, 
  pch = 16
)

## Continuous to factorial conversion
df_pca3$PC3a <- cut(df_pca3$PC3, breaks = 2)

## 2D graph of the 3 Principal components
ggplot(df_pca3, aes(x = PC1, y = PC2, color = PC3a, shape = PC3a, size= PC3a)) +
  geom_point(size = 3)



# Data summary ------------------------------------------------------------
rename %>%
  group_by(name) %>%
  summarise_all(.funs = list(mean)) %>% 
  na.omit(.) %>% 
  arrange(FRAP) #Change the response name to observe each particular response
