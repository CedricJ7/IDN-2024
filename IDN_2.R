library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)
library(viridis)

setwd("~/BUT SD/semestre 4/SAE_IDN/")
d = read.csv("nouvelle_colonnes.csv",sep=";", header=TRUE)

colnames(d)
moy <- colMeans(d[,4:ncol(d)])
summary(d)
#boxplot(d[,4:ncol(d)])  
moy = moy[-1]



# représentation graphiques : 
couleurs <- viridis(length(moy))
barplot(moy, col = couleurs , main = "Moyennes des variables", xlab = "Variables", ylab = "Moyennes", names.arg = colnames(d[,5:ncol(d)]), cex.names = 0.7)
abline(h = mean(moy), col = "black", lwd = 2,lty = 3)

  boxplot(d[,5:ncol(d)], col = couleurs, main = "Boxplot des variables", xlab = "Variables", ylab = "Valeurs", names = colnames(d[,5:ncol(d)]), cex.names = 0.7)


d2 <- d %>%
  group_by(Username) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))# Calcule la moyenne pour les colonnes numériques
d2 <- select(d2, -c("age"))  
# toutes les colonnes dans une même colonne
d3 <- d2 %>%
  pivot_longer(cols = -Username, names_to = "variable", values_to = "value")

# nuage de points : 
g <- ggplot(d3, aes(x = Username, y = value, color = variable)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Nuage de points des variables", x = "Usernames", y = "Valeurs")

#en plotly : 
library(plotly)
g <- ggplotly(g)
g

#############
data <- read.csv("content_10.csv",sep="\t", header=TRUE)
data = select(data, c("Username","gender","age","handedness","L..l","E..e","O..o","N..n","A..a","R..r","D..d","O..o.1","X..","D..d.1","I..i","C..c","A..a.1","P..p","R..r.1","I..i.1","O..o.2"))

moy2 <- colMeans(data[,5:ncol(data)])
couleurs2 <- viridis(length(moy2))
barplot(moy2, col = couleurs2 , main = "Moyennes des variables", xlab = "Variables", ylab = "Moyennes", names.arg = colnames(data[,5:ncol(data)]), cex.names = 0.7)
abline(h = mean(moy2), col = "black", lwd = 2,lty = 3)



