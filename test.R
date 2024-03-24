
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)

setwd("C:/Users/cedri/Documents/SAE IDN/")
d = read.csv("content_10.csv",sep="\t", header=TRUE)

colnames(d)
d = select(d, -c("Text","L..l","E..e","O..o","N..n","A..a","R..r","D..d","O..o.1","X..","D..d.1","I..i","C..c","A..a.1","P..p","R..r.1","I..i.1","O..o.2"))
#ACP sur les données
library(FactoMineR)
library(tidyverse)
#moyenne par individu
d <- d %>%
  group_by(Username) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), # Calcule la moyenne pour les colonnes numériques
            across(where(is.character), ~first(.x, order_by = .x))) # Prend la première valeur pour les colonnes non numériques
colnames(d)
d <- d %>% select(Username,gender, handedness, age,everything())
write.csv(d, "analyse.csv", row.names = FALSE)

#normaliser les données

##### puis on fait l'ACP
d[,2:ncol(d)-2]
colnames(d)
match("gender", names(d))
match("handedness", names(d))
match("age", names(d))

pca = PCA(d, quanti.sup = 2, quali.sup = 19:20, scale.unit = TRUE, graph = FALSE)

#representation des individus
library(factoextra)
fviz_pca_ind(pca, col.ind = "cos2", pointsize = 2, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Évite le chevauchement des étiquettes
             legend.title = "Cos2")

#ajoute une variable indicatrice
fviz_pca_ind(pca, col.ind = as.factor(d$gender), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)


# en fonction de la main 
fviz_pca_ind(pca, col.ind = as.factor(d$handedness), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)

#selon l'age
fviz_pca_ind(pca, col.ind = as.factor(d$age), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)

#d'autres representations
fviz_pca_var(pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)




#une méthode des K_means
#choix methode silhouette pour k
#sans graph
#determiner k via méthode calcul silhouette
k = c(2:10)
sil = c()
for (i in k) {
  km = kmeans(pca$ind$coord, centers = i, nstart = 25)
  sil[i-1] = silhouette(km$cluster, dist(pca$ind$coord))
}
plot(k, sil, type = "b", xlab = "k", ylab = "silhouette")
kmeans = kmeans(pca$ind$coord, centers = 9, nstart = 25)
pca$ind$cluster = kmeans$cluster
fviz_pca_ind(pca, col.ind = as.factor(d$age), pointsize = 2, 
             
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)
unique(d$age)
#créer un intervalle d'age de 10 en 10 
#selon le min et max
summary(d$age)
d$age = cut(d$age, breaks = seq(10, 70, by = 20))
d$age


###maintenant on va faire une classification ascendante hiérarchique
#avec la méthode de ward
cah = hclust(dist(pca$ind$coord), method = "ward.D2")
plot(cah, hang = -1, cex = 0.6)
rect.hclust(cah, k = 3, border = 2:5)
groups = cutree(cah, k = 3)
pca$ind$group = as.factor(groups)
fviz_pca_ind(pca, col.ind = as.factor(d$age), pointsize = 2, 
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)



d = read.csv("analyse.csv",sep=",", header=TRUE)
d
#CAH

#on va faire une classification ascendante hiérarchique
#avec la méthode de ward

#colonne m avec les moyennes pour chaque individu des temps L.. etc 

d$m = rowMeans(d[5:ncol(d)], na.rm = TRUE)

#on va faire une classification ascendante hiérarchique
#avec la méthode de ward
cah = hclust(dist(d[5:ncol(d)]), method = "ward.D2")
plot(cah, hang = -1, cex = 0.6)
rect.hclust(cah, k = 3, border = 2:5)
groups = cutree(cah, k = 3)
d$group = as.factor(groups)
d$group
rect.hclust(cah, k = 3, border = 2:5)

#qu'est ce qui caractèrise ces groupes 
summary(d$m)
sd(d$m)**2/1000

#une regression de m selon l'age
reg = lm(m ~ age, data = d) #individu extreme à enlever
plot(reg)

#on enleve les individus extremes : 29, 35 , 76 (index)

d = d[-c(29,35,76),]
length(d$m)
#on refait la regression
reg = lm(m ~ age, data = d)
plot(reg)

##### Coreelation age et temps de transition touche
ggplot(d, aes(x = age, y = m)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "blue") +
  labs(title = "Temps de latence selon l'âge",
       x = "Âge",
       y = "Moyenne des temps")
#######


scale(d$m)

#on va faire une K_mean

#choix methode silhouette pour k
#sans graph
fviz_nbclust(scale(d$m), kmeans, method = "silhouette")

k_mean = kmeans(scale(d$m), centers = 2)
length(colnames(d))
fviz_cluster(k_mean, geom = "point",data = d[c(4,21)], ellipse = T, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal())



#fait une acp sur l'age et m
d2 = d %>% select(age,handedness, gender , m)
summary(d2)
pca2 = PCA(d2[c(1,4)], scale.unit = TRUE, graph = FALSE)

#representation des individus
fviz_pca_ind(pca2, col.ind = "cos2", pointsize = 2, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Évite le chevauchement des étiquettes
             legend.title = "Cos2")

pca2$eig
fviz_eig(pca2)

#ajoute une variable indicatrice
fviz_pca_ind(pca2, col.ind = as.factor(d$gender), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)

fviz_pca_var(pca2, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#selon l'age par intervalle de 20 avec cut min max

d2$age = cut(d2$age, breaks = seq(10, 70, by = 20))
d2$age

fviz_pca_ind(pca2, col.ind = as.factor(d2$age), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)

#moyenne de m par age

m_1030 = mean(d2$m[d2$age == "(10,30]"])
m_3050 = mean(d2$m[d2$age == "(30,50]"])
m_5070 = mean(d2$m[d2$age == "(50,70]"])

cat("Moyenne de m pour les 10-30 ans : ", m_1030, "\n")
cat("Moyenne de m pour les 30-50 ans : ", m_3050, "\n")
cat("Moyenne de m pour les 50-70 ans : ", m_5070, "\n")

#regression de m moyen selon l'age

reg2 = lm(m ~ age, data = d2)
summary(reg2)


ggplot(d2, aes(x = age, y = m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Régression de m selon l'âge",
       x = "Âge",
       y = "Moyenne des temps")
hist(d2$m[d2$age == "(10,30]"], col = "blue", main = "Distribution de m pour les 10-30 ans", xlab = "moyenne des temps")
abline(v = m_1030, col = "red", lwd = 2)


table(d2$age) # nombre de personnes par intervalles d'age


par(mfrow = c(1, 3))
hist(d2$m[d2$age == "(10,30]"], col = "grey50", main = "Distribution de m pour les 10-30 ans", xlab = "moyenne des temps")
abline(v = m_1030, col = "red", lwd = 2)
hist(d2$m[d2$age == "(30,50]"], col = "grey50", main = "Distribution de m pour les 30-50 ans", xlab = "moyenne des temps")
abline(v = m_3050, col = "red", lwd = 2)
hist(d2$m[d2$age == "(50,70]"], col = "grey50", main = "Distribution de m pour les 50-70 ans", xlab = "moyenne des temps")
abline(v = m_5070, col = "red", lwd = 2)


#on va faire une K_mean
d2
KM = kmeans(d2[,c(1,4)], centers = 3)

fviz_cluster(KM, geom = "point", data = d2[c(1,4)], ellipse = T, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal())

#indique la main dominante
d2$handedness = as.factor(d2$handedness)
d2$handedness


d2$age2 = cut(d2$age, breaks = seq(10, 70, by = 20))
fviz_pca_ind(pca2, col.ind = as.factor(d2$age2), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)
