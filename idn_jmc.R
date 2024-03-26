library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)

setwd("C:/Users/cedri/Documents/SAE IDN/")

colnames(d)
library(FactoMineR)
library(tidyverse)
#moyenne par individu

d= read.csv("nouvelle_colonnes.csv", header = TRUE,sep=";")
d <- d %>%
  group_by(Username) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), # Calcule la moyenne pour les colonnes numériques
            across(where(is.character), ~first(.x, order_by = .x))) # Prend la première valeur pour les colonnes non numériques
colnames(d)

d = select(d, -c("Text"))
d <- d %>% select(Username,gender, age,everything())
colnames(d)
class(d[,4:ncol(d)])
d$m = rowMeans((d[,4:ncol(d)]))

write.csv(d, "analyse.csv", row.names = FALSE)
d= read.csv("nouvelle_colonnes.csv", header = TRUE,sep=";")
colnames(d)


d$m = rowMeans(d[,5:20])
colnames(d)
#une regression de m selon l'age
reg = lm(m ~ age, data = d) #individu extreme à enlever
plot(reg)

#on enleve les individus extremes : 29, 35 , 76 (index)

d = d[-c(281, 341, 752),]
length(d$m)
#on refait la regression
reg = lm(m ~ age, data = d)
summary(reg)
# plot(reg)
##### Coreelation age et temps de transition touche
#enlever les 5% extremes 
df = d %>% filter(m < quantile(d$m, 0.975) & m > quantile(d$m, 0.025))
ggplot(df, aes(x = age, y = m)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "blue") +
  labs(title = "Temps de latence selon l'âge",
       x = "Âge",
       y = "Moyenne des temps")
#######

summary(lm(df$m ~ df$age))
scale(d$m)

#on va faire une K_mean

#choix methode silhouette pour k
#sans graph
fviz_nbclust(scale(d$m), kmeans, method = "silhouette")

k_mean = kmeans(scale(d[,5:20]), centers = 2)
length(colnames(d))
colnames(d)
length(colnames(d))


fviz_cluster(k_mean, data = d[,5:20])
#comment caractériser les 2 groupes
d$cluster = k_mean$cluster
d %>% group_by(cluster) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE))

# ajoute proportion des non numériques

a=d %>% group_by(cluster) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE), # Calcule la moyenne pour les colonnes numériques
                                     across(where(is.character), ~first(.x, order_by = .x))) # Prend la première valeur pour les colonnes non numériques



cat("moyenne m pour le cluster 1 : ", mean(d$m[d$cluster == 1]), "\n")
cat("moyenne m pour le cluster 2 : ", mean(d$m[d$cluster == 2]), "\n")

#on va mettre l'IC
cat("Intervalle de confiance de la moyenne de m pour le cluster 1 : ", t.test(d$m[d$cluster == 1], conf.level = 0.95)$conf.int, "\n")
cat("Intervalle de confiance de la moyenne de m pour le cluster 2 : ", t.test(d$m[d$cluster == 2], conf.level = 0.95)$conf.int, "\n")

#fait une acp sur l'age et m
colnames(d)
d2 = d %>% select(age, gender , m)

summary(d2)
colnames(d2)
pca2 = PCA(d2[c(1,3)], scale.unit = TRUE, graph = FALSE)


pca2$eig
fviz_eig(pca2)

#selon l'age par intervalle de 20 avec cut min max

d2$age = cut(d2$age, breaks = seq(10, 70, by = 20))
d2$age

fviz_pca_ind(pca2, col.ind = as.factor(d2$age), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = F,
             label = "none",
             title = "ACP des individus selon l'âge")

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

par(mfrow = c(1,1))


library(ggplot2)

ggplot(d2, aes(x = m)) + 
  geom_histogram(aes(fill = ..count..), color = "black", linewidth = 1) + # Ajuster binwidth et utiliser color pour les bordures
  facet_wrap(~ age) + # Enlever scales="free" pour avoir la même échelle sur toutes les facettes
  geom_vline(aes(xintercept = mean(m)), color = "red", linetype = "dashed", size = 1) + 
  scale_fill_gradient(low = "#D3D3D3", high = "#696969") + # Gradients de gris
  xlim(0, 1000) + # Fixer les limites de l'axe des abscisses de 0 à 1000
  labs(title = "Distribution de m par groupe d'âge",
       x = "Moyenne des temps",
       y = "Fréquence") +
  theme_minimal()
#on va faire une K_mean
mean_values <- d2 %>%
  group_by(age) %>%
  summarise(mean_m = mean(m))

# Transformer 'age' en facteur si ce n'est pas déjà le cas pour s'assurer de la correspondance dans facet_wrap
d2$age <- as.factor(d2$age)

# Créer le graphique
ggplot(d2, aes(x = m)) + 
  geom_histogram(aes(fill = ..count..), color = "black", binwidth = 20, linewidth = 0.8) + # Ajustez binwidth selon vos données
  facet_wrap(~ age) +
  geom_vline(data = mean_values, aes(xintercept = mean_m, group = age), color = "red", size = 0.7) +
  geom_text(data = mean_values, aes(x = mean_m, label = round(mean_m, 1), y = Inf), 
            color = "red", vjust = 2, size = 3.5,hjust=-0.1) + # Ajustez 'y' et 'vjust' pour la position du texte
  scale_fill_gradient(low = "#D3D3D3", high = "#696969") +
  xlim(0, 1000) +
  labs(title = "Distribution de m par groupe d'âge",
       x = "Moyenne des temps",
       y = "Fréquence") +
  theme_minimal()

colnames(d2)
KM = kmeans(d2[,c(1,3)], centers = 3)

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



m_1030 = mean(d2$m[d2$age == "(10,30]"])
m_3050 = mean(d2$m[d2$age == "(30,50]"])
m_5070 = mean(d2$m[d2$age == "(50,70]"])

cat("Moyenne de m pour les 10-30 ans : ", m_1030, "\n")
cat("Moyenne de m pour les 30-50 ans : ", m_3050, "\n")
cat("Moyenne de m pour les 50-70 ans : ", m_5070, "\n")

#### avec intervalle de confiance de 95%
d2 = read.csv("analyse.csv", header = TRUE)
d2$m = rowMeans(d2[,5:20])
d2$age = cut(d2$age, breaks = seq(10, 70, by = 20))
IC_1030 = t.test(d2$m[d2$age == "(10,30]"], conf.level = 0.95)$conf.int
IC_1030
IC_3050 = t.test(d2$m[d2$age == "(30,50]"], conf.level = 0.95)$conf.int
IC_3050
IC_5070 = t.test(d2$m[d2$age == "(50,70]"], conf.level = 0.95)$conf.int
IC_5070
t.test(d2$m[d2$age == "(10,30]"], conf.level = 0.95)
cat("Intervalle de confiance de la moyenne de m pour les 10-30 ans : ", IC_1030, "\n")
cat("Intervalle de confiance de la moyenne de m pour les 30-50 ans : ", IC_3050, "\n")
cat("Intervalle de confiance de la moyenne de m pour les 50-70 ans : ", IC_5070, "\n")




colnames(d)
pca2 = PCA(d, quali.sup = 1:3 , quanti.sup =  4, scale.unit = TRUE, graph = FALSE)

fviz_pca_ind(pca2, col.ind = as.factor(d$gender), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)

d$age = cut(d$age, breaks = seq(10, 70, by = 20))

fviz_pca_ind(pca2, col.ind = as.factor(d$age), pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.level = 0.95,
             repel = TRUE)

colnames(d)
d=read.csv("analyse.csv", header = TRUE)
d_lm_mult= d %>% select(-c("Username","gender","handedness"))
res=lm(d$age~., data = d_lm_mult)
plot(res)
summary(res)
#test de fisher
anova(res)
par(mfrow = c(1,1))

hist(d$m, col = "grey50", main = "Distribution de m", xlab = "moyenne des temps")
abline(v = mean(d$m), col = "red", lwd = 2)



data <- data.frame(
  age_group = factor(c("(10,30]", "(30,50]", "(50,70]"), levels = c("(10,30]", "(30,50]", "(50,70]")),
  lower = c(IC_1030[1], IC_3050[1], IC_5070[1]),
  upper = c(IC_1030[2], IC_3050[2], IC_5070[2]),
  mean = c(mean(IC_1030), mean(IC_3050), mean(IC_5070))
)

# Création du graphique avec ggplot2
ggplot(data, aes(x = age_group, y = mean, ymin = lower, ymax = upper)) +
  geom_point() +  # Ajoute les points de moyenne
  geom_errorbar(width = 0.2) +  # Ajoute les barres d'erreur pour l'intervalle de confiance
  labs(x = "Tranche d'âge", y = "Moyenne", title = "Intervalle de confiance de la moyenne par tranche d'âge") +
  theme_minimal()  # Utilise un thème minimal pour une meilleure apparence


