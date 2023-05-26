
  ## Chargement des donnÃ©es ##
## Avec la 1ere colonne en tant qu'observation ##
happy <- read.table("C:/Users/marti/Desktop/Projet BD/world_happiness_2020.txt",row.names = 1,   header = T, sep = "\t")


happy2 <- read.table("C:/Users/marti/Desktop/Projet BD/world_happiness_2020.txt",   header = T, sep = "\t")

  ## Visualisation jeu de donnÃ©es ##
## Apercus des trois premieres lignes du jeu de données  ##
head(happy,3)

## Structure du jeu de donnÃ©es ##
## b <- happy[,-1] Rappel : permet de retirer des éléments 
str(happy)

## Renomage colonnes ##
names(happy)
names(happy2)
colnames(happy) <- c("Region", "Happiness score", "Log (GDP Per Capital)", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")
colnames(happy2) <- c("Country","Region", "Happiness score", "Log (GDP Per Capital)", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")
attach(happy)
Region

## Si necessaire transofrmer deux varibles de chaines de caracteres en facteurs  ##
happy$Region <- as.factor(happy$Region)
happy$Country <- as.factor(happy$Country)

## Pré-analyse des données : variables qualitatives ##
NbPaysParRegion <- table(Region)
NbPaysParRegion <- as.data.frame(NbPaysParRegion)
names(NbPaysParRegion)
colnames(NbPaysParRegion) <- c("Region", "Nombre total de pays intÃ©rogÃ©s") ## Faire analyse avec le nombre total https://www.ined.fr/fr/tout-savoir-population/chiffres/tous-les-pays-du-monde/?lst_continent=900&lst_pays=
plot(NbPaysParRegion)
hist(NbPaysParRegion)
str(NbPaysParRegion)

## Stat descriptive et sa matrice de rÃ©sultats ##
res = matrix(NA,7,7)
summary(happy)
apply(happy, 2,summary)
res[1,]<-c(summary(happy$`Happiness score`),sd(happy$`Happiness score`))
res[2,]<-c(summary(happy$`Log (GDP Per Capital)`), sd(happy$`Log (GDP Per Capital)`)) 
res[3,]<-c(summary(happy$`Social support`), sd(happy$`Social support`))
res[4,]<-c(summary(happy$`Healthy life expectancy`), sd(happy$`Healthy life expectancy`))
res[5,]<-c(summary(happy$`Freedom to make life choices`), sd(happy$`Freedom to make life choices`))
res[6,]<-c(summary(happy$Generosity), sd(happy$Generosity))
res[7,]<-c(summary(happy$`Perceptions of corruption`), sd(happy$`Perceptions of corruption`))
row.names(res) <- c("Happiness score", "Log (GDP Per Capital)", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")
colnames(res) <- c("Minimum","1er quartile","Mediane","Moyenne","3Ã¨me quartile","Maximum","Ecart-type")
res <- as.data.frame(res)

  ## Partie analyse descirptive quantitative :  Correlation ##
## Graphique de la corelation entre Log (GDP Per Capital) et VS Healthy life expectancy ##

plot(happy$`Log (GDP Per Capital)`, happy$`Healthy life expectancy`)

## Graphique de la corelation entre Happiness score VS Log (GDP Per Capital) ##
plot(happy$`Happiness score`,happy$`Log (GDP Per Capital)`)

plot(happy$`Happiness score`,happy$`Healthy life expectancy`)


library(corrplot)

correlation <- happy2[,-c(1,2)]
mcor <- cor(correlation)



corrplot(mcor, method = "number", type="upper")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(mcor, method="color", col=col(200), 
         type="upper",
         addCoef.col = "black", order="hclust", tl.srt = 30, diag = F)


## Fonction de calcul de la significativité de nos corrélations##
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(correlation)
p.mat

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05,  insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)


str(correlation)
  ## ## 
## Shapiro test ##
shapiro.test(happy$`Happiness score`) ## W = 0.98899, p-value = 0.2742 -> Suit une loi normale
shapiro.test(happy$`Log (GDP Per Capital)`) ## W = 0.9624, p-value = 0.000352
shapiro.test(happy$`Social support`) ## W = 0.90486, p-value = 1.958e-08 -> Ne suit pas une loi normale
shapiro.test(happy$`Healthy life expectancy`) ## W = 0.95457, p-value = 6.744e-05 -> Ne suit pas une loi normale
shapiro.test(happy$`Freedom to make life choices`) ## W = 0.95689, p-value = 0.0001084
shapiro.test(happy$Generosity) ## W = 0.95663, p-value = 0.0001029
shapiro.test(happy$`Perceptions of corruption`) ## W = 0.82274, p-value = 2.478e-12 -> Ne suit pas une loi normale

## https://sites.google.com/site/rgraphiques/4--stat/comparaison-de-moyennes-avec-r?authuser=0#h.p_BXUmufSaTb0p


## Vérification graphique de la normalité de nos données ## 
hist(happy$`Social support`)
hist(happy$`Happiness score`)
hist(happy$Generosity)

par(mfrow=c(1,1))
plot.ecdf(happy$`Social support`)
plot.ecdf(rnorm(1000,mean=mean(happy$`Social support`),sd=sqrt(var(happy$`Social support`))),add=T,lty="dotted",pch=" ")

plot.ecdf(Generosity)
plot.ecdf(rnorm(1000,mean=mean(happy$Generosity),sd=sqrt(var(happy$Generosity))),add=T,lty="dotted",pch=" ")

plot.ecdf(`Happiness score`)
plot.ecdf(rnorm(1000,mean=mean(`Happiness score`),sd=sqrt(var(`Happiness score`))),add=T,lty="dotted",pch=" ")
## t.test(happy$Generosity, var.equal = F) ##

plot.ecdf(`Log (GDP Per Capital)`)
plot.ecdf(rnorm(1000,mean=mean(`Log (GDP Per Capital)`),sd=sqrt(var(`Log (GDP Per Capital)`))),add=T,lty="dotted",pch=" ")

plot.ecdf(`Perceptions of corruption`)
plot.ecdf(rnorm(1000,mean=mean(`Perceptions of corruption`),sd=sqrt(var(`Perceptions of corruption`))),add=T,lty="dotted",pch=" ")

plot.ecdf(`Freedom to make life choices`)
plot.ecdf(rnorm(1000,mean=mean(`Freedom to make life choices`),sd=sqrt(var(`Freedom to make life choices`))),add=T,lty="dotted",pch=" ")

plot.ecdf(`Healthy life expectancy`)
plot.ecdf(rnorm(1000,mean=mean(`Healthy life expectancy`),sd=sqrt(var(`Healthy life expectancy`))),add=T,lty="dotted",pch=" ")


## Solution envisagée par rapport a la non normalité des données : Normalisation des doonées ##

c1 <- colSums(correlation)
normalisation <- scale(correlation, center = T, scale = c1)
normalisation <- as.data.frame(normalisation)
attach(normalisation)


par(mfrow=c(1,1))
plot.ecdf(normalisation$`Happiness score`)
plot.ecdf(rnorm(1000,mean=mean(normalisation$`Happiness score`),sd=sqrt(var(normalisation$`Happiness score`))),add=T,lty="dotted",pch=" ")
shapiro.test(normalisation$`Happiness score`)


par(mfrow=c(1,1))
plot.ecdf(normalisation$`Healthy life expectancy`)
plot.ecdf(rnorm(1000,mean=mean(normalisation$`Healthy life expectancy`),sd=sqrt(var(normalisation$`Healthy life expectancy`))),add=T,lty="dotted",pch=" ")
shapiro.test(normalisation$`Healthy life expectancy`)


par(mfrow=c(1,1))
plot.ecdf(normalisation$`Log (GDP Per Capital)`)
plot.ecdf(rnorm(1000,mean=mean(normalisation$`Log (GDP Per Capital)`),sd=sqrt(var(normalisation$`Log (GDP Per Capital)`))),add=T,lty="dotted",pch=" ")


par(mfrow=c(1,1))
plot.ecdf(normalisation$`Social support`)
plot.ecdf(rnorm(1000,mean=mean(normalisation$`Social support`),sd=sqrt(var(normalisation$`Social support`))),add=T,lty="dotted",pch=" ")


par(mfrow=c(1,1))
plot.ecdf(normalisation$`Freedom to make life choices`)
plot.ecdf(rnorm(1000,mean=mean(normalisation$`Freedom to make life choices`),sd=sqrt(var(normalisation$`Freedom to make life choices`))),add=T,lty="dotted",pch=" ")


par(mfrow=c(1,1))
plot.ecdf(normalisation$Generosity)
plot.ecdf(rnorm(1000,mean=mean(normalisation$Generosity),sd=sqrt(var(normalisation$Generosity))),add=T,lty="dotted",pch=" ")
shapiro.test(normalisation$Generosity)

par(mfrow=c(1,1))
plot.ecdf(normalisation$`Perceptions of corruption`)
plot.ecdf(rnorm(1000,mean=mean(normalisation$`Perceptions of corruption`),sd=sqrt(var(normalisation$`Perceptions of corruption`))),add=T,lty="dotted",pch=" ")


## Analyse effectuée : croisement du score de bonheur et de la région séparés en deux échantillons (Europe VS Reste du monde)##

boxplot(list(happy.Europe$`Happiness score`, happy.ResteDuMonde$`Happiness score`), names = c("Europe", "Reste du monde"), main="Score de bonheur")

happy.Europe <- happy[which(Region=="Central and Eastern Europe"),]
attach(happy.Europe)
happy.ResteDuMonde <- happy[which(Region!="Central and Eastern Europe"),]
attach(happy.ResteDuMonde)

## Se qu'on conserve ##
shapiro.test(happy.Europe$`Happiness score`)
shapiro.test(happy.ResteDuMonde$`Happiness score`)

## Vérification sur quelques variables ##
shapiro.test(happy.Europe$`Log (GDP Per Capital)`)
shapiro.test(happy.Europe$`Social support`)
shapiro.test(happy.Europe$Generosity)


## Analyse descriptive des deux échantillons ## 
mean(happy.Europe$`Happiness score`)
sd(happy.Europe$`Happiness score`)
mean(happy.ResteDuMonde$`Happiness score`)
sd(happy.ResteDuMonde$`Happiness score`)
var.test(happy.Europe$`Happiness score`, happy.ResteDuMonde$`Happiness score`)

## H0 : les moyennes des deux échantillons sont égales H1 : les moyennes sont différentes entre eux. 
t.test(happy.Europe$`Happiness score`, happy.ResteDuMonde$`Happiness score`, var.equal = F)
t.test(happy.Europe$`Happiness score`, happy.ResteDuMonde$`Happiness score`, var.equal = F, alternative ="greater")

