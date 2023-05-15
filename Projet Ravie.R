## Chargement des données ##
happy <- read.table("C:/Users/boklo/OneDrive/Documents/ENSC/S6/Rstudio/world_happiness_2020.txt", header = T, sep = "\t")

## Renomage colonnes ##
names(happy)
colnames(happy) <- c("Country", "Region", "Happiness score", "Log (GDP Per Capital)", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")
attach(happy)

regions<-unique(happy$Region)

#' RassembleDonneesRegion
#'
#' @param regions # vecteur de chr contenant tous les noms des régions présentes dans donnees
#' @param donnees # donnees brute pour l'analyse
#' @return table # matrice contenant les informations de tous les pays présent dans la région donnée en paramètre (region)
#' @export
#'
#' @examples
RassembleDonneesRegion<-function(regions,donnees)
{
  nbOccurence=0
  for(i in 1:153)
  {
    if (donnees[i,2]==regions)
    {
      nbOccurence=nbOccurence+1
    }
  }
  #browser()
  #on prépare le data frame dans lequel on va mettre les éléments, on en profite pour nommer les colonnes
  table=data.frame(
    "Country"=c(1:nbOccurence),
    "Happiness score"=c(1:nbOccurence),
    "Log (GDP Per Capital)"=c(1:nbOccurence),
    "Social support"=c(1:nbOccurence),
    "Healthy life expectancy"=c(1:nbOccurence),
    "Freedom to make life choices"=c(1:nbOccurence),
    "Generosity"=c(1:nbOccurence),
    "Perceptions of corruption"=c(1:nbOccurence)
  )
  k=1
  #1e colonne
  for (i in 1:153)
  {
    if (donnees[i,2]==regions)
    {
      #première colonne = nom du pays
      table[k,1]<-donnees[i,1]
      
      #puis on remplis avec les autres valeurs
      for (j in 3:9)
      {
        table[k,j-1]<-donnees[i,j]
      }
      k=k+1
    }
  }
  return(table)
}

#remarque peut être faire un apply
Western_Europe<-RassembleDonneesRegion(regions[1],happy)
North_America_and_ANZ<-RassembleDonneesRegion(regions[2],happy)
Middle_East_and_North_Africa<-RassembleDonneesRegion(regions[3],happy)
Latin_America_and_Caribbean<-RassembleDonneesRegion(regions[4],happy)
Central_and_Eastern_Europe<-RassembleDonneesRegion(regions[5],happy)
East_Asia<-RassembleDonneesRegion(regions[6],happy)
Southeast_Asia<-RassembleDonneesRegion(regions[7],happy)
Commonwealth_of_Independent_States<-RassembleDonneesRegion(regions[8],happy)
Sub_Saharan_Africa<-RassembleDonneesRegion(regions[9],happy)
South_Asia<-RassembleDonneesRegion(regions[10],happy)

## Stat descriptive et sa matrice de résultats ##

#' RegionSummary
#'
#' @param region # date.frame de l'ensemble des données pour une seule région
#' @return regionSummary # data.frame regroupant l'ensemble le summary de region
#' @export
#'
#' @examples
RegionSummary<-function(region)
{
  regionSummary=matrix(NA,7,7)
  regionSummary[1,]<-c(summary(region$Happiness.score),sd(region$Happiness.score))
  regionSummary[2,]<-c(summary(region$Log..GDP.Per.Capital), sd(region$Log..GDP.Per.Capital)) 
  regionSummary[3,]<-c(summary(region$Social.support), sd(region$Social.support))
  regionSummary[4,]<-c(summary(region$Healthy.life.expectancy), sd(region$Healthy.life.expectancy))
  regionSummary[5,]<-c(summary(region$Freedom.to.make.life.choices), sd(region$Freedom.to.make.life.choices))
  regionSummary[6,]<-c(summary(region$Generosity), sd(region$Generosity))
  regionSummary[7,]<-c(summary(region$Perceptions.of.corruption), sd(region$Perceptions.of.corruption))
  row.names(regionSummary) <- c("Happiness score", "Log (GDP Per Capital)", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")
  colnames(regionSummary) <- c("Minimum","1er quartile","Mediane","Moyenne","3ème quartile","Maximum","Ecart-type")
  regionSummary <- as.data.frame(regionSummary)
  
  return(regionSummary)
}

#on fait summary pour chacune des régions
Western_Europe_Summary=RegionSummary(Western_Europe)
North_America_and_ANZ_Summary<-RegionSummary(North_America_and_ANZ)
Middle_East_and_North_Africa_Summary<-RegionSummary(Middle_East_and_North_Africa)
Latin_America_and_Caribbean_Summary<-RegionSummary(Latin_America_and_Caribbean)
Central_and_Eastern_Europe_Summary<-RegionSummary(Central_and_Eastern_Europe)
East_Asia_Summary<-RegionSummary(East_Asia)
Southeast_Asia_Summary<-RegionSummary(Southeast_Asia)
Commonwealth_of_Independent_States_Summary<-RegionSummary(Commonwealth_of_Independent_States)
Sub_Saharan_Africa_Summary<-RegionSummary(Sub_Saharan_Africa)
South_Asia_Summary<-RegionSummary(South_Asia)
