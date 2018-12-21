#Analise de Dados - Livro: Data Science Para Negocios 
#Conjunto de Dados: Mushroom - UCI 
#Objetivo: Identificar quais Atributos Tem maior ganho de informacao 

sourcePath <- "/Users/victortelles/Documents/R Studio/mushroom/dataset/"
fileName <- "mushroom.csv"
mushroom <- read.csv(file=paste(sourcePath, fileName, sep = ""), header=TRUE, sep=";")

levels(mushroom$flg_eat)

#versao1 - Manual
prob1 = length(subset(mushroom, mushroom$flg_eat == "EDIBLE")$flg_eat)/length(mushroom$flg_eat) 
prob2 = length(subset(mushroom, mushroom$flg_eat != "EDIBLE")$flg_eat)/length(mushroom$flg_eat)
entropiaPai = -((prob1*log2(prob1)) + (prob2*log2(prob2)))

#Entropia filhos Cor-Lamela
levels(mushroom$gill.color)

mushfilho1 = subset(mushroom, mushroom$gill.color == "BLACK")
mushfilho2 = subset(mushroom, mushroom$gill.color == "BROWN")
mushfilho3 = subset(mushroom, mushroom$gill.color == "BUFF")
mushfilho4 = subset(mushroom, mushroom$gill.color == "CHOCOLATE")
mushfilho5 = subset(mushroom, mushroom$gill.color == "GRAY")
mushfilho6 = subset(mushroom, mushroom$gill.color == "GREEN")
mushfilho7 = subset(mushroom, mushroom$gill.color == "ORANGE")
mushfilho8 = subset(mushroom, mushroom$gill.color == "PINK")
mushfilho9 = subset(mushroom, mushroom$gill.color == "PURPLE")
mushfilho10 = subset(mushroom, mushroom$gill.color == "RED")
mushfilho11 = subset(mushroom, mushroom$gill.color == "WHITE")
mushfilho12 = subset(mushroom, mushroom$gill.color == "YELLOW")

calcEntropiaFilho <- function(dataset) {
  prob1 = length(subset(dataset, dataset$flg_eat == "EDIBLE")$flg_eat)/length(dataset$flg_eat) 
  print(prob1)
  prob2 = length(subset(dataset, dataset$flg_eat != "EDIBLE")$flg_eat)/length(dataset$flg_eat)
  print(prob2)
  entropiaFilho = -((prob1*log2(prob1)) + (prob2*log2(prob2)))
  print(entropiaFilho)
}

entropiFilho1 = calcEntropiaFilho(mushfilho1)
entropiFilho2 = calcEntropiaFilho(mushfilho2)
entropiFilho3 = calcEntropiaFilho(mushfilho3)
entropiFilho4 = calcEntropiaFilho(mushfilho4)
entropiFilho5 = calcEntropiaFilho(mushfilho5)
entropiFilho6 = calcEntropiaFilho(mushfilho6)
entropiFilho7 = calcEntropiaFilho(mushfilho7)
entropiFilho8 = calcEntropiaFilho(mushfilho8)
entropiFilho9 = calcEntropiaFilho(mushfilho9)
entropiFilho10 = calcEntropiaFilho(mushfilho10)
entropiFilho11 = calcEntropiaFilho(mushfilho11)
entropiFilho12 = calcEntropiaFilho(mushfilho12)

#calculo do Ganho de Informacao



#Function to calculate entropy of dataset
calcEntropy <- function(dataset, target) {
  entr <- c()
  print(nlevels(dataset$target))
  for (i in 1:nlevels(dataset[,target]))
  {
      entr[i] = -(length(dataset)/length(dataset))
  }
}

calcEntropy(mushroom, mushroom$flg_eat)