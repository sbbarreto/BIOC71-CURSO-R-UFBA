######################################################
# Introduçao Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# https://github.com/vrios/Intro-Linguagem-R/wiki
#####################################################

#diferencie a funçao source da funçao load()
#a funçao source permite que o R diretamente aceite como input arquivos, URL, conexoes ou expressoes
#com essa funçao voce pode chamar um script de dentro do outro e executar o conteudo de um arquivo de codigo
#source é uma funçao especialmente util para carregar funçoes especificas ou scripts de analise
#a funçao load difere porque recarrega os datasets criados com as funçoes save() e save.image()

#baixe para seu diretorio de trabalho o arquivo toroidal.distance.R, disponivel
#em https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/toroidal.distance.R, e o arquivo distancias.csv, disponivel em https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/distancias.csv

#Abra toridal.distances.R no R
# O que voce precisa fazer para que as funçoes contidas nesse arquivo estejam disponiveis para uso no R?
#Usar a funçao source()

# Faça o procedimento que voce descreveu acima
source("toroidal.distance.R")
ls()

toroidal.distance()
toroidal.distances()
#Funçoes disponiveis para uso no R

# dentro do script toroidal.distance.R, altere o nome da funçao toroidal.distances para distance.matrix (substitua o nome dentro do arquivo) e salve o arquivo
source("toroidal.distance.R")

# Carregue o arquivo distancias.csv para dentro do objeto distancias
#chame a funçao distance.matrix() com o objeto distancias, e o argumento tam = 10
distancias = read.table("distancias.csv", header = T, sep = ",", dec = ".")
distance.matrix(distancias, tam = 10)

# o que aconteceu?
#A funçao retornou uma matriz de distancia com os valores variando em funçao do argumento tam = 10

# O que voce precisa fazer para que voce possa usar a funçao distance.matrix?
#Carregar novamente com a funçao source() o script alterado

# Usando a linha de comando, importe o arquivo dragoes.xsls para o objeto DnD
install.packages("writexl")
install.packages("readxl")
library(writexl)
library(readxl)
DnD = read_xlsx("dragoes.xlsx")

# Confira a estrutura do arquivo
str(DnD)

# Ha algum NA? houve algum problema com o cabeçalho? #Sim, ha NA tanto na primeira coluna quanto 1 NA na coluna de virgens
# O cabeçalho foi lido corretamente, mas apresenta caracteres que podem acarretar em problemas durante as analises futuras, como parenteses e acento ~

colnames(DnD) = c("peso em centenas de kg", "Dragao identidade", "vacas", "fazendeiros", "virgens", "aventureiros")
DnD #comando para retirar parenteses e acento no cabeçalho

#modificando uma linha do script

# Refaça o script da aula 2, salvando o objeto caixeta com a coluna coletor e desvio em um arquivo chamado caixeta_com_desvio.csv

caixeta = read.table("caixeta.csv", header=T, sep=",", as.is=T)
caixeta$coletor= "Darwin"
caixeta$desvio = caixeta$h - mean(caixeta$h)
write.csv(x = caixeta, file = "caixeta_com_desvio.csv")

1+1 #script modificado