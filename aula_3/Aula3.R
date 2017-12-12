######################################################
# Introdução Linguagem R
# Vitor Passos Rios
# Universidade Federal da Bahia
# https://github.com/vrios/Intro-Linguagem-R/wiki
#####################################################

#diferencie a função source da função load()
#a função source permite que o R diretamente aceite como input arquivos, URL, conexões ou expressões
#com essa função você pode chamar um script de dentro do outro e executar o conteúdo de um arquivo de código
#source é uma função especialmente útil para carregar funções específicas ou scripts de análise
#a função load difere porque recarrega os datasets criados com as funções save() e save.image()

#baixe para seu diretório de trabalho o arquivo toroidal.distance.R, disponível
#em https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/toroidal.distance.R, e o arquivo distancias.csv, disponivel em https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/distancias.csv

#Abra toridal.distances.R no R
# O que você precisa fazer para que as funções contidas nesse arquivo estejam disponíveis para uso no R?
#Usar a função source()

# Faça o procedimento que você descreveu acima
source("toroidal.distance.R")
ls()

toroidal.distance()
toroidal.distances()
#Funções disponíveis para uso no R

# dentro do script toroidal.distance.R, altere o nome da função toroidal.distances para distance.matrix (substitua o nome dentro do arquivo) e salve o arquivo
source("toroidal.distance.R")

# Carregue o arquivo distancias.csv para dentro do objeto distancias
#chame a função distance.matrix() com o objeto distancias, e o argumento tam = 10
distancias = read.table("distancias.csv", header = T, sep = ",", dec = ".")
distance.matrix(distancias, tam = 10)

# o que aconteceu?
#A função retornou uma matriz de distância com os valores variando em função do argumento tam = 10

# O que você precisa fazer para que você possa usar a função distance.matrix?
#Carregar novamente com a função source() o script alterado

# Usando a linha de comando, importe o arquivo dragoes.xsls para o objeto DnD
install.packages("writexl")
install.packages("readxl")
library(writexl)
library(readxl)
DnD = read_xlsx("dragoes.xlsx")

# Confira a estrutura do arquivo
str(DnD)

# Há algum NA? houve algum problema com o cabeçalho? #Sim, há NA tanto na primeira coluna quanto 1 NA na coluna de virgens
# O cabeçalho foi lido corretamente, mas apresenta caracteres que podem acarretar em problemas durante as análises futuras, como parênteses e acento ~

colnames(DnD) = c("peso em centenas de kg", "Dragao identidade", "vacas", "fazendeiros", "virgens", "aventureiros")
DnD #comando para retirar parênteses e acento no cabeçalho

# Refaça o script da aula 2, salvando o objeto caixeta com a coluna coletor e desvio em um arquivo chamado caixeta_com_desvio.csv

caixeta = read.table("caixeta.csv", header=T, sep=",", as.is=T)
caixeta$coletor= "Darwin"
caixeta$desvio = caixeta$h - mean(caixeta$h)
write.csv(x = caixeta, file = "caixeta_com_desvio.csv")