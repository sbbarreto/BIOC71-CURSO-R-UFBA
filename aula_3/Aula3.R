######################################################
# Introdu??o Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# https://github.com/vrios/Intro-Linguagem-R/wiki
#####################################################

#diferencie a fun??o source da fun??o load()
#a fun??o source permite que o R diretamente aceite como input arquivos, URL, conex?es ou express?es
#com essa fun??o voc? pode chamar um script de dentro do outro e executar o conte?do de um arquivo de c?digo
#source ? uma fun??o especialmente ?til para carregar fun??es espec?ficas ou scripts de an?lise
#a fun??o load difere porque recarrega os datasets criados com as fun??es save() e save.image()

#baixe para seu diret?rio de trabalho o arquivo toroidal.distance.R, dispon?vel
#em https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/toroidal.distance.R, e o arquivo distancias.csv, disponivel em https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/distancias.csv

#Abra toridal.distances.R no R
# O que voc? precisa fazer para que as fun??es contidas nesse arquivo estejam dispon?veis para uso no R?
#Usar a fun??o source()

# Fa?a o procedimento que voc? descreveu acima
source("toroidal.distance.R")
ls()

toroidal.distance()
toroidal.distances()
#Fun??es dispon?veis para uso no R

# dentro do script toroidal.distance.R, altere o nome da fun??o toroidal.distances para distance.matrix (substitua o nome dentro do arquivo) e salve o arquivo
source("toroidal.distance.R")

# Carregue o arquivo distancias.csv para dentro do objeto distancias
#chame a fun??o distance.matrix() com o objeto distancias, e o argumento tam = 10
distancias = read.table("distancias.csv", header = T, sep = ",", dec = ".")
distance.matrix(distancias, tam = 10)

# o que aconteceu?
#A fun??o retornou uma matriz de dist?ncia com os valores variando em fun??o do argumento tam = 10

# O que voc? precisa fazer para que voc? possa usar a fun??o distance.matrix?
#Carregar novamente com a fun??o source() o script alterado

# Usando a linha de comando, importe o arquivo dragoes.xsls para o objeto DnD
install.packages("writexl")
install.packages("readxl")
library(writexl)
library(readxl)
DnD = read_xlsx("dragoes.xlsx")

# Confira a estrutura do arquivo
str(DnD)

# H? algum NA? houve algum problema com o cabe?alho? #Sim, h? NA tanto na primeira coluna quanto 1 NA na coluna de virgens
# O cabe?alho foi lido corretamente, mas apresenta caracteres que podem acarretar em problemas durante as an?lises futuras, como par?nteses e acento ~

colnames(DnD) = c("peso em centenas de kg", "Dragao identidade", "vacas", "fazendeiros", "virgens", "aventureiros")
DnD #comando para retirar par?nteses e acento no cabe?alho

# Refa?a o script da aula 2, salvando o objeto caixeta com a coluna coletor e desvio em um arquivo chamado caixeta_com_desvio.csv

caixeta = read.table("caixeta.csv", header=T, sep=",", as.is=T)
caixeta$coletor= "Darwin"
caixeta$desvio = caixeta$h - mean(caixeta$h)
write.csv(x = caixeta, file = "caixeta_com_desvio.csv")