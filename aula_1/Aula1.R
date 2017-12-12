######################################################
# Introdução Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# https://github.com/vrios/Intro-Linguagem-R/wiki
#####################################################

#####Usando o R como calculadora#####

#Digite no R os códigos abaixo.
#Dica: escreva neste arquivo, ao lado do comando, o que ele faz,
#como no exemplo abaixo

1 + 1 #soma os valores

# O símbolo # representa um comentário, isto é, tudo que vier à direita
# dele será ignorado quando o R ler o arquivo

34 - 72.3 #subtrai 72.3 de 34

# use pontos como separador de decimal. O que acontece se você usar virgula?
#Aparece a mensagem Error: unexpected ',' in "34 - 72,"
78 / 3 #divide 78 por 3
2 ^ 3 #eleva 2 à terceira potência
3 * 2 #multiplica 3 por 2
sqrt(2) #retorna a raiz quadrada de 2
pi #dá o valor de pi
log(x = 3,base = 10) #fornece log de 3 na base 10
exp(x = 1) #fornece log de 1 por default na base 10
#o que querem dizer estes valores dentro do parêntese? porque usamos o = aqui? 
#Os valores dentro dos parênteses são os argumentos dentro da função, e usamos o = aqui porque estamos atribuindo valores aos objetos

#####Atribuição de valores e criação de objetos#####
#O que os comandos abaixo fazem?
objeto1=3 #Cria um objeto nomeado objeto1 com o elemento 3 dentro
objeto1 #Mostra o que tem dentro do objeto1
objeto = 42 #Cria um objeto nomeado objeto com o elemento 42
objeto #Mostra o que tem dentro do objeto
objeto.2 <- 42 #Cria um objeto nomeado objeto.2 com o elemento 42 dentro
objeto.2 #Mostra o que tem dentro do objeto.2
# Os símbolos = e <- fazem basicamente a mesma coisa. Que coisa?
#Faz a atribuição de valores dentro de objetos
objeto.cubico <- objeto^3 #Cria um novo objeto onde o elemento dentro do objeto nomeado objeto é elevado à terceira potência
resultado.1 <- (objeto.cubico / 3) + 7 #Cria um novo objeto onde o elemento de objeto.cubico é dividido por 3 e em seguida somado com 7
objeto.texto <- "texto sempre vem entre aspas" #Cria um objeto nomeado objeto.texto com o elemento "texto sempre vem entre aspas" dentro
objeto.texto #Mostra o que tem dentro do objeto.texto
objeto.texto.2 <- "42" #Cria um objeto nomeado objeto.texto.2 com o elemento 42 dentro, sendo que o número é transformado em texto

objeto.vetor.1 = 1:34 #Cria um objeto com uma sequência numérica de 1 a 34
objeto.vetor.1 #Mostra o que tem dentro desse objeto
objeto.vetor2 = c(1, 74.5, 48) #Atribui esses valores ao objeto.vetor2
objeto.vetor2 #Mostra o que tem dentro do objeto

?seq #Mostra na aba help as informações sobre a função seq
objeto.vetor3 = seq(from = 20, to = 32, by = 0.5) #Cria uma sequência de 20 a 32 com intervalos de 0.5

#podemos calcular estatísticas com o r
mean(objeto.vetor3) #media
var(objeto.vetor3) #variancia
median(objeto.vetor3) #mediana
min(objeto.vetor3) #Fornece o menor valor dentre os elementos de objeto.vetor3
max(objeto.vetor3) #Fornece o maior valor dentre os elementos de objeto.vetor3
diff(objeto.vetor3) #calcula a diferença entre os elementos sucessivos do vetor

# R também faz comparações entre objetos
42 > 7 #Compara se o valor 42 é maior do que 7
objeto == objeto.2 #Compara se objeto é exatamente igual a objeto.2 nos elementos numéricos
objeto == objeto.texto.2 #Compara se objeto é exatamente igual a objeto.texto.2 nos elementos de texto
# o que aconteceu acima?
#Apareceu TRUE em todos os comandos acima
7.1 <= 7.001 #Compara se 7.1 é menor igual a 7.001
?"<" #Mostra na aba help as informações sobre o operador < e outros utilizados para comparação

#pedindo ajuda
?pi #Mostra na aba help as informações sobre pi e outras constantes
?log #Mostra na aba help as informações sobre a função log
??lm #Mostra na aba help todas as citações da função lm
help(log) #Também mostra na aba help as informações sobre a função log
help.search("anova") #Mostra na aba help todas as citações da função anova

#####Arquivos e diretórios#####
# Como você descobre o diretório de trabalho?
#Utilizando o comando getwd()

# Como você lista o conteúdo do diretório de trabalho?
#Utilizando o comando dir()

# Como você define o diretório de trabalho?
#Utilizando o comando setwd()

# Como você carrega um arquivo de script?
#Utilizando o menu File > Open File e carregando o arquivo com extensão .R ou pelo atalho ctrl+o

# Como você salva os objetos que criou?
#Utilizando o comando save(nomedoobjeto, file="nomedoarquivo.RData")

# Como você carrega os objetos que criou?
#Utilizando o comando load(file="nomedoarquivo.RData")

# Como ver quis objetos estão na sua área de trabalho?
#Utilizando o comando ls()

# Como remover objetos da área de trabalho?
#Utilizando o comando rm()

##### Lidando com erros#####
# O que acontece quando você digita os comandos abaixo? Como consertar cada erro?
objetol #Error: object 'objetol' not found. Consertar removendo o l
objeto .texto #Error: unexpected symbol in "objeto .texto". Consertar removendo o espaço antes do ponto
Objeto #Error: object 'Objeto' not found. Consertar colocando a letra O inicial em minúsculo
source("chuchu.R") #Error in file(filename, "r", encoding = encoding) : 
#cannot open the connection
#In addition: Warning message:
#  In file(filename, "r", encoding = encoding) :
#  cannot open file 'chuchu.R': No such file or directory
#Colocar o nome de um arquivo existente
source(chuchu.R) #Error in source(chuchu.R) : object 'chuchu.R' not found
#Colocar chuchu.R entre aspas considerando que o arquivo exista
setwd("C:/CavernaDoDragão") #Error in setwd("C:/CavernaDoDragão") : cannot change working directory
#Colocar o nome de um diretório existente 
getwd #function () 
#.Internal(getwd())
#<bytecode: 0x000000000f562e40>
#  <environment: namespace:base>
#Problema é resolvido colocando () após o getwd para exibir o diretório atual
Getwd() #Error in Getwd() : could not find function "Getwd"
#Problema é resolvido colocando o G em minúsculo
#dica: quando o R der erro, copie e cole a mensagem de erro no google