###################################
# Introdu��o Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# Aula 4 - Gr�ficos
###################################

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
area.cate <- rep(c("pequeno", "grande"), each=6)

plot(riqueza~area) #gera um gr�fico de dispers�o, sendo que cada ponto no plot representa uma das r�plicas 
plot(area,riqueza) #o mesmo que o anterior
boxplot(riqueza~area.cate) #gera gr�fico de boxplot, onde uma das vari�veis � categ�rica
barplot(riqueza) #gera gr�fico de barras que mostra cada ponto da vari�vel especificada como uma barra

#Exerc�cio 1 - Fazendo os Primeiros Gr�ficos
#Construa "plot", boxplot e barplot usando as vari�veis do conjunto de dados Conjunto de Dados: Dados de Biomassa de �rvores de Eucalyptus Saligna, para explorar rela��es entre:

esaligna <- read.csv("esaligna.csv", head=T, sep=",", dec=".")

plot(esaligna$dap~esaligna$ht) #gera gr�fico de dispers�o de dap em fun��o de ht
boxplot(esaligna$dap~esaligna$ht) #gr�fico gerado, mas sem um padr�o de boxplot porque falta uma vari�vel categ�rica
barplot(esaligna$dap~esaligna$ht) #Error in barplot.default(esaligna$dap ~ esaligna$ht) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot s� aceita uma vari�vel

plot(esaligna$ht, esaligna$tronco) #gera gr�fico de dispers�o de ht em fun��o de tronco
boxplot(esaligna$ht~esaligna$tronco) #gr�fico gerado, mas sem um padr�o de boxplot porque falta uma vari�vel categ�rica
barplot(esaligna$ht~esaligna$tronco) #Error in barplot.default(esaligna$ht ~ esaligna$tronco) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot s� aceita uma vari�vel

boxplot(esaligna$dap~esaligna$classe)#gera gr�fico de boxplot de dap para cada uma das classes
plot(esaligna$dap~esaligna$classe) #gera um gr�fico, mas n�o como um plot, mas sim semelhante ao gr�fico anterior de boxplot porque classe � uma vari�vel categ�rica e o R j� interpreta isso
barplot(esaligna$dap~esaligna$classe) #Error in barplot.default(esaligna$dap ~ esaligna$classe) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot s� aceita uma vari�vel

boxplot(esaligna$dap~esaligna$talhao)#gera um gr�fico de boxplot de dap para cada um dos talhao
plot(esaligna$dap~esaligna$talhao) #gr�fico gerado, mas com os pontos distribu�dos em tr�s pontos do gr�fico porque talhao � uma vari�vel categ�rica
barplot(esaligna$dap~esaligna$talhao) #Error in barplot.default(esaligna$dap ~ esaligna$talhao) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot s� aceita uma vari�vel

barplot(esaligna$dap) #gr�fico de barplot gerado para dap
plot(esaligna$dap) #gr�fico gerado, mas com os pontos distribu�dos com base em apenas uma vari�vel
boxplot(esaligna$dap) #gr�fico gerado com apenas um boxplot correspondente ao dap

barplot(esaligna$ht) #gr�fico de barplot gerado para ht
boxplot(esaligna$ht) #gr�fico gerado com apenas um boxplot correspondente ao dap
plot(esaligna$ht) #gr�fico gerado, mas com os pontos distribu�dos com base em apenas uma vari�vel

par(las=1) #fun��o par() especifica os par�metros antes de chamar a fun��o plot. Nesse caso, pe�o as legendas dos eixos do gr�fico escritas na horizontal
plot(riqueza~area, las=3) #gera gr�fico com legendas na vertical

par(cex=2)
plot(riqueza~area, cex=2) #gera um plot com todas as fontes e pontos de tamanho 2

#Exerc�cio 2 - Aprendendo a Editar Gr�ficos

?plot #help da fun��o plot

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)

plot(riqueza~area)

#Mude:
  
#  O nome do eixo x para "Tamanho da Ilha (ha)"

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)")

# O nome do eixo y para "Riqueza de Esp�cies"

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Esp�cies")

# O t�tulo do gr�fico para "Aves das Ilhas Samoa"

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Esp�cies", main = "Aves das Ilhas Samoa")

?par #help da fun��o par

#Usando o mesmo gr�fico anterior, mude:
  
#  O tipo de ponto (numero de 0 a 25)

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Esp�cies", main = "Aves das Ilhas Samoa", cex = 0.7)

#O tamanho dos pontos e legendas
par(cex = 1)
plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Esp�cies", main = "Aves das Ilhas Samoa", cex = 1)

#A dire��o da escala do gr�fico (para ficar tudo na horizontal)

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Esp�cies", main = "Aves das Ilhas Samoa", las = 3)

#O tipo de fonte das legendas (para ficar tudo como em Times New Roman - dica= "serif")

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Esp�cies", main = "Aves das Ilhas Samoa", family = "serif")

#Exemplos de par(mfrow=c())

par(mfrow=c(2,1), bg="gray93") #par(mfrow=c()) controla "quantas figuras" ser�o desenhadas dentro de um mesmo dispositivo. O vetor contido dentro da fun��o mfrow=() controla o n�mero de gr�ficos que ser�o desenhados no eixo x (1� n�mero) e no eixo y (2� n�mero)
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gr�ficos, um embaixo do outro (duas linhas, uma coluna), sendo um de dispers�o e um boxplot

par(mfrow=c(1,2))
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gr�ficos, um do lado do outro (uma linha, duas colunas), sendo um de dispers�o e um boxplot

#Exemplos de par(mar=c())
#O par(mar=c()) controla o "tamanho das margens" do gr�fico e como a figura ficar� disposta dentro do dispositivo. O vetor contido dentro da fun��o mar=(), controla as posi��es das margens, sendo que o 1� n�mero controla a margem da parte de baixo do gr�fico, o 2� controla a margem do lado esquerdo, o 3� n�mero controla a parte de cima e o 4� n�mero controla o tamanho da margem do lado direito do gr�fico.

par(mfrow=c(2,1))
par(mar=c(4,14,2,6))
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gr�ficos (duas linhas, uma coluna), sendo um plot e um boxplot com uma margem grande do lado esquerdo

par(mfrow=c(1,2))
par(mar=c(14,4,8,2))
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gr�ficos (uma linha, duas colunas), sendo um plot e um boxplot com uma margem grande na parte de baixo e na parte de cima dos gr�ficos

par(mfrow=c(1,2))
par(mar=c(8,4,8,1))
plot(riqueza~area)
par(mar=c(14,2,4,0.5))
boxplot(riqueza~area.cate)
#gera dois gr�ficos um do lado do outro e com posi��es diferentes na �rea de disposi��o. O boxplot est� posicionado mais acima do plot

#Exerc�cio 3 - Mudando diferentes gr�ficos

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
area.cate <- rep(c("pequeno", "grande"), each=6)

plot(riqueza~area) #gera um plot da riqueza em fun��o da �rea

plot(riqueza~area, bty="l", tcl=0.3) #gera um novo plot que n�o tem as linhas superior e direita e os tra�os das vari�veis est�o para dentro do gr�fico

boxplot(riqueza~area.cate, bty="l", tcl=0.3) #mudou para um gr�fico de boxplot

par(bty="l")
par(tcl=0.3)
boxplot(riqueza~area.cate)
# fez as mesmas modifica��es de tirar as linhas superior e direita do gr�fico e colocar os tra�os das vari�veis para dentro do gr�fico, mas agora para o boxplot

#Exerc�cio 4

#Usando as vari�veis:
  
riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
abundancia <- rev(riqueza)

#Crie gr�ficos inserindo os par�metros abaixo.
#lines() Para inserir linhas retas ou curvas n�o-param�tricas (como lowess, loess, gam, etc).

plot(riqueza~area)
lines(lowess(area, riqueza))

#abline() Para inserir linhas de tend�ncia criadas a partir de um modelo linear. Para isso � primeiro necess�rio criar o modelo, para depois criar a linha.

model <-  lm(riqueza~area) #criando o modelo
plot(riqueza~area) #fazendo o plot
abline(model) #inserindo a linha de tend�ncia

#Com a fun��o abline voc� pode tamb�m inserir uma linha reta com intercepto e inclina��o definidos por voc�, com os dois primeiros argumentos:
  
plot(riqueza~area)
abline(15,0.4)

#A fun��o abline tamb�m serve para acrescentar linhas verticais e horizontais, com os argumentos v e h. No c�digo abaixo tra�amos estas linhas passando pelas m�dias das duas vari�veis do diagrama de dispers�o:

plot(riqueza~area) #fazendo o plot
abline(v=mean(area)) #adicionando a linha vertical com a m�dia da area
abline(h=mean(riqueza)) #adicionando a linha horizontal com a m�dia da riqueza

#text() Para inserir texto dentro do gr�fico. O texto pode ser uma letra, um s�mbolo (muito usado para mostrar diferenciar classes no gr�fico), uma palavra ou at� mesmo uma frase.

plot(riqueza~area)
text(20,40,"texto")

#mtext() Este comando acrescenta texto nas margens do gr�fico ou da janela gr�fica.

plot(riqueza~area)
mtext("legenda no lado errado", side=4, line=0.9, at=20,cex=2, family="serif")

#par(new=TRUE) Este argumento ir� desenhar o novo gr�fico sobre o gr�fico anterior.

plot(riqueza~area)
par(new=TRUE)
plot(abundancia~area)

#axis() Para se inserir um eixo novo.

plot(riqueza~area)
par(new=TRUE)
plot(abundancia~area, axes=FALSE, ann=FALSE, pch=16)
axis(4) #adicionando um novo eixo do lado direito

#axes=F para suprimir a cria��o dos eixos do gr�fico inicial de abund�ncia
#ann=F para suprimir a legenda de abund�ncia do lado direito
#pch=16 ou qualquer outro n�mero para diferenciar os pontos entre os dois plots
#Para inserir a legenda de abund�ncia do lado direito ser� necess�rio usar mtext(), mas da� ser� necess�rio mudar outros par�metros como dist�ncia da margem

#arrows(), rect(), polygon()
#Para inserir flechas ou barras de erros usa-se arrows(). J� para inserir ret�ngulos, pol�gonos e outros formatos usa-se rect() e polygon().

plot(riqueza~area)
rect(20,20,40,40) #desenha um ret�ngulo no plot riqueza~area com as posi��es especificadas

jpeg(filename = "Algumnome.jpg") #argumento filename especifica o nome do arquivo a salvar. 

jpeg(filename = "Rplotaula.jpg", width = 480, height = 480, 
     units = "px", pointsize = 12, quality = 100,
     bg = "white",  res = NA, restoreConsole = TRUE)

par(mfrow=c(1,2))
par(mar=c(14,4,8,2))
plot(riqueza~area)
boxplot(riqueza~area.cate)

dev.off() #fecha o dispositivo e salva um arquivo em jpeg chamado Rplotaula com dois gr�ficos (um plot e um boxplot) um do lado do outro (1 linha e duas colunas) com as especifica��es dadas acima (exemplo: largura e altura = 480).

#png("meugrafico%02d.png") para gravar uma sequ�ncia de gr�ficos


######################################################################


#Exerc�cios 5 - Cria��o de Gr�ficos

#Usando o conjunto de dados Conjunto de Dados: Dados de Biomassa de �rvores de Eucalyptus Saligna, construa os seguintes gr�ficos:

esaligna  = read.table("esaligna.csv", header=T, sep=",", dec=".")

#5.1 Editando alguns par�metros gr�ficos

#Crie um gr�fico de dispers�o entre "dap"1) e "ht"2) com:

plot(esaligna$dap~esaligna$ht)

#Legendas dos eixos com nomes das vari�veis e suas unidades

plot(esaligna$dap~esaligna$ht, xlab = "Altura (m)", ylab = "Di�metros � altura do peito (cm)")

#Marca��es do eixos (ticks) para dentro da �rea do gr�fico

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Di�metros � altura do peito (cm)", tcl=0.3)

#Apenas dois eixos (formato "L")

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Di�metros � altura do peito (cm)", tcl=0.3, bty="l")

#T�tulo informativo

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Di�metros � altura do peito (cm)", tcl=0.3, bty="l", main="Rela��o entre DAP e altura")

#Tamanho das fontes maiores que o padr�o

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Di�metros � altura do peito (cm)", cex.lab=1.5, tcl=0.3, bty="l", cex.axis=1.5, main="Rela��o entre DAP e altura", cex.main=1.5)

#5.2 Dois gr�ficos juntos

#Use as vari�veis "dap" e "talhao" para construir dois gr�ficos, colocando-os lado a lado. O primeiro deve ser um gr�fico de desenho de caixa (boxplot) da vari�vel "dap" em fun��o do fator "talh�o". O segundo deve ter apenas a m�dia e uma barra de desvio-padr�o do dap, para cada talh�o.

#Insira tamb�m uma letra para dizer qual � o gr�fico "a" e qual � o "b" (tanto faz, quem � um e quem � outro).
#Dica: voc�s ter�o que calcular a m�dia e os desvios-padr�o do dap das �rvores em cada talh�o. Depois crie uma matriz com estes valores e crie o plot destes valores.

boxplot(esaligna$dap~esaligna$talhao) #gerando o boxplot dap em fun��o de talhao

esaligna$talhao=as.factor(esaligna$talhao)
media.dap = aggregate(x=esaligna$dap,by = list(esaligna$talhao), FUN = mean, na.rm=T)
desvio.dap = aggregate(x=esaligna$dap,by = list(esaligna$talhao), FUN = sd, na.rm=T)

(matriz.dap = matrix(c(media.dap$x, desvio.dap$x),ncol=2))

# Criando gr�fico a
par(mfrow = c(1,2))
boxplot(esaligna$dap~esaligna$talhao, ylab="DAP", xlab="Talh�o") #gerando o boxplot
mtext("a") #colocando a letra a

# Criando gr�fico b
(y0=media.dap$x-desvio.dap$x)
(y1=media.dap$x+desvio.dap$x)
(limite=range(max(y1),min(y0)))

plot(media.dap$x, ylab="DAP (m�dia)", xlab="Talh�o",  ylim = limite) #fazendo o plot
arrows(x0=1:6, y0=media.dap$x-desvio.dap$x, x1=1:6, y1=media.dap$x+desvio.dap$x, length = 0.05, angle = 90, code = 3) #colocando os segmentos
mtext("b") #colocando a letra b

#5.3 Adivinhando o c�digo
#Leia os dados deste arquivo e usando as vari�veis "x1" e "y1" e "x2" e "y2" tente reproduzir esta figura:

exercicio3 = read.csv("exercicio3.csv", header=T, sep=",", dec=".")

par(mfrow=c(1,2)) #par�metro para criar gr�ficos em uma linha e duas colunas
par(bty="l", tcl=0.2, family="serif") #par�metro para criar gr�fico sem as linhas superior e direita e letra tipo times
par(mar=c(4,4,2,2)) #ajustando tamanho das margens
#dados = lm(y1~x1,data=exercicio3) #criando modelo para a linha do primeiro gr�fico, mas a linha gerada depois n�o apresenta a posi��o da figura original
#Gr�fico a
plot(exercicio3$x1,exercicio3$y1
     ,ylab=expression(bold("Euclidean distances")) #nome no eixo y em negrito
     ,xlim=c(0.5,2.2) #estabelecendo os limites do eixo x
     ,xlab="Log(Patch size) (ha)" #nome no eixo x
     ,cex.lab=1 #tamanho da fonte das legendas
     ,cex.axis=0.75 #tamanho da fonte dos eixos
     ,pch=17 #s�mbolos como tri�ngulos
     ,cex=1 #tamanho dos s�mbolos
     ,ylim=c(0,3) #limite de y
     ,mgp=c(1.5,0.5,0)) #dist�ncia das legendas dos eixos x e y
#abline(dados,lwd=2.5) #adicionando a linha com espessura mais grossa, mas n�o consegui gerar na posi��o como aparece na imagem
segments(x0=0.5, y0=2.2, x1=2.2, y1=1, lwd=2.5) #gerando a linha com posi��o mais correta
text(2.5,3,"a",cex=1.5) #adicionando a letra a no canto superior direito do gr�fico

#Gr�fico b
par(mgp=c(0.9,0.7,0))
boxplot(x2~y2, data=exercicio3
        ,ylab=NULL #sem nome no eixo y
        ,xlab=NULL #sem nome no eixo x
        ,cex.lab=0.85 #tamanho da fonte das legendas
        ,cex.axis=0.75 #tamanho da fonte dos eixos
        ,ylim=c(0,3) #limite de y
        ,names=c("Small", "Medium \n Edge","Medium \n Interior", "Large \n Edge", "Large\n Interior", "Control") #colocando os nomes
        ,outline=F)#suprimindo outliers
text(6,3,"b",cex=1.5) #adicionando a letra b
text(x=1:5, y=3, labels=c("*", "*","**","*","***"), cex=1.5) #adicionando asteriscos de signific�ncia