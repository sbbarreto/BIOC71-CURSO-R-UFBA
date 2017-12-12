###################################
# Introdução Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# Aula 4 - Gráficos
###################################

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
area.cate <- rep(c("pequeno", "grande"), each=6)

plot(riqueza~area) #gera um gráfico de dispersão, sendo que cada ponto no plot representa uma das réplicas 
plot(area,riqueza) #o mesmo que o anterior
boxplot(riqueza~area.cate) #gera gráfico de boxplot, onde uma das variáveis é categórica
barplot(riqueza) #gera gráfico de barras que mostra cada ponto da variável especificada como uma barra

#Exercício 1 - Fazendo os Primeiros Gráficos
#Construa "plot", boxplot e barplot usando as variáveis do conjunto de dados Conjunto de Dados: Dados de Biomassa de Árvores de Eucalyptus Saligna, para explorar relações entre:

esaligna <- read.csv("esaligna.csv", head=T, sep=",", dec=".")

plot(esaligna$dap~esaligna$ht) #gera gráfico de dispersão de dap em função de ht
boxplot(esaligna$dap~esaligna$ht) #gráfico gerado, mas sem um padrão de boxplot porque falta uma variável categórica
barplot(esaligna$dap~esaligna$ht) #Error in barplot.default(esaligna$dap ~ esaligna$ht) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot só aceita uma variável

plot(esaligna$ht, esaligna$tronco) #gera gráfico de dispersão de ht em função de tronco
boxplot(esaligna$ht~esaligna$tronco) #gráfico gerado, mas sem um padrão de boxplot porque falta uma variável categórica
barplot(esaligna$ht~esaligna$tronco) #Error in barplot.default(esaligna$ht ~ esaligna$tronco) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot só aceita uma variável

boxplot(esaligna$dap~esaligna$classe)#gera gráfico de boxplot de dap para cada uma das classes
plot(esaligna$dap~esaligna$classe) #gera um gráfico, mas não como um plot, mas sim semelhante ao gráfico anterior de boxplot porque classe é uma variável categórica e o R já interpreta isso
barplot(esaligna$dap~esaligna$classe) #Error in barplot.default(esaligna$dap ~ esaligna$classe) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot só aceita uma variável

boxplot(esaligna$dap~esaligna$talhao)#gera um gráfico de boxplot de dap para cada um dos talhao
plot(esaligna$dap~esaligna$talhao) #gráfico gerado, mas com os pontos distribuídos em três pontos do gráfico porque talhao é uma variável categórica
barplot(esaligna$dap~esaligna$talhao) #Error in barplot.default(esaligna$dap ~ esaligna$talhao) : 'height' deve ser um vetor ou uma matriz. Erro porque barplot só aceita uma variável

barplot(esaligna$dap) #gráfico de barplot gerado para dap
plot(esaligna$dap) #gráfico gerado, mas com os pontos distribuídos com base em apenas uma variável
boxplot(esaligna$dap) #gráfico gerado com apenas um boxplot correspondente ao dap

barplot(esaligna$ht) #gráfico de barplot gerado para ht
boxplot(esaligna$ht) #gráfico gerado com apenas um boxplot correspondente ao dap
plot(esaligna$ht) #gráfico gerado, mas com os pontos distribuídos com base em apenas uma variável

par(las=1) #função par() especifica os parâmetros antes de chamar a função plot. Nesse caso, peço as legendas dos eixos do gráfico escritas na horizontal
plot(riqueza~area, las=3) #gera gráfico com legendas na vertical

par(cex=2)
plot(riqueza~area, cex=2) #gera um plot com todas as fontes e pontos de tamanho 2

#Exercício 2 - Aprendendo a Editar Gráficos

?plot #help da função plot

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)

plot(riqueza~area)

#Mude:
  
#  O nome do eixo x para "Tamanho da Ilha (ha)"

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)")

# O nome do eixo y para "Riqueza de Espécies"

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Espécies")

# O título do gráfico para "Aves das Ilhas Samoa"

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Espécies", main = "Aves das Ilhas Samoa")

?par #help da função par

#Usando o mesmo gráfico anterior, mude:
  
#  O tipo de ponto (numero de 0 a 25)

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Espécies", main = "Aves das Ilhas Samoa", cex = 0.7)

#O tamanho dos pontos e legendas
par(cex = 1)
plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Espécies", main = "Aves das Ilhas Samoa", cex = 1)

#A direção da escala do gráfico (para ficar tudo na horizontal)

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Espécies", main = "Aves das Ilhas Samoa", las = 3)

#O tipo de fonte das legendas (para ficar tudo como em Times New Roman - dica= "serif")

plot(riqueza~area, xlab = "Tamanho da Ilha (ha)", ylab = "Riqueza de Espécies", main = "Aves das Ilhas Samoa", family = "serif")

#Exemplos de par(mfrow=c())

par(mfrow=c(2,1), bg="gray93") #par(mfrow=c()) controla "quantas figuras" serão desenhadas dentro de um mesmo dispositivo. O vetor contido dentro da função mfrow=() controla o número de gráficos que serão desenhados no eixo x (1° número) e no eixo y (2° número)
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gráficos, um embaixo do outro (duas linhas, uma coluna), sendo um de dispersão e um boxplot

par(mfrow=c(1,2))
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gráficos, um do lado do outro (uma linha, duas colunas), sendo um de dispersão e um boxplot

#Exemplos de par(mar=c())
#O par(mar=c()) controla o "tamanho das margens" do gráfico e como a figura ficará disposta dentro do dispositivo. O vetor contido dentro da função mar=(), controla as posições das margens, sendo que o 1° número controla a margem da parte de baixo do gráfico, o 2° controla a margem do lado esquerdo, o 3° número controla a parte de cima e o 4° número controla o tamanho da margem do lado direito do gráfico.

par(mfrow=c(2,1))
par(mar=c(4,14,2,6))
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gráficos (duas linhas, uma coluna), sendo um plot e um boxplot com uma margem grande do lado esquerdo

par(mfrow=c(1,2))
par(mar=c(14,4,8,2))
plot(riqueza~area)
boxplot(riqueza~area.cate)
#gera dois gráficos (uma linha, duas colunas), sendo um plot e um boxplot com uma margem grande na parte de baixo e na parte de cima dos gráficos

par(mfrow=c(1,2))
par(mar=c(8,4,8,1))
plot(riqueza~area)
par(mar=c(14,2,4,0.5))
boxplot(riqueza~area.cate)
#gera dois gráficos um do lado do outro e com posições diferentes na área de disposição. O boxplot está posicionado mais acima do plot

#Exercício 3 - Mudando diferentes gráficos

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
area.cate <- rep(c("pequeno", "grande"), each=6)

plot(riqueza~area) #gera um plot da riqueza em função da área

plot(riqueza~area, bty="l", tcl=0.3) #gera um novo plot que não tem as linhas superior e direita e os traços das variáveis estão para dentro do gráfico

boxplot(riqueza~area.cate, bty="l", tcl=0.3) #mudou para um gráfico de boxplot

par(bty="l")
par(tcl=0.3)
boxplot(riqueza~area.cate)
# fez as mesmas modificações de tirar as linhas superior e direita do gráfico e colocar os traços das variáveis para dentro do gráfico, mas agora para o boxplot

#Exercício 4

#Usando as variáveis:
  
riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
abundancia <- rev(riqueza)

#Crie gráficos inserindo os parâmetros abaixo.
#lines() Para inserir linhas retas ou curvas não-paramétricas (como lowess, loess, gam, etc).

plot(riqueza~area)
lines(lowess(area, riqueza))

#abline() Para inserir linhas de tendência criadas a partir de um modelo linear. Para isso é primeiro necessário criar o modelo, para depois criar a linha.

model <-  lm(riqueza~area) #criando o modelo
plot(riqueza~area) #fazendo o plot
abline(model) #inserindo a linha de tendência

#Com a função abline você pode também inserir uma linha reta com intercepto e inclinação definidos por você, com os dois primeiros argumentos:
  
plot(riqueza~area)
abline(15,0.4)

#A função abline também serve para acrescentar linhas verticais e horizontais, com os argumentos v e h. No código abaixo traçamos estas linhas passando pelas médias das duas variáveis do diagrama de dispersão:

plot(riqueza~area) #fazendo o plot
abline(v=mean(area)) #adicionando a linha vertical com a média da area
abline(h=mean(riqueza)) #adicionando a linha horizontal com a média da riqueza

#text() Para inserir texto dentro do gráfico. O texto pode ser uma letra, um símbolo (muito usado para mostrar diferenciar classes no gráfico), uma palavra ou até mesmo uma frase.

plot(riqueza~area)
text(20,40,"texto")

#mtext() Este comando acrescenta texto nas margens do gráfico ou da janela gráfica.

plot(riqueza~area)
mtext("legenda no lado errado", side=4, line=0.9, at=20,cex=2, family="serif")

#par(new=TRUE) Este argumento irá desenhar o novo gráfico sobre o gráfico anterior.

plot(riqueza~area)
par(new=TRUE)
plot(abundancia~area)

#axis() Para se inserir um eixo novo.

plot(riqueza~area)
par(new=TRUE)
plot(abundancia~area, axes=FALSE, ann=FALSE, pch=16)
axis(4) #adicionando um novo eixo do lado direito

#axes=F para suprimir a criação dos eixos do gráfico inicial de abundância
#ann=F para suprimir a legenda de abundância do lado direito
#pch=16 ou qualquer outro número para diferenciar os pontos entre os dois plots
#Para inserir a legenda de abundância do lado direito será necessário usar mtext(), mas daí será necessário mudar outros parâmetros como distância da margem

#arrows(), rect(), polygon()
#Para inserir flechas ou barras de erros usa-se arrows(). Já para inserir retângulos, polígonos e outros formatos usa-se rect() e polygon().

plot(riqueza~area)
rect(20,20,40,40) #desenha um retângulo no plot riqueza~area com as posições especificadas

jpeg(filename = "Algumnome.jpg") #argumento filename especifica o nome do arquivo a salvar. 

jpeg(filename = "Rplotaula.jpg", width = 480, height = 480, 
     units = "px", pointsize = 12, quality = 100,
     bg = "white",  res = NA, restoreConsole = TRUE)

par(mfrow=c(1,2))
par(mar=c(14,4,8,2))
plot(riqueza~area)
boxplot(riqueza~area.cate)

dev.off() #fecha o dispositivo e salva um arquivo em jpeg chamado Rplotaula com dois gráficos (um plot e um boxplot) um do lado do outro (1 linha e duas colunas) com as especificações dadas acima (exemplo: largura e altura = 480).

#png("meugrafico%02d.png") para gravar uma sequência de gráficos


######################################################################


#Exercícios 5 - Criação de Gráficos

#Usando o conjunto de dados Conjunto de Dados: Dados de Biomassa de Árvores de Eucalyptus Saligna, construa os seguintes gráficos:

esaligna  = read.table("esaligna.csv", header=T, sep=",", dec=".")

#5.1 Editando alguns parâmetros gráficos

#Crie um gráfico de dispersão entre "dap"1) e "ht"2) com:

plot(esaligna$dap~esaligna$ht)

#Legendas dos eixos com nomes das variáveis e suas unidades

plot(esaligna$dap~esaligna$ht, xlab = "Altura (m)", ylab = "Diâmetros à altura do peito (cm)")

#Marcações do eixos (ticks) para dentro da área do gráfico

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Diâmetros à altura do peito (cm)", tcl=0.3)

#Apenas dois eixos (formato "L")

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Diâmetros à altura do peito (cm)", tcl=0.3, bty="l")

#Título informativo

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Diâmetros à altura do peito (cm)", tcl=0.3, bty="l", main="Relação entre DAP e altura")

#Tamanho das fontes maiores que o padrão

plot(esaligna$dap~esaligna$ht, xlab="Altura (m)", ylab="Diâmetros à altura do peito (cm)", cex.lab=1.5, tcl=0.3, bty="l", cex.axis=1.5, main="Relação entre DAP e altura", cex.main=1.5)

#5.2 Dois gráficos juntos

#Use as variáveis "dap" e "talhao" para construir dois gráficos, colocando-os lado a lado. O primeiro deve ser um gráfico de desenho de caixa (boxplot) da variável "dap" em função do fator "talhão". O segundo deve ter apenas a média e uma barra de desvio-padrão do dap, para cada talhão.

#Insira também uma letra para dizer qual é o gráfico "a" e qual é o "b" (tanto faz, quem é um e quem é outro).
#Dica: vocês terão que calcular a média e os desvios-padrão do dap das árvores em cada talhão. Depois crie uma matriz com estes valores e crie o plot destes valores.

boxplot(esaligna$dap~esaligna$talhao) #gerando o boxplot dap em função de talhao

esaligna$talhao=as.factor(esaligna$talhao)
media.dap = aggregate(x=esaligna$dap,by = list(esaligna$talhao), FUN = mean, na.rm=T)
desvio.dap = aggregate(x=esaligna$dap,by = list(esaligna$talhao), FUN = sd, na.rm=T)

(matriz.dap = matrix(c(media.dap$x, desvio.dap$x),ncol=2))

# Criando gráfico a
par(mfrow = c(1,2))
boxplot(esaligna$dap~esaligna$talhao, ylab="DAP", xlab="Talhão") #gerando o boxplot
mtext("a") #colocando a letra a

# Criando gráfico b
(y0=media.dap$x-desvio.dap$x)
(y1=media.dap$x+desvio.dap$x)
(limite=range(max(y1),min(y0)))

plot(media.dap$x, ylab="DAP (média)", xlab="Talhão",  ylim = limite) #fazendo o plot
arrows(x0=1:6, y0=media.dap$x-desvio.dap$x, x1=1:6, y1=media.dap$x+desvio.dap$x, length = 0.05, angle = 90, code = 3) #colocando os segmentos
mtext("b") #colocando a letra b

#5.3 Adivinhando o código
#Leia os dados deste arquivo e usando as variáveis "x1" e "y1" e "x2" e "y2" tente reproduzir esta figura:

exercicio3 = read.csv("exercicio3.csv", header=T, sep=",", dec=".")

par(mfrow=c(1,2)) #parâmetro para criar gráficos em uma linha e duas colunas
par(bty="l", tcl=0.2, family="serif") #parâmetro para criar gráfico sem as linhas superior e direita e letra tipo times
par(mar=c(4,4,2,2)) #ajustando tamanho das margens
#dados = lm(y1~x1,data=exercicio3) #criando modelo para a linha do primeiro gráfico, mas a linha gerada depois não apresenta a posição da figura original
#Gráfico a
plot(exercicio3$x1,exercicio3$y1
     ,ylab=expression(bold("Euclidean distances")) #nome no eixo y em negrito
     ,xlim=c(0.5,2.2) #estabelecendo os limites do eixo x
     ,xlab="Log(Patch size) (ha)" #nome no eixo x
     ,cex.lab=1 #tamanho da fonte das legendas
     ,cex.axis=0.75 #tamanho da fonte dos eixos
     ,pch=17 #símbolos como triângulos
     ,cex=1 #tamanho dos símbolos
     ,ylim=c(0,3) #limite de y
     ,mgp=c(1.5,0.5,0)) #distância das legendas dos eixos x e y
#abline(dados,lwd=2.5) #adicionando a linha com espessura mais grossa, mas não consegui gerar na posição como aparece na imagem
segments(x0=0.5, y0=2.2, x1=2.2, y1=1, lwd=2.5) #gerando a linha com posição mais correta
text(2.5,3,"a",cex=1.5) #adicionando a letra a no canto superior direito do gráfico

#Gráfico b
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
text(x=1:5, y=3, labels=c("*", "*","**","*","***"), cex=1.5) #adicionando asteriscos de significância