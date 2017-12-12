###################################
# Introdução Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# Aula 5
###################################

#4. Tutoriais de Análise Exploratória de Dados
#Conferindo Data Frames

aves.c <- read.table("aves_cerrado.csv", row.names=1, header=T, sep=";", dec=",", as.is=T) #Lendo a planilha

head(aves.c) #vendo o cabeçalho
tail(aves.c) #vendo a parte final do arquivo aves.c
str(aves.c) #verificando estrutura do arquivo
summary(aves.c) #Verificação inicial do data frame

aves.c[aves.c$urubu==NA,] #Verificando os registros para os problemas do NA. Não funciona.

is.na(aves.c) #teste lógico para saber se tem NA
is.na(aves.c$urubu) #O correto é usar um teste lógico para NA. Teste lógico para saber se tem NA na coluna urubu
#Isso retorna um vetor lógico que usamos para indexar o data frame ou um de seus vetores

aves.c[is.na(aves.c$urubu),] #verificando onde tem NA na coluna urubu
aves.c[is.na(aves.c$urubu)|is.na(aves.c$carcara)|is.na(aves.c$seriema),] #verificando onde tem NA nas colunas das demais aves

temp1 <- aves.c[is.na(aves.c$urubu)|is.na(aves.c$carcara)|is.na(aves.c$seriema),] #Guardando esse pedaço da planilha em um objeto temporário

aves.c$urubu[is.na(aves.c$urubu)] <- 0 #Corrigindo os valores de NA para 0 em urubu

aves.c$carcara[is.na(aves.c$carcara)] <- 0
aves.c$seriema[is.na(aves.c$seriema)] <- 0 #Corrigindo para as outras aves

aves.c[aves.c$urubu==0|aves.c$carcara==0|aves.c$seriema==0,]
temp1 #Verificando se deu certo

table(aves.c$fisionomia) #Verificando os valores da coluna que será um fator

aves.c$fisionomia[aves.c$fisionomia=="ce"] <- "Ce"
table(aves.c$fisionomia) #Corrigindo um erro de digitação no código de fisionomia de uma parcela porque o c está minúsculo

aves.c$fisionomia <- factor(aves.c$fisionomia, levels=c("CL","CC","Ce")) #Convertendo para fator, que ordenamos da fisionomia mais aberta para a menos

str(aves.c) #verificando a estrutura do arquivo
summary(aves.c) #Verificando tudo novamente

###################

#Média, Mediana e Quantis

mean(aves.c[,2:4]) #Explorando as estatísticas descritivas básicas. Aparece a seguinte mensagem: Warning message:
#In mean.default(aves.c[, 2:4]) :
  #argumento não é numérico nem lógico: retornando NA

#Melhor: aplicar as funções a cada elemento do data frame
sapply(aves.c[,2:4],mean) #tirando a media das colunas 2 a 4
sapply(aves.c[,2:4],median) #tirando a mediana das colunas 2 a 4

#No caso de data frames, consegue-se o mesmo com a função apply, indicando no segundo argumento a margem a aplicar a função (1 para linhas e 2 para colunas)
apply(aves.c[,2:4],2,median)

apply(aves.c[,2:4], 2, mean, trim=0.1) #Calculando a média truncada

#Há muita diferença entre essas três medidas de tendência central? Como você as explicaria?
#As três medidas não apresentam muita diferença
#A média representa o valor mais próximo do centro de uma série de valores
#A mediana é semelhante à média, mas ela despreza os valores muito altos ou muito baixos que podem distorcer o resultado
#A média truncada é semelhante à média e à mediana. Ela é calculada retirando uma determinada percentagem de observações, em partes iguais, de uma amostra ou distribuição de probabilidade, nos extremos superior e inferior.

#Calculando os quantis para o número de avistamentos de urubus. O padrão da função quantile são quartis, como na função summary
quantile(aves.c$urubu) ## O mesmo que o retornado pelo summary
summary(aves.c$urubu)

#Mas podemos mudar para qualquer quantil
quantile(aves.c$urubu, probs= seq(from=0,to=1,by=0.1)) #calculando os quantis de 0 a 1, a cada 0.1

#Por fim, obtenha quartis, médias e medianas de uma vez para todas as variáveis, com o comando:
summary(aves.c[,2:4])

#Exploração de uma Variável Categórica
caixeta <- read.csv("caixeta.csv", as.is=T) #lendo o arquivo
names(caixeta) #vendo os nomes dentro do objeto
table(caixeta$especie) #gera uma tabela de contingência para a contagem de cada espécie

sort(table(caixeta$especie), decreasing=T)
#com a função sort e decreasing = T os nomes das espécies são apresentados como uma classificação em ordem decrescente

#O resultado da função table pode ser fornecido à função de gráficos de barras
barplot(sort(table(caixeta$especie), decreasing=T)) #ordenando de maneira decrescente as contagens das espécies no barplot
barplot(table(caixeta$local)) #contagem das espécies de cada local em um barplot

#Gráficos para uma Variável
#Criando em uma só página os quatro gráficos básicos de diagnóstico de uma variável numérica, para o número de avistamentos de urubus
par(mfrow=c(2,2)) #gerando os quatro gráficos em uma página só (duas linhas, duas colunas)
boxplot(aves.c$urubu) #gerando o boxplot do avistamento de urubus
hist(aves.c$urubu) #gerando histograma do avistamento de urubus
plot(density(aves.c$urubu)) #gerando plot da densidade do avistamento de urubus
stripchart(aves.c$urubu, method="stack") #gerando um gráfico de stripchart pelo método stack para o avistamento de urubus
par(mfrow=c(1,1))

#O que acontece se você omite a primeira linha? E a última?

boxplot(aves.c$urubu)
hist(aves.c$urubu)
plot(density(aves.c$urubu))
stripchart(aves.c$urubu, method="stack")
par(mfrow=c(1,1))
#Se eu omito a primeira linha, os gráficos são gerados independentemente e aparecem em páginas diferentes

par(mfrow=c(2,2))
boxplot(aves.c$urubu)
hist(aves.c$urubu)
plot(density(aves.c$urubu))
stripchart(aves.c$urubu, method="stack")
#Se eu omito a última linha, não é observada nenhuma diferença na hora de gerar os gráficos

#Variações do Histograma
#Variações de histogramas do número de avistamentos de urubus: Você pode acrescentar marcas (traços) indicando a posição de cada observação no eixo x.

## Histograma com os valores (funcao rug)
hist(aves.c$urubu) #gerando histograma do avistamento de urubus
rug(jitter(aves.c$urubu)) #adicionando marcas no eixo x indicando a posição de cada observação

#O que acontece se você omite a função jitter neste caso? Por que?
hist(aves.c$urubu)
#Se eu omito a função jitter, eu omito do histograma as marcas no eixo x indicando a posição de cada observação

#Fazendo um histograma re-escalonado de modo que as áreas das barras somem um. Com isto, podemos sobrepor ao histograma um ajuste não paramétrico de densidade probabilística, que também mantém área um
hist(aves.c$urubu, prob=T) #gerando o histograma com argumento de probabilidade
lines( density(aves.c$urubu),col="blue" ) #adicionando ao histograma um ajuste de densidade

#Também sobre este histograma podemos sobrepor a curva normal. Para os parâmetros da normal, usamos a média e o desvio-padrão da amostra.
hist(aves.c$urubu, prob=T) #gerando o histograma
curve(expr = dnorm(x,mean=mean(aves.c$urubu),sd=sd(aves.c$urubu)),add=T, col="red") #sobrepondo uma curva normal ao histograma

#Por fim, vamos sobrepor a curva empírica de densidade probabilística com a curva normal
plot(density(aves.c$urubu),col="blue", ylim=c(0,0.08)) #plotando a curva de densidade para a observação de urubus
curve(expr = dnorm(x,mean=mean(aves.c$urubu),sd=sd(aves.c$urubu)),add=T, col="red") #sobrepondo a curva de densidade à curva normal

#O que estes gráficos revelam sobre a distribuição do número de avistamentos de urubus neste estudo fictício?
#Esses gráficos revelam que a curva empírica não segue uma distribuição normal, mas aparentemente apresenta dois picos, sendo portando bimodal

#table e aggregate
#A relação entre duas ou mais variáveis categóricas pode ser explorada com tabelas cruzadas
table(caixeta$especie,caixeta$local) #tabela com relação entre as observações de cada espécie por local no objeto caixeta

#Quando temos uma variavel categórica (fator) e uma numérica, as funções aggregate e tapply são muito úteis. A função aggregate é o equivalente das tabelas dinâmicas das planilhas eletrônicas.
caixeta.alt <- aggregate(caixeta$h, by=list(local=caixeta$local,especie=caixeta$especie),
                         FUN=mean)
#obtendo do objeto caixeta um data frame com a altura média dos fustes de cada espécie de árvore por local

#Consulte a ajuda da função aggregate e experimente outras combinações de fatores e funções, com este conjunto de dados
?aggregate
caixeta
caixeta.cap <- aggregate(caixeta$cap, by=list(local=caixeta$parcela,especie=caixeta$especie),
                         FUN=mean)
#obtendo do objeto caixeta um data frame com o cap médio dos fustes de cada espécie de árvore por parcela

caixeta.alt <- aggregate(caixeta$h, by=list(local=caixeta$local,especie=caixeta$especie),
                         FUN=median)
#obtendo do objeto caixeta um data frame com a mediana da altura dos fustes de cada espécie de árvore por local

#xtabs
#Crie um objeto com este arquivo e faça as seguintes tabulações
Titanic.df=read.csv("titanic.csv", header=T, sep=",") #lendo o arquivo e criando o objeto
xtabs(Freq~Sex+Survived, data=Titanic.df) #tabela de contingência da frequência dos sexos que sobreviveram ou não
prop.table(xtabs(Freq~Sex+Survived, data=Titanic.df), margin=1) #proporção da frequência dos sexos que sobreviveram ou não
xtabs(Freq~Class+Survived, data=Titanic.df) #tabela de contingência da frequência de cada classe que sobreviveu ou não
prop.table(xtabs(Freq~Class+Survived, data=Titanic.df), margin=1) #proporção da frequência das classes que sobreviveu ou não

#Por que usamos a função xtabs neste caso e não a função table? P.ex:
table(Titanic.df$Sex,Titanic.df$Survived)
#Porque xtabs cria uma tabela de contingência, através do cruzamento de fatores ao invés de transformar os valores de x em uma tabela, que é o que a função table faz. Com a tabela de contingência, consigo apresentar, por exemplo, duas variáveis na amostra estudada

#Fórmula Estatística em Gráficos
#A função xtabs (tutorial anterior) usa a notação de fórmula estatística do R, que é: variável dependente ~ variáveis preditoras
#Esta notação foi criada para os modelos estatísticos, como a regressão linear, mas foi estendida para vários gráficos no R. Experimente os comandos abaixo em que esta notação é usada

boxplot(urubu~fisionomia, data=aves.c) #boxplot das observações de urubus em cada uma das fisionomias
plot(seriema~urubu, data=aves.c, subset=fisionomia=="Ce") #observações de siriema em função das observações de urubu dentro da fisionomia Ce 
plot(seriema~urubu, data=aves.c, subset=fisionomia=="CC") #observações de siriema em função das observações de urubu dentro da fisionomia CC
plot(seriema~urubu, data=aves.c, subset=fisionomia!="CL") ##observações de siriema em função das observações de urubu dentro da fisionomia CL 

#Que tipo de padrão ou diferenças estes gráficos podem revelar?
#Eles revelam a relação entre as observações de duas variáveis dentro de cada uma das fisionomias

#O quarteto de Anscombe
data(anscombe)#carrega para a area de trabalho
ls() #agora o objeto está no workspace

#Este objeto é composto de 4 pares de variáveis, nomeadas x1 a x4 (variáveis independentes ou preditoras) e y1 a y4 (variáveis dependentes ou resposta)
names(anscombe) #vendo os nomes das variáveis

#Compare as médias de cada uma das variáveis.
apply(anscombe[1:4], MARGIN=2, FUN=mean) #média das colunas 1 a 4
apply(anscombe[5:8], 2, mean) #média das colunas 5 a 8

#Faça o mesmo para obter as variâncias:
apply(anscombe[1:4], 2, var) #variância das colunas 1 a 4
apply(anscombe[5:8], 2, var) #variância das colunas 5 a 8

#A pergunta principal para este conjunto de dados é se há relação entre cada variável x e y. Isso pode ser testado com o coeficiente de correlação de Pearson, que vai de zero (nenhuma correlação) a um (positivo ou negativo, correlação perfeita).

with(anscombe,cor(x1,y1)) #calculando coeficiente de correlação entre as variáveis x1 e y1
with(anscombe,cor(x2,y2)) #calculando coeficiente de correlação entre as variáveis x2 e y2
with(anscombe,cor(x3,y3)) #calculando coeficiente de correlação entre as variáveis x3 e y3
with(anscombe,cor(x4,y4)) #calculando coeficiente de correlação entre as variáveis x4 e y4
#O coeficiente de correlação deu aproximadamente o mesmo valor para todas as relações entre as variáveis x e y, e esse valor indica que há correlação porque o valor é mais próximo de 1

#O que se pode concluir até aqui? Faça os gráficos de dispersão:
par(mfrow=c(2,2)) # 4 graficos em uma janela
plot(y1~x1, data=anscombe)
plot(y2~x2, data=anscombe)
plot(y3~x3, data=anscombe)
plot(y4~x4, data=anscombe)
par(mfrow=c(1,1))

#Acrescentando as linhas de regressão linear
par(mfrow=c(2,2)) # 4 graficos em uma janela
plot(y1~x1, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8])) #adicionando o plot entre as duas variáveis e especificando os limites de x e y, que correspondem ao range das variáveis de x e de y, respectivamente
abline(lm(y1~x1, data=anscombe))
plot(y2~x2, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y2~x2, data=anscombe))
plot(y3~x3, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y3~x3, data=anscombe))
plot(y4~x4, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y4~x4, data=anscombe))
par(mfrow=c(1,1))
#A distribuição dos pontos difere entre os 4 graficos, mas em todos o coeficiente de correlação de Pearson e a linha de regressão linear mostram que há correlação entre as variáveis x e y testadas.

################################################################################################

#Exercícios 4 - Análises Exploratórias

#4.1 Rios
#Exercício resolvido no sistema notaR.

#4.2 Cervejas
#Uma amostra de 30 estudantes foi indagada sobre seu tipo de cerveja preferida, com o seguinte resultado

cervejas <-c("chope","lata","garrafa","chope","garrafa", "garrafa","lata","lata","nenhuma","lata","garrafa","garrafa", "garrafa","lata","lata","lata","garrafa","lata","chope","nenhuma", "garrafa","garrafa","garrafa","chope","garrafa","garrafa","chope","garrafa","lata","lata")

#Represente este resultado como um gráfico de barras e um dotplot (função dotchart).

unique(cervejas) #verificando todos os nomes únicos dentro do objeto cervejas
par(mfrow=c(1,2)) #especificando que os dois gráficos serão gerados numa mesma página
barplot(table(cervejas), xlab = "Cervejas", ylab = "Quantidade de estudantes") #gerando um barplot da quantidade de estudantes que prefere cada tipo de cerveja
dotchart(table(cervejas), xlab = "Quantidade de estudantes") #gerando um dotchart que mostra as mesmas informações do barplot

#Qual tem maior razão dado/tinta?
#Com o dotchart eu tenho as mesmas informações do barplot, mas usando menos tinta

#4.3 Caixetais
#Neste exercício, use o objeto caixeta, criado no tutorial Exploração de uma Variável Categórica.
#Construa um histograma do dap dos fustes dos caixetais.
caixeta.dap = (caixeta$cap) / (pi*10) #obtendo o dap dos fustes com base nos valores de cap do objeto caixeta
hist(caixeta.dap) #fazendo um histograma do dap

#Construa histogramas da altura das árvores para os diferentes caixetais ('local').
unique(caixeta$local) #verificando todos os nomes únicos dentro de local
hist(caixeta$h[caixeta$local=="chauas"]) #histograma da altura das árvores para chauas
hist(caixeta$h[caixeta$local=="jureia"]) #histograma da altura das árvores para jureia
hist(caixeta$h[caixeta$local=="retiro"]) #histograma da altura das árvores para retiro

#Há diferenças entre as estruturas (distribuição de tamanhos) dos caixetais?
#Sim. Em Retiro há uma maior frequência de árvores de menor altura

#4.4 Eucaliptos
#Neste exercício, use o conjunto de dados Inventário em Florestas Plantadas de Eucalyptus grandis.
egrandis = read.csv("egrandis.csv", header = T, sep = ";") #lendo o arquivo

#Utilize o gráfico boxplot para analisar o DAP de árvores de E. grandis em função das variáveis região (regiao) e rotação (rotacao).
boxplot(dap~rotacao*regiao, data = egrandis)

#Avalie a normalidade da altura do conjunto total de árvores com um gráfico quantil-quantil contra a distribuição normal.
qqnorm(egrandis$ht) #plota os dados contra uma distribuição normal
qqline(egrandis$ht) #plota uma linha para facilitar a comparação
#as distribuições não são exatamente iguais porque nem todos os pontos caem em cima da linha, então a altura das árvores não segue uma distribuição normal

#4.5 Mais Caixetais
#Aqui usaremos novamente o objeto caixeta, criado no tutorial Exploração de uma Variável Categórica.
caixeta <- read.csv("caixeta.csv", as.is=T) #lendo o arquivo

#Analise a relação dap-altura ('dap' e 'h') em função do caixetal (local) com a função plot, mas somente para as árvores de caixeta (Tabebuia cassinoides).
#primeiro calculamos o dap em cm e depois transformamos a altura em metros
caixeta$dap = caixeta$cap / (pi*10)
caixeta$h = caixeta$h / 10
head(caixeta)
coplot(h ~ dap | local, data = caixeta[caixeta$especie=="Tabebuia cassinoides",])

#Para a mesma relação do item anterior, verifique linearidade com a função scatter.smooth
scatter.smooth( caixeta$dap, cax$h , col="red")

#Utilizando o pacote lattice, analise a relação dap-altura ('dap' e 'h') em função do caixetal (local), mas somente para as árvores de caixeta (Tabebuia cassinoides).
library(lattice) #chamando o pacote
xyplot(h~dap | local, data=caixeta[caixeta$especie=="Tabebuia cassinoides",])

################################################################################################

#4. Análise Exploratória de Dados

#Estatísticas Descritivas
#A forma mais direta de se obter um resumo estatístico das variáveis num 'data.frame' é através da função 'summary'. Ela apresenta estatísticas descritivas para as variáveis numéricas.

cax = read.csv("caixeta.csv", header=TRUE, as.is=TRUE) #lendo o arquivo
summary(cax) #obtendo um resumo estatístico do objeto

#Outras estatísticas devem ser calculadas individualmente pelo analista
resumo1 = aggregate( cax[ , c("cap","h")], list(local=cax$local), mean ) #média do cap e da altura das árvores de cada local dentro de caixeta
resumo2 = aggregate( cax[ , c("cap","h")], list(local=cax$local), sd ) #desvio padrão do cap e da altura das árvores de cada local dentro de caixeta
resumo = merge( resumo1, resumo2, by="local", suffixes=c(".mean",".sd") ) #combinando as tabelas
resumo

#Exercício: Estatísticas do Caixetal
cax = read.csv("caixeta.csv", header=TRUE, as.is=TRUE)
cax$area.basal=cax$cap^2/4*pi #calculando area basal

#Construa um 'data.frame' com os dados de área basal por 'local' e 'parcela'.
areabasal = data.frame(cax$parcela, cax$local, cax$area.basal)

#Encontre a média e desvio padrão da área basal por 'local'.
mean.areabasal = tapply(areabasal$cax.area.basal, areabasal$cax.local, FUN=mean) #calculando a média
sd.areabasal = tapply(areabasal$cax.area.basal, areabasal$cax.local, FUN=sd) #calculando o desvio padrão

#Calcule o intervalo de confiança de 95% da área basal por 'local'.

#Analisando a Distribuição das Variáveis: Gráficos Univariados
#Histogramas
#A primeira abordagem ao se estudar a distribuição de uma variável é o uso de histogramas:
cax$dap = (pi/4)* (cax$cap/10)
hist( cax$dap ) #histograma do dap das árvores dentro do objeto cax
hist( cax$dap[ cax$local == "chauas" ] ) #histograma do dap das árvores do local chauas
hist( cax$dap[ cax$local == "jureia" ] ) #histograma do dap das árvores do local jureia
hist( cax$dap[ cax$local == "retiro" ] ) #histograma do dap das árvores do local retiro

#A função 'hist' também pode fornecer os dados do histograma, sem gerar o histograma propriamente dito:
hist( cax$dap, plot=FALSE ) #fornecendo os dados do histograma sem gerá-lo

#Note que o objeto gerado pela função 'hist' tem classes 'histogram', logo pode ser guardado e grafado posteriormente:
dap.hist = hist( cax$dap, plot=FALSE ) #colocando em um novo objeto os dados do histograma para gerá-lo posteriormente
class(dap.hist) #verificando a classe do objeto
plot(dap.hist) #plotando o histograma

#Alguns parâmetros gráficos podem tornar o gráfico mais apresentável. Esses parâmetros gráficos pode ser utilizados como argumentos em diversas funções gráficas onde são pertinentes:
hist( cax$dap[ cax$local=="chauas" ],
      xlab="Diâmetro à Altura do Peito - DAP (cm)",
      ylab="Freqüência", #'xlab' e 'ylab' = nomes dos eixos X e Y, respectivamente
      main="Histograma DAP - Chauás", #'main' = nome do título do histograma
      col = "blue" ) #cor da barra (histograma), ou de linhas e símbolos plotados

#Muitas vezes desejamos comparar gráficos, sendo útil termos mais de uma janela gráfica. A função 'X11()' (no UNIX) abre uma janela gráfica, sendo que podemos abrir várias:
hist( cax$dap[ cax$local=="chauas" ] , main="Chauás" ) #histograma do dap das árvores apenas do local chauas, especificando o nome do título principal
X11() #abre uma nova janela gráfica
hist( cax$dap[ cax$local=="jureia" ] , main="Juréia" ) #histograma do dap das árvores apenas do local jureia, especificando o nome do título principal
X11() #abre uma nova janela gráfica
hist( cax$dap[ cax$local=="retiro" ] , main="Retiro" ) #histograma do dap das árvores apenas do local retiro, especificando o nome do título principal
dev.off()
dev.off() #A função 'dev.off()' fecha uma janela gráfica e faz parte de um conjunto de funções que manipula as janelas gráficas.

#'dev.off()' - fecha a janela gráfica;
#'dev.cur()' - diz qual janela gráfica está 'ACTIVE';
#'dev.set(which=dev.cur())' - define qual janela deverá ficar ativa, o argumento 'which' deve ser o número da janela;
#'dev.next(which=dev.cur())' - informa o número da próxima janela gráfica;
#'dev.prev(which=dev.cur())' - informa o número da janela gráfica anterior;
#'graphics.off()' - fecha todas as janelas gráficas.

X11() #abre nova janela gráfica
hist( cax$dap[ cax$local=="retiro" ] , main="Retiro" ) #gera o histograma
X11() #abre nova janela gráfica
hist( cax$dap[ cax$local=="jureia" ] , main="Juréia" ) #gera o histograma
dev.cur() #vê qual janela gráfica está ativa
dev.set(2) #define a janela 2 que ficará ativa
hist( cax$dap[ cax$local=="retiro" ] , main="Retiro" , col="blue") #gera histograma
dev.set(3) #define a janela 3 que ficará ativa
hist( cax$dap[ cax$local=="jureia" ] , main="Juréia" , col="green") #gera o histograma
graphics.off() #fecha todas as janelas gráficas

#Exercício: Altura de Árvores em Caixetais I
#Construa um histograma da altura das árvores do caixetal.
hist(cax$h)

#Construa histogramas da altura das árvores para os diferentes caixetais ('local').
hist(cax$h[cax$local=="retiro"], main = "Retiro")
hist(cax$h[cax$local=="jureia"], main = "Juréia")
hist(cax$h[cax$local=="chauas"], main = "Chauas")

#Há diferenças entre as estruturas (distribuição de tamanhos) dos caixetais?
#Há, pois aparentemente Jureia segue um padrão de distribuição normal e em Retiro essa distribuição parece ser bimodal. Em Chauas, a maior parte das árvores tende a apresentar uma altura próxima

#Exercício: Diâmetros de Árvores de Eucalipto
#Construa um histogram do DAP das árvores de E. saligna. Procure interpretar o histograma.
esaligna = read.csv("esaligna.csv", header=TRUE, as.is=TRUE)
hist(esaligna$dap)
#O histograma mostra uma ampla variação no dap de árvores de E. saligna, com alguns valores apresentando maior frequência, mas vários valores de dap apresentando a mesma frequência

#Gráficos de Densidade
#Uma outra forma de explorar a distribuição de uma variável
#é gerado como se fosse um histograma com uma classe móvel, isto é, a classe que tem uma certa amplitude, se move da esquerda para direita e em cada ponto estima a densidade probabilística da variável. Tecnicamente, a função 'density' é um estimador de densidade de kernel gaussiano.

plot( density(cax$dap) ) #gerando plot estimando a densidade probabilística do dap do objeto cax
plot( density(cax$dap, bw=0.5), col="red" ) #O parâmetro que controla o comportamento do estimador de densidade é a amplitude da janela de observação bandwidth ('bw').
lines( density(cax$dap, bw=5), col="blue" ) #adicionando linha modificando o comportamento e a cor do estimador de densidade
lines( density(cax$dap, bw=1.5), col="green" ) #adicionando linha modificando o comportamento e a cor do estimador de densidade

#Exercício: Distribuição de DAP nos Caixetais
#Realize uma análise de densidade do DAP para cada um dos caixetais. Os resultados confirmam o que foi visto nos histogramas?

#Boxplot
esa = read.csv("esaligna.csv",header=TRUE) #lendo o arquivo
boxplot( esa$dap ) #gerando boxplot para o dap de esaligna 

#O boxplot é útil para analisar a simetria de uma distribuição, o espalhamento das observações e a presença de observações discrepantes. Ele é problemático quando a variável analisada não é unimodal. Ele também é uma ferramenta útil para comparar distribuições, isso é realizado quando desejamos um boxplot para cada situação:
boxplot( dap ~ local, data=cax ) #boxplot do dap em função dos locais do objeto cax

#Note que o primeiro argumento da função 'boxplot' não é um vetor nesse caso
class( dap ~ local ) #verificando a classe de dap ~ local

#O primeiro argumento é uma 'formula' estatística onde o símbolo ~ tem um significado especial.
#A fórmula 'dap ~ local' deve ser lida como: modele a variável dap como função da variável local.
#O argumento 'data' informa em qual 'data.frame' estão as variáveis citadas na fórmula e é um argumento essencial toda vez que se utiliza uma fórmula.

#A utilização da fórmula permite a construção de gráficos mais complexos, pensando na interação entre dois fatores influenciando a variável DAP:
boxplot( dap ~ local * parcela, data=cax) #gerando boxplot do dap em função do local e da parcela

boxname = paste( sort(rep(unique(cax$local),5)), rep(1:5,3) ) #renomeando os boxplots
boxcor = sort(rep(c("navy","darkgreen","salmon1"),5)) #escolhendo as cores
boxplot( dap ~ local * parcela, data=cax , names=boxname, col=boxcor, horizontal=T, las=1, xlab="DAP (cm)") #sofisticando os gráficos com um pouco mais de programação
#colocando os nomes dos boxplots, colorindo, colocando-os na horizontal, definindo a orientação dos valores dos eixos...

#Exercício: Altura de Árvores em Caixetais II
#Utilize o gráfico boxplot para analisar a altura das árvores em caixetais.
boxplot(cax$h)

#Exercício: Estrutura de Eucaliptais
#Utilize o gráfico boxplot para analisar o DAP de árvores de E. grandis em função das variáveis região ('regiao') e rotação ('rot').
boxplot(dap ~ regiao * rotacao, data=egrandis)

#Gráficos Quantil-Quantil
#Gráficos Quantil-Quantil também são uma forma de estudar o comportamento de variáveis, mas utilizando as propriedades que emergem de uma variável quando trabalhamos com os seus quantis.

#O gráfico quantil-quantil mais tradicional é aquele usado para verificar se uma variável possui distribuição Normal. No R isso é realizado com a função 'qqnorm', associada à função 'qqline' que adiciona uma linha ao gráfico:

qqnorm( cax$dap ) #calculando os quantis do dap
qqline( cax$dap ) #adicionando a linha

#A idéia central do gráfico quantil-quantil é a seguinte: quando um variável segue uma dada distribuição (como a distribuição Normal) os quantis empíricos, isto é, calculados a partir de uma amostra, formam uma linha reta contra os quantis teóricos, calculados a partir das estimativas dos parâmetros da distribuição (no caso da Normal: média e desvio padrão)

#É isso que a função 'qqnorm' faz para distribuição Normal:
vn1 = rnorm( 10000 ) #gerando os dados com distribuição normal
qqnorm( vn1 ) #calculando os quantis
qqline( vn1 ) #adicionando uma linha ao gráfico

ve1 = rexp( 100000 ) #gerando os dados
qqnorm( ve1 ) #calculando os quantis
qqline( ve1 ) #adicionando uma linha ao gráfico

ve2 = apply( matrix(ve1, ncol=100), 1, mean) #criando uma matriz aos dados de v1
qqnorm( ve2 ) #calculando os quantis
qqline( ve2 ) #adicionando uma linha ao gráfico

#Também é possível comparar duas distribuições a partir dos quantis empíricos:
qqplot( cax$dap[ cax$local=="retiro" ], cax$dap[ cax$local=="jureia" ] )
abline( 0, 1, col="red" )

a = min( cax$dap[ cax$local=="jureia" ] )
abline( a, 1, col="navy" )
#a função 'abline( a, b)' adiciona a um gráfico um reta com intercepto a e inclinação b.

#Exercício: Inventário em Floresta Plantada
#Verifique se as variáveis quantitativas obtidas no inventário de florestas plantadas tem distribuição Normal: 'dap', 'ht' e 'hdom'.
#egrandis

qqnorm(egrandis$dap)
qqline(egrandis$dap) #não tem distribuição normal

qqnorm(egrandis$ht)
qqline(egrandis$ht) #não tem distribuição normal

#Exercício: Altura de Árvores em Caixetais III
#Verifique se a distribuição da altura das árvores tem o mesmo comportamento nos diferentes caixetais.

qqnorm(cax$h[cax$local=="retiro"])
qqline(cax$h[cax$local=="retiro"])

qqnorm(cax$h[cax$local=="jureia"])
qqline(cax$h[cax$local=="jureia"])

qqnorm(cax$h[cax$local=="chauas"])
qqline(cax$h[cax$local=="chauas"])

#Gráfico de Variável Quantitativa por Classes
#A maneira clássica de se apresentar uma variável quantitativa associada a uma classe é o famoso gráfico de barras.
#Vejamos um exemplo comum em fitossociologia que é apresentar a densidade relativa das espécies:
  
da = table( cax$especie[ cax$local=="jureia" ] )
da = sort(da, decreasing=TRUE )
dr = da/sum(da) * 100

#Para obter o gráfico de barras basta usar a função 'barplot'
barplot( dr )

#O resultado não é muito apropriado para interpretações, mas podemos fazer algumas melhoras
barplot( dr , xlab="Densidade Relativa (%)", horiz=T, las=1)

#O nome das espécies precisam de mais espaço. É possível alterar o espaço trabalhando os parâmetros da função 'par' que controla todos os parâmetros gráficos de uma janela gráfica. Nesse caso, o parâmetro 'omd=c(x1,x2,y1,y2)' define o início e final da região de plotagem em termos relativos. O valor default é 'omd=c(0, 1, 0, 1)'.
par( omd=c(0.2,1,0,1) )
barplot( dr , xlab="Densidade Relativa (%)", horiz=T, las=1)

#Existe no R, um gráfico que faz a mesma coisa de modo muito mais simples e direto
par( omd=c(0,1,0,1) )     # Primeiro é necessário re-estabelecer o parâmetro omd
dotchart( dr )
#No 'dotchart', somos levados a comparar a posição relativa dos pontos, e a relação entre as categorias fica muito mais rápida e direta.

#Como nessa floresta a Tabebuia cassinoides (caixeta) é a espécie dominante, é interessantes fazer o gráfico na escala logarítmica para enfatizar a diferença entre as outras espécies:
dotchart( log(dr), xlab="Logaritmo Natural da Densidade Relativa (%)")

#Exercício: Dominância em Caixetais
#Construa um gráfico da dominância das espécies nos caixetais.
da = table( cax$especie )
da = sort(da, decreasing=TRUE )
dr = da/sum(da) * 100 #calculando a densidade relativa de cada espécie

dotchart(log(dr), xlab="Logaritmo Natural da Densidade Relativa (%)") #fazendo o gráfico usando o log de densidade relativa para enfatizar a diferença entre as espécies

#Exercício: Inventário em Floresta Plantada
#Utilizando a função 'dotchart' investigue o número de árvores no inventário em função da região ('regiao') e rotação ('rot').

#Análise Gráfica: Relação entre Variáveis
#Gráfico de Dispersão
#Os gráficos de dispersão (ou gráficos x-y) são os gráficos mais utilizados para estudar a relação entre duas variáveis.
#A função genérica no R para gráficos de dispersão é a função 'plot':

plot( x = cax$dap, y = cax$h )
#Na função 'plot', o primeiro argumento é plotado nas abscissas (eixo-x) e o segundo argumento nas ordenadas (eixo-y).

#Há no R uma função adicional que auxilia o julgamento adicionando ao gráfico de dispersão uma linha não-paramétrica de tendência (smooth ou suavização):
scatter.smooth( cax$dap, cax$h , col="red")

#Uma série de parâmetros gráficos podem ser utilizados diretamente nas funções 'plot' e 'scatter.smooth':
scatter.smooth( cax$dap, cax$h , col="red", xlab="DAP (cm)", ylab="Altura (dm)", main="Caixetais")
scatter.smooth( cax$dap, cax$h , col="red", xlab="DAP (cm)", ylab="Altura (dm)", log="x")
scatter.smooth( cax$dap, cax$h , col="red", xlab="DAP (cm)", ylab="Altura (dm)", log="y")
scatter.smooth( cax$dap, cax$h , col="red", xlab="DAP (cm)", ylab="Altura (dm)", log="xy")

#O R também permite um certo grau de interação com gráficos de dispersão. Uma delas é a identificação de observações no gráfico:
scatter.smooth( cax$dap, cax$h )
dim( cax )
identify( cax$dap, cax$h, 1:1027 ) #A função 'identify' atua sobre um gráfico produzido (plot) e possui três argumentos. Os dois primeiros são os mesmos argumentos que geraram o gráfico. O terceiro argumento é uma variável de identificação. No exemplo acima a variável de identificação é o índice que identifica a observação (linha do data.frame).
cax[ c(362, 556, 557), ]

#No exemplo acima, as três observações discrepantes do gráfico parecem de fato muito erradas.Assim, podemos eliminá-las e continuar o estudo da relação:
cax2 = cax[ -c(362, 556, 557), ]
scatter.smooth( cax2$dap, cax2$h , col="red" )

#Também na função 'plot' é possível se utilizar como argumento inicial uma 'formula', seguida do data.frame que contem as variáveis:
plot( h ~ dap, data=cax2 )

#Nesse caso, para adicionar a linha não-paramétrica de tendência é necessário um segundo comando:
plot( h ~ dap, data=cax2 )
lines( lowess( cax2$dap, cax2$h ) , col="red")

#O uso da formula permite a utilização da função 'coplot' para formação de gráficos de dispersão em função de variáveis categóricas:
coplot( h ~ dap | local , data=cax2 )
coplot( h ~ dap | local*parcela , data=cax2 )

#Também é possível adicionar uma linha de tendência em cada gráfico gerado pela função 'coplot':
coplot( h ~ dap | local , data=cax2 , panel= panel.smooth)
coplot( h ~ dap | local*parcela , data=cax2 , panel=panel.smooth)

#A barra vertical indica uma situação condicional, no caso fazer um gráfico de dispersão para cada local.
#O asterísco (*) indica interação, no caso o gráfico de dispersão é realizado para cada interação entre as variáveis local e parcela.

#A função 'coplot' atua de forma diferente, se as variáveis que classificam o gráfico de dispersão são variáveis categóricas ('factor') ou numéricas ('numeric'):
egr = read.table("egrandis.csv",header=TRUE,sep=";")
coplot( ht ~ dap | idade, data=egr, panel = panel.smooth )
coplot( ht ~ dap | idade * rotacao , data=egr, panel = panel.smooth ) #corrigindo rot para rotacao
coplot( ht ~ dap | idade * as.factor(rotacao) , data=egr, panel = panel.smooth ) ##corrigindo rot para rotacao

#Exercício: Relação Hipsométrica da Caixeta
#Analise a relação dap-altura ('dap' e 'h') em função do caixetal, mas somente para as árvores de caixeta (Tabebuia cassinoides).

coplot(h~dap | local, data=cax2[cax2$especie=="Tabebuia cassinoides",])

#Exercício: Inventário em Floresta Plantada II
#Analise a relação entre as variáveis 'hdom' (altura das árvores dominantes) e 'dap' para diferentes regiões ('regiao') e rotações ('rot').

#Painel de Gráficos de Dispersão
#Quando o objetivo é explorar a relação entre variáveis quantitativas com o objetivo de construir modelos ou analisar a estrutura de correlação é útil poder fazer gráficos de dispersão das variáveis duas-a-duas. A função pairs realiza essa operação automaticamente:
pairs( egr[ , c("dap","ht","hdom","idade")] )

#Sempre é possível sofisticar os gráficos. No exemplo abaixo o painel apresenta a relação entre as variáveis quantitativas utilizando cores para mostrar as variáveis região e rotação:
pairs( egr[ , c("dap","ht","hdom","idade")] , pch=21, bg=c("red","blue","green")[unclass(egr$regiao)] )
pairs( egr[ , c("dap","ht","hdom","idade")] , pch=21, bg=c("red","green")[unclass(egr$rot)] )

#Exercício: Biomassa de Árvores de Eucalipto
#Analise a relação entre as variáveis quantitativas do conjunto de dados sobre biomassa das árvores de E. saligna.
#Qual a influência da variável classe ('classe') sobre a relação entre as variáveis?

#Gráficos em Painel: O Pacote Lattice
#Para ampliar a capacidade de análise gráfica exploratória e mesmo modelagem gráfica dos dados, existe no R o pacote lattice. Para carregar o pacote usa-se o comando:
library(lattice) #permite a construção de paineis. Um painel é um série de gráficos de mesmo tipo (dispersão, histograma, etc.) colocados lado-a-lado acompanhando uma variável categórica ou quantitativa.

#Gráficos de Dispersão
#Para construir gráficos de dispersão no lattice usa-se a função xyplot
egr = read.csv("egrandis.csv",header=T, sep = ";")
xyplot( ht ~ dap, data=egr ) #plot mostrando a relação entre dap e altura

#Note que no lattice, os gráficos são construídos com base em fórmulas. Essas fórmulas permitem estrutura mais complexas de análise:

xyplot( ht ~ dap | regiao , data=egr ) #plot da relação entre dap e altura por região
xyplot( ht ~ dap | regiao * rot , data=egr ) #plot da relação entre dap e altura em função da região e rotação

#Também é possível construir gráficos com suavização:
xyplot( ht ~ dap | regiao * rotacao , data=egr, #corrigido para rotacao
         panel = function(x,y)
         {
                     panel.xyplot(x,y)
                     panel.loess(x,y, span=1, col="red")
             } )

#Exercício: Relação Hipsométrica da Caixeta II
#Utilizando o pacote lattice, analise a relação dap-altura ('dap' e 'h') em função do caixetal, mas somente para as árvores de caixeta (Tabebuia cassinoides).
library(lattice)
xyplot(h~dap | local, data=cax2[cax2$especie=="Tabebuia cassinoides",])

#Exercício: Relação Altura das Dominantes - Idade em Florestas Plantadas
#Utilizando os dados de floresta plantada (E. grandis), analise a relação entre altura das árvores dominantes ('hdom') e idade ('idade') por rotação ('rot') e região ('regiao').

#Painel de Gráficos de Dispersão
#O pacote lattice também possui uma função específica para fazer um painel de gráficos de dispersão: splom (scatter plot):
splom( egr[ , c("dap","ht","hdom","idade")]  )

#Identificar grupos em cada gráfico de dispersão é mais fácil com a função splom, basta utilizar o argumento 'group':
splom( egr[ , c("dap","ht","hdom","idade")] , group=egr$regiao )
splom( egr[ , c("dap","ht","hdom","idade")] , group=egr$rot )

#Também é possível adicionar uma linha de suavização, mas é necessário definir a função de painel (argumento 'painel'):
splom( egr[ , c("dap","ht","hdom","idade")] , group=egr$regiao,
       panel = function(x,y,...)
        {
            panel.splom(x,y,...)
            panel.loess(x,y,...)
        }
        )

#A função panel.loess é a função que efetivamente faz a suavização em cada gráfico de dispersão.

#Exercício: Biomassa de Árvores de Eucalipto
#Analise a relação entre as variáveis quantitativas dos dados de biomassa de E. saligna utilizando a função splom. Inclua na sua análise a variável 'classe'.

splom( esaligna[ , c("tronco","sobra","folha")] , group=esaligna$classe )

#Histogramas e Gráficos de Densidade
#No lattice, todos os tipos de gráficos podem ser construídos na forma de painel. Para estudar a distribuição de variáveis temos a função histogram e densityplot:
cax = read.csv("caixeta.csv",header=T)
cax$dap = cax$cap / pi

histogram( ~ dap, data=cax )
histogram( ~ dap | local , data=cax )

densityplot( ~ dap, data=cax )
densityplot( ~ dap | local , data=cax )

#Também é possível construir um histograma com linhas de densidade, para isso o tipo do histograma deve ser definido como 'density':
histogram( ~ ht | regiao * rotacao , dat=egr, type="density", #corrigido para rotacao
                panel = function(x, ...){
                          panel.histogram(x, ...)
                          panel.densityplot(x, col="red", ...)
                  }
            )

#As funções de histograma e densidade podem se tornar mais complexas. No exemplo abaixo, uma curva de densidade assumindo a distribuição Normal é adicionada aos histogramas, os quais são construídos com a densidade nas ordenadas:
histogram( ~ ht | regiao * rotacao , dat=egr, type="density", #corrigido para rotacao
                panel = function(x, ...){
                        panel.histogram(x, ...)
                        panel.mathdensity(dmath=dnorm, col="black", args=list(mean=mean(x),sd=sd(x)))
                  }
            )

#Exercício: Altura das Árvores Dominantes em Florestas Plantadas
#Explore o comportamento da variável altura das árvores dominantes ('hdom') por região ('regiao') e rotação ('rot') na floresta plantada de E. grandis.

#Exercício: Altura de Árvores de Caixeta
#Analise o comportamento da variável altura ('h') das árvores de caixeta.

#Gráficos Quantil-Quantil
#O pacote lattice implementa a construção de gráficos sempre através de fórmulas, isso pode ser conveniente no caso de se verificar a distribuição de uma variável em várias situações:
qqmath( ~ dap | local, data=cax )

#Para adicionar a linha do gráfico qq é necessário editar a função de painel:
qqmath( ~ dap | local, data=cax,
             panel = function(x, ...)
              {
                   panel.qqmath(x, ...)
                   panel.qqmathline(x, ...)
                }
         )

#Uma vantagem do pacote lattice é a possibilidade de gráficos quantil-quantil com outras distribuições além da distribuição normal. Nos gráficos abaixo, o DAP das árvores dos caixetais é comparada com a distribuição exponencial ('qexp').

qqmath( ~ dap | local , data=cax, distribution = function(p) qexp(p, 1/mean(x)) )

qqmath( ~ dap | local , data=cax, distribution = function(p) qexp(p, 1/mean(x)),
                panel = function(x,...)
                  {
                       panel.qqmath(x,...)
                       panel.qqmathline(x,...)
                    }
             )

#Também é possível fazer gráficos quantil-quantil de um conjuntos de dados empíricos usando a função qq:

qq( local ~ dap, data=cax, subset = ( local=="chauas" | local=="jureia" ) )
qq( local ~ dap, data=cax, subset = ( local=="chauas" | local=="retiro" ) )
qq( local ~ dap, data=cax, subset = ( local=="jureia" | local=="retiro" ) )

#Exercício: Altura das Árvores em Florestas Plantadas
#Verifique se a altura das árvores ('ht') nas florestas plantadas de E. grandis segue distribuição Normal.
#Faça uma análise geral e depois por região ('regiao') e rotação ('rot').

#Exercício: Biomassa de Árvores de Eucalipto
#Verifique se biomassa total ('total') e a biomassa do tronco ('tronco') das árvores de E. saligna possuem distribuição semelhante. E a biomassa das folhas ('folha'), tem distribuição semelhante à biomassa do tronco?