###################################
# Introdução Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# Aula 6
###################################

#9. Tutoriais de Programação
#Uma funções muito simples

media <-function(x)
{
  soma=sum(x) #criando um objeto chamado soma com a soma dos valores do objeto
  nobs=length(x) #criando um objeto chamado nobs com o comprimento do objeto
  media=soma/nobs #criando um objeto media com o valor do objeto soma dividido pelo valor do objeto nobs
  return(media) #retornando o valor de media
}

#Testando a função
ls() #função lista de objetos no environment
media
media()
dados=rnorm(20,2,1) #gerando um conjunto de dados
media(dados) #aplicando a função media sobre o conjunto de dados
dados1=rnorm(200,2,1) #gerando um outro conjunto de dados
media(dados1) #aplicando a função media sobre o conjunto de dados
dados2=(rnorm(10000,2,1)) #gerando um conjunto de dados
media(dados2) #aplicando a função media sobre o conjunto de dados
sd(dados) #desvio padrão dos dados
dados3=rnorm(20,2,0.01) #gerando um conjunto de dados
media(dados3) #aplicando a função media sobre o conjunto de dados
dados4=rnorm(200,2,0.01) #gerando um conjunto de dados
media(dados4) #aplicando a função media sobre o conjunto de dados
dados[2]<-NA #trocando o segundo item dos dados por NA
dados
media(dados) #aplicando a função media sobre o conjunto de dados

#Uma função mais elaborada
#Função que calcula a média na presença de NA's, mas que lança na tela uma mensagem sobre o número de NA's removidos do cálculo

media<-function(x,rmNA=TRUE)  
{
  if(rmNA==TRUE) #se tiver NA's, crie um novo objeto omitindo esses NA's e mostre o número de NA's removidos
  {
    dados=(na.omit(x))
    n.NA=length(x)-length(dados)
    cat("\t", n.NA," valores NA excluídos\n")
  }
  else
  {
    dados=x #calcule a media desse conjunto de dados sem os NA's
  }
  soma=sum(dados)
  nobs=length(dados)
  media=soma/nobs
  return(media)
}

#Calcular a média do vetor dados
media(dados)

#Função para calcular variância
var.curso<-function(x)
{
  media=media(x)
  dados=na.omit(x)	
  disvquad=(dados-media)^2
  var.curso=sum(disvquad)/(length(dados)-1)
  return(var.curso)
}

#Calcular a variância de dados e comparando com a função do R!
var.curso(dados) #fornece a variância dos dados e informa quantos NA's foram excluídos
var(dados)### dica: veja o help dessa função "help(var)"
#Não calcula a variância porque dados possui NA
var(dados,na.rm=TRUE) #calcula a variância dos dados porque os NA's foram removidos com o argumento na.rm
var(dados,na.rm=FALSE) #não calcula a variância porque dados possui NA

#Função para calcular o Índice de Dispersão
#Os índices de dispersão nos ajudam a avaliar se contagens por amostras estão distribuídas de modo aleatório, agregado ou uniforme

ID.curso<-function(x)
{
  id=var.curso(x)/media(x) #razão variância / média
  return(id)
}

#Simulando dados com parâmetros conhecidos
#Simulando Aleatório
aleat=rpois(200,2) #gerando o conjunto de dados aleatório com distribuição de Poisson com parâmetro lambda
aleat #confere objeto

#Uniforme
unif=runif(200,0,4) #gera distribuição aleatória
unif #confere objeto
unif=round(unif,0) #arredondando os valores, removendo as casas decimais
unif #confere objeto

#Agregado
agreg=round(c(runif(100,0,1),runif(100,5,10))) #agrega em um conjunto dois dados com distribuição aleatória
agreg #confere

#Calculando o coeficiente de dispersão para cada conjunto de dados gerado
ID.curso(aleat)
ID.curso(unif)
ID.curso(agreg)

#Quando o valor é próximo a 1 a distribuição é considerada aleatória. Isto quer dizer que que a ocorrência de cada indivíduo na parcela é indendente da ocorrência das demais. Neste caso, o número de indivíduos por parcela é descrito por uma variável Poisson, que tem exatamente a média igual à variância. Podemos então fazer um teste de significância simulando uma distribuição Poisson com a mesma média dos dados.

#Função para criar o teste de signficância do ID
test.ID <- function(x, nsim=1000)
{ 
  ID.curso=function(x){var(x)/mean(x)}# essa função precisa das funcoes media e ID.curso
  dados=na.omit(x) #omitindo os NA's
  ndados=length(dados) #objeto com o comprimento dos dados
  med=mean(dados) #objeto com a media dos dados
  id=var(dados)/med #objeto com a variância dos dados / media
  simula.aleat=rpois(length(dados)*nsim, lambda=med) #aleatorizando os dados com distribuição Poisson e parâmetro lambda
  sim.dados=matrix(simula.aleat,ncol= ndados) #gerando uma matriz com os dados anteriormente gerados e especificando o número de colunas
  sim.ID=apply(sim.dados,1,ID.curso) #aplicando à matriz a função ID.curso
  quant.ID=quantile(sim.ID, probs=c(0.025,0.975)) #calculando os quantis para as probabilidades especificadas
  if(id>=quant.ID[1] & id<=quant.ID[2])
  { 
    cat("\n\n\n\t distribuição aleatória para alfa=0.05 \n\t ID= ",id,"\n\n\n") #se o valor de id for maior igual ao quant.ID[1] e menor igual ao quant.ID[2], mostre essa mensagem
  }
  if(id < quant.ID[1]) #se o valor de id for menor do que quant.ID[1], mostre essa mensagem
  { 
    cat("\n\n\n\t distribuição uniforme, p<0.025 \n\t ID= ",id,"\n\n\n")
  }
  if(id>quant.ID[2]) #se o valor de id for maior do que quant.ID[2], mostre essa mensagem
  { 
    cat("\n\n\n\t distribuição agregado, p>0.975 \n\t ID= ",id,"\n\n\n")
  }
  resulta=c(id,quant.ID) #retorne um conjunto com os valores de id e os quantis
  names(resulta)<-c("Indice de Dispersão", "critico 0.025", "critico 0.975") #especificando os nomes que aparecem em resulta
  return(resulta)
}

#Testanto os dados simulados
test.ID(aleat)
test.ID(agreg)
test.ID(unif)

#Outra função
eda.shape <- function(x)
{
  x11() 
  par(mfrow = c(2,2))	## muda o dispositivo gráfico para 2x2
  hist(x)                 ## produz histograma de x
  boxplot(x)
  iqd <- summary(x)[5] -	summary(x)[2]     ## faz a diferença entre o quinto elemento x e o segundo
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l")
  qqnorm(x) #produz um QQ plot normal dos valores
  qqline(x) #adiciona uma linha para comparar com o QQ plot normal
  par(mfrow=c(1,1))
  
}

#Criando um vetor de dados com 20 valores simulando a densidade de árvores por parcelas
set.seed(22) ## estabelece uma semente aleatória 
dados.pois20<-rpois(20,lambda=6) ## sorteia dados aleatórios de uma função poisson com média 6
sum(dados.pois20) ## a somatória aqui sempre dará 131, somente porque a semente é a mesma
set.seed(22)
dados.norm20<-rnorm(20,mean=6, sd=2) ## sorteia 20 dados de uma função normal com média 6 e dp = 1
sum (dados.norm20)               ### aqui o resultado dará sempre 130.48

###aplicar eda.shape para dados.dens

eda.shape(dados.pois20)

eda.shape(dados.norm20)

###aumentando a amostra

eda.shape(rpois(500,6))

eda.shape(rnorm(500,6))

#Modificando uma função
eda.shape1 <- function(x)
{
  x11() 
  par(mfrow = c(2,2)) ## muda o dispositivo gráfico para 2x2
  hist(x,main="Histograma de x") #produz histograma
  boxplot(x, main="BoxPlot de x") #produz boxplot
  iqd <- summary(x)[5] -	summary(x)[2] ## faz a diferença entre o quinto elemento x e o segundo
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribuição de densidade de x") #gerando plot da densidade
  qqnorm(x,col="red",main="Gráfico Quantil x Quantil",xlab="Quantil Teórico",ylab="Quantil da Amostra") #produz um QQ plot normal dos valores
  qqline(x) #adiciona uma linha para comparar com o QQ plot normal
  par(mfrow=c(1,1))
  
}

#Executando a função modificada
eda.shape1(rnorm(500,6)) #4 gráficos gerados: histograma, boxplot, plot da densidade e teste de normalidade

#Fazendo ciclos de operações
#Permite a aplicação de uma função ou tarefa a uma sequência pré determinada de dados. Ou seja, repete a mesma sequência de comandos um número determinado de vezes
#Simulando dados de novo!

x1=rpois(20,1) #gerando os dados com distribuição de Poisson
x2=rpois(20,2) #gerando outro conjunto de dados com distribuição de Poisson
x3=rpois(20,3) #gerando outro conjunto de dados com distribuição de Poisson
x4=rpois(20,1) #gerando outro conjunto de dados com distribuição de Poisson
sp.oc=matrix(c(x1,x2,x3,x4), ncol=4) #gerando uma matriz de 4 colunas com os dados acima gerados combinados
colnames(sp.oc)<-c("plot A", "plot B", "plot C", "plot D") #modificando os nomes das colunas
rownames(sp.oc)<-paste("sp", c(1:20)) #modificando os nomes das linhas
str(sp.oc) #verificando estrutura da matriz
dim(sp.oc) #verificando as dimensões do objeto
head(sp.oc) #retorna o cabeçalho do objeto

#Uma função para contar espécies por parcelas. Mais uma vez uma função já existente em versão piorada!!

n.spp<-function(dados)
{
  nplot=dim(dados)[2] #dimensão de dados[2]
  resultados=rep(0,nplot) #objeto com repetição de 0
  names(resultados)<-paste("n.spp", c(1:nplot)) #renomeando as colunas
  dados[dados>0]=1 #contando 1 para os valores em dados maior do que 0
  for(i in 1:(dim(dados)[2]))
  {
    cont.sp=sum(dados[,i]) #somando os valores em dados
    resultados[i]=cont.sp #fornecendo resultado da contagem de espécies
  }
  return(resultados)
}


##### Aplicando a função 

n.spp(sp.oc) #função dá a frequência de cada espécie, mas não especifica por parcela

#Mais função!! SIMILARIDADE
sim<-function(dados)
{
  nplot=dim(dados)[2] #dimensão de dados[2]
  similar=matrix(1,ncol=nplot,nrow=nplot) #criando uma matriz com igual número de linhas e colunas
  rownames(similar)<-paste("plot", c(1:nplot)) #renomeando as linhas
  colnames(similar)<-paste("plot", c(1:nplot)) #renomeando as colunas
  dados[dados>0]=1 #contando 1 para os valores em dados maior do que 0
  for(i in 1:nplot-1)
  {
    m=i+1
    for(m in m:nplot)
    {
      co.oc=sum(dados[,i]>0 & dados[,m]>0) #contando co-ocorrência o somatório de quem apresenta valor maior do que 0 na coluna i e maior do que 0 na coluna m
      total.sp=sum(dados[,i])+sum(dados[,m])-co.oc #total de espécies calculado contando a soma de dados [,i] e dados[,m], descontando a co-ocorrência 
      similar[i,m]=co.oc/total.sp #similaridade de i com m igual à co-ocorrência dividida pelo total
      similar[m,i]=co.oc/total.sp #similaridade de m com i igual à co-ocorrência dividida pelo total
    }
    
  }
  return(similar)
}

#Aplicando a função SIM
sim(sp.oc) #gerando uma matriz com os valores de similaridade

###########################################################################

#Exercícios 9 - Construção de Funções
#9.2 Exercício Análise exploratória simultânea de duas variáveis

#Tarefa: Crie uma função para saída gráfica de análises exploratórias de duas variáveis. Essa saída pode ter por exemplo boxplot, histograma, qq norm e y por x. A função deve permitir a entrada de dois objetos vetores de mesmo tamanho (x e y). Caso os valores de x e y forem desenhados em um mesmo gráfico, defina cores diferentes. As legendas devem ser em português e a saída (return) deve apresentar o sumário das duas variáveis e o coeficiente de correlação.

explo.two = function(x,y) {
 
  x11()
  par(mfrow = c(3,2)) #estabelecendo todos os gráficos em um mesmo painel
  
  hist(x, col = "blue", xlab = "x", ylab = "Frequência", main = "Histograma de x") #gerando histograma de x
  hist(y, col = "red", xlab = "y", ylab = "Frequência", main = "Histograma de y") #gerando histograma de y
  
  qqnorm(x, #plotando os dados de x contra uma distribuição normal
         col = "blue",
         xlab = "Quantil teórico",
         ylab = "Quantil da amostra",
         main = "Normal Q-Q Plot de x"
         )
  qqline(x) #plotando uma linha para facilitar a comparação
  
  qqnorm(y, #plotando os dados de y contra uma distribuição normal
         col = "red",
         xlab = "Quantil teórico",
         ylab = "Quantil da amostra",
         main = "Normal Q-Q Plot de y"
         )
  qqline(y) #plotando uma linha para facilitar a comparação
  
  boxplot(x, y, col = c("blue", "red"), main = "Boxplot de x e y", names = c("x", "y")) #boxplot de x e y
  
  sum.x = summary(x) #summary de x
  sum.y = summary(y) #summary de y
  correlacao = cor(x, y) #coeficiente de correlação entre x e y
  
  saida = list(sum.x, sum.y, correlacao) #espeficiando os objetos que aparecem na saída
  names(saida) = c("Sumário de x", "Sumário de y", "Correlação entre x e y") #renomeando os objetos
  return(saida)
}

x=rnorm(100, 9, 2) #dois conjuntos de dados hipotéticos
y=rpois(100,15)

explo.two(x,y) #aplicando a função

##############################################################

#9.3 Índices de Diversidade de Espécies
#Resolvido no notaR.
