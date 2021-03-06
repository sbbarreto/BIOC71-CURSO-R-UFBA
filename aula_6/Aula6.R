###################################
# Introdu��o Linguagem R
# Silvia Britto Barreto
# Universidade Federal da Bahia
# Aula 6
###################################

#9. Tutoriais de Programa��o
#Uma fun��es muito simples

media <-function(x)
{
  soma=sum(x) #criando um objeto chamado soma com a soma dos valores do objeto
  nobs=length(x) #criando um objeto chamado nobs com o comprimento do objeto
  media=soma/nobs #criando um objeto media com o valor do objeto soma dividido pelo valor do objeto nobs
  return(media) #retornando o valor de media
}

#Testando a fun��o
ls() #fun��o lista de objetos no environment
media
media()
dados=rnorm(20,2,1) #gerando um conjunto de dados
media(dados) #aplicando a fun��o media sobre o conjunto de dados
dados1=rnorm(200,2,1) #gerando um outro conjunto de dados
media(dados1) #aplicando a fun��o media sobre o conjunto de dados
dados2=(rnorm(10000,2,1)) #gerando um conjunto de dados
media(dados2) #aplicando a fun��o media sobre o conjunto de dados
sd(dados) #desvio padr�o dos dados
dados3=rnorm(20,2,0.01) #gerando um conjunto de dados
media(dados3) #aplicando a fun��o media sobre o conjunto de dados
dados4=rnorm(200,2,0.01) #gerando um conjunto de dados
media(dados4) #aplicando a fun��o media sobre o conjunto de dados
dados[2]<-NA #trocando o segundo item dos dados por NA
dados
media(dados) #aplicando a fun��o media sobre o conjunto de dados

#Uma fun��o mais elaborada
#Fun��o que calcula a m�dia na presen�a de NA's, mas que lan�a na tela uma mensagem sobre o n�mero de NA's removidos do c�lculo

media<-function(x,rmNA=TRUE)  
{
  if(rmNA==TRUE) #se tiver NA's, crie um novo objeto omitindo esses NA's e mostre o n�mero de NA's removidos
  {
    dados=(na.omit(x))
    n.NA=length(x)-length(dados)
    cat("\t", n.NA," valores NA exclu�dos\n")
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

#Calcular a m�dia do vetor dados
media(dados)

#Fun��o para calcular vari�ncia
var.curso<-function(x)
{
  media=media(x)
  dados=na.omit(x)	
  disvquad=(dados-media)^2
  var.curso=sum(disvquad)/(length(dados)-1)
  return(var.curso)
}

#Calcular a vari�ncia de dados e comparando com a fun��o do R!
var.curso(dados) #fornece a vari�ncia dos dados e informa quantos NA's foram exclu�dos
var(dados)### dica: veja o help dessa fun��o "help(var)"
#N�o calcula a vari�ncia porque dados possui NA
var(dados,na.rm=TRUE) #calcula a vari�ncia dos dados porque os NA's foram removidos com o argumento na.rm
var(dados,na.rm=FALSE) #n�o calcula a vari�ncia porque dados possui NA

#Fun��o para calcular o �ndice de Dispers�o
#Os �ndices de dispers�o nos ajudam a avaliar se contagens por amostras est�o distribu�das de modo aleat�rio, agregado ou uniforme

ID.curso<-function(x)
{
  id=var.curso(x)/media(x) #raz�o vari�ncia / m�dia
  return(id)
}

#Simulando dados com par�metros conhecidos
#Simulando Aleat�rio
aleat=rpois(200,2) #gerando o conjunto de dados aleat�rio com distribui��o de Poisson com par�metro lambda
aleat #confere objeto

#Uniforme
unif=runif(200,0,4) #gera distribui��o aleat�ria
unif #confere objeto
unif=round(unif,0) #arredondando os valores, removendo as casas decimais
unif #confere objeto

#Agregado
agreg=round(c(runif(100,0,1),runif(100,5,10))) #agrega em um conjunto dois dados com distribui��o aleat�ria
agreg #confere

#Calculando o coeficiente de dispers�o para cada conjunto de dados gerado
ID.curso(aleat)
ID.curso(unif)
ID.curso(agreg)

#Quando o valor � pr�ximo a 1 a distribui��o � considerada aleat�ria. Isto quer dizer que que a ocorr�ncia de cada indiv�duo na parcela � indendente da ocorr�ncia das demais. Neste caso, o n�mero de indiv�duos por parcela � descrito por uma vari�vel Poisson, que tem exatamente a m�dia igual � vari�ncia. Podemos ent�o fazer um teste de signific�ncia simulando uma distribui��o Poisson com a mesma m�dia dos dados.

#Fun��o para criar o teste de signfic�ncia do ID
test.ID <- function(x, nsim=1000)
{ 
  ID.curso=function(x){var(x)/mean(x)}# essa fun��o precisa das funcoes media e ID.curso
  dados=na.omit(x) #omitindo os NA's
  ndados=length(dados) #objeto com o comprimento dos dados
  med=mean(dados) #objeto com a media dos dados
  id=var(dados)/med #objeto com a vari�ncia dos dados / media
  simula.aleat=rpois(length(dados)*nsim, lambda=med) #aleatorizando os dados com distribui��o Poisson e par�metro lambda
  sim.dados=matrix(simula.aleat,ncol= ndados) #gerando uma matriz com os dados anteriormente gerados e especificando o n�mero de colunas
  sim.ID=apply(sim.dados,1,ID.curso) #aplicando � matriz a fun��o ID.curso
  quant.ID=quantile(sim.ID, probs=c(0.025,0.975)) #calculando os quantis para as probabilidades especificadas
  if(id>=quant.ID[1] & id<=quant.ID[2])
  { 
    cat("\n\n\n\t distribui��o aleat�ria para alfa=0.05 \n\t ID= ",id,"\n\n\n") #se o valor de id for maior igual ao quant.ID[1] e menor igual ao quant.ID[2], mostre essa mensagem
  }
  if(id < quant.ID[1]) #se o valor de id for menor do que quant.ID[1], mostre essa mensagem
  { 
    cat("\n\n\n\t distribui��o uniforme, p<0.025 \n\t ID= ",id,"\n\n\n")
  }
  if(id>quant.ID[2]) #se o valor de id for maior do que quant.ID[2], mostre essa mensagem
  { 
    cat("\n\n\n\t distribui��o agregado, p>0.975 \n\t ID= ",id,"\n\n\n")
  }
  resulta=c(id,quant.ID) #retorne um conjunto com os valores de id e os quantis
  names(resulta)<-c("Indice de Dispers�o", "critico 0.025", "critico 0.975") #especificando os nomes que aparecem em resulta
  return(resulta)
}

#Testanto os dados simulados
test.ID(aleat)
test.ID(agreg)
test.ID(unif)

#Outra fun��o
eda.shape <- function(x)
{
  x11() 
  par(mfrow = c(2,2))	## muda o dispositivo gr�fico para 2x2
  hist(x)                 ## produz histograma de x
  boxplot(x)
  iqd <- summary(x)[5] -	summary(x)[2]     ## faz a diferen�a entre o quinto elemento x e o segundo
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l")
  qqnorm(x) #produz um QQ plot normal dos valores
  qqline(x) #adiciona uma linha para comparar com o QQ plot normal
  par(mfrow=c(1,1))
  
}

#Criando um vetor de dados com 20 valores simulando a densidade de �rvores por parcelas
set.seed(22) ## estabelece uma semente aleat�ria 
dados.pois20<-rpois(20,lambda=6) ## sorteia dados aleat�rios de uma fun��o poisson com m�dia 6
sum(dados.pois20) ## a somat�ria aqui sempre dar� 131, somente porque a semente � a mesma
set.seed(22)
dados.norm20<-rnorm(20,mean=6, sd=2) ## sorteia 20 dados de uma fun��o normal com m�dia 6 e dp = 1
sum (dados.norm20)               ### aqui o resultado dar� sempre 130.48

###aplicar eda.shape para dados.dens

eda.shape(dados.pois20)

eda.shape(dados.norm20)

###aumentando a amostra

eda.shape(rpois(500,6))

eda.shape(rnorm(500,6))

#Modificando uma fun��o
eda.shape1 <- function(x)
{
  x11() 
  par(mfrow = c(2,2)) ## muda o dispositivo gr�fico para 2x2
  hist(x,main="Histograma de x") #produz histograma
  boxplot(x, main="BoxPlot de x") #produz boxplot
  iqd <- summary(x)[5] -	summary(x)[2] ## faz a diferen�a entre o quinto elemento x e o segundo
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribui��o de densidade de x") #gerando plot da densidade
  qqnorm(x,col="red",main="Gr�fico Quantil x Quantil",xlab="Quantil Te�rico",ylab="Quantil da Amostra") #produz um QQ plot normal dos valores
  qqline(x) #adiciona uma linha para comparar com o QQ plot normal
  par(mfrow=c(1,1))
  
}

#Executando a fun��o modificada
eda.shape1(rnorm(500,6)) #4 gr�ficos gerados: histograma, boxplot, plot da densidade e teste de normalidade

#Fazendo ciclos de opera��es
#Permite a aplica��o de uma fun��o ou tarefa a uma sequ�ncia pr� determinada de dados. Ou seja, repete a mesma sequ�ncia de comandos um n�mero determinado de vezes
#Simulando dados de novo!

x1=rpois(20,1) #gerando os dados com distribui��o de Poisson
x2=rpois(20,2) #gerando outro conjunto de dados com distribui��o de Poisson
x3=rpois(20,3) #gerando outro conjunto de dados com distribui��o de Poisson
x4=rpois(20,1) #gerando outro conjunto de dados com distribui��o de Poisson
sp.oc=matrix(c(x1,x2,x3,x4), ncol=4) #gerando uma matriz de 4 colunas com os dados acima gerados combinados
colnames(sp.oc)<-c("plot A", "plot B", "plot C", "plot D") #modificando os nomes das colunas
rownames(sp.oc)<-paste("sp", c(1:20)) #modificando os nomes das linhas
str(sp.oc) #verificando estrutura da matriz
dim(sp.oc) #verificando as dimens�es do objeto
head(sp.oc) #retorna o cabe�alho do objeto

#Uma fun��o para contar esp�cies por parcelas. Mais uma vez uma fun��o j� existente em vers�o piorada!!

n.spp<-function(dados)
{
  nplot=dim(dados)[2] #dimens�o de dados[2]
  resultados=rep(0,nplot) #objeto com repeti��o de 0
  names(resultados)<-paste("n.spp", c(1:nplot)) #renomeando as colunas
  dados[dados>0]=1 #contando 1 para os valores em dados maior do que 0
  for(i in 1:(dim(dados)[2]))
  {
    cont.sp=sum(dados[,i]) #somando os valores em dados
    resultados[i]=cont.sp #fornecendo resultado da contagem de esp�cies
  }
  return(resultados)
}


##### Aplicando a fun��o 

n.spp(sp.oc) #fun��o d� a frequ�ncia de cada esp�cie, mas n�o especifica por parcela

#Mais fun��o!! SIMILARIDADE
sim<-function(dados)
{
  nplot=dim(dados)[2] #dimens�o de dados[2]
  similar=matrix(1,ncol=nplot,nrow=nplot) #criando uma matriz com igual n�mero de linhas e colunas
  rownames(similar)<-paste("plot", c(1:nplot)) #renomeando as linhas
  colnames(similar)<-paste("plot", c(1:nplot)) #renomeando as colunas
  dados[dados>0]=1 #contando 1 para os valores em dados maior do que 0
  for(i in 1:nplot-1)
  {
    m=i+1
    for(m in m:nplot)
    {
      co.oc=sum(dados[,i]>0 & dados[,m]>0) #contando co-ocorr�ncia o somat�rio de quem apresenta valor maior do que 0 na coluna i e maior do que 0 na coluna m
      total.sp=sum(dados[,i])+sum(dados[,m])-co.oc #total de esp�cies calculado contando a soma de dados [,i] e dados[,m], descontando a co-ocorr�ncia 
      similar[i,m]=co.oc/total.sp #similaridade de i com m igual � co-ocorr�ncia dividida pelo total
      similar[m,i]=co.oc/total.sp #similaridade de m com i igual � co-ocorr�ncia dividida pelo total
    }
    
  }
  return(similar)
}

#Aplicando a fun��o SIM
sim(sp.oc) #gerando uma matriz com os valores de similaridade

###########################################################################

#Exerc�cios 9 - Constru��o de Fun��es
#9.2 Exerc�cio An�lise explorat�ria simult�nea de duas vari�veis

#Tarefa: Crie uma fun��o para sa�da gr�fica de an�lises explorat�rias de duas vari�veis. Essa sa�da pode ter por exemplo boxplot, histograma, qq norm e y por x. A fun��o deve permitir a entrada de dois objetos vetores de mesmo tamanho (x e y). Caso os valores de x e y forem desenhados em um mesmo gr�fico, defina cores diferentes. As legendas devem ser em portugu�s e a sa�da (return) deve apresentar o sum�rio das duas vari�veis e o coeficiente de correla��o.

explo.two = function(x,y) {
 
  x11()
  par(mfrow = c(3,2)) #estabelecendo todos os gr�ficos em um mesmo painel
  
  hist(x, col = "blue", xlab = "x", ylab = "Frequ�ncia", main = "Histograma de x") #gerando histograma de x
  hist(y, col = "red", xlab = "y", ylab = "Frequ�ncia", main = "Histograma de y") #gerando histograma de y
  
  qqnorm(x, #plotando os dados de x contra uma distribui��o normal
         col = "blue",
         xlab = "Quantil te�rico",
         ylab = "Quantil da amostra",
         main = "Normal Q-Q Plot de x"
         )
  qqline(x) #plotando uma linha para facilitar a compara��o
  
  qqnorm(y, #plotando os dados de y contra uma distribui��o normal
         col = "red",
         xlab = "Quantil te�rico",
         ylab = "Quantil da amostra",
         main = "Normal Q-Q Plot de y"
         )
  qqline(y) #plotando uma linha para facilitar a compara��o
  
  boxplot(x, y, col = c("blue", "red"), main = "Boxplot de x e y", names = c("x", "y")) #boxplot de x e y
  
  sum.x = summary(x) #summary de x
  sum.y = summary(y) #summary de y
  correlacao = cor(x, y) #coeficiente de correla��o entre x e y
  
  saida = list(sum.x, sum.y, correlacao) #espeficiando os objetos que aparecem na sa�da
  names(saida) = c("Sum�rio de x", "Sum�rio de y", "Correla��o entre x e y") #renomeando os objetos
  return(saida)
}

x=rnorm(100, 9, 2) #dois conjuntos de dados hipot�ticos
y=rpois(100,15)

explo.two(x,y) #aplicando a fun��o

##############################################################

#9.3 �ndices de Diversidade de Esp�cies
#Resolvido no notaR.
