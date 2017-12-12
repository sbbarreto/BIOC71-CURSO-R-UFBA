#Silvia Britto Barreto
#Atividades preliminares
message("Hello World") #aparece a mensagem Hello World

letters()   #aparecem as letras de a a z
library(cars)   #Error in library(cars) : there is no package called 'cars'
plot(cars) #? plotado um gr?fico com speed no eixo x e dist no eixo y   
rep("Oi", 10) #aparece a repeti??o de Oi dez vezes 
seq(from = 100, to = 1000, by = 50) #? gerada uma sequ?ncia de 100 a 1000 de 50 em 50 
help(seq)  #aparece o help de seq
??NA  
a = sqrt(2) #novo objeto com raiz quadrada de 2  
iris  #aparece uma tabela com informa??es de 4 medidas de esp?cies
summary(iris)  #aparece um sum?rio de iris
x = c(1:70)  #gera um conjunto de 1 a 70
plot(x)  #plota x
mean(x)  #m?dia de x
mean(x^2)  #m?dia de x ao quadrado
mean(x)^2 #m?dia de x elevada ao quadrado

#Gerando os dados

Idade = c(3.0, 4.0, 5.0, 6.0, 8.0, 9.0, 10.0, 11.0, 12.0, 14.0, 15.0, 16.0, 17.0) #gera um objeto Idade com os valores especificados
TamanhoDaAsa = c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0) #gera um objeto TamanhoDaAsa com os valores especificados

plot(x = Idade, y = TamanhoDaAsa)  #plota Idade e TamanhoDaAsa
modelo = lm(TamanhoDaAsa~Idade)  #calcula modelo lm 
summary(modelo) #mostra sum?rio do objeto criando anteriormente

#Fazendo an?lise na unha

media.Idade = mean(Idade)  #tira m?dia da idade
media.TamanhoDaAsa = mean(TamanhoDaAsa)  #tira m?dia do tamanho da asa
desvio.quadrado.Idade = (Idade - media.Idade)^2  #desvio quadrado da idade
desvio.quadrado.TamanhoDaAsa = (TamanhoDaAsa - media.TamanhoDaAsa)^2  #desvio quadrado do tamanho da asa
ss.Idade = sum(desvio.quadrado.Idade)  #somat?rio dos desvios quadrados de Idade
produto.cruzado = (Idade - media.Idade) * (TamanhoDaAsa - media.TamanhoDaAsa)  #
ss.prod.cruz = sum(produto.cruzado)  
b = ss.prod.cruz/ss.Idade  
a = media.TamanhoDaAsa- b * media.Idade  
TamanhoDaAsa.previsto = a + b * Idade  
ss.t = sum(desvio.quadrado.TamanhoDaAsa)  
desvio.quadrado.previsto = (TamanhoDaAsa.previsto - media.TamanhoDaAsa)^2  
ss.regressao = sum(desvio.quadrado.previsto)  
desvio.quadrado.residuo = (TamanhoDaAsa.previsto - TamanhoDaAsa)^2  
ss.residuo = sum(desvio.quadrado.residuo)  
df.regressao = 1  
df.residuo = length(Idade) - 2  
ms.regressao = ss.regressao / df.regressao  
ms.residuo = ss.residuo / df.residuo  
f = ms.regressao/ms.residuo  
r.quad = ss.regressao/ss.residuo

#Fazendo os gr?ficos

plot(TamanhoDaAsa ~ Idade, xlab = "Idade (d?cadas)",  #plot do tamanho da asa com comprimento em metros em fun??o da idade em d?cadas
     ylab = "Comprimento (metros)",  
     main = "Tamanho da asa"  
     , bty = "n"  
     , xlim = c(0, 23)  
     , ylim = c(0, max(TamanhoDaAsa))  
     , lwd = 2  
     , yaxt = "n"  
     , xaxt = "n"  
)  
axis(1, pos = 0)  
axis(2, pos = 0)  
abline(a = a, b = b, col = "red", lwd = 2)  
anova(modelo)  
coefficients(modelo)  
plot(modelo)  
summary(modelo)  
plot(Idade, modelo$residuals)  
abline(h = 0)

#Testando a signific?ncia e plotando

test.dist = rnorm(length(TamanhoDaAsa), mean = media.TamanhoDaAsa, sd = sqrt(var(TamanhoDaAsa)))  #gerando uma distribui??o normal com o comprimento de TamanhoDaAsa, com m?dia igual a TamanhoDaAsa e desvio padr?o igual ? raiz quadrada da vari?ncia de TamanhoDaAsa
qqnorm(TamanhoDaAsa); qqline(TamanhoDaAsa)  #verificando normalidade do TamanhoDaAsa
hist(modelo$residuals) #gerando histograma dos res?duos do objeto modelo  
df <- summary(modelo)$fstatistic  #gerando sum?rios das estat?sticas f do modelo
curve(df(x, df1 = df[2], df2 = df[3]), from = 0, to = 20, xlab = "F", bty = "l", ylab = "")  
abline(v = 12.04, col = "red")  #adicionando linha vertical
f.crit = qf(.95, df1 = df[2], df2 = df[3])  
abline(v = f.crit, col = "blue")  
qt(1-0.05/2, df.residuo)  
confint.lm(modelo)  
prd<-predict(modelo, newdata = data.frame(Idade), interval = c("confidence"), level = 0.95, type = "response")
