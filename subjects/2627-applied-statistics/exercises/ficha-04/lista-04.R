#### Ficha 04 ----

# 01

a1 = c(1.56,1.50,1.54,1.49,1.51)
a2 = c(1.38,1.41,1.44,1.37,1.40)
a3 = c(1.49,1.54,1.48,1.51,1.48)
a4 = c(1.46,1.49,1.44,1.52,1.49)

df = data.frame(peso=c(a1,a2,a3,a4),grupo=factor(gl(4,5,20)))   
attach(df)

mod = aov(df$peso~df$grupo)
summary(mod)

boxplot(df$peso~df$grupo)
pairwise.t.test(df$peso,df$grupo)

# pressupostos de normalidade dos residuos
qqnorm(mod$residuals)
qqline(mod$residuals)
shapiro.test(mod$residuals)
leveneTest(df$peso~df$grupo) 
plot(fitted(mod),residuals(mod))

# ex 02

sem_trat = c(117, 124, 40, 88, 40)
a = c(440, 264, 221, 136)
b = c(605, 626, 385, 475)
c = c(2664, 2078, 3584, 1540, 1840)

efeitos = c(sem_trat,a,b,c)
trat = c(rep('sem trat',5),rep('A',4),rep('B',4),rep('C',5))

mod = aov(efeitos~trat)
summary(mod)
boxplot(efeitos~trat)
pairwise.t.test(efeitos,trat)

# normalidade dos residuos
qqnorm(mod$residuals)
qqline(mod$residuals)
shapiro.test(mod$residuals)
# homoscedasticidade
plot(mod$fitted.values,mod$residuals)

# trasnformacao log
mod = aov(log(efeitos)~trat)
summary(mod)
# normalidade dos residuos
qqnorm(mod$residuals)
qqline(mod$residuals)
shapiro.test(mod$residuals)
# homoscedasticidade
plot(mod$fitted.values,mod$residuals)

# trasnformacao 1/y
mod = aov(1/efeitos~trat)
summary(mod)
# normalidade dos residuos
qqnorm(mod$residuals)
qqline(mod$residuals)
shapiro.test(mod$residuals)
# homoscedasticidade
plot(mod$fitted.values,mod$residuals)


# ex 03

n_fumou = c(35,120,90,109,82,40,68,84,124,77,140,127,58,110,42,57,93,70,51,74,74)
fuma = c(96,107,63,134,140,103,158)
ja_fumou = c(62, 78, 95, 69,73, 47, 82, 118,60, 85, 141, 131,77, 105, 64, 76,52, 46, 124, 69,115, 66, 65, 69,82, 91, 42, 97,52, 151, 53, 137,105, 40, 67, 103,143, 80, 95, 108,80, 57, 99, 56)

trat = c(rep('NF',length(n_fumou)),rep('F',length(fuma)),rep('JF',length(ja_fumou)))
efeitos = c(n_fumou,fuma,ja_fumou)

summary(aov(lm(efeitos~trat)))
pairwise.t.test(efeitos,trat)

boxplot(efeitos~trat)

# ex 04

homicidio = c(
  0.78, 1.71, 0.19, 1.55, 0.27, 4.08, 0.16, 1.88,
  1.88, 4.10, 0.14, 3.11, 0.42, 1.52, 0.35,
  0.25, 0.38, 2.38, 2.49, 0.35, 0.41, 1.49,
  0.81, 2.50, 0.21, 4.70, 2.39, 0.35, 1.18,
  0.04, 1.80, 0.13, 1.81, 4.38, 1.79, 2.26,
  0.04, 0.12, 1.32, 1.15, 0.10, 0.27, 0.19,
  0.09, 0.30, 3.58, 3.49, 1.24, 2.77, 0.47)
acidente = c(
  1.18, 1.46, 0.03, 0.65, 0.40, 7.62, 0.04, 0.05,
  3.85, 0.46, 0.47, 2.96)
suicidio = c(1.15, 0.54, 0.92, 0.35, 3.22, 0.21, 0.54, 1.82)

efeitos = c(homicidio,acidente,suicidio)
trat= c(rep('Homicidio',length(homicidio)),rep('Acidente',length(acidente)),rep('Suicidio',length(suicidio)))

summary(aov(lm(efeitos~trat)))
boxplot(efeitos~trat)
aov(lm(efeitos~trat))


# ex 05

a = c(2946, 2913, 2280, 3685, 2310, 2582, 3002, 2408)
b = c(3186, 2857, 3099, 2761, 3290, 2937, 3347)
c = c(2300, 2903, 2572, 2584, 2675, 2571)
d = c(2286,2938,2952,2348,2691,2858,2414,2008,2850,2762)

efeitos = c(a,b,c,d)
trat = c(rep('A',length(a)),rep('B',length(b)),rep('C',length(c)),rep('D',length(d)))

summary(aov(lm(efeitos~trat)))
boxplot(efeitos~trat)
pairwise.t.test(efeitos,trat)


# ex 06

## prof mode
or = gl(3,10,30,labels=c('asian','european','african'))##
parto = gl(2,5,30,labels=c('eutocito','distocito'))
##

peso = c(
  2.9, 3.5, 2.1,
  3.3, 3.4, 2.2,
  2.7, 3.3, 2.3,
  2.8, 3.4, 2.4,
  3.2, 3.3, 2.3,
  2.9, 3.9, 2.0,
  3.3, 4.1, 2.3,
  3.1, 4.0, 2.2,
  3.0, 4.0, 2.1,
  3.2, 3.9, 2.0
)
library(dplyr)
df6 = data.frame(origem,tipo_parto,peso)
df6 %>% group_by(origem,tipo_parto) %>% summarise(media_peso=mean(peso))
origem = rep(c('asiatica','europeia','africana'),10)
tipo_parto = c(rep('eutocito',15),rep('distocito',15))


length(peso)
length(origem)
length(tipo_parto)
boxplot(peso~origem)
boxplot(peso~tipo_parto)

summary(aov(peso~origem)) # efeito origem 11.174
summary(aov(peso~tipo_parto)) # efeito tipo parto 0.28
summary(aov(peso~origem+tipo_parto)) # 
summary(aov(peso~origem*tipo_parto)) # efeitos origem*tipo_parto 0.705


# ex 07
dias = c(7, 9, 10, 8, 9, 10, 9, 9, 12, 10, 9, 12, 11, 12, 14)
grupo_etario = factor(rep(c('<20','20 a 29','30 a 39','40 a 49','>50'),each=3),levels = c('<20','20 a 29','30 a 39','40 a 49','>50'))
tipo_manual = rep(c('A','B','C'),5)

summary(aov(dias~grupo_etario)) # pvalue 0.0827
pairwise.t.test(dias,grupo_etario)
boxplot(dias~grupo_etario)

summary(aov(dias~tipo_manual)) # 0.0491
pairwise.t.test(dias,tipo_manual)
boxplot(dias~tipo_manual)

summary(aov(dias~grupo_etario+tipo_manual))

# nao dá pra usar interacao porque temos so uma info dentro de cada celula
summary(aov(dias~grupo_etario*tipo_manual)) 

# ex 08

consonante = c(58, 68, 60, 68, 64,62, 70, 65, 80, 69,67, 78, 68, 81, 70,70, 81, 70, 89, 74)
nivel_inicial = factor(rep(c('Nulo','Muito Baixo','Baixo','Médio'),each=5),levels=c('Nulo','Muito Baixo','Baixo','Médio'))
metodo_motivacao = rep(c('A','B','C','D','E'),4)


length(consonante)
length(nivel_inicial)
length(metodo_motivacao)

summary(aov(consonante~nivel_inicial)) # 0.0365
boxplot(consonante~nivel_inicial)
pairwise.t.test(consonante,nivel_inicial)

summary(aov(consonante~metodo_motivacao)) # 0.0144
boxplot(consonante~metodo_motivacao)
pairwise.t.test(consonante,metodo_motivacao)

summary(aov(consonante~metodo_motivacao+nivel_inicial)) # 0.0144


# ex 09

maturidade_emocional= c(25, 18, 17, 28, 23, 24,22, 19, 19,28, 16, 18, 32, 24, 22,30, 20, 20,25, 14, 10, 35, 16, 8,30, 15, 12)
grupo_etario = factor(rep(c('15-19','20-24','25-29'),each=9),levels=c('15-19','20-24','25-29'))
consumo = factor(rep(c("Nunca","Ocasional","Diario"), times = 9),
                 levels = c("Nunca","Ocasional","Diario"))

mod <- aov(maturidade_emocional ~ grupo_etario * consumo)
mod <- aov(maturidade_emocional ~ grupo_etario+consumo+consumo*grupo_etario)
summary(mod)
# check residuals
hist(mod$residuals)
shapiro.test(mod$residuals)

# check homoscedasticity
plot(fitted(mod),residuals(mod))
interaction.plot(consumo, grupo_etario, maturidade_emocional,col=rainbow(3))

# try transformation

mod2 <- aov(1/maturidade_emocional ~ grupo_etario * consumo)
summary(mod2)
# check residuals
hist(mod2$residuals)
# check homoscedasticity
plot(fitted(mod2),residuals(mod2))
interaction.plot(consumo, grupo_etario, 1/maturidade_emocional,col=rainbow(3))



# ex 10

m <- c(4.31,4.68,4.17,5.75, 4.89,6.18,3.77,5.70, 4.05,4.48,5.20,5.53,
       4.44,4.23,5.28,5.97, 4.59,5.92,4.44,5.52, 4.13,3.41,3.89,4.58,
       4.61,3.64,3.64,5.21, 3.91,3.32,4.18,5.50, 4.52,3.51,4.48,5.18,
       4.43,3.75,4.27,4.15, 3.79,4.63,5.81,6.89, 4.17,4.59,5.20,6.18,
       4.47,4.90,5.34,6.21, 4.35,5.31,5.94,7.56, 3.59,4.81,5.56,6.73)
grupo_etario <- factor(rep(c(1,2,3),each=20))
ocupacao <- factor(rep(c('a','b','c','d'),15))
length(ocupacao)
mod2 <- aov(m ~ grupo_etario * ocupacao)
summary(mod2)
interaction.plot(ocupacao, grupo_etario, m)

# check residuals
hist(mod2$residuals)
shapiro.test(mod2$residuals)

# check homoscedasticity
plot(fitted(mod2),residuals(mod2))
interaction.plot(ocupacao, grupo_etario, m,col=rainbow(3))


# Efeito do consumo. Todas as linhas descem de Nunca para Ocasional. Quem nunca consome tem maior maturidade emocional (média 28,3, contra 18,3 e 16,7). É o efeito mais forte (F = 38,9).
# Efeito da idade. As linhas estão a alturas diferentes, mas a diferença depende do consumo. Entre os que nunca consomem, os mais velhos (20-24 e 25-29, ambos com 30) estão acima dos 15-19 (25). Entre os que consomem diariamente, os 25-29 ficam muito abaixo (10). Na média geral, o grupo 25-29 é o mais baixo (18,3).
# Interacção. As linhas não são paralelas e chegam a cruzar-se: a 25-29 parte do topo e termina bem abaixo das outras.
# Nos 25-29 a maturidade cai sempre, e de forma acentuada: 30 → 15 → 10.
# Nos 15-19 e 20-24 cai de Nunca para Ocasional e depois estabiliza (20 → 20). Consumir diariamente não piora em relação a consumir ocasionalmente.
# A queda total também difere: 5 pontos nos 15-19, 10 nos 20-24 e 20 nos 25-29.

# Conclusão para o relatório: o consumo de marijuana está associado a menor maturidade emocional, mas a magnitude desse efeito depende da idade. É muito mais forte no grupo 25-29, o único em que o consumo diário se distingue do ocasional. Como a interacção é significativa (p = 0,007), não faz sentido falar de “o efeito do consumo” sem indicar o grupo etário.



x=c(-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,
    3,3,3,3,3)
y=c(8,1,1,3,1,5,6,9,7,4,13,7,12,10,11,17,14,14,19,
    13,17,17,20,18,15)
plot(x,y)



x=c(8,6,11,12,14,17,18,24,19,23,26,40)
y=c(59,58,56,53,50,45,43,42,39,38,30,27)
plot(x,y)
modelo=lm(y~x)
summary(modelo)
confint(modelo) # intervalos de confianca para o modelo, o alpha e beta estará nesse intervalo
confint(modelo,level=0.99)
predict(modelo,data.frame(x=20))
predict(modelo,data.frame(x=20),int="c")
predict(modelo,data.frame(x=20),int="confidence")
predict(modelo,data.frame(x=20),int="p")

predict(modelo,data.frame(x=20),int="prediction")
novo=data.frame(x=seq(7,40,1))
bandas.prev= predict(modelo, novo, interval="prediction")
bandas.conf= predict(modelo, novo, interval="c")
matplot(novo$x,cbind(bandas.conf, bandas.prev[,-1]),lty=c(1,2,2,3,3), type="l",xlab="x", ylab="y")
points(x,y)
