############### FICHA 05 ----

#### ex 01 ----
precox <- c(100, 90, 80, 70, 70, 70, 70, 65, 60, 60, 55, 60)
vendasy <- c(55, 70, 90, 100, 90, 105, 80, 110, 125, 115, 130, 130)
df = data.frame(preco=precox,vendas=vendasy)
n = length(df$vendas)

x0 = 85

# i. Desenhe o diagrama de dispersão dos dados.
ggplot(df,aes(x=preco,y=vendas))+geom_point(size=4)+theme_bw(base_size=16)

# ii. Determine os coeficientes da recta de regressão usando o MMQ.

# a mao
x_avg = mean(df$preco) # 70.83
y_avg = mean(df$vendas) # 100
ssx = sum((df$preco-x_avg)^2) # 1941.66
ssy = sum((df$vendas-y_avg)^2) # 6300
ssxy = sum((df$preco-x_avg)*(df$vendas-y_avg)) # -3250
beta = ssxy/ssx # -1.67
alpha = y_avg-beta*x_avg # 218.56

# iii. Calcule o coeficiente de correlação linear de Pearson e verifique se é significativamente diferente de zero.
cor.test(df$preco,df$vendas,method='pearson') # t = -7.9529, df = 10, p-value = 1.24e-05

# iv. Crie uma tabela ANOVA e use um teste F para testar a hipótese nula de que β=0.

mod <- lm(vendasy~precox)
summary(mod)
aov(mod)
anova(mod)
ssr = beta*ssxy
sse = ssy-ssr
# sse = sum(mod$residuals^2)
sigma2 = sse/(n-2)
F_ = (ssr/1)/sigma2

qf(0.95,1,n-2)

# v. Construa um intervalo de confiança a 95% para β.
se_beta = sqrt(sigma2/ssx)
t_ = beta/se_beta # -7.95
tc = qt(0.975,n-2)
beta+c(-1,1)*tc*se_beta
confint(mod)

# vi. Construa intervalos de previsão para y e para μy, para x0 dado.
y_hat = predict(mod,data.frame(precox=x0))
y_hat2 = alpha+beta*x0

h = 1/n+(x0-x_avg)^2/ssx

y_hat + c(-1,1)*tc*sqrt(sigma2*h)
y_hat + c(-1,1)*tc*sqrt(sigma2*(1+h))

predict(mod,data.frame(precox=x0),interval='confidence')
predict(mod,data.frame(precox=x0),interval='prediction')


# vii. Calcule o coeficiente de determinação.
ssr = sum((ychapeu-media_y)^2)
r2 = ssr/ssy
cor.test(df$preco,df$vendas)


#### ex 02 ----

altura_pai = c(63, 64, 70, 72, 65, 67, 68, 66)
altura_filho = c(65, 67, 69, 70, 64, 68, 71, 63)
df = data.frame(altura_pai=altura_pai,altura_filho=altura_filho)
n = length(altura_filho)
g = length(colnames(df))

# i diagrama de dispersao
ggplot(df,aes(x=altura_pai,y=altura_filho))+geom_point()+theme_bw()
# ii coeficientes
mod = lm(altura_filho~altura_pai,data=df)
mod$coefficients
# iii coeficiente de corr de pearson e pvalue
cor.test(df$altura_pai,df$altura_filho)$estimate
cor.test(df$altura_pai,df$altura_filho)$p.value
# iv tabela anova e hipotese nula de que beta=0
anova(mod)
qf(0.95,g-1,n-2) # teste f é unilateral
predict(mod,data.frame(altura_pai=71))

# v ic para beta
beta = mod$coefficients[1]
sse = anova(mod)$`Sum Sq`[2]
sigma2 = sse/(n-2)

#### ex 03 ----

temperatura = rep(c(50, 60, 70, 80, 90, 100),2)
calor = c(1.60, 1.63, 1.67, 1.70, 1.71, 1.71, 1.64, 1.65, 1.67, 1.72, 1.72, 1.74)
df = data.frame(temperatura=temperatura,calor=calor)
n = length(temperatura)
x0 = 75 # temperatura

# i grafico dispersao
ggplot(df,aes(x=calor,y=temperatura))+geom_point()

# ii coef da reta de reg
avg_x = mean(df$temperatura)
avg_y = mean(df$calor)
ssxy = sum((df$temperatura-avg_x)*(df$calor-avg_y))
ssxx = sum((df$temperatura-avg_x)^2)
ssyy = sum((df$calor-avg_y)^2)
beta = ssxy/ssxx
alpha = avg_y-beta*avg_x

# iii coeficiente de correlação linear de pearson
cor.test(df$temperatura,df$calor)$estimate
cor.test(df$temperatura,df$calor)$p.value

# iv tabela anova e testar hipotese beta=0
mod = lm(calor~temperatura,data=df)
anova(mod)
qf(0.95,1,10)

# v ic para beta
s2 = ssyy-beta*ssxy
sigma2 = s2/(n-2)
se = sqrt(sigma2/ssxx)
tc = qt(1-0.05/2,n-2)
beta + c(-1,1)*tc*se
confint(mod)

# ip para y
predict(mod,data.frame(temperatura=75),int='prediction')
predict(mod,data.frame(temperatura=75),int='confidence')

# coef determinacao
ssr = (beta^2)*ssxx
sse = ssyy-ssr
(r2 = 1-sse/ssyy)
summary(mod)$r.squared
cor(df$temperatura, df$calor)^2

#### ex 04 ----

peso = c(64.0, 75.3, 73.0, 82.1, 76.2, 95.7, 59.4, 93.4,
         82.1, 78.9, 76.7, 82.1, 83.9, 73.0, 64.4, 77.6)
glucose = c(108, 109, 104, 102, 105, 121, 79, 107,
            101, 85, 99, 100, 108, 104, 102, 87)
df = data.frame(peso = peso, glucose = glucose)
n = length(peso)
avg_x = mean(df$peso)
avg_y = mean(df$glucose)
# x: peso | y: glucose
ssxx = sum((df$peso-avg_x)^2) # 1419.298
ssyy = sum((df$glucose-avg_y)^2) # 1573.438
ssxy = sum((df$peso-avg_x)*(df$glucose-avg_y)) # 723.4875

# i scatter plot
ggplot(df,aes(x=peso,y=glucose))+geom_point()

# ii coef da reta reg
beta = ssxy/ssxx # 0.5097504
alpha = avg_y-beta*avg_x # 61.87693

# iii coef de corr
cor.test(df$peso,df$glucose)$estimate # 0.4841384
cor.test(df$peso,df$glucose)$p.value # 0.05739728

# iv tabela anova e testar beta=0
mod = lm(glucose~peso,data=df)
anova(mod)
mod$coefficients
qf(0.95,1,n-2) # por pouco nao rejeita

# v ic para beta
s2 = ssyy-beta*ssxy
sigma2 = s2/(n-2)
se = sqrt(sigma2/ssxx)
beta+c(-1,-1)*qt(1-0.05/2,n-2)*se
confint(mod)

# vi
predict(mod,data.frame(peso=95),int='c')
predict(mod,data.frame(peso=95),int='p')

#### ex 05 ----

idade = c(20, 43, 63, 26, 53, 31, 58, 46, 58, 70,
          46, 53, 70, 20, 63, 43, 26, 19, 31, 23)
pressao = c(120, 128, 141, 126, 134, 128, 136, 132, 140, 144,
            128, 136, 146, 124, 143, 130, 124, 121, 126, 123)
df = data.frame(idade = idade, pressao = pressao)
# x: idade | y: pressao

# i scatter plot
ggplot(df,aes(x=idade,y=pressao))+geom_point()
mod = lm(pressao~idade,data=df)

# ii determine coef reta de reg
x_avg = mean(df$idade) # 43.1
y_avg = mean(df$pressao) # 131.5
ssxx = sum((df$idade-x_avg)^2) # 5825.8
ssyy = sum((df$pressao-y_avg)^2) # 1235
ssxy = sum((df$idade-x_avg)*(df$pressao-y_avg)) # 2593
beta = ssxy/ssxx # 0.4450891
alpha = y_avg-beta*x_avg # 112.3167
n = length(idade)

# iii coef corr pearson
cor.test(df$idade,df$pressao,method = 'pearson')$estimate # 0.966699
cor.test(df$idade,df$pressao,method = 'pearson')$p.value # 4.238718e-12

# iv tabela anova e testar beta=0
anova(mod)
mod$coefficients # 112.3166604   0.4450891 
qf(0.95,1,n-2)

# v ic para beta
tc = qt(1-0.05/2,n-2)
s2 = ssyy-beta*ssxy
sigma2 = s2/(n-2)
se = sqrt(sigma2/ssxx)
beta+c(-1,1)*tc*se
confint(mod)

# vi
predict(mod,data.frame(idade=25),int='c')
predict(mod,data.frame(idade=25),int='p')

#### ex 06 ----

tempo = seq(24, 60, by = 4)
temperatura = c(38.8, 39.5, 40.3, 40.7, 41.0, 41.1, 41.4, 41.6, 41.8, 41.9)
df = data.frame(tempo = tempo, temperatura = temperatura)
# x: tempo | y: temperatura
mod = lm(temperatura~tempo,data=df)

# i scatter plot
ggplot(df,aes(x=tempo,y=temperatura))+geom_point()

# ii coef reta regression
x_mean = mean(df$tempo)
y_mean = mean(df$temperatura)
ssxx = sum((df$tempo-x_mean)^2) # 1320
ssyy = sum((df$temperatura-y_mean)^2) # 9.289
ssxy = sum((df$tempo-x_mean)*(df$temperatura-y_mean)) # ssxy
beta = ssxy/ssxx # 0.07984848
alpha = y_mean-beta*x_mean # 37.45636
mod$coefficients

# iii 
cor.test(df$temperatura,df$tempo)$estimate
cor.test(df$temperatura,df$tempo)$p.value

# iv tabela anova e testar beta=0
anova(mod)
n=length(tempo)
qf(0.95,1,n-2) # 5.317655

# v ic para beta
s2=ssyy-beta*ssxy
sigma=s2/(n-2)
se = sqrt(sigma2/ssxx)
tc=qt(1-0.05/2,n-2)
beta+c(-1,1)*tc*se
confint(mod)

## exercicio 07

url = 'https://hastie.su.domains/ElemStatLearn/datasets/galaxy.data'
galaxy = read.csv(url)
head(galaxy)
rownames(galaxy) = galaxy$row.names
cor.test(galaxy$velocity,galaxy$north.south)$estimate
cor.test(galaxy$velocity,galaxy$radial.position)$estimate
cor.test(galaxy$velocity,galaxy$east.west)$estimate
#cor.test(galaxy$velocity,galaxy$angle)$p.value

mod = lm(galaxy$velocity~galaxy$north.south)
anova(mod)

ggplot(galaxy,aes(x=north.south,y=velocity))+geom_point()
ggplot(galaxy,aes(x=radial.position,y=velocity))+geom_point()
ggplot(galaxy,aes(x=east.west,y=velocity))+geom_point()
ggplot(galaxy,aes(x=angle,y=velocity))+geom_point()
