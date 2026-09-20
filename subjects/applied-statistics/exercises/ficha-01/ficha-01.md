Ficha 01 - Estatística Aplicada
================
Patricia Akemi
2026-09-20

## Exercício 1

**Enunciado:** Um grupo de investigadores recolheu os seguintes dados
para o peso da glândula pituitária numa amostra constituída por quatro
ratinhos: média = 9.0 mg, desvio padrão = 0.6 mg. Pretende-se determinar
um intervalo de confiança a 95% para o peso médio da glândula pituitária
da população de ratinhos. Sob que hipóteses é que este intervalo é
válido?

### Dados do problema

``` r
alpha <- 0.05
n <- 4
mean.peso <- 9.0
sd.peso <- 0.6
```

### Cálculo do intervalo de confiança a 95%

Como a amostra é pequena (n = 4) e o desvio padrão populacional é
desconhecido, usa-se a distribuição t de Student com n − 1 graus de
liberdade:

``` r
IC.low  <- mean.peso - sd.peso / sqrt(n) * qt(1 - alpha / 2, df = n - 1)
IC.high <- mean.peso + sd.peso / sqrt(n) * qt(1 - alpha / 2, df = n - 1)

cat("IC a 95% para o peso médio da glândula pituitária: [", 
    round(IC.low, 3), ",", round(IC.high, 3), "] mg\n")
```

    ## IC a 95% para o peso médio da glândula pituitária: [ 8.045 , 9.955 ] mg

### Conclusão

O intervalo de confiança a 95% para o peso médio da glândula pituitária
é aproximadamente **\[8.05, 9.95\] mg**.

**Hipótese necessária para a validade deste intervalo:** é preciso
assumir que a variável “peso da glândula pituitária” segue uma
**distribuição Normal** na população de ratinhos. Isto é essencial
porque, com n = 4 (amostra muito pequena), não é possível invocar o
Teorema do Limite Central para justificar o uso da distribuição t — a
normalidade dos dados tem de ser uma hipótese assumida à partida (ou
verificada, se houvesse dados brutos disponíveis).
