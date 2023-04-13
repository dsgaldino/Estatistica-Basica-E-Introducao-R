## AULA 07 - ANÁLISE DE REGRESSÃO
## REGRESSÃO LINEAR SIMPLES E MÚLTIPLA
## DIEGO SOARES GALDINO


## ----------------- Instalando Pacotes ----------------- 

#Pacote para manipulação de dados, como filtragem, seleção, agregação, ordenação e junção de tabelas.
install.packages("dplyr")

#Pacote para análise de desempenho de funções.
install.packages("performance")

#Pacote para visualização de dados e diagnóstico de problemas de performance em funções.
install.packages("see")

#Pacote para transformação de dados, como separação, pivoteamento e junção de colunas.
install.packages("tidyr")

## ----------------- Carregando Pacotes ----------------- 

require(dplyr)
require(see)
library(dplyr)
library(ggplot2)
library(tidyr)

## ----------------- Examinando mtcars dataset ----------------- 

?mtcars

dim(mtcars)
str(mtcars)

# O conjunto de dados tem 32 observações (carros) e 11 variáveis que descrevem características dos carros,
# como consumo de combustível (mpg), número de cilindros (cyl), deslocamento do motor (disp),
# peso (wt), potência do motor (hp) e outras.

# Boxplots das variáveis numéricas
mtcars %>%
  select(-vs, -am, -gear, -carb) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free")

# Pode-se veririfcar que o número de cilindros econsumo de combustível tem uma variação simétrica, enquanto deslocamento
# e potência do motor, bem como qsec possuem dados assimétricos postivos e os demais assimétricos negativos.


# Scatterplots das variáveis numéricas
mtcars %>%
  select(mpg, wt, hp, disp) %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point() +
  labs(x = "Weight (lb/1000)", y = "Miles per gallon")

# Ao verificar o consumo de combustível, percebe-se que o quanto menor o peso do carro, menor é o ocnsumo de combustível, ou seja,
# este veículo andará mais miles do que um carro mais pessoa, considerando a mesma quantidade de combustível.

mtcars %>%
  select(mpg, wt, hp, disp) %>%
  ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  labs(x = "Gross horsepower", y = "Miles per gallon")

# Também pode-se perceber que o consumo de combustível está ligado a potência do motor, quanto maior a potência
# do motor, mais consumirá de combustível.

mtcars %>%
  select(mpg, wt, hp, disp) %>%
  ggplot(aes(x = disp, y = mpg)) +
  geom_point() +
  labs(x = "Displacement (cu.in.)", y = "Miles per gallon")

# Outro fator importante de se levar em consideração é o deslocamento do motor (capacidade total do motor, expressa em polegadas cúbicas (cu.in.) ou em litros (L).
# Quanto maior o deslocamento do motor, mais potência ele é capaz de gerar.) Com isso, percebe-se um comportamenteo semelhante ao peso e a potência do motor no quesito
# consumo de combustível, quando maior o deslocamento menor é a autonomia.

## ----------------- Selecionando as variáveis de interesse ----------------- 

dados <- mtcars %>%
  dplyr::select(mpg, wt, hp, disp, vs)
str(dados)

## ----------------- Scatter Plot de mpg com wt,hp e disp ----------------- 

par(mfrow=c(2,2))
plot(dados$wt, dados$mpg, main = "peso")
plot(dados$hp, dados$mpg, main = "potência")
plot(dados$disp, dados$mpg, main = "deslocamento")
dev.off()

## ----------------- Verificando Correlações ----------------- 

cor(dados)

# Com base na matriz de correlação gerada, temos:
#   
#   1 - mpg tem um forte correlação (negativa) com wt e disp, ou seja, carros mais pesados 
#   e com maior delocamento de motor tendem a ser menos eficiente no consumo de combustível
#   
#   2 - mpg tem uma correlação moderada (negativa) com hp, o que indica que carros com maior
#   potência também tendem a ser menos eficiente no consumo de combustível
#   
#   3 - vs tem um correlação moderada (negativa) com hp e disp, o que indica que motores
#   em linha tendem a ser menores e menos potentes do que motores em v.
  
## ----------------- Ajustando modelos de regressão linear múltipla (diferentes combinações) ----------------- 

# wt, hp e disp
m1 <- stats::lm(mpg ~ wt + hp + disp, data = dados)
summary(m1)

# As variáveis wt e hp têm um efeito significativo em mpg, enquanto a variável disp não
# apresenta um efeito significativo. O R² ajustado do modelo é de 0,8083, o que significa
# que as variáveis juntas explicam cerca de 80,8% da variável resposta mpg.

# wt e hp
m2 <- stats::lm(mpg ~ wt + hp, data = dados)
summary(m2)

# Os coeficientes indicam que para cada aumento de uma unidade em wt e hp, a variável mpg
# diminui em cerca de 3,88 e 0,032 unidades, respectivamente.
# 
# Os valores de p (Pr(>|t|)) indicam que tanto wt quanto hp são estatisticamente significativos
# para explicar a variabilidade em mpg, já que seus valores p são menores do que 0,05.
# 
# O R² ajustado é de 0,815, o que indica que cerca de 81,5% da variabilidade na variável
# mpg é explicada pelas variáveis wt e hp. O F-estatístico, juntamente com o valor de p,
# indica que o modelo geral é estatisticamente significativo.
# 
# A análise sugere que, em média, quanto maior o peso do carro e a potência do motor,
# menor será o consumo de combustível.


# disp e hp
m3 <- stats::lm(mpg ~ disp + hp, data = dados)
summary(m3)

# Os coeficientes para disp e hp são -0,0303 e -0,0248, respectivamente, o que significa que,
# mantendo todas as outras variáveis constantes, um aumento de uma unidade no deslocamento leva a uma
# redução de aproximadamente 0,0303 unidades no consumo de combustível, enquanto um aumento de uma unidade
# em cavalos de potência resulta em uma redução de cerca de 0,0248 unidades no consumo de combustível.
# 
# O p-valor associado ao coeficiente de disp é menor que 0,05, indicando que essa variável é estatisticamente
# significante para explicar a variação no consumo de combustível. Já o valor associado ao coeficiente de hp
# é maior que 0,05, o que indica que essa variável não é estatisticamente significante para explicar a variação
# no consumo de combustível.
# 
# O coeficiente R² é de 0,7482, o que significa que as variáveis explicativas conseguem explicar cerca
# de 74,82% da variação observada no consumo de combustível. O F-statistic tem valor de 43.09 com p-valor
# muito baixo (2.062e-09), indicando que pelo menos uma das variáveis explicativas é estatisticamente 
# significativa para explicar a variação no consumo de combustível.


# wt e disp
m4 <- stats::lm(mpg ~ wt + disp, data = dados)
summary(m4)

# Os resultados indicam que o Intercept tem valor de 34.96055 e é significativo (p-valor < 0.001), assim 
# como a variável wt (p-valor = 0.00743), mas a variável disp não é significativa (p-valor = 0.06362).
# O R² ajustado é de 0.7658, o que indica que o modelo explica cerca de 76,58% da variabilidade nos
# dados. O teste F indica que pelo menos uma das variáveis preditoras é significativa no modelo, com um 
# p-valor muito baixo de 2.744e-10.

# Portanto, com base no R² ajustado, o modelo m2 apresenta o melhor desempenho e deve
# ser selecionado para a análise.

## ----------------- Examinando os resíduos -----------------

par(mfrow=c(2,2))
plot(x = dados$mpg, y = stats::rstandard(m2))
abline(0,0)
plot(x = dados$wt, y = stats::rstandard(m2))
abline(0,0)
plot(x = dados$hp, y = stats::rstandard(m2))
abline(0,0)
dev.off()

# Com base na análise dos gráficos dos resíduos padronizados, pode-se concluir que o modelo de regressão
# linear múltipla escolhido se ajusta bem aos dados. Isso se deve ao fato de que os resíduos não apresentam
# padrões claros, como tendências ou estruturas não aleatórias, e estão distribuídos de forma relativamente
# homogênea em torno da linha central em zero.

plot (m2)

## ----------------- Verificando as suposições  -----------------

require(performance)
require(see)

performance::check_model(m2)
performance::check_model(m1) #Para teste

# Com base na avaliação do desempenho do modelo m2, pode-se concluir que o modelo é válido e confiável,
# pois os gráficos de Posterior Predictive Check, Linearity e Homogeneity of Variance, Influential
# Observations e Collinearity não apresentaram nenhum problema significativo.
# Além disso, embora alguns pontos dos gráficos de Normality of Residuals possam estar fora da linha padrão, 
# isso não é uma preocupação importante. Em geral, a análise indica que o modelo m2 é uma boa escolha para
# a análise dos dados em questão.

## ----------------- New Variable  -----------------

dados <- mtcars %>%
  dplyr::mutate(vs_fator = factor(dplyr::case_when(vs == 1 ~'reto',TRUE ~ 'formato v'),
                                  levels=c('reto','formato v')))
summary(dados)

## ----------------- Ajustando um novo modelo de regressão linear  -----------------

m5 <- lm(mpg ~ wt + vs_fator, data = dados)
summary(m5)

# Os coeficientes do modelo mostram que, mantendo-se todas as outras variáveis constantes, um aumento de uma
# unidade em wt está associado a uma diminuição de cerca de 4.44 unidades em mpg. Já a variável
# "vs_fator" possui duas categorias (formato u e formato v), e o coeficiente negativo de -3.15 para 
# "vs_fatorformato v" indica que a categoria v tem uma média menor de mpg em comparação com a categoria u.
# 
# O R² ajustado sugere que o modelo explica aproximadamente 78.73% da variação na variável de
# resposta, e o teste F indica que o modelo como um todo é significativo. Além disso, os p-values dos 
# coeficientes mostram que tanto wt quanto vs_fator são significativos para explicar a variação em mpg.
# 
# Em resumo, pode-se concluir que o modelo de regressão linear múltipla ajusta bem aos dados e as variáveis
# escolhidas são estatisticamente significativas para explicar a variação em mpg.

m6 <- lm(mpg ~ hp + vs_fator, data = dados)
summary(m6)

# Os resultados indicam que a potência do motor é um preditor estatisticamente significativo do consumo de
# combustível, com um coeficiente negativo indicando que, em geral, quanto maior a potência, menor o consumo
# de combustível. No entanto, o tipo de motor não apresentou uma relação significativa com o consumo de 
# combustível. O R² de 0,6246, indicando que a variação no consumo de combustível pode ser explicada em 
# cerca de 62% pela variação na potência do motor e no tipo de motor. No entanto, a falta de significância
# estatística do coeficiente para o tipo de motor sugere que ele não está fornecendo informações úteis para
# a previsão do consumo de combustível. O teste F para o modelo como um todo é altamente significativo 
# (p < 0,001), indicando que o modelo é útil para prever o consumo de combustível com base na potência do
# motor e no tipo de motor.

## ----------------- Comparando os valores de AIC  -----------------

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m5)
AIC(m6)

# Como o AIC é uma medida que busca selecionar o melhor modelo dentre um conjunto de modelos candidatos 
# com base no seu ajuste aos dados e na sua complexidade. Quanto menor o valor de AIC, melhor é considerado
# o modelo.
# 
# Nesse caso, pode-se concluir que o modelo m2 apresenta o menor valor de AIC (156.6523), o que indica que
# é o modelo que melhor equilibra o ajuste aos dados e a complexidade do modelo. Portanto, entre esses 
# seis modelos, o m2 é o mais adequado para explicar a relação entre as variáveis dependentes e independentes.
