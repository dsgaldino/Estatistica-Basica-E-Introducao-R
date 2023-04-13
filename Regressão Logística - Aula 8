## AULA 08 - ANÁLISE DE REGRESSÃO
## REGRESSÃO LOGÍSTICA

## ----------------- Instalando Pacotes ----------------- 

#Pacote para manipulação de dados, como filtragem, seleção, agregação, ordenação e junção de tabelas.
install.packages("dplyr")

# Pacote para visualização de dados em gráficos.
install.packages("ggplot2")

# Pacote para transformação de dados entre formatos largos e longos.
install.packages("reshape2")

# Pacote para manipulação de dados em formato longo
install.packages("tidyr")

# Pacote que contém funções para análise de regressão, incluindo diagnóstico de modelos.
install.packages("car")

# Pacote para visualização e exploração de dados multivariados
install.packages("GGally")

# Pacote para análise de modelos lineares generalizados, incluindo funções para cálculo de estatísticas de ajuste.
install.packages("glmtoolbox")

## ----------------- Carregando Pacotes ----------------- 

require(dplyr)
require(MASS)
require(stats)
library(ggplot2)
library(reshape2)
library(tidyr)
library(car)
require(glmtoolbox)

## ----------------- Examinando birthwt dataset ----------------- 

?birthwt

dim(birthwt)
str(birthwt)

dados <- birthwt %>%
  dplyr::select(low, age, lwt, smoke)

str(dados)

# Pode-se verificar que as categorias low e smoke são categoricas

dados %>% group_by(low) %>% tally()

# Então temos uma indicação de que 130 crianças não estavam abaixo do peso (1), enquanto 59 crianças estavam (0).

dados %>% group_by(smoke) %>% tally()

# Então temos uma indicação de que 115 crianças a mãe não e fumou (1), enquanto 74 crianças a mãe fumava (0).

dados <- birthwt %>%
  dplyr::mutate(peso_mae_kg = lwt*0.453,
                low_cat = factor(low),
                smoke_cat = factor(smoke)) %>%
  dplyr::select(low_cat, age, peso_mae_kg, smoke_cat) 
str(dados)
summary(dados)

## ----------------- Analisando as variáveis de interesse ----------------- 

#Verificando a os casos relacionado ao baixo peso ao nascer
dados %>%
  ggplot(aes(x = low_cat)) +
  geom_bar() +
  labs(x = "Peso ao nascer", y = "Casos") +
  scale_x_discrete(labels = c("Normal", "Abaixo"))

# A partir do gráfico, pode-se dizer é que a maioria das crianças no conjunto de dados não nasceu abaixo
# do peso, já que há 130 casos classificados como Normal (0) e apenas 59 casos classificados como Abaixo (1).

#Verificando a distribuição das idades das mães
print(dados %>% group_by(age) %>% summarize(n = n()), n = 24)

dados %>%
  group_by(age) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = age, y = n, fill = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Idade", y = "Casos", fill = "Nascimentos") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = seq(14, 40, by = 2))

# Com base nos dados apresentados, podemos concluir que a maioria dos nascimentos ocorrem entre as idades de 19 e 25 anos,
# com o maior número de nascimentos ocorrendo aos 20 anos. Também podemos observar que o número de nascimentos
# diminui à medida que a idade da mãe aumenta, com apenas um nascimento registrado para mães com 45 anos.
# não é uma distruibuição normal

#Verificando a distribuição do peso da mãe
ggplot(dados, aes(x = peso_mae_kg)) +
  geom_density(alpha = 0.5) +
  labs(x = "Peso da mãe (kg)", y = "Densidade")

# Com base nos dados apresentados, podemos concluir que a maioria das mães tem um peso entre 45 e 64 kg,
# como é possível observar na densidade apresentada no gráfico. Além disso, podemos notar que a densidade
# começa a diminuir consideravelmente após aproximadamente 80 kg, indicando que há poucas mães com pesos
# mais elevados.


#Verificando a os casos relacionado a mãe ser fumante ou não
dados %>%
  ggplot(aes(x = smoke_cat)) +
  geom_bar() +
  labs(x = "Fumante", y = "Casos") +
  scale_x_discrete(labels = c("Não", "Sim"))

# A partir do gráfico, pode-se dizer é que a maioria das crianças no conjunto de dados não nasceram de mães
# fumantes, já que há 115 casos as mães são classificadas como não-fumantes e 74 casos estas sãoclassificadas
# como fumantes.

## ----------------- Verificando Correlações ----------------- 

# Veririfcando a correlação do peso com a idade da mãe
dados$low_cat <- factor(dados$low_cat, levels = c(0, 1), labels = c("Normal", "Abaixo"))
boxplot(dados$age ~ dados$low_cat, 
        xlab = "Baixo Peso", 
        ylab = "Idade das Mães")

# Confirmando que a distruição age não é simétrica
shapiro.test(dados$age)

# Com base no resultado do teste de Shapiro-Wilk, com um p-valor muito baixo de 3.189e-05, podemos concluir
# que não há evidências para assumir que a variável age segue uma distribuição normal. Ou seja, a distribuição
# dos dados não é simétrica em torno da média.

ggplot(dados, aes(x = age, y = peso_mae_kg, color = low_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Idade da mãe", y = "Peso pré-gestacional (kg)", color = "Baixo peso") +
  scale_color_manual(values = c("black", "red"))

wilcox.test(dados$age ~ dados$low_cat)

# No teste de Wilcoxon, a hipótese nula é que não há diferença significativa entre as medianas das amostras
# Como o p-valor (0.2471) é maior que o nível de significância de 0.05, não há evidência suficiente para 
# rejeitar a hipótese nula e, portanto, podemos considerar que não há diferença significativa entre as
# medianas das amostras de acordo com a variável low_cat.

# Veririfcando a correlação do peso com a peso da mãe
boxplot(dados$peso_mae_kg ~ dados$low_cat)
hist(dados$peso_mae_kg)
table(dados$low_cat)

shapiro.test(dados$peso_mae_kg)

# Resultado indica que a hipótese nula de que a distribuição é normal pode ser rejeitada com um p-valor 
# muito baixo (p-value = 2.242e-10). Isso significa que a distribuição dessa variável não segue uma 
# distribuição normal.

stats::wilcox.test(dados$peso_mae_kg ~ dados$low_cat)

# O teste não paramétrico comparou as medianas de duas amostras independentes, peso da mãe entre o grupo de 
# baixo peso (low_cat=0) e o grupo normal (low_cat=1). O resultado do teste mostra um valor p igual a 0,01278,
# o que sugere que há evidência estatística suficiente para rejeitar a hipótese nula de que as medianas dos
# dois grupos são iguais. Ou seja, há indícios de que há uma diferença significativa entre o peso da mãe nos
# dois grupos e que isso deve ter influência no baixo peso.

# Veririfcando a correlação do peso com a mãe fumante e não fumante
tab <- table(dados$low_cat, dados$smoke_cat)
prop.table(table(dados$smoke_cat, dados$low_cat), margin=1)*100

chisq.test(tab)

# O resultado indica que o valor do qui-quadrado calculado foi de 4.2359, com 1 grau de liberdade e um
# p-valor de 0.03958. Como o p-valor é menor do que 0.05, rejeitamos a hipótese nula de que as duas variáveis
# são independentes e concluímos que há evidência suficiente para afirmar que há uma associação entre as 
# variáveis baixo peso e fumantes.

## ----------------- ABORDAGEM HOSMER E LEMESHOW - PASSO 1 ----------------- 
m_passo1_1 <- stats::glm(low_cat ~ age, data = dados, family = "binomial")
summary(m_passo1_1)

m_passo1_2 <- stats::glm(low_cat ~ peso_mae_kg, data = dados, family = "binomial")
summary(m_passo1_2)

m_passo1_3 <- stats::glm(low_cat ~ smoke_cat, data = dados, family = "binomial")
summary(m_passo1_3)

summary(m_passo1_1)$coefficients["age", "Pr(>|z|)"]
summary(m_passo1_2)$coefficients["peso_mae_kg", "Pr(>|z|)"]
summary(m_passo1_3)$coefficients["smoke_cat1", "Pr(>|z|)"]

## ----------------- ABORDAGEM HOSMER E LEMESHOW - PASSO 2 -----------------
# selecionar todos os x com p < 0.25

# Todas as variaveis no passo anterior possuem p < 0.25
# age  = 0.104548
# peso_mae_kg = 0.02268856
# smoke_cat1 = 0.02761962

m_passo2 <- stats::glm(low_cat ~ age + peso_mae_kg + smoke_cat, 
                       data = dados, family = "binomial")
summary(m_passo2)

## ----------------- ABORDAGEM HOSMER E LEMESHOW - PASSO 3 -----------------
# Excluir todos os x com p > 0.05

# Após o passo anterior, verificou-se que a variável age possui um valor de p > 0.05, 
# com isso sendo desconsiderada.
# age  = 0.2334
# peso_mae_kg = 0.0479
# smoke_cat1 = 0.0396

m_passo3 <- stats::glm(low_cat ~ peso_mae_kg + smoke_cat, 
                       data = dados, family = "binomial")
summary(m_passo3)

## ----------------- ADQUAÇÃO DO MODELO -----------------
## 1. Teste de H&L - H0: modelo está bem ajustado

glmtoolbox::hltest(m_passo3)

## 2. Avaliação dos Resíduos

# Residuos de Pearson
residuos_pearson <- data.frame(
  id = 1:nrow(dados),
  res_pearson = residuals(m_passo3, type="pearson"))
head(residuos_pearson)

par(mfrow = c(2, 1)) 
plot(x=sample(residuos_pearson$id), y=residuos_pearson$res_pearson, main="Resíduos de Pearson")
abline(0, 0)
# Não há um padrão,que nos faz acreditar que o modelo esteja mal ajustado.

pchisq(sum(residuos_pearson$res_pearson^2), df=m_passo3$df.residual)

# O valor de p é 0.54, o que indica que não há evidência suficiente para rejeitar a hipótese nula
# de que os resíduos de Pearson ao quadrado seguem uma distribuição qui-quadrado com o número adequado
# de graus de liberdade. Portanto, podemos concluir que o modelo parece adequado para os dados, ou seja,
# não há evidência de falta de ajuste significativa.

## 3. Residuos deviance
residuos_deviance <- data.frame(
  id = 1:nrow(dados),
  res_deviance = residuals(m_passo3, type="deviance"))
head(residuos_deviance)
# Não há um padrão,que nos faz acreditar que o modelo esteja mal ajustado.

plot(x=sample(residuos_deviance$id), 
     y=residuos_deviance$res_deviance)
abline(0, 0)
dev.off()

pchisq(sum(residuos_deviance$res_deviance^2), df=m_passo3$df.residual)  
anova(m_passo3, test="Chisq")

# O valor p para o teste qui-quadrado é 0.971, o que indica que não há evidências significativas de
# falta de ajuste do modelo binomial. O valor p para o teste de ANOVA para as variáveis explicativas
# peso_mae_k e smoke_cat são, respectivamente, 0.01446 e 0.03701, o que indica que ambas as variáveis
# estão significativamente associadas à resposta low_cat quando consideradas em um modelo linear generalizado.

## ----------------- INTERPRETAÇÃO DO MODELO -----------------

summary(m_passo3)
exp(coef(m_passo3))

# A unidade de aumento no peso da mãe está associada a uma redução de 0,03% na chance de ter um baixo peso
# ao nascer. Já para a variável smoke_cat, estar exposto ao fumo está associado a um aumento de 0,97% na
# chance de ter um baixo peso ao nascer.


## ----------------- TESTANDO O MODELO -----------------

# Sequência de valores de peso da mãe
peso_mae_seq <- seq(from = 30, to = 120, by = 10)

# Todas as combinações de peso e status de fumante
newdata <- expand.grid(peso_mae_kg = peso_mae_seq, smoke_cat = c("0", "1"))

# As probabilidades para cada combinação de peso e status de fumante
newdata$prob <- predict.glm(m_passo3, newdata = newdata, type = "response")

library(ggplot2)

ggplot(newdata, aes(x = peso_mae_kg, y = prob, group = smoke_cat, color = smoke_cat)) +
  geom_line() +
  scale_x_continuous("Peso da mãe (kg)", breaks = seq(30, 120, by = 10)) +
  scale_y_continuous("Probabilidade de baixo peso", limits = c(0, 0.6)) +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Não-Fumante", "Fumante")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))


