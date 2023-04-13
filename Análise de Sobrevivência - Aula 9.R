## Aula 09 - Aula la Análise de sobrevivência
## Funções básicas de sobrevivência; Teste de log-rank; Modelo de Cox.

## ---------------------- INSTALANDO BIBILOTECAS ----------------------

#Pacote para manipulação de dados, como filtragem, seleção, agregação, ordenação e junção de tabelas.
install.packages("dplyr")

# Pacote para visualização de dados em gráficos.
install.packages("ggplot2")

# Pacote para transformação de dados entre formatos largos e longos.
install.packages("reshape2")

# Pacote para é usado para visualização de dados de sobrevivência e análise de modelos de sobrevivência
install.packages("survminer")

## ---------------------- CARREGANDO BIBLIOTECAS ----------------------

require(dplyr)
require(MASS)
require(stats)
library(ggplot2)
library(reshape2)
require(survival)
library(survminer)

## ---------------------- EXAMINANDO O DATASET ----------------------

?lung
dim(lung)

dados <- lung %>%
  dplyr::select(time, status, age, sex) %>%
  dplyr::mutate(sex_cat = factor(dplyr::case_when(sex == 1 ~ 'H',
                                                  sex == 2 ~ 'M')),
                status_adj =  dplyr::recode(status, '1' = 0,
                                            '2' = 1))

str(dados)

# Com base no dataset, o conjunto de dados "lung" contém informações sobre pacientes com câncer de pulmão 
# avançado, incluindo sexo e idade, bem como informações sobre a sobrevivência dos pacientes. Para melhor
# entendimento processamos esses dados e criamos um novo conjunto de dados mais adequado para análises de 
# sobrevivência, com variáveis selecionadas e transformadas de acordo com suas catacterísticas.

## --------------------- ESTIMAR S(T) VIA KAPLAN-MEIER ----------------------

km <- survival::survfit(formula =Surv(time, status_adj) ~ 1, data = dados)
km

summary(km)

plot(km, xlab = "Tempo (dias)", ylab = "s(t)")
plot(km, xlab = "Tempo (dias)", ylab = "s(t)", conf.int = FALSE)
plot(km, xlab = "Tempo (dias)", ylab = "s(t)", mark.time = TRUE)

# A análise de sobrevivência como é uma técnica estatística usada para estimar a probabilidade de um evento
# ocorrer ao longo do tempo, como a falha de um produto, ou seja, neste caso a morte de um indivíduo. As
# saídas mostram a curva de sobrevivência (representada pelos valores medianos, 0.95LCL e 0.95UCL) e a 
# tabela de resumo, que lista o número de eventos e as estimativas de sobrevivência em cada ponto do tempo. 
# Por exemplo, na tabela é possível identificar que aos 5 anos de acompanhamento, a estimativa de sobrevivência
# é de 99,56%, com um intervalo de confiança de 95% de 98,71% a 100%. Ainda, pode ser observado que 165 eventos
# ocorreram durante o período de acompanhamento de 228 indivíduos.

## --------------------- COMPARAR CURVAS ENTRE H E M ----------------------

table(dados$sex_cat)
prop.table(table(dados$sex_cat))
prop.table(table(dados$sex_cat, dados$status_adj), margin = 2)

# A saída indica que há 138 indivíduos do sexo masculino (H) e 90 indivíduos do sexo feminino (M) na amostra.
# Tendo que 60,53% dos indivíduos são do sexo masculino e 39,47% são do sexo feminino. Outro ponto, é a 
# indicação de que a proporção de indivíduos masculinos e femininos que sobreviveram ou morreram em cada grupo.
# Por exemplo, 41,27% dos indivíduos do sexo masculino vieram a óbito, enquanto no grupo feminino este valor
# é mais da metade dos casos, sendo 58,73%.

km_sex<- survival::survfit(formula =Surv(time, status_adj) ~ sex_cat, data = dados)
summary(km_sex)

plot(km_sex,
     conf.int = FALSE,
     xlab = "Tempo (dias)", ylab = "s(t)",
     lty = c(1,2))
legend(legend = c('H', 'M'),
       lty = c(1,2),
       x = "topright",
       bty = 'n')

# Pode-se verificar que a estimativa de sobrevivência é menor para homens do que para mulheres em quase todos
# os períodos de tempo. Por exemplo, aos 11 dias de seguimento, a sobrevivência estimada para homene mulheres
# é de aproximadamente 98% Com o passaqr do tempo, cria-se uma diferença estatisticamente significativa, aos
# 400 dias aproximadamente a estimativa de sobrevivência de homens é de aproximadamente 30% enquanto que para
# mulheres é em torno de 49%.

log_rank<- survival::survdiff(formula = Surv(time, status_adj) ~ sex_cat, data = dados)
log_rank

# Com base na saída apresentada, podemos concluir que há uma diferença significativa na sobrevivência entre
# homens e mulheres, com um p-valor de 0,001. A estatística qui-quadrado é de 10,3 com 1 grau de liberdade.
# Observando que o grupo de homens teve uma taxa de óbito maior do que a taxa esperada, enquanto o grupo de
# mulheres teve uma taxa de óbito menor do que a taxa esperada. O grupo de homens teve 112 óbitos observados,
# enquanto 91,6 eram esperados, enquanto o grupo de mulheres teve apenas 53 óbitos observados, enquanto 73,4
# eram esperados. Portanto, podemos concluir que, com base nos dados fornecidos, os homens têm uma menor 
# chance de sobrevivência em relação às mulheres.


summary(km, times = 365)

# Com base no tempo (time) e status (status_adj) de um grupo de indivíduos (Homem e Mulher).O número de eventos
# ocorridos (n.event), a taxa de sobrevivência (survival) e os intervalos de confiança (lower 95% CI e upper
# 95% CI) para um período de tempo de 365 dias (1 ano). Pode-se concluir que a taxa de sobrevivência estimada
# para o grupo de indivíduos foi de 0,409 (ou 40,9%) após 1 ano. Além disso, pode-se observar que ocorreram 121
# eventos (mortes) no período analisado e que havia 65 indivíduos em risco no início do estudo.

summary(km_sex, times = 365)

# Com tempo de acompanhamento de 365 dias, o resultado para os homens (sex_cat=H), demonstaram uma probabilidade
# de sobrevivência de 0,3361 com um intervalo de confiança de 95% entre 0,2609 e 0,4329. Para mulheres 
# (sex_cat=M), a probabilidade de sobrevivência após o mesmo período é de 0,5265 com um intervalo de confiança
# de 95% entre 0,4215 e 0,6576. Portanto, podemos concluir que, no período de 365 dias, a probabilidade de 
# sobrevivência de mulheres é maior em comparação com homens.

## --------------------- MODELO DE COX ----------------------

m_cox <- survival::coxph(formula = Surv(time, status_adj) ~ sex_cat + age, data = dados)
m_cox

# A partir dos coeficientes de regressão, podemos concluir que a razão de chances (odds ratio) para a variável
# sexo é 0,59 (exp(coef) da categoria M), o que indica que as mulheres têm uma chance 41% menor de morrer em
# relação aos homens, considerando a mesma idade. O valor de p associado a essa variável é 0,00218, indicando
# que a diferença entre homens e mulheres é estatisticamente significativa. Já para a variável idade, o 
# coeficiente de regressão é 0,017, o que indica que, mantendo o sexo constante, a razão de chances de morte
# aumenta em 1,7% para cada aumento de 1 ano na idade. O valor de p associado a essa variável é 0,06459, 
# indicando que a relação entre idade e mortalidade não é estatisticamente significativa ao nível de 5% de
# significância. O teste de razão de verossimilhança indica que o modelo ajustado é estatisticamente 
# significativo como um todo, com p-valor de 0,0008574. O modelo é composto por 228 indivíduos, dos quais 165
# tiveram o evento de interesse (morte).

summary(m_cox)

# Avaliação dajuste
# 1. Suposição de riscos proporcionais

survival::cox.zph(m_cox)
survminer::ggcoxzph(survival::cox.zph(m_cox))

survminer::ggcoxdiagnostics(m_cox, type = "deviance")

# As informações apresenta os resultados do teste, com os valores do qui-quadrado (chisq), graus de liberdade
# (df) e p-valor (p) para cada uma das variáveis incluídas no modelo (sexo e idade), bem como para o modelo 
# global. Nenhum dos resultados sugere uma violação significativa da suposição de proporcionalidade dos riscos.
# O gráfico de resíduos gerado para avaliar a validade das suposições do modelo Cox-VHH, mostra uma curva suave
# para cada uma das variáveis incluídas no modelo (sexo e idade), com uma linha horizontal no valor 0 para
# indicar o valor esperado dos resíduos sob a hipótese de proporcionalidade dos riscos. Sugerindo assim, que as
# suposições do modelo são razoavelmente válidas para ambas as variáveis.

