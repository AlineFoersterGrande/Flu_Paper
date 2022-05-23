##############################################################################################################################
############################ GRANGER CAUSALITY ##############################################################################
#########################################################################################################################

# Data 
library(readxl)
matriz_ncont_analise <- read_excel("matriz_ncont_analise.xlsx")
matriz_ncont_analise=as.matrix(matriz_ncont_analise)

matriz_ncont_2019 <- read_excel("matriz_ncont_2019.xlsx")
matriz_ncont_2019=as.matrix(matriz_ncont_2019)

matriz_ncont_2019_genetica <- read_excel("matriz_ncont_2019_genetica.xlsx")
matriz_ncont_2019_genetica=as.matrix(t(matriz_ncont_2019_genetica))

matriz_ncont_analise_genetica <- read_excel("matriz_ncont_analise_genetica.xlsx")
matriz_ncont_analise_genetica=as.matrix(matriz_ncont_analise_genetica)
###################

# Analysis
library(vars)
library(aod)

# Assembling matrices: (incidence and genetics)
matriz_ncont_granger_2008_2019=rbind(matriz_ncont_analise,matriz_ncont_2019)
rownames(matriz_ncont_granger_2008_2019)<-c(1:134) # data from October 2008 to November 2019

matriz_ncont_granger_genetica_2008_2019=rbind(matriz_ncont_analise_genetica,t(matriz_ncont_2019_genetica[,1:8]))
rownames(matriz_ncont_granger_genetica_2008_2019)<-c(1:131) # data from October 2008 to August 2019

# Granger Causality: Let's see if the incidences of the 6 different regions Granger-cause the incidence of Brazil

# North America Region and Brazil (Incidence)
a=matriz_ncont_granger_2008_2019[,"Brasil"]
b=matriz_ncont_granger_2008_2019[,"RegiaoAmericaNorte"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

#(1) Supondo que as séries são estacionárias - Teste de Phillips nos informa
# Observação: Na etapa 1 se ambas séries forem estacionárias, então automaticamente ambas tem I(0), ou seja, m=0 na etapa 2. Fazer etapa 2 com m=0 e a mesma etapa 3. Já a etapa 4 não precisa fazer, pois m=0.
PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram
#po.test(testeab, demean = FALSE) # Rejeitar H0=> Cointegram. Teste de Cointegração de Phillips-Ouliaris
modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0
# Nesse caso como temos I(0), então "modeloVAR=modeloVAR_aumentado
# Por fim, a etapa 5 fazemos:
causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

#(2) Supondo que as séries não são estacionárias- Análise gráfica nos informa
# Etapa 1
PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

# Etapa 2: Cointegração (Séries não estacionárias)
# (i) Se as duas tiveram a mesma ordem de integração(ex: ambas forem I(1));
# (ii) Se o resíduo da regressão de uma série pela outra for estacionária
PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

# Etapa 3: Definir modelo VAR básico
# Existem os modelos de Var(1), Var(2),..., Var(p), onde o "p" ideal é o menor que não rejeita o teste de Serial, digamos "pp"
modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

# Etapa 4: Definir modelo VAR aumentado
modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)
# Acima definimos "pp" como sendo o p ideal(número de atrasos) achado na etapa anterior
# e definimos "m" como o máximo das cointegrações encontrado na etapa 2

# Etapa 5: Teste de Wald- B granger causa A
pp=2 #Definir o número de "pp's"
termos=2*(1:pp) # Condição para que apenas coeficientes pares devem ser testados em conjunto, usando um de Wald abaixo
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos) # Definimos os coeficientes que queremos do modelo VAR aumentado(Nesse caso queremos estimar resultados para a equação "a" pois "varresult[[1]]", e queremos apenas os coeficientes pares da equação "a = a.l1 + b.l1 + a.l2 + b.l2 + a.l3 + b.l3 + const" ou seja, queremos usar o teste de Wald testar em conjunto os coeficientes pares "b.l1", "b.l2" e "b.l3"). # Rejeitamos H0=> Existe Granger Causalidade
# P(> X2) = 0.49
#################################

# South America Region and Brazil (Incidence)
a=matriz_ncont_granger_2008_2019[,"Brasil"]
b=matriz_ncont_granger_2008_2019[,"RegiaoAmericaSul"]
testeab=cbind(a,b)

PP.test(a)
PP.test(b)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos)

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso rejeitamos H0 => a causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.0062
#################################

# Central America Region and Brazil (Incidence)
a=matriz_ncont_granger_2008_2019[,"Brasil"]
b=matriz_ncont_granger_2008_2019[,"RegiaoAmericaCentral"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.94
#################################

# Europe Region and Brazil (Incidence)
a=matriz_ncont_granger_2008_2019[,"Brasil"]
b=matriz_ncont_granger_2008_2019[,"RegiaoEuropeia"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.017
#################################

# South Asia Region and Brazil (Incidence)
a=matriz_ncont_granger_2008_2019[,"Brasil"]
b=matriz_ncont_granger_2008_2019[,"RegiaoSouthAsia"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.34
#################################

# Western Pacific Region and Brazil (Incidence)
a=matriz_ncont_granger_2008_2019[,"Brasil"]
b=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.83
######################################################

# Observação: Segundo estudos a gripe migra da Europa para o Brasil (o que indica que a Europa Granger causa o Brasil, isso foi confirmado acima), mas a gripe antes de estar na Europa ela está na região do Pacífico Ocidental, ou seja, a gripe migra do Pacífico ocidental para a Europa (o que indica que o Pacífico ocidental Granger causa a Europa, isso foi confirmado abaixo).

# Western Pacific and Europe Region (Incidence)
a=matriz_ncont_granger_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.0035
#################################

# Observação: Segundo estudos a gripe migra da Região do Pacífico Ocidental para a América do Norte (o que indica que o Pacífico Ocidental Granger causa a América do Norte, isso foi confirmado abaixo).

# Western Pacific and North America Region (Incidence)
a=matriz_ncont_granger_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.026
#################################

# North America Western Pacific Region (Incidence)
a=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_2008_2019[,"RegiaoAmericaNorte"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.029
#################################
# Central America and North America Region (Incidence)
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.061
###############

# Central America and South America Region (Incidence)
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=4) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=4) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso rejeitamos H0 => a causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=4) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 5) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=4
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.06
###############
# Central America and Europe Region (Incidence)
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.0056
###############
# Central America and Western Pacific Region (Incidence)
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.000073
##########################################################################

# Granger causality: genetic data

# AllH1 and Brazil
a=matriz_ncont_granger_genetica_2008_2019[,"Brasil"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

#(1) Supondo que as séries são estacionárias- Teste de Phillips nos informa
# Observação: Na etapa 1 se ambas séries forem estacionárias, então automaticamente ambas tem I(0), ou seja, m=0 na etapa 2. Fazer etapa 2 com m=0 e a mesma etapa 3. Já a etapa 4 não precisa fazer, pois m=0.
PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram
#po.test(testeab, demean = FALSE) # Rejeitar H0=> Cointegram. Teste de Cointegração de Phillips-Ouliaris
modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0
# Nesse caso como temos I(0), então "modeloVAR=modeloVAR_aumentado
# Por fim, a etapa 5 fazemos:
causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

#(2) Supondo que as séries não são estacionárias- Análise gráfica nos informa
# Etapa 1
PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

# Etapa 2: Cointegração (Séries não estacionárias)
# (i) Se as duas tiveram a mesma ordem de integração(ex: ambas forem I(1));
# (ii) Se o resíduo da regressão de uma série pela outra for estacionária
PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

# Etapa 3: Definir modelo VAR básico
# Existem os modelos de Var(1), Var(2),..., Var(p), onde o "p" ideal é o menor que não rejeita o teste de Serial, digamos "pp"
modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

# Etapa 4: Definir modelo VAR aumentado
modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)
# Acima definimos "pp" como sendo o p ideal(número de atrasos) achado na etapa anterior
# e definimos "m" como o máximo das cointegrações encontrado na etapa 2

# Etapa 5: Teste de Wald- B granger causa A
pp=1 #Definir o número de "pp's"
termos=2*(1:pp) # Condição para que apenas coeficientes pares devem ser testados em conjunto, usando um de Wald abaixo
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos) # Definimos os coeficientes que queremos do modelo VAR aumentado(Nesse caso queremos estimar resultados para a equação "a" pois "varresult[[1]]", e queremos apenas os coeficientes pares da equação "a = a.l1 + b.l1 + a.l2 + b.l2 + a.l3 + b.l3 + const" ou seja, queremos usar o teste de Wald testar em conjunto os coeficientes pares "b.l1", "b.l2" e "b.l3"). # Rejeitamos H0=> Existe Granger Causalidade
# P(> X2) = 0.52
#################################

# AllH3 and Brazil
a=matriz_ncont_granger_genetica_2008_2019[,"Brasil"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.12
#################################

# NorthAmericaH1 and Brazil
a=matriz_ncont_granger_genetica_2008_2019[,"Brasil"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.59
#################################

# NorthAmericaH3 and Brazil
a=matriz_ncont_granger_genetica_2008_2019[,"Brasil"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.23
#################################

# AsiaH1 and Brazil
a=matriz_ncont_granger_genetica_2008_2019[,"Brasil"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.55
#################################

# AsiaH3 and Brazil
a=matriz_ncont_granger_genetica_2008_2019[,"Brasil"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.36
#################################

# AllH1 and South Asia Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.065
#####################################################################

# NorthAmericaH1 and South Asia Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.0068
######################################################

# AsiaH3 and South Asia Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.072
######################################################

# NorthAmericaH3 and Central America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.05
######################################################

# AsiaH1 and Central America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=5) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=5) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=5) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 6) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=5
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.000054
################################################################################################

# 2) Genetic Granger cause Genetic:

# NorthAmericaH1 and AsiaH1
a=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.00087
###########################################################################################

# 2) The other Granger causality tests:

# Western Pacific Region and South America Region (Incidence)
a=matriz_ncont_granger_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=5) # Número de atrasos=5, pois não rejeitamos H0 no teste de Serial quando pp=5
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=5) # Lembrando que p=pp+m, logo p=5+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=5) # Número de atrasos=5. Vemos que com pp=5 não rejeitou-se H0, logo vamos utilizar pp=5 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 6) # p= pp+m= 5+1
summary(modeloVAR_aumentado)

pp=5
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.52
#################################

# Western Pacific Region and South Asia Region 
a=matriz_ncont_granger_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.0076
#################################

# Western Pacific Region and Central America Region
a=matriz_ncont_granger_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_2008_2019[,"RegiaoWesternPacific"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.35
#################################

# 3) 

# Central America Region and South Asia Region 
a=matriz_ncont_granger_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_2008_2019[,"RegiaoAmericaCentral"]
testeab=cbind(a,b) # Combinação das séries "a" e "b"

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso rejeitamos H0 => b causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.68
#################################

# 5) 

# NothAmericaH1 and North America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.96
#################################

# NothAmericaH1 and South America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.24
#################################

# NothAmericaH1 and Central America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.63
#################################

# NothAmericaH1 and Europe Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.55
#################################

# NothAmericaH1 and Western Pacific Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.57
#################################

# NothAmericaH3 and North America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.55
#################################

# NothAmericaH3 and South America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.82
#################################

# NothAmericaH3 and Europe Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.47
#################################

# NothAmericaH3 and South Asia Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.84
#################################

# NothAmericaH3 and Western Pacific Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.87
#################################

# AsiaH1 and North America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.97
#################################

# AsiaH1 and Europe Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.86
#################################

# AsiaH1 and South Asia Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.28
#################################

# AsiaH1 and Western Pacific Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.94
#################################

# AsiaH3 and North America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.26
#################################

# AsiaH3 and South America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.41
#################################

# AsiaH3 and Central America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.99
#################################

# AsiaH3 and Europe Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=3) # Número de atrasos=3, pois não rejeitamos H0 no teste de Serial quando pp=3
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=3) # Lembrando que p=pp+m, logo p=3+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=3) # Número de atrasos=3. Vemos que com pp=3 não rejeitou-se H0, logo vamos utilizar pp=3 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 4) # p= pp+m= 3+1
summary(modeloVAR_aumentado)

pp=3
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.29
#################################

# AsiaH3 and Western Pacific Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.15
#################################

# AllH1 and North America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.99
#################################

# AllH1 and South America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.31
#################################

# AllH1 and Central America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.37
#################################

# AllH1 and Europe Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.55
#################################

# AllH1 and Western Pacific Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.67
#################################

# AllH3 and North America Region
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaNorte"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.69
#################################

# AllH3 and South America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaSul"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.51
#################################

# AllH3 and Central America Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoAmericaCentral"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.83
#################################

# AllH3 and Europe Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoEuropeia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=11) # Número de atrasos=11, pois não rejeitamos H0 no teste de Serial quando pp=11
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=11) # Lembrando que p=pp+m, logo p=11+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=11) # Número de atrasos=11. Vemos que com pp=11 não rejeitou-se H0, logo vamos utilizar pp=11 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 12) # p= pp+m= 11+1
summary(modeloVAR_aumentado)

pp=11
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.57
#################################

# AllH3 and South Asia Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoSouthAsia"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.41
#################################

# AllH3 and Western Pacific Region 
a=matriz_ncont_granger_genetica_2008_2019[,"RegiaoWesternPacific"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 2+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.91
#################################

# 6)

# NorthAmericaH3 and AsiaH3 
a=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
b=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.37
#################################

# AsiaH1 and NothAmericaH1 
a=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.71
#################################

# AsiaH3 and NothAmericaH3 
a=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
b=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.71
#################################

# AllH1 and NothAmericaH1 
a=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH1"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.00000018
#################################

# AllH1 and AsiaH1 
a=matriz_ncont_granger_genetica_2008_2019[,"AsiaH1"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH1"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.00012
#################################

# AllH3 and NothAmericaH3 
a=matriz_ncont_granger_genetica_2008_2019[,"NorthAmericaH3"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2, pois não rejeitamos H0 no teste de Serial quando pp=2
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=2) # Lembrando que p=pp+m, logo p=2+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=2) # Número de atrasos=2. Vemos que com pp=2 não rejeitou-se H0, logo vamos utilizar pp=2 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 3) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=2
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.7
#################################

# AllH3 and AsiaH3
a=matriz_ncont_granger_genetica_2008_2019[,"AsiaH3"]
b=matriz_ncont_granger_genetica_2008_2019[,"AllH3"]
testeab=cbind(a,b)

PP.test(a) # Rejeita-se H0=> Série Estacionária=> I(0), ou seja m=0
PP.test(b) # Rejeita-se H0=> Série Estacionária=> I(0)
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1, pois não rejeitamos H0 no teste de Serial quando pp=1
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0
modeloVAR_aumentado=VAR(testeab,p=1) # Lembrando que p=pp+m, logo p=1+0

causality(modeloVAR_aumentado, cause= "a")$Granger # H0: a não causa b. Nesse caso não rejeitamos H0 => a não causa b
causality(modeloVAR_aumentado, cause= "b")$Granger # H0: b não causa a. Nesse caso não rejeitamos H0 => b não causa a

PP.test(a) # Rejeita-se H0=> Série Estacionária
PP.test(b) # Rejeita-se H0=> Série Estacionária
plot.ts(a) # Pela análise gráfica parece que as séries não são estacionárias
plot.ts(b)

PP.test(diff(a,differences = 1)) # Rejeita H0=> I(1), ou seja, m=1
PP.test(diff(b,differences = 1)) # Rejeita H0=> I(1). Além disso vemos que as séries "a" e "b" tem a mesma ordem
regressao = lm(a ~ b)
residuos = regressao$residuals
PP.test(residuos) # Rejeitar H0=> Cointegram pois rejeitamos H0 e as séries "a" e "b" tem a mesma ordem

modeloVAR=VAR(testeab, p=1) # Número de atrasos=1. Vemos que com pp=1 não rejeitou-se H0, logo vamos utilizar pp=1 atrasos
serial.test(modeloVAR) # H0: Correlação dos resíduos até lag determinado=0

modeloVAR_aumentado=VAR(testeab,p= 2) # p= pp+m= 1+1
summary(modeloVAR_aumentado)

pp=1
termos=2*(1:pp)
wald.test(b=coef(modeloVAR_aumentado$varresult[[1]]), Sigma=vcov(modeloVAR_aumentado$varresult[[1]]),Terms=termos)
# P(> X2) = 0.16

################################################################################
################################################################################

# Final Results 

# 1) Incidence Granger-cause Brazil (6 tests)

# North America Region and Brazil (Incidence) -> p-value=0.49
# South America Region and Brazil (Incidence) -> p-value=0.0062
# Central America Region and Brazil (Incidence) -> p-value=0.94
# Europe Region and Brazil (Incidence) -> p-value=0.017
# South Asia Region and Brazil (Incidence) -> p-value=0.34
# Western Pacific Region and Brazil (Incidence) -> p-value=0.83


# 2) Western Pacific Region Granger-cause other regions (incidence) (5 tests)

# Western Pacific Region and North America Region -> p-value=0.026
# Western Pacific Region and South America Region -> p-value=0.52
# Western Pacific Region and Europe Region -> p-value=0.0035
# Western Pacific Region and South Asia Region -> p-value=0.0076
# Western Pacific Region and Central America Region -> p-value=0.35


# 3) Central America Region Granger-cause other regions (incidence) (5 tests)

# Central America Region and North America Region -> p-value=0.061
# Central America Region and South America Region -> p-value=0.06
# Central America Region and Europe Region -> p-value=0.0056
# Central America Region and South Asia Region -> p-value=0.68
# Central America Region and Western Pacific Region -> p-value=0.000073


# 4) Genetic diversities Granger-cause Brazil (incidence) (6 tests)

# AllH1 and Brazil (Incidence) -> p-value=0.52
# AllH3 and Brazil (Incidence) -> p-value=0.12
# NorthAmericaH1 and Brazil (Incidence) -> p-value=0.59
# NorthAmericaH3 and Brazil (Incidence) -> p-value=0.23
# AsiaH1 and Brazil (Incidence) -> p-value=0.55
# AsiaH3 and Brazil (Incidence) -> p-value=0.36


# 5) Six genetic diversities Granger-cause other six regions (incidence) (36 tests) - but one test did not worked, so 35 tests

# NorthAmericaH1 and North America Region -> p-value=0.96
# NorthAmericaH1 and South America Region -> p-value=0.24
# NorthAmericaH1 and Central America Region -> p-value=0.63
# NorthAmericaH1 and Europe Region -> p-value=0.55
# NorthAmericaH1 and South Asia Region -> p-value=0.0068
# NorthAmericaH1 and Western Pacific Region -> p-value=0.57

# NorthAmericaH3 and North America Region -> p-value=0.55
# NorthAmericaH3 and South America Region -> p-value=0.82
# NorthAmericaH3 and Central America Region -> p-value=0.05
# NorthAmericaH3 and Europe Region -> p-value=0.47
# NorthAmericaH3 and South Asia Region -> p-value=0.84
# NorthAmericaH3 and Western Pacific Region -> p-value=0.87

# AsiaH1 and North America Region -> p-value=0.97
# AsiaH1 and South America Region -> PROBLEMAS, NÃO DÁ PARA FAZER
# AsiaH1 and Central America Region -> p-value=0.000053
# AsiaH1 and Europe Region -> p-value=0.86
# AsiaH1 and South Asia Region -> p-value=0.28
# AsiaH1 and Western Pacific Region -> p-value=0.94

# AsiaH3 and North America Region -> p-value=0.26
# AsiaH3 and South America Region -> p-value=0.41
# AsiaH3 and Central America Region -> p-value=0.99
# AsiaH3 and Europe Region -> p-value=0.29
# AsiaH3 and South Asia Region -> p-value=0.072
# AsiaH3 and Western Pacific Region -> p-value=0.15

# AllH1 and North America Region -> p-value=0.99
# AllH1 and South America Region -> p-value=0.31
# AllH1 and Central America Region -> p-value=0.37
# AllH1 and Europe Region -> p-value=0.55
# AllH1 and South Asia Region -> p-value=0.065
# AllH1 and Western Pacific Region -> p-value=0.67

# AllH3 and North America Region -> p-value=0.69
# AllH3 and South America Region -> p-value=0.51
# AllH3 and Central America Region -> p-value=0.83
# AllH3 and Europe Region -> p-value=0.57
# AllH3 and South Asia Region -> p-value=0.41
# AllH3 and Western Pacific Region -> p-value=0.91


# 6) Diversities Granger-cause other Diversities (8 tests)

# NorthAmericaH1 and AsiaH1 -> p-value=0.00087
# NorthAmericaH3 and AsiaH3 -> p-value=0.37
# AsiaH1 and NorthAmericaH1 -> p-value=0.71
# AsiaH3 and NothAmericaH3 -> p-value=0.71
# AllH1 and NorthAmericaH1 -> p-value=0.00000018
# AllH1 and AsiaH1 -> p-value=0.00012
# AllH3 and NorthAmericaH3 -> p-value=0.7
# AllH3 and AsiaH3 -> p-value=0.16

################################################################################

# False Discovery Rate:

library(stats)
pvec=c(0.49,0.0062,0.94,0.017,0.34,0.83,0.026,0.52,0.0035,0.0076,0.35,0.061,0.06,
       0.0056,0.68,0.000073,0.52,0.12,0.59,0.23,0.55,0.36,0.96,0.24,0.63,0.55,0.0068,
       0.57,0.55,0.82,0.05,0.47,0.84,0.87,0.97,0.000053,0.86,0.28,0.94,0.26,0.41,0.99,
       0.29,0.072,0.15,0.99,0.31,0.37,0.55,0.065,0.67,0.69,0.51,0.83,0.57,0.41,0.91,
       0.00087,0.37,0.71,0.71,0.00000018,0.00012,0.7,0.16,0.029) # 66 tests
p.adjust(pvec,method="fdr")
# 5% of significance we have: 2,9,10,14,16,27,36,58,62,63 
# Which would be the following cases:
# South America Granger-cause Brazil (lag 2)
# Western Pacific Granger-cause Europe (lag 3)
# Western Pacific Granger-cause South Asia (lag 3)
# Central America Granger-cause Europe (lag 3)
# Central America Granger-cause Western Pacific (lag 3)
# Diversity NorthAmericaH1 Granger-cause South Asia (lag 3) 
# Diversity AsiaH1 Granger-cause Central America (lag 6) 
# Diversity NorthAmericaH1 Granger-cause diversity AsiaH1 (lag 2) 
# Diversity AllH1 Granger-cause diversity NorthAmericaH1 (Gabi acha bom colocar, mas não comentar)
# Diversity AllH1 Granger-cause diversity AsiaH1 (Gabi acha bom colocar)

# 10% of significance we have: 4
# Which would be the case:
# Europe Granger-cause Brazil (lag 3)
