##############################################################################################################################
######################## FLU INCIDENCE DATABASE ###########################################################################
##############################################################################################################################
 
ddd=read.csv(file="dados.csv",header=T,sep=";") #matriz com todos número de casos, de todos países
names(ddd)

pais=unique((ddd$Country..area.or.territory[ddd$WHO.region=="European Region of WHO"]))

sem=unique((ddd$Start.date[ddd$WHO.region=="European Region of WHO"]))

banco_menor= ddd[ddd == 0] <- NA
banco_menor=ddd[,c("Country..area.or.territory", "Total.number.of.influenza.positive.viruses","Start.date","WHO.region")]
# 1) Select countries from Europe only
banco_menor=banco_menor[banco_menor[, "WHO.region"] == "European Region of WHO",]
# 2) Select from 2008 to June 2018
banco_menor=banco_menor[1:19204,]

data.frame2matrix = function(data, rowtitle, coltitle, datatitle,
                             rowdecreasing = FALSE, coldecreasing = FALSE,
                             default_value = NA) {
  
  # check, whether titles exist as columns names in the data.frame data
  if ( (!(rowtitle%in%names(data)))
       || (!(coltitle%in%names(data)))
       || (!(datatitle%in%names(data))) ) {
    stop('data.frame2matrix: bad row-, col-, or datatitle.')
  }
  
  # get number of rows in data
  ndata = dim(data)[1]
  
  # extract rownames and colnames for the matrix from the data.frame
  rownames = sort(unique(data[[rowtitle]]), decreasing = rowdecreasing)
  nrows = length(rownames)
  colnames = sort(unique(data[[coltitle]]), decreasing = coldecreasing)
  ncols = length(colnames)
  
  # initialize the matrix
  out_matrix = matrix(NA,
                      nrow = nrows, ncol = ncols,
                      dimnames=list(rownames, colnames))
  
  # iterate rows of data
  for (i1 in 1:ndata) {
    # get matrix-row and matrix-column indices for the current data-row
    iR = which(rownames==data[[rowtitle]][i1])
    iC = which(colnames==data[[coltitle]][i1])
    
    # throw an error if the matrix entry (iR,iC) is already filled.
    if (!is.na(out_matrix[iR, iC])) stop('data.frame2matrix: double entry in data.frame')
    out_matrix[iR, iC] = data[[datatitle]][i1]
  }
  
  # set empty matrix entries to the default value
  out_matrix[is.na(out_matrix)] = default_value
  
  # return matrix
  return(out_matrix)
  
}
myData=as.data.frame(list('dim1'=banco_menor$Country..area.or.territory,
                          'dim2'=banco_menor$Start.date,
                          'values'=banco_menor$Total.number.of.influenza.positive.viruses))
myMatrix=data.frame2matrix(myData, 'dim1','dim2','values')
myMatrix
# 3) Select the countries with the least missions (countries with a maximum of 50% missions)
View(rowMeans(is.na(myMatrix))) #Vê-se a prop de missing de cada país
myMatrix_europ=myMatrix[c("Belgium","Denmark","Estonia","Germany","Ireland","Israel","Italy","Latvia","Netherlands","Norway","Poland","Russian Federation","Slovenia","Spain","Sweden","Switzerland","Turkey","United Kingdom of Great Britain and Northern Ireland"),] #Selecionando aqueles com pelo menos 50% dos dados presentes
myMatrix_europ

# Assembling the matrix mat_v
tot=sum(myMatrix_europ,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop=numeric(nrow(myMatrix_europ))

for(i in 1:nrow(myMatrix_europ)){
  prop[i]= sum(myMatrix_europ[i,], na.rm=T)/tot # proporção de cada país, removidos os NA's
}

media_sem=colMeans(myMatrix_europ,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

# Matriz final
mat_v=matrix(ncol=ncol(myMatrix_europ), nrow=nrow(myMatrix_europ))
for(j in 1:ncol(myMatrix_europ)){
  for(i in 1:nrow(myMatrix_europ)){
    mat_v[i,j]<-ifelse(((is.na(myMatrix_europ[i,j]))),media_sem[j]*prop[i],myMatrix_europ[i,j])
  }
}
colnames(mat_v)=colnames(myMatrix_europ)
rownames(mat_v)=rownames(myMatrix_europ) 
########################
# 4) Matrix mat_v with the column ordered by date (increasing or decreasing)
ordemaleatoria_data_myMatrix_europ=as.Date(colnames(mat_v),"%d/%m/%Y")
ordemaleatoria_data_myMatrix_europ=as.character(ordemaleatoria_data_myMatrix_europ)
index=sort(ordemaleatoria_data_myMatrix_europ, index.return=TRUE)

mat_v_ordenada=mat_v[,index$ix]
myMatrix_europ_ordenada=myMatrix_europ[,index$ix]

# Graphics
plot.ts(mat_v_ordenada[1,])
points(mat_v_ordenada[1,],pch=1,col="blue")
points(myMatrix_europ_ordenada[1,],pch=1,col="red")

for (i in 1:18) {
  plot.ts(mat_v_ordenada[i,],main=rownames(mat_v_ordenada)[i])
  points(mat_v_ordenada[i,],pch=1,col="blue")
  points(myMatrix_europ_ordenada[i,],pch=1,col="red")}

# Assembling the matrix mat_v ordenada
tot=sum(myMatrix_europ_ordenada,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop=numeric(nrow(myMatrix_europ_ordenada))

for(i in 1:nrow(myMatrix_europ_ordenada)){
  prop[i]= sum(myMatrix_europ_ordenada[i,], na.rm=T)/tot # proporção de cada país, removidos os NA's
}

media_sem=colMeans(myMatrix_europ_ordenada,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

# Matriz final ordenada (Final ordered matrix)
mat_v_ordenada=matrix(ncol=ncol(myMatrix_europ_ordenada), nrow=nrow(myMatrix_europ_ordenada))
for(j in 1:ncol(myMatrix_europ_ordenada)){
  for(i in 1:nrow(myMatrix_europ_ordenada)){
    mat_v_ordenada[i,j]<-ifelse(((is.na(myMatrix_europ_ordenada[i,j]))),media_sem[j]*prop[i],myMatrix_europ_ordenada[i,j])
  }
}
colnames(mat_v_ordenada)=colnames(myMatrix_europ_ordenada)
rownames(mat_v_ordenada)=rownames(myMatrix_europ_ordenada)
################################

# Creating the database matrix teste_Europa_18paises
library(readxl)
teste_Europa_18paises <- read_excel("teste_Europa_18paises.xlsx")

banco_menor_2017= teste_Europa_18paises[teste_Europa_18paises == 0] <- NA #trocar zeros por NA's
banco_menor_2017=teste_Europa_18paises[,c("Country..area.or.territory", "Total.number.of.influenza.positive.viruses","Start.date","WHO.region")]

# Comparing matrices
View(banco_menor_2017) #matriz do banco teste_Euroa_18paises (2017 até 2019) e com o número de casos da Europa dos 18 países com menos missings. Nessa matriz as datas estão no formato: ano/mês/dia
View(mat_v_ordenada[,470:547]) # matriz mat_v ordenada (2008 até 2018/1) e com o número de casos da Europa dos 18 países com menos missings. Nessa matriz as datas estão no formato: dia/mês/ano
#########################################################################

# Reading the database
banco_menor_asia= ddd[ddd == 0] <- NA #trocar zeros por NA's
banco_menor_asia=ddd[,c("Country..area.or.territory", "Total.number.of.influenza.positive.viruses","Start.date","WHO.region")]
# 1) Select countries in South Asia only
banco_menor_asia=banco_menor_asia[banco_menor_asia[, "WHO.region"] == "South-East Asia Region of WHO",]
# 2) Select from 2008 until the beginning of July 2018
banco_menor_asia=banco_menor_asia[1:1173,]

myData_asia=as.data.frame(list('dim1'=banco_menor_asia$Country..area.or.territory,
                               'dim2'=banco_menor_asia$Start.date,
                               'values'=banco_menor_asia$Total.number.of.influenza.positive.viruses))
myMatrix_asia=data.frame2matrix(myData_asia, 'dim1','dim2','values')
myMatrix_asia
# 3) Select the countries with the least missions (countries with a maximum of 50% missions)
View(rowMeans(is.na(myMatrix_asia))) #Vê-se a prop de missing de cada país
myMatrix_asia=myMatrix_asia[c("Indonesia","Thailand"),] #Selecionando aqueles com pelo menos 50% dos dados presentes. Nessa caso, dos 5 países da Ásia, apenas a Indonésia e a Tailândia possuem menos de 50% de missings.
myMatrix_asia

# Assembling the mat_v matrix for Asia
tot_asia=sum(myMatrix_asia,na.rm=T)

prop_asia=numeric(nrow(myMatrix_asia))

for(i in 1:nrow(myMatrix_asia)){
  prop_asia[i]= sum(myMatrix_asia[i,], na.rm=T)/tot_asia
}

media_sem_asia=colMeans(myMatrix_asia,na.rm=T)

# Matriz final
mat_v_asia=matrix(ncol=ncol(myMatrix_asia), nrow=nrow(myMatrix_asia))
for(j in 1:ncol(myMatrix_asia)){
  for(i in 1:nrow(myMatrix_asia)){
    mat_v_asia[i,j]<-ifelse(((is.na(myMatrix_asia[i,j]))),media_sem_asia[j]*prop_asia[i],myMatrix_asia[i,j])
  }
}
colnames(mat_v_asia)=colnames(myMatrix_asia)
rownames(mat_v_asia)=rownames(myMatrix_asia)
########################
# 4) Matrix mat_v with the column ordered by date (increasing or decreasing)
ordemaleatoria_data_myMatrix_asia=as.Date(colnames(mat_v_asia),"%d/%m/%Y")
ordemaleatoria_data_myMatrix_asia=as.character(ordemaleatoria_data_myMatrix_asia)
index_asia=sort(ordemaleatoria_data_myMatrix_asia, index.return=TRUE)

mat_v_asia_ordenada=mat_v_asia[,index_asia$ix]
myMatrix_asia_ordenada=myMatrix_asia[,index_asia$ix]

# Graphics
for (i in 1:2) {
  plot.ts(mat_v_asia_ordenada[i,],main=rownames(mat_v_asia_ordenada)[i])
  points(mat_v_asia_ordenada[i,],pch=1,col="blue")
  points(myMatrix_asia_ordenada[i,],pch=1,col="red")}

# Mounting the ordered mat_v array
tot_asia=sum(myMatrix_asia_ordenada,na.rm=T)

prop_asia=numeric(nrow(myMatrix_asia_ordenada))

for(i in 1:nrow(myMatrix_asia_ordenada)){
  prop_asia[i]= sum(myMatrix_asia_ordenada[i,], na.rm=T)/tot_asia
}

media_sem_asia=colMeans(myMatrix_asia_ordenada,na.rm=T)

# Matriz final ordenada (Final ordered matrix)
mat_v_asia_ordenada=matrix(ncol=ncol(myMatrix_asia_ordenada), nrow=nrow(myMatrix_asia_ordenada))
for(j in 1:ncol(myMatrix_asia_ordenada)){
  for(i in 1:nrow(myMatrix_asia_ordenada)){
    mat_v_asia_ordenada[i,j]<-ifelse(((is.na(myMatrix_asia_ordenada[i,j]))),media_sem_asia[j]*prop_asia[i],myMatrix_asia_ordenada[i,j])
  }
}
colnames(mat_v_asia_ordenada)=colnames(myMatrix_asia_ordenada)
rownames(mat_v_asia_ordenada)=rownames(myMatrix_asia_ordenada)
################################

# Criando a matriz do banco teste_Europa_18paises
library(readxl)
teste_Asia_2paises <- read_excel("teste_Asia_2paises.xlsx")

banco_menor_2017_asia=teste_Asia_2paises[teste_Asia_2paises == 0] <- NA
banco_menor_2017_asia=teste_Asia_2paises[,c("Country..area.or.territory", "Total.number.of.influenza.positive.viruses","Start.date","WHO.region")]

# Comparando as matrizes
View(banco_menor_2017_asia) #matriz do banco teste_asia_2paises (2017 até 2019) e com o número de casos da Asia dos 2 países com menos missings. Nessa matriz as datas estão no formato: ano/mês/dia
View(mat_v_asia_ordenada[,468:547]) # matriz mat_v_asia ordenada (2008 até 2018/1) e com o número de casos da Asia dos 2 países com menos missings. Nessa matriz as datas estão no formato: dia/mês/ano
# Comparando as matrizes acima (mat_v_ordenada_asia e teste_asia_2paises), no período de 2017 até 2018/1 (intersecção) percebe-se que os valores do número de casos no ano de 2017 são os mesmos, já no ano de 2018 os número de casos na Tailândia continuam os mesmos e na Índia esses números de casos aumentam na matriz teste_asia_2paises.

myData_2017=as.data.frame(list('dim1'=as.character(banco_menor_2017$Country..area.or.territory),
                               'dim2'=as.character(banco_menor_2017$Start.date),
                               'values'=banco_menor_2017$Total.number.of.influenza.positive.viruses))
myMatrix_2017=data.frame2matrix(myData_2017, 'dim1','dim2','values')
myMatrix_2017
###########################################################################################################################

## 1) Pegar "myMatrix2017" (2017 até 2019) e fazer a mesma imputação feita anteriormente na "matriz_v_ordenada" (média da semana*proporção do país). Com isso irá se criar a "matriz_v_europa2017".
# Obs: Antes disso tirar os dados da Espanha e trocar por novos que estão no site da Flunet, desde 2008 até 2019.

# Montando "myMatrix_spain" com os novos dados Espanha (2008 até 2019)
library(readxl)
Espanha_novosdados_2008_2019 <- read_excel("Spain_newdata_2008_2019.xlsx")

d1_spain=as.character(Espanha_novosdados_2008_2019$Country..area.or.territory)
d2_spain=as.character(Espanha_novosdados_2008_2019$Start.date)

myData_Spain=as.data.frame(list('dim1'=Espanha_novosdados_2008_2019$Country..area.or.territory,
                                'dim2'=Espanha_novosdados_2008_2019$Start.date,
                                'values'=Espanha_novosdados_2008_2019$Total.number.of.influenza.positive.viruses))

paises_spain=sort(unique(as.character(myData_Spain$dim1)))
datas_spain=sort(unique(as.character(myData_Spain$dim2)))
values_spain=as.numeric(as.character(myData_Spain$values))

myMatrix_spain=matrix(nrow=length(paises_spain),ncol=length(datas_spain))
colnames(myMatrix_spain)=datas_spain
rownames(myMatrix_spain)=paises_spain
for (k in 1:dim(myData_Spain)[1]){
  i=d1_spain[k] #país
  j=d2_spain[k] #data
  myMatrix_spain[i,j]=values_spain[k]
  
}
myMatrix_spain[myMatrix_spain == 0] <- NA #trocar zeros por NA's
##########

# Criando novamente "myMatrix2017" (2017 até 2019), mas com os novos dados da Espanha, criados acima
library(readxl)
teste_Europa_18paises <- read_excel("teste_Europa_18paises.xlsx")

banco_menor_2017= teste_Europa_18paises[teste_Europa_18paises == 0] <- NA #trocar zeros por NA's
banco_menor_2017=teste_Europa_18paises[,c("Country..area.or.territory", "Total.number.of.influenza.positive.viruses","Start.date","WHO.region")]

d1=as.character(banco_menor_2017$Country..area.or.territory)
d2=as.character(banco_menor_2017$Start.date)

paises=sort(unique(as.character(myData_2017$dim1)))
datas=sort(unique(as.character(myData_2017$dim2)))
values=as.numeric(as.character(myData_2017$values))

myMatrix_2017=matrix(nrow=length(paises),ncol=length(datas))
colnames(myMatrix_2017)=datas
rownames(myMatrix_2017)=paises
for (k in 1:dim(myData_2017)[1]){
  i=d1[k] #país
  j=d2[k] #data
  myMatrix_2017[i,j]=values[k]
  
}

myMatrix_2017=myMatrix_2017[-14,] #tirando a linha com os dados antigos da Espanha

myMatrix_2017=rbind(myMatrix_2017, myMatrix_spain[,451:601]) # juntando os outros 17 países da Europa com a Espanha(novos dados da Espanha), desde 02/01/2017 até 18/11/2019.
rownames(myMatrix_2017)=c("Belgium","Denmark","Estonia","Germany","Ireland","Israel","Italy","Latvia","Netherlands","Norway","Poland","Russian Federation","Slovenia","Sweden","Switzerland","Turkey","United Kingdom of Great Britain and Northern Ireland","Spain")
##########

# Imputação
# Montando a matriz mat_v_europa2017 (02/01/2017 até 18/11/2019 e com os novos dados da Espanha)
tot_europ2017=sum(myMatrix_2017,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_europ2017=numeric(nrow(myMatrix_2017))

for(i in 1:nrow(myMatrix_2017)){
  prop_europ2017[i]= sum(myMatrix_2017[i,], na.rm=T)/tot_europ2017 # proporção de cada país, removidos os NA's
}

media_sem_europ2017=colMeans(myMatrix_2017,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final mat_v_europa2017
mat_v_europa2017=matrix(ncol=ncol(myMatrix_2017), nrow=nrow(myMatrix_2017))
for(j in 1:ncol(myMatrix_2017)){
  for(i in 1:nrow(myMatrix_2017)){
    mat_v_europa2017[i,j]<-ifelse(((is.na(myMatrix_2017[i,j]))),media_sem_europ2017[j]*prop_europ2017[i],myMatrix_2017[i,j])
  }
}
colnames(mat_v_europa2017)=colnames(myMatrix_2017)
rownames(mat_v_europa2017)=rownames(myMatrix_2017)
#########################################################################################

## 2) Pegar "mat_v_ordenada" e ao invés de começar em janeiro de 2008, começar em outubro de 2008. Além disso trocar dados da Espanha no "mat_v_ordenado" e para isso vai ser necessário imputar novamente a matriz "myMatrix_europ_ordenada", mas dessa vez com os dados novos da Espanha.
myMatrix_europ_ordenada=myMatrix_europ[,index$ix] # matriz myMatrix_europ ordenada
myMatrix_europ_ordenada=myMatrix_europ_ordenada[-14,] #tirando dados antigos da Espanha
myMatrix_europ_ordenada # 07/01/2008 a 25/06/2018
myMatrix_spain #2008-01-07 a 2019-12-23

myMatrix_europ_ordenada=myMatrix_europ_ordenada[,40:547] #europa começando em outubro de 2008 (06/10/2008 até 25/06/2018)
myMatrix_spain=myMatrix_spain[,21:528] #Espanha começando em outubro de 2008 (06/10/2008 até 25/06/2018)
myMatrix_spain=t(myMatrix_spain)
colnames(myMatrix_spain)=colnames(myMatrix_europ_ordenada)
myMatrix_europ_ordenada=rbind(myMatrix_europ_ordenada,myMatrix_spain) # 07/01/2008 a 25/06/2018
rownames(myMatrix_europ_ordenada)=c("Belgium","Denmark","Estonia","Germany","Ireland","Israel","Italy","Latvia","Netherlands","Norway","Poland","Russian Federation","Slovenia","Sweden","Switzerland","Turkey","United Kingdom of Great Britain and Northern Ireland","Spain")
##########

# Imputação
# Montando a matriz mat_v_ordenada (outubro de 2008 (06/10/2008) até dezembro de 2016 (26/12/2016), com os novos dados da Espanha), que é a matriz com os dados da matriz myMatrix_europ_ordenada imputados.

myMatrix_europ_ordenada=myMatrix_europ_ordenada[,1:430] #deixando nas datas da matriz mat_v_ordenada (outubro de 2008 (06/10/2008) até dezembro de 2016 (26/12/2016))
tot_europ=sum(myMatrix_europ_ordenada,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_europ=numeric(nrow(myMatrix_europ_ordenada))

for(i in 1:nrow(myMatrix_europ_ordenada)){
  prop_europ[i]= sum(myMatrix_europ_ordenada[i,], na.rm=T)/tot_europ # proporção de cada país, removidos os NA's
}

media_sem_europ=colMeans(myMatrix_europ_ordenada,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final mat_v_ordenada
mat_v_ordenada=matrix(ncol=ncol(myMatrix_europ_ordenada), nrow=nrow(myMatrix_europ_ordenada))
for(j in 1:ncol(myMatrix_europ_ordenada)){
  for(i in 1:nrow(myMatrix_europ_ordenada)){
    mat_v_ordenada[i,j]<-ifelse(((is.na(myMatrix_europ_ordenada[i,j]))),media_sem_europ[j]*prop_europ[i],myMatrix_europ_ordenada[i,j])
  }
}
colnames(mat_v_ordenada)=colnames(myMatrix_europ_ordenada)
rownames(mat_v_ordenada)=rownames(myMatrix_europ_ordenada)
#########################################################################################

## 3) Organizar as matrizes da seguinte maneira: mat_v_ordenada (06/10/2008 até 26/12/2016), mat_v_europa2017 (02/01/2017 até 25/12/2017) e mat_v_europa_valid (01/01/2018 até 11/11/2019) usando os dados novos da Espanha. Para isso será necessário repartir a matriz mat_v_europa2017 feita anteriormente (2017 até 2019) nas duas matrizes desejadas, ou seja,  mat_v_europa2017 (2017) e mat_v_europa_valid (2018 e 2019). Além disso trocar o número de casos em 01/01/2018 na Itália de 89 para 620 casos.
mat_v_ordenada # 06/10/2008 até 26/12/2016 - matriz com número de casos da Europa imputados
mat_v_europa_valid=mat_v_europa2017[,53:150] # 01/01/2018 até 11/11/2019
mat_v_europa_valid[7,1]=620 # trocando o número de casos em 01/01/2018 na Itália de 89 para 620 casos - matriz com número de casos da Europa imputados
mat_v_europa2017=mat_v_europa2017[,1:52] # 02/01/2017 até 25/12/2017 - matriz com número de casos da Europa imputados
#############################################################################################################################

## 4) Matriz_v Western Pacific

# Montando "myMatrix_WesternPacific" com os novos dados (2008 até 2019)
library(readxl)
Novosdados_WesternPacific_2008_2019 <- read_excel("Novosdados_WesternPacific_2008_2019.xlsx")

d1_WesternPacific=as.character(Novosdados_WesternPacific_2008_2019$Country..area.or.territory)
d2_WesternPacific=as.character(Novosdados_WesternPacific_2008_2019$Start.date)

myData_WesternPacific=as.data.frame(list('dim1'=Novosdados_WesternPacific_2008_2019$Country..area.or.territory,
                                         'dim2'=Novosdados_WesternPacific_2008_2019$Start.date,
                                         'values'=Novosdados_WesternPacific_2008_2019$Total.number.of.influenza.positive.viruses))

paises_WesternPacific=sort(unique(as.character(myData_WesternPacific$dim1)))
datas_WesternPacific=sort(unique(as.character(myData_WesternPacific$dim2)))
values_WesternPacific=as.numeric(as.character(myData_WesternPacific$values))

myMatrix_WesternPacific=matrix(nrow=length(paises_WesternPacific),ncol=length(datas_WesternPacific))
colnames(myMatrix_WesternPacific)=datas_WesternPacific
rownames(myMatrix_WesternPacific)=paises_WesternPacific
for (k in 1:dim(myData_WesternPacific)[1]){
  i=d1_WesternPacific[k] #país
  j=d2_WesternPacific[k] #data
  myMatrix_WesternPacific[i,j]=values_WesternPacific[k]
  
}
myMatrix_WesternPacific[myMatrix_WesternPacific == 0] <- NA #trocar zeros por NA's
myMatrix_WesternPacific=myMatrix_WesternPacific[,41:620] #Deixando com a data de 06/10/2008 até 11/11/2019
##########

# Selecionar os países com menos missings (países com no máximo 50% de missings)
View(rowMeans(is.na(myMatrix_WesternPacific))) #Vê-se a prop de missing de cada país
myMatrix_WesternPacific=myMatrix_WesternPacific[c("Australia","Cambodia","China","Japan","Lao People's Democratic Republic","Malaysia","New Caledonia","Philippines","Republic of Korea","Singapore","Viet Nam"),] #Selecionando aqueles com pelo menos 50% dos dados presentes
myMatrix_WesternPacific # Dos 15 países selecionou-se 11, pois esses 11 têm pelo menos 50% dos dados sem missings.
##########

# Montando a matriz mat_v_WesternPacific2017 (02/01/2017 até 11/11/2019)
myMatrix_WesternPacific2017=myMatrix_WesternPacific[,431:580]

tot_WesternPacific2017=sum(myMatrix_WesternPacific2017,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_WesternPacific2017=numeric(nrow(myMatrix_WesternPacific2017))

for(i in 1:nrow(myMatrix_WesternPacific2017)){
  prop_WesternPacific2017[i]= sum(myMatrix_WesternPacific2017[i,], na.rm=T)/tot_WesternPacific2017 # proporção de cada país, removidos os NA's
}

media_sem_WesternPacific2017=colMeans(myMatrix_WesternPacific2017,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_WesternPacific2017=matrix(ncol=ncol(myMatrix_WesternPacific2017), nrow=nrow(myMatrix_WesternPacific2017))
for(j in 1:ncol(myMatrix_WesternPacific2017)){
  for(i in 1:nrow(myMatrix_WesternPacific2017)){
    mat_v_WesternPacific2017[i,j]<-ifelse(((is.na(myMatrix_WesternPacific2017[i,j]))),media_sem_WesternPacific2017[j]*prop_WesternPacific2017[i],myMatrix_WesternPacific2017[i,j])
  }
}
colnames(mat_v_WesternPacific2017)=colnames(myMatrix_WesternPacific2017)
rownames(mat_v_WesternPacific2017)=rownames(myMatrix_WesternPacific2017)
##########

# Montando a matriz mat_v_WesternPacific (outubro de 2008 (06/10/2008) até dezembro de 2016 (26/12/2016))
myMatrix_WesternPacific=myMatrix_WesternPacific[,1:430]

tot_WesternPacific=sum(myMatrix_WesternPacific,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_WesternPacific=numeric(nrow(myMatrix_WesternPacific))

for(i in 1:nrow(myMatrix_WesternPacific)){
  prop_WesternPacific[i]= sum(myMatrix_WesternPacific[i,], na.rm=T)/tot_WesternPacific # proporção de cada país, removidos os NA's
}

media_sem_WesternPacific=colMeans(myMatrix_WesternPacific,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_WesternPacific=matrix(ncol=ncol(myMatrix_WesternPacific), nrow=nrow(myMatrix_WesternPacific))
for(j in 1:ncol(myMatrix_WesternPacific)){
  for(i in 1:nrow(myMatrix_WesternPacific)){
    mat_v_WesternPacific[i,j]<-ifelse(((is.na(myMatrix_WesternPacific[i,j]))),media_sem_WesternPacific[j]*prop_WesternPacific[i],myMatrix_WesternPacific[i,j])
  }
}
colnames(mat_v_WesternPacific)=colnames(myMatrix_WesternPacific)
rownames(mat_v_WesternPacific)=rownames(myMatrix_WesternPacific)
#########################################################################################

# Organizar as matrizes da seguinte maneira: mat_v_WesternPacific (06/10/2008 até 26/12/2016), mat_v_WesternPacific2017 (02/01/2017 até 25/12/2017) e mat_v_WesternPacific_valid (01/01/2018 até 11/11/2019). Para isso será necessário repartir a matriz mat_v_WesternPacific2017 feita anteriormente (2017 até 2019) nas duas matrizes desejadas, ou seja, mat_v_WesternPacific2017 (2017) e mat_v_WesternPacific_valid (2018 e 2019).
mat_v_WesternPacific # 06/10/2008 até 26/12/2016 - matriz com número de casos da Wester Pacific imputados
mat_v_WesternPacific_valid=mat_v_WesternPacific2017[,53:150] # 01/01/2018 até 11/11/2019
mat_v_WesternPacific2017=mat_v_WesternPacific2017[,1:52] # 02/01/2017 até 25/12/2017 - matriz com número de casos da Western Pacific imputados
#############################################################################################################################

## 4) Matriz_v South Asia

# Montando "myMatrix_SouthAsia" com os novos dados (2008 até 2019)
library(readxl)
Novosdados_Asia_2008_2019 <- read_excel("Novosdados_Asia_2008_2019.xlsx")

d1_SouthAsia=as.character(Novosdados_Asia_2008_2019$Country..area.or.territory)
d2_SouthAsia=as.character(Novosdados_Asia_2008_2019$Start.date)

myData_SouthAsia=as.data.frame(list('dim1'=Novosdados_Asia_2008_2019$Country..area.or.territory,
                                    'dim2'=Novosdados_Asia_2008_2019$Start.date,
                                    'values'=Novosdados_Asia_2008_2019$Total.number.of.influenza.positive.viruses))

paises_SouthAsia=sort(unique(as.character(myData_SouthAsia$dim1)))
datas_SouthAsia=sort(unique(as.character(myData_SouthAsia$dim2)))
values_SouthAsia=as.numeric(as.character(myData_SouthAsia$values))

myMatrix_SouthAsia=matrix(nrow=length(paises_SouthAsia),ncol=length(datas_SouthAsia))
colnames(myMatrix_SouthAsia)=datas_SouthAsia
rownames(myMatrix_SouthAsia)=paises_SouthAsia
for (k in 1:dim(myData_SouthAsia)[1]){
  i=d1_SouthAsia[k] #país
  j=d2_SouthAsia[k] #data
  myMatrix_SouthAsia[i,j]=values_SouthAsia[k]
  
}
myMatrix_SouthAsia[myMatrix_SouthAsia == 0] <- NA #trocar zeros por NA's
myMatrix_SouthAsia=myMatrix_SouthAsia[,41:620] #Deixando com a data de 06/10/2008 até 11/11/2019
##########

# Selecionar os países com menos missings (países com no máximo 50% de missings)
View(rowMeans(is.na(myMatrix_SouthAsia))) #Vê-se a prop de missing de cada país
myMatrix_SouthAsia=myMatrix_SouthAsia[c("Bangladesh","Bhutan","India","Indonesia","Nepal","Sri Lanka","Thailand"),] #Selecionando aqueles com pelo menos 50% dos dados presentes
myMatrix_SouthAsia # Dos 11 países selecionou-se 7, pois esses 7 têm pelo menos 50% dos dados sem missings.
##########

# Montando a matriz mat_v_SouthAsia2017 (02/01/2017 até 11/11/2019)
myMatrix_SouthAsia2017=myMatrix_SouthAsia[,431:580]

tot_SouthAsia2017=sum(myMatrix_SouthAsia2017,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_SouthAsia2017=numeric(nrow(myMatrix_SouthAsia2017))

for(i in 1:nrow(myMatrix_SouthAsia2017)){
  prop_SouthAsia2017[i]= sum(myMatrix_SouthAsia2017[i,], na.rm=T)/tot_SouthAsia2017 # proporção de cada país, removidos os NA's
}

media_sem_SouthAsia2017=colMeans(myMatrix_SouthAsia2017,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_SouthAsia2017=matrix(ncol=ncol(myMatrix_SouthAsia2017), nrow=nrow(myMatrix_SouthAsia2017))
for(j in 1:ncol(myMatrix_SouthAsia2017)){
  for(i in 1:nrow(myMatrix_SouthAsia2017)){
    mat_v_SouthAsia2017[i,j]<-ifelse(((is.na(myMatrix_SouthAsia2017[i,j]))),media_sem_SouthAsia2017[j]*prop_SouthAsia2017[i],myMatrix_SouthAsia2017[i,j])
  }
}
colnames(mat_v_SouthAsia2017)=colnames(myMatrix_SouthAsia2017)
rownames(mat_v_SouthAsia2017)=rownames(myMatrix_SouthAsia2017)
##########

# Montando a matriz mat_v_SouthAsia (outubro de 2008 (06/10/2008) até dezembro de 2016 (26/12/2016))
myMatrix_SouthAsia=myMatrix_SouthAsia[,1:430]

tot_SouthAsia=sum(myMatrix_SouthAsia,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_SouthAsia=numeric(nrow(myMatrix_SouthAsia))

for(i in 1:nrow(myMatrix_SouthAsia)){
  prop_SouthAsia[i]= sum(myMatrix_SouthAsia[i,], na.rm=T)/tot_SouthAsia # proporção de cada país, removidos os NA's
}

media_sem_SouthAsia=colMeans(myMatrix_SouthAsia,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_SouthAsia=matrix(ncol=ncol(myMatrix_SouthAsia), nrow=nrow(myMatrix_SouthAsia))
for(j in 1:ncol(myMatrix_SouthAsia)){
  for(i in 1:nrow(myMatrix_SouthAsia)){
    mat_v_SouthAsia[i,j]<-ifelse(((is.na(myMatrix_SouthAsia[i,j]))),media_sem_SouthAsia[j]*prop_SouthAsia[i],myMatrix_SouthAsia[i,j])
  }
}
colnames(mat_v_SouthAsia)=colnames(myMatrix_SouthAsia)
rownames(mat_v_SouthAsia)=rownames(myMatrix_SouthAsia)
#########################################################################################

# Organizar as matrizes da seguinte maneira: mat_v_SouthAsia (06/10/2008 até 26/12/2016), mat_v_SouthAsia2017 (02/01/2017 até 25/12/2017) e mat_v_SouthAsia_valid (01/01/2018 até 11/11/2019). Para isso será necessário repartir a matriz mat_v_SouthAsia2017 feita anteriormente (2017 até 2019) nas duas matrizes desejadas, ou seja, mat_v_SouthAsia2017 (2017) e mat_v_SouthAsia_valid (2018 e 2019).
mat_v_SouthAsia # 06/10/2008 até 26/12/2016 - matriz com número de casos da South Asia imputados
mat_v_SouthAsia_valid=mat_v_SouthAsia2017[,53:150] # 01/01/2018 até 11/11/2019
mat_v_SouthAsia2017=mat_v_SouthAsia2017[,1:52] # 02/01/2017 até 25/12/2017 - matriz com número de casos da South Asia imputados
#############################################################################################################################

# Obs: trocar os NAN pela média da semana seguinte e da semana anterior
for (i in 1:7){
  mat_v_SouthAsia[i,12]=mean(c(mat_v_SouthAsia[i,11],mat_v_SouthAsia[i,13])) #trocando o NAN da data 22/12/2008 pela média entre 15/12/2008 e 29/12/2008.
}
for (i in 1:7){
  mat_v_SouthAsia[i,28]=mean(c(mat_v_SouthAsia[i,27],mat_v_SouthAsia[i,29])) #trocando o NAN
}
###########################################################################################################################

## 1) Matriz_v South America: Tudo o que foi montado anteriormente para a Europa, South Asia e Western Pacific, montar para SouthAmerica, ou seja criar uma matriz com os paisessul. Ou seja, montar para SouthAmerica uma mat_v_ordenada (dados imputados da SouthAmerica de 2008 até 2016), mat_v_SouthAmerica_valid (dados imputados da SouthAmerica de janeiro de 2018 até final de 2019) e mat_v_SouthAmerica2017 (dados imputados da SouthAmerica de janeiro de 2017 até dezembro de 2017).

# Montando "myMatrix_SouthAmerica" com os novos dados (2008 até 2019)
library(readxl)
Novosdados_SouthAmerica_2008_2019 <- read_excel("Novosdados_SouthAmerica_2008_2019.xlsx") # Para esse banco utilizou-se o número de casos dos países das regiões "Temperate South America" e "Tropical South America".

d1_SouthAmerica=as.character(Novosdados_SouthAmerica_2008_2019$Country..area.or.territory)
d2_SouthAmerica=as.character(Novosdados_SouthAmerica_2008_2019$Start.date)

myData_SouthAmerica=as.data.frame(list('dim1'=Novosdados_SouthAmerica_2008_2019$Country..area.or.territory,
                                       'dim2'=Novosdados_SouthAmerica_2008_2019$Start.date,
                                       'values'=Novosdados_SouthAmerica_2008_2019$Total.number.of.influenza.positive.viruses))

paises_SouthAmerica=sort(unique(as.character(myData_SouthAmerica$dim1)))
datas_SouthAmerica=sort(unique(as.character(myData_SouthAmerica$dim2)))
values_SouthAmerica=as.numeric(as.character(myData_SouthAmerica$values))

myMatrix_SouthAmerica=matrix(nrow=length(paises_SouthAmerica),ncol=length(datas_SouthAmerica))
colnames(myMatrix_SouthAmerica)=datas_SouthAmerica
rownames(myMatrix_SouthAmerica)=paises_SouthAmerica
for (k in 1:dim(myData_SouthAmerica)[1]){
  i=d1_SouthAmerica[k] #país
  j=d2_SouthAmerica[k] #data
  myMatrix_SouthAmerica[i,j]=values_SouthAmerica[k]
  
}
myMatrix_SouthAmerica[myMatrix_SouthAmerica == 0] <- NA #trocar zeros por NA's
myMatrix_SouthAmerica=myMatrix_SouthAmerica[,9:588] #Deixando com a data de 06/10/2008 até 11/11/2019
##########

# Selecionar os países com menos missings (países com no máximo 50% de missings)
View(rowMeans(is.na(myMatrix_SouthAmerica))) #Vê-se a prop de missing de cada país
myMatrix_SouthAmerica=myMatrix_SouthAmerica[c("Argentina","Bolivia (Plurinational State of)","Chile","Colombia","Ecuador","French Guiana","Paraguay", "Peru"),] #Selecionando aqueles com pelo menos 50% dos dados presentes
myMatrix_SouthAmerica # Dos 12 países selecionou-se 8, pois esses 8 têm pelo menos 50% dos dados sem missings.
##########

# Montando a matriz mat_v_SouthAmerica2017 (02/01/2017 até 11/11/2019)
myMatrix_SouthAmerica2017=myMatrix_SouthAmerica[,431:580]

tot_SouthAmerica2017=sum(myMatrix_SouthAmerica2017,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_SouthAmerica2017=numeric(nrow(myMatrix_SouthAmerica2017))

for(i in 1:nrow(myMatrix_SouthAmerica2017)){
  prop_SouthAmerica2017[i]= sum(myMatrix_SouthAmerica2017[i,], na.rm=T)/tot_SouthAmerica2017 # proporção de cada país, removidos os NA's
}

media_sem_SouthAmerica2017=colMeans(myMatrix_SouthAmerica2017,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_SouthAmerica2017=matrix(ncol=ncol(myMatrix_SouthAmerica2017), nrow=nrow(myMatrix_SouthAmerica2017))
for(j in 1:ncol(myMatrix_SouthAmerica2017)){
  for(i in 1:nrow(myMatrix_SouthAmerica2017)){
    mat_v_SouthAmerica2017[i,j]<-ifelse(((is.na(myMatrix_SouthAmerica2017[i,j]))),media_sem_SouthAmerica2017[j]*prop_SouthAmerica2017[i],myMatrix_SouthAmerica2017[i,j])
  }
}
colnames(mat_v_SouthAmerica2017)=colnames(myMatrix_SouthAmerica2017)
rownames(mat_v_SouthAmerica2017)=rownames(myMatrix_SouthAmerica2017)
##########

# Montando a matriz mat_v_SouthAmerica (outubro de 2008 (06/10/2008) até dezembro de 2016 (26/12/2016))
myMatrix_SouthAmerica=myMatrix_SouthAmerica[,1:430]

tot_SouthAmerica=sum(myMatrix_SouthAmerica,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_SouthAmerica=numeric(nrow(myMatrix_SouthAmerica))

for(i in 1:nrow(myMatrix_SouthAmerica)){
  prop_SouthAmerica[i]= sum(myMatrix_SouthAmerica[i,], na.rm=T)/tot_SouthAmerica # proporção de cada país, removidos os NA's
}

media_sem_SouthAmerica=colMeans(myMatrix_SouthAmerica,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_SouthAmerica=matrix(ncol=ncol(myMatrix_SouthAmerica), nrow=nrow(myMatrix_SouthAmerica))
for(j in 1:ncol(myMatrix_SouthAmerica)){
  for(i in 1:nrow(myMatrix_SouthAmerica)){
    mat_v_SouthAmerica[i,j]<-ifelse(((is.na(myMatrix_SouthAmerica[i,j]))),media_sem_SouthAmerica[j]*prop_SouthAmerica[i],myMatrix_SouthAmerica[i,j])
  }
}
colnames(mat_v_SouthAmerica)=colnames(myMatrix_SouthAmerica)
rownames(mat_v_SouthAmerica)=rownames(myMatrix_SouthAmerica)
#########################################################################################

# Organizar as matrizes da seguinte maneira: mat_v_SouthAmerica (06/10/2008 até 26/12/2016), mat_v_SouthAmerica2017 (02/01/2017 até 25/12/2017) e mat_v_SouthAmerica_valid (01/01/2018 até 11/11/2019). Para isso será necessário repartir a matriz mat_v_SouthAmerica2017 feita anteriormente (2017 até 2019) nas duas matrizes desejadas, ou seja, mat_v_SouthAmerica2017 (2017) e mat_v_SouthAmerica_valid (2018 e 2019).
mat_v_SouthAmerica # 06/10/2008 até 26/12/2016 - matriz com número de casos da South America imputados
mat_v_SouthAmerica_valid=mat_v_SouthAmerica2017[,53:150] # 01/01/2018 até 11/11/2019
mat_v_SouthAmerica2017=mat_v_SouthAmerica2017[,1:52] # 02/01/2017 até 25/12/2017 - matriz com número de casos da South America imputados
#############################################################################################################################

## 2) Matriz_v Central America

# Montando "myMatrix_CentralAmerica" com os novos dados (2008 até 2019)
library(readxl)
Novosdados_CentralAmerica_2008_2019 <- read_excel("Novosdados_CentralAmerica_2008_2019.xlsx") # Para esse banco utilizou-se o número de casos dos países da região "Central America and Caribean".

d1_CentralAmerica=as.character(Novosdados_CentralAmerica_2008_2019$Country..area.or.territory)
d2_CentralAmerica=as.character(Novosdados_CentralAmerica_2008_2019$Start.date)

myData_CentralAmerica=as.data.frame(list('dim1'=Novosdados_CentralAmerica_2008_2019$Country..area.or.territory,
                                         'dim2'=Novosdados_CentralAmerica_2008_2019$Start.date,
                                         'values'=Novosdados_CentralAmerica_2008_2019$Total.number.of.influenza.positive.viruses))

paises_CentralAmerica=sort(unique(as.character(myData_CentralAmerica$dim1)))
datas_CentralAmerica=sort(unique(as.character(myData_CentralAmerica$dim2)))
values_CentralAmerica=as.numeric(as.character(myData_CentralAmerica$values))

myMatrix_CentralAmerica=matrix(nrow=length(paises_CentralAmerica),ncol=length(datas_CentralAmerica))
colnames(myMatrix_CentralAmerica)=datas_CentralAmerica
rownames(myMatrix_CentralAmerica)=paises_CentralAmerica
for (k in 1:dim(myData_CentralAmerica)[1]){
  i=d1_CentralAmerica[k] #país
  j=d2_CentralAmerica[k] #data
  myMatrix_CentralAmerica[i,j]=values_CentralAmerica[k]
  
}
myMatrix_CentralAmerica[myMatrix_CentralAmerica == 0] <- NA #trocar zeros por NA's
myMatrix_CentralAmerica=myMatrix_CentralAmerica[,9:588] #Deixando com a data de 06/10/2008 até 11/11/2019
##########

# Selecionar os países com menos missings (países com no máximo 50% de missings)
View(rowMeans(is.na(myMatrix_CentralAmerica))) #Vê-se a prop de missing de cada país
myMatrix_CentralAmerica=myMatrix_CentralAmerica[c("Costa Rica","Cuba","Dominican Republic","El Salvador","Guatemala","Honduras","Jamaica", "Mexico", "Nicaragua","Panama"),] #Selecionando aqueles com pelo menos 50% dos dados presentes
myMatrix_CentralAmerica # Dos 28 países selecionou-se 10, pois esses 10 têm pelo menos 50% dos dados sem missings.
##########

# Montando a matriz mat_v_CentralAmerica2017 (02/01/2017 até 11/11/2019)
myMatrix_CentralAmerica2017=myMatrix_CentralAmerica[,431:580]

tot_CentralAmerica2017=sum(myMatrix_CentralAmerica2017,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_CentralAmerica2017=numeric(nrow(myMatrix_CentralAmerica2017))

for(i in 1:nrow(myMatrix_CentralAmerica2017)){
  prop_CentralAmerica2017[i]= sum(myMatrix_CentralAmerica2017[i,], na.rm=T)/tot_CentralAmerica2017 # proporção de cada país, removidos os NA's
}

media_sem_CentralAmerica2017=colMeans(myMatrix_CentralAmerica2017,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_CentralAmerica2017=matrix(ncol=ncol(myMatrix_CentralAmerica2017), nrow=nrow(myMatrix_CentralAmerica2017))
for(j in 1:ncol(myMatrix_CentralAmerica2017)){
  for(i in 1:nrow(myMatrix_CentralAmerica2017)){
    mat_v_CentralAmerica2017[i,j]<-ifelse(((is.na(myMatrix_CentralAmerica2017[i,j]))),media_sem_CentralAmerica2017[j]*prop_CentralAmerica2017[i],myMatrix_CentralAmerica2017[i,j])
  }
}
colnames(mat_v_CentralAmerica2017)=colnames(myMatrix_CentralAmerica2017)
rownames(mat_v_CentralAmerica2017)=rownames(myMatrix_CentralAmerica2017)
##########

# Montando a matriz mat_v_CentralAmerica (outubro de 2008 (06/10/2008) até dezembro de 2016 (26/12/2016))
myMatrix_CentralAmerica=myMatrix_CentralAmerica[,1:430]

tot_CentralAmerica=sum(myMatrix_CentralAmerica,na.rm=T) # soma global do numero de casos em todos os países, removidos os NA's

prop_CentralAmerica=numeric(nrow(myMatrix_CentralAmerica))

for(i in 1:nrow(myMatrix_CentralAmerica)){
  prop_CentralAmerica[i]= sum(myMatrix_CentralAmerica[i,], na.rm=T)/tot_CentralAmerica # proporção de cada país, removidos os NA's
}

media_sem_CentralAmerica=colMeans(myMatrix_CentralAmerica,na.rm=T) # médias semanais de todos os países juntos, removidos os NA's

#Matriz final
mat_v_CentralAmerica=matrix(ncol=ncol(myMatrix_CentralAmerica), nrow=nrow(myMatrix_CentralAmerica))
for(j in 1:ncol(myMatrix_CentralAmerica)){
  for(i in 1:nrow(myMatrix_CentralAmerica)){
    mat_v_CentralAmerica[i,j]<-ifelse(((is.na(myMatrix_CentralAmerica[i,j]))),media_sem_CentralAmerica[j]*prop_CentralAmerica[i],myMatrix_CentralAmerica[i,j])
  }
}
colnames(mat_v_CentralAmerica)=colnames(myMatrix_CentralAmerica)
rownames(mat_v_CentralAmerica)=rownames(myMatrix_CentralAmerica)
#########################################################################################

# Organizar as matrizes da seguinte maneira: mat_v_CentralAmerica (06/10/2008 até 26/12/2016), mat_v_CentralAmerica2017 (02/01/2017 até 25/12/2017) e mat_v_CentralAmerica_valid (01/01/2018 até 11/11/2019). Para isso será necessário repartir a matriz mat_v_CentralAmerica2017 feita anteriormente (2017 até 2019) nas duas matrizes desejadas, ou seja, mat_v_CentralAmerica2017 (2017) e mat_v_CentralAmerica_valid (2018 e 2019).
mat_v_CentralAmerica # 06/10/2008 até 26/12/2016 - matriz com número de casos da Central America imputados
mat_v_CentralAmerica_valid=mat_v_CentralAmerica2017[,53:150] # 01/01/2018 até 11/11/2019
mat_v_CentralAmerica2017=mat_v_CentralAmerica2017[,1:52] # 02/01/2017 até 25/12/2017 - matriz com número de casos da South America imputados
#############################################################################################################################
# Obs: trocar os NAN pela média da semana seguinte e da semana anterior
for (i in 1:10){
  mat_v_CentralAmerica[i,11]=mean(c(mat_v_CentralAmerica[i,10],mat_v_CentralAmerica[i,12])) #trocando o NAN da data 15/12/2008 pela média entre 08/12/2008 e 22/12/2008.
}
#############################################################################################################################

## 3) Agrupar os dados em meses ao invés de semanas (2008 até 2019)

# 3.1) Central América (mat_v_CentralAmerica, mat_v_CentralAmerica_valid e mat_v_CentralAmerica2017)
# mat_v_CentralAmerica (outubro de 2008 até dezembro de 2016)
mat_v_CentralAmerica

anos_mat_v_CentralAmerica=as.numeric(format(as.Date(colnames(mat_v_CentralAmerica)), "%Y"))
meses_mat_v_CentralAmerica=as.numeric(format(as.Date(colnames(mat_v_CentralAmerica)), "%m"))
totais_mat_v_CentralAmerica_meses=matrix(nrow=dim(mat_v_CentralAmerica)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_mat_v_CentralAmerica_meses)<-rownames(mat_v_CentralAmerica)
colnames(totais_mat_v_CentralAmerica_meses)<- c(1:99)

for (i in 1:10){ #for para todos países de um mesmo mês e ano
  totais_mat_v_CentralAmerica_meses[i,"1"]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==10&anos_mat_v_CentralAmerica==2008],na.rm=T)
  totais_mat_v_CentralAmerica_meses[i,"2"]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==11&anos_mat_v_CentralAmerica==2008],na.rm=T)
  totais_mat_v_CentralAmerica_meses[i,"3"]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==12&anos_mat_v_CentralAmerica==2008],na.rm=T)
}

for (i in 1:10){
  for (j in 1:12){
    totais_mat_v_CentralAmerica_meses[i,(j+3)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2009],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+15)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2010],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+27)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2011],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+39)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2012],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+51)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2013],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+63)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2014],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+75)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2015],na.rm=T)
    totais_mat_v_CentralAmerica_meses[i,(j+87)]<-sum(mat_v_CentralAmerica[i,meses_mat_v_CentralAmerica==j&anos_mat_v_CentralAmerica==2016],na.rm=T)
  }  }
#####

# mat_v_CentralAmerica_valid (janeiro de 2018 até novembro de 2019)
mat_v_CentralAmerica_valid

anos_mat_v_CentralAmerica_valid=as.numeric(format(as.Date(colnames(mat_v_CentralAmerica_valid)), "%Y"))
meses_mat_v_CentralAmerica_valid=as.numeric(format(as.Date(colnames(mat_v_CentralAmerica_valid)), "%m"))
totais_mat_v_CentralAmerica_valid_meses=matrix(nrow=dim(mat_v_CentralAmerica_valid)[1],ncol=23) #criando a matriz 2*12-1
rownames(totais_mat_v_CentralAmerica_valid_meses)<-rownames(mat_v_CentralAmerica_valid)
colnames(totais_mat_v_CentralAmerica_valid_meses)<- c(1:23)

# 2018
for (i in 1:10){
  for (j in 1:12){
    totais_mat_v_CentralAmerica_valid_meses[i,j]<-sum(mat_v_CentralAmerica_valid[i,meses_mat_v_CentralAmerica_valid==j&anos_mat_v_CentralAmerica_valid==2018],na.rm=T)
  } }
# 2019
for (i in 1:10){
  for (j in 1:11){
    totais_mat_v_CentralAmerica_valid_meses[i,(j+12)]<-sum(mat_v_CentralAmerica_valid[i,meses_mat_v_CentralAmerica_valid==j&anos_mat_v_CentralAmerica_valid==2019],na.rm=T)
  } }
#####

# mat_v_CentralAmerica2017 (janeiro de 2017 até dezembro de 2017)
mat_v_CentralAmerica2017

anos_mat_v_CentralAmerica2017=as.numeric(format(as.Date(colnames(mat_v_CentralAmerica2017)), "%Y"))
meses_mat_v_CentralAmerica2017=as.numeric(format(as.Date(colnames(mat_v_CentralAmerica2017)), "%m"))
totais_mat_v_CentralAmerica2017_meses=matrix(nrow=dim(mat_v_CentralAmerica2017)[1],ncol=12) #criando a matriz 1*12
rownames(totais_mat_v_CentralAmerica2017_meses)<-rownames(mat_v_CentralAmerica2017)
colnames(totais_mat_v_CentralAmerica2017_meses)<- c(1:12)

for (i in 1:10){
  for (j in 1:12){
    totais_mat_v_CentralAmerica2017_meses[i,j]<-sum(mat_v_CentralAmerica2017[i,meses_mat_v_CentralAmerica2017==j&anos_mat_v_CentralAmerica2017==2017],na.rm=T)
  }
}
##########################################################

# 3.2) South América (mat_v_SouthAmerica, mat_v_SouthAmerica_valid e mat_v_SouthAmerica2017)

# mat_v_SouthAmerica (outubro de 2008 até dezembro de 2016)
mat_v_SouthAmerica

anos_mat_v_SouthAmerica=as.numeric(format(as.Date(colnames(mat_v_SouthAmerica)), "%Y"))
meses_mat_v_SouthAmerica=as.numeric(format(as.Date(colnames(mat_v_SouthAmerica)), "%m"))
totais_mat_v_SouthAmerica_meses=matrix(nrow=dim(mat_v_SouthAmerica)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_mat_v_SouthAmerica_meses)<-rownames(mat_v_SouthAmerica)
colnames(totais_mat_v_SouthAmerica_meses)<- c(1:99)

for (i in 1:8){
  totais_mat_v_SouthAmerica_meses[i,"1"]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==10&anos_mat_v_SouthAmerica==2008],na.rm=T)
  totais_mat_v_SouthAmerica_meses[i,"2"]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==11&anos_mat_v_SouthAmerica==2008],na.rm=T)
  totais_mat_v_SouthAmerica_meses[i,"3"]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==12&anos_mat_v_SouthAmerica==2008],na.rm=T)
}

for (i in 1:8){
  for (j in 1:12){
    totais_mat_v_SouthAmerica_meses[i,(j+3)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2009],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+15)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2010],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+27)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2011],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+39)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2012],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+51)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2013],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+63)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2014],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+75)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2015],na.rm=T)
    totais_mat_v_SouthAmerica_meses[i,(j+87)]<-sum(mat_v_SouthAmerica[i,meses_mat_v_SouthAmerica==j&anos_mat_v_SouthAmerica==2016],na.rm=T)
  }  }
#####

# mat_v_SouthAmerica_valid (janeiro de 2018 até novembro de 2019)
mat_v_SouthAmerica_valid

anos_mat_v_SouthAmerica_valid=as.numeric(format(as.Date(colnames(mat_v_SouthAmerica_valid)), "%Y"))
meses_mat_v_SouthAmerica_valid=as.numeric(format(as.Date(colnames(mat_v_SouthAmerica_valid)), "%m"))
totais_mat_v_SouthAmerica_valid_meses=matrix(nrow=dim(mat_v_SouthAmerica_valid)[1],ncol=23) #criando a matriz 2*12-1
rownames(totais_mat_v_SouthAmerica_valid_meses)<-rownames(mat_v_SouthAmerica_valid)
colnames(totais_mat_v_SouthAmerica_valid_meses)<- c(1:23)
 
# 2018
for (i in 1:8){
  for (j in 1:12){
    totais_mat_v_SouthAmerica_valid_meses[i,j]<-sum(mat_v_SouthAmerica_valid[i,meses_mat_v_SouthAmerica_valid==j&anos_mat_v_SouthAmerica_valid==2018],na.rm=T)
  } }
# 2019
for (i in 1:8){
  for (j in 1:11){
    totais_mat_v_SouthAmerica_valid_meses[i,(j+12)]<-sum(mat_v_SouthAmerica_valid[i,meses_mat_v_SouthAmerica_valid==j&anos_mat_v_SouthAmerica_valid==2019],na.rm=T)
  } }
#####

# mat_v_SouthAmerica2017 (janeiro de 2017 até dezembro de 2017)
mat_v_SouthAmerica2017

anos_mat_v_SouthAmerica2017=as.numeric(format(as.Date(colnames(mat_v_SouthAmerica2017)), "%Y"))
meses_mat_v_SouthAmerica2017=as.numeric(format(as.Date(colnames(mat_v_SouthAmerica2017)), "%m"))
totais_mat_v_SouthAmerica2017_meses=matrix(nrow=dim(mat_v_SouthAmerica2017)[1],ncol=12) #criando a matriz 1*12
rownames(totais_mat_v_SouthAmerica2017_meses)<-rownames(mat_v_SouthAmerica2017)
colnames(totais_mat_v_SouthAmerica2017_meses)<- c(1:12)

for (i in 1:8){
  for (j in 1:12){
    totais_mat_v_SouthAmerica2017_meses[i,j]<-sum(mat_v_SouthAmerica2017[i,meses_mat_v_SouthAmerica2017==j&anos_mat_v_SouthAmerica2017==2017],na.rm=T)
  }
}
##########################################################

# 3.3) Europa (mat_v_ordenada, mat_v_europa_valid e mat_v_europa2017)

# mat_v_ordenada (outubro de 2008 até dezembro de 2016)
mat_v_ordenada
colnames(mat_v_ordenada)=colnames(mat_v_SouthAmerica)

anos_mat_v_ordenada=as.numeric(format(as.Date(colnames(mat_v_ordenada)), "%Y"))
meses_mat_v_ordenada=as.numeric(format(as.Date(colnames(mat_v_ordenada)), "%m"))
totais_mat_v_ordenada_meses=matrix(nrow=dim(mat_v_ordenada)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_mat_v_ordenada_meses)<-rownames(mat_v_ordenada)
colnames(totais_mat_v_ordenada_meses)<- c(1:99)

for (i in 1:18){
  totais_mat_v_ordenada_meses[i,"1"]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==10&anos_mat_v_ordenada==2008],na.rm=T)
  totais_mat_v_ordenada_meses[i,"2"]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==11&anos_mat_v_ordenada==2008],na.rm=T)
  totais_mat_v_ordenada_meses[i,"3"]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==12&anos_mat_v_ordenada==2008],na.rm=T)
}

for (i in 1:18){
  for (j in 1:12){
    totais_mat_v_ordenada_meses[i,(j+3)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2009],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+15)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2010],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+27)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2011],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+39)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2012],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+51)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2013],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+63)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2014],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+75)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2015],na.rm=T)
    totais_mat_v_ordenada_meses[i,(j+87)]<-sum(mat_v_ordenada[i,meses_mat_v_ordenada==j&anos_mat_v_ordenada==2016],na.rm=T)
  }  }
#####

# mat_v_europa_valid (janeiro de 2018 até novembro de 2019)
mat_v_europa_valid

anos_mat_v_europa_valid=as.numeric(format(as.Date(colnames(mat_v_europa_valid)), "%Y"))
meses_mat_v_europa_valid=as.numeric(format(as.Date(colnames(mat_v_europa_valid)), "%m"))
totais_mat_v_europa_valid_meses=matrix(nrow=dim(mat_v_europa_valid)[1],ncol=23) #criando a matriz 2*12-1
rownames(totais_mat_v_europa_valid_meses)<-rownames(mat_v_europa_valid)
colnames(totais_mat_v_europa_valid_meses)<- c(1:23)

# 2018
for (i in 1:18){
  for (j in 1:12){
    totais_mat_v_europa_valid_meses[i,j]<-sum(mat_v_europa_valid[i,meses_mat_v_europa_valid==j&anos_mat_v_europa_valid==2018],na.rm=T)
  } }
# 2019
for (i in 1:18){
  for (j in 1:11){
    totais_mat_v_europa_valid_meses[i,(j+12)]<-sum(mat_v_europa_valid[i,meses_mat_v_europa_valid==j&anos_mat_v_europa_valid==2019],na.rm=T)
  } }
#####

# mat_v_europa2017 (janeiro de 2017 até dezembro de 2017)
mat_v_europa2017

anos_mat_v_europa2017=as.numeric(format(as.Date(colnames(mat_v_europa2017)), "%Y"))
meses_mat_v_europa2017=as.numeric(format(as.Date(colnames(mat_v_europa2017)), "%m"))
totais_mat_v_europa2017_meses=matrix(nrow=dim(mat_v_europa2017)[1],ncol=12) #criando a matriz 1*12
rownames(totais_mat_v_europa2017_meses)<-rownames(mat_v_europa2017)
colnames(totais_mat_v_europa2017_meses)<- c(1:12)

for (i in 1:18){
  for (j in 1:12){
    totais_mat_v_europa2017_meses[i,j]<-sum(mat_v_europa2017[i,meses_mat_v_europa2017==j&anos_mat_v_europa2017==2017],na.rm=T)
  }
}
##########################################################

# 3.4) South Asia (mat_v_SouthAsia, mat_v_SouthAsia_valid, mat_v_SouthAsia2017)

# mat_v_SouthAsia (outubro de 2008 até dezembro de 2016)
mat_v_SouthAsia

anos_mat_v_SouthAsia=as.numeric(format(as.Date(colnames(mat_v_SouthAsia)), "%Y"))
meses_mat_v_SouthAsia=as.numeric(format(as.Date(colnames(mat_v_SouthAsia)), "%m"))
totais_mat_v_SouthAsia_meses=matrix(nrow=dim(mat_v_SouthAsia)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_mat_v_SouthAsia_meses)<-rownames(mat_v_SouthAsia)
colnames(totais_mat_v_SouthAsia_meses)<- c(1:99)

for (i in 1:7){
  totais_mat_v_SouthAsia_meses[i,"1"]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==10&anos_mat_v_SouthAsia==2008],na.rm=T)
  totais_mat_v_SouthAsia_meses[i,"2"]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==11&anos_mat_v_SouthAsia==2008],na.rm=T)
  totais_mat_v_SouthAsia_meses[i,"3"]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==12&anos_mat_v_SouthAsia==2008],na.rm=T)
}

for (i in 1:7){
  for (j in 1:12){
    totais_mat_v_SouthAsia_meses[i,(j+3)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2009],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+15)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2010],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+27)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2011],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+39)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2012],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+51)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2013],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+63)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2014],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+75)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2015],na.rm=T)
    totais_mat_v_SouthAsia_meses[i,(j+87)]<-sum(mat_v_SouthAsia[i,meses_mat_v_SouthAsia==j&anos_mat_v_SouthAsia==2016],na.rm=T)
  }  }
#####

# mat_v_SouthAsia_valid (janeiro de 2018 até novembro de 2019)
mat_v_SouthAsia_valid

anos_mat_v_SouthAsia_valid=as.numeric(format(as.Date(colnames(mat_v_SouthAsia_valid)), "%Y"))
meses_mat_v_SouthAsia_valid=as.numeric(format(as.Date(colnames(mat_v_SouthAsia_valid)), "%m"))
totais_mat_v_SouthAsia_valid_meses=matrix(nrow=dim(mat_v_SouthAsia_valid)[1],ncol=23) #criando a matriz 2*12-1
rownames(totais_mat_v_SouthAsia_valid_meses)<-rownames(mat_v_SouthAsia_valid)
colnames(totais_mat_v_SouthAsia_valid_meses)<- c(1:23)

# 2018
for (i in 1:7){
  for (j in 1:12){
    totais_mat_v_SouthAsia_valid_meses[i,j]<-sum(mat_v_SouthAsia_valid[i,meses_mat_v_SouthAsia_valid==j&anos_mat_v_SouthAsia_valid==2018],na.rm=T)
  } }
# 2019
for (i in 1:7){
  for (j in 1:11){
    totais_mat_v_SouthAsia_valid_meses[i,(j+12)]<-sum(mat_v_SouthAsia_valid[i,meses_mat_v_SouthAsia_valid==j&anos_mat_v_SouthAsia_valid==2019],na.rm=T)
  } }
#####

# mat_v_SouthAsia2017 (janeiro de 2017 até dezembro de 2017)
mat_v_SouthAsia2017

anos_mat_v_SouthAsia2017=as.numeric(format(as.Date(colnames(mat_v_SouthAsia2017)), "%Y"))
meses_mat_v_SouthAsia2017=as.numeric(format(as.Date(colnames(mat_v_SouthAsia2017)), "%m"))
totais_mat_v_SouthAsia2017_meses=matrix(nrow=dim(mat_v_SouthAsia2017)[1],ncol=12) #criando a matriz 1*12
rownames(totais_mat_v_SouthAsia2017_meses)<-rownames(mat_v_SouthAsia2017)
colnames(totais_mat_v_SouthAsia2017_meses)<- c(1:12)

for (i in 1:7){
  for (j in 1:12){
    totais_mat_v_SouthAsia2017_meses[i,j]<-sum(mat_v_SouthAsia2017[i,meses_mat_v_SouthAsia2017==j&anos_mat_v_SouthAsia2017==2017],na.rm=T)
  }
}
##########################################################

# 3.5) Western Pacific (mat_v_WesternPacific, mat_v_WesternPacific_valid, mat_v_WesternPacific2017)

# mat_v_WesternPacific (outubro de 2008 até dezembro de 2016)
mat_v_WesternPacific

anos_mat_v_WesternPacific=as.numeric(format(as.Date(colnames(mat_v_WesternPacific)), "%Y"))
meses_mat_v_WesternPacific=as.numeric(format(as.Date(colnames(mat_v_WesternPacific)), "%m"))
totais_mat_v_WesternPacific_meses=matrix(nrow=dim(mat_v_WesternPacific)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_mat_v_WesternPacific_meses)<-rownames(mat_v_WesternPacific)
colnames(totais_mat_v_WesternPacific_meses)<- c(1:99)

for (i in 1:11){
  totais_mat_v_WesternPacific_meses[i,"1"]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==10&anos_mat_v_WesternPacific==2008],na.rm=T)
  totais_mat_v_WesternPacific_meses[i,"2"]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==11&anos_mat_v_WesternPacific==2008],na.rm=T)
  totais_mat_v_WesternPacific_meses[i,"3"]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==12&anos_mat_v_WesternPacific==2008],na.rm=T)
}

for (i in 1:11){
  for (j in 1:12){
    totais_mat_v_WesternPacific_meses[i,(j+3)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2009],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+15)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2010],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+27)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2011],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+39)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2012],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+51)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2013],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+63)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2014],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+75)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2015],na.rm=T)
    totais_mat_v_WesternPacific_meses[i,(j+87)]<-sum(mat_v_WesternPacific[i,meses_mat_v_WesternPacific==j&anos_mat_v_WesternPacific==2016],na.rm=T)
  }  }
#####

# mat_v_WesternPacific_valid (janeiro de 2018 até novembro de 2019)
mat_v_WesternPacific_valid

anos_mat_v_WesternPacific_valid=as.numeric(format(as.Date(colnames(mat_v_WesternPacific_valid)), "%Y"))
meses_mat_v_WesternPacific_valid=as.numeric(format(as.Date(colnames(mat_v_WesternPacific_valid)), "%m"))
totais_mat_v_WesternPacific_valid_meses=matrix(nrow=dim(mat_v_WesternPacific_valid)[1],ncol=23) #criando a matriz 2*12-1
rownames(totais_mat_v_WesternPacific_valid_meses)<-rownames(mat_v_WesternPacific_valid)
colnames(totais_mat_v_WesternPacific_valid_meses)<- c(1:23)

# 2018
for (i in 1:11){
  for (j in 1:12){
    totais_mat_v_WesternPacific_valid_meses[i,j]<-sum(mat_v_WesternPacific_valid[i,meses_mat_v_WesternPacific_valid==j&anos_mat_v_WesternPacific_valid==2018],na.rm=T)
  } }
# 2019
for (i in 1:11){
  for (j in 1:11){
    totais_mat_v_WesternPacific_valid_meses[i,(j+12)]<-sum(mat_v_WesternPacific_valid[i,meses_mat_v_WesternPacific_valid==j&anos_mat_v_WesternPacific_valid==2019],na.rm=T)
  } }
#####

# mat_v_WesternPacific2017 (janeiro de 2017 até dezembro de 2017)
mat_v_WesternPacific2017

anos_mat_v_WesternPacific2017=as.numeric(format(as.Date(colnames(mat_v_WesternPacific2017)), "%Y"))
meses_mat_v_WesternPacific2017=as.numeric(format(as.Date(colnames(mat_v_WesternPacific2017)), "%m"))
totais_mat_v_WesternPacific2017_meses=matrix(nrow=dim(mat_v_WesternPacific2017)[1],ncol=12) #criando a matriz 1*12
rownames(totais_mat_v_WesternPacific2017_meses)<-rownames(mat_v_WesternPacific2017)
colnames(totais_mat_v_WesternPacific2017_meses)<- c(1:12)

for (i in 1:11){
  for (j in 1:12){
    totais_mat_v_WesternPacific2017_meses[i,j]<-sum(mat_v_WesternPacific2017[i,meses_mat_v_WesternPacific2017==j&anos_mat_v_WesternPacific2017==2017],na.rm=T)
  }
}
###############################################################################################################################

## 1) Criando matrizes mensais com dados de 2017 até 2019: Brasil e North América

# Criando matrizes com número de casos de gripe mais recentes (2017 até 2019), para verificar se o modelo do número de casos dos dados antigos (2008 até 2018), consegue predizer certo essa matriz com número de casos mais recentes
# 150 colunas: 2017-01-02 até 2019-11-11

## Matriz Brasil (semanas)
library(readxl)
teste_brasil <- read_excel("teste_brasil.xlsx")
matriz_BR_2017_semana=as.matrix(teste_brasil$Brasil,nrow=teste_brasil$Data)
colnames(matriz_BR_2017_semana)<-("Brasil")
rownames(matriz_BR_2017_semana)<-c("2017-01-02","2017-01-09","2017-01-16","2017-01-23","2017-01-30",
                                   "2017-02-06","2017-02-13","2017-02-20","2017-02-27","2017-03-06",
                                   "2017-03-13","2017-03-20","2017-03-27","2017-04-03","2017-04-10",
                                   "2017-04-17","2017-04-24","2017-05-01","2017-05-08","2017-05-15",
                                   "2017-05-22","2017-05-29","2017-06-05","2017-06-12","2017-06-19",
                                   "2017-06-26","2017-07-03","2017-07-10","2017-07-17","2017-07-24",
                                   "2017-07-31","2017-08-07","2017-08-14","2017-08-21","2017-08-28",
                                   "2017-09-04","2017-09-11","2017-09-18","2017-09-25","2017-10-02",
                                   "2017-10-09","2017-10-16","2017-10-23","2017-10-30","2017-11-06",
                                   "2017-11-13","2017-11-20","2017-11-27","2017-12-04","2017-12-11",
                                   "2017-12-18","2017-12-25","2018-01-01","2018-01-08","2018-01-15",
                                   "2018-01-22","2018-01-29","2018-02-05","2018-02-12","2018-02-19",
                                   "2018-02-26","2018-03-05","2018-03-12","2018-03-19","2018-03-26",
                                   "2018-04-02","2018-04-09","2018-04-16","2018-04-23","2018-04-30",
                                   "2018-05-07","2018-05-14","2018-05-21","2018-05-28","2018-06-04",
                                   "2018-06-11","2018-06-18","2018-06-25","2018-07-02","2018-07-09",
                                   "2018-07-16","2018-07-23","2018-07-30","2018-08-06","2018-08-13",
                                   "2018-08-20","2018-08-27","2018-09-03","2018-09-10","2018-09-17",
                                   "2018-09-24","2018-10-01","2018-10-08","2018-10-15","2018-10-22",
                                   "2018-10-29","2018-11-05","2018-11-12","2018-11-19","2018-11-26",
                                   "2018-12-03","2018-12-10","2018-12-17","2018-12-24","2018-12-31",
                                   "2019-01-07","2019-01-14","2019-01-21","2019-01-28","2019-02-04",
                                   "2019-02-11","2019-02-18","2019-02-25","2019-03-04","2019-03-11",
                                   "2019-03-18","2019-03-25","2019-04-01","2019-04-08","2019-04-15",
                                   "2019-04-22","2019-04-29","2019-05-06","2019-05-13","2019-05-20",
                                   "2019-05-27","2019-06-03","2019-06-10","2019-06-17","2019-06-24",
                                   "2019-07-01","2019-07-08","2019-07-15","2019-07-22","2019-07-29",
                                   "2019-08-05","2019-08-12","2019-08-19","2019-08-26","2019-09-02",
                                   "2019-09-09","2019-09-16","2019-09-23","2019-09-30","2019-10-07",
                                   "2019-10-14","2019-10-21","2019-10-28","2019-11-04","2019-11-11",
                                   "2019-11-18") #temos as datas do "start date"
matriz_BR_2017_semana=t(matriz_BR_2017_semana) #Vemos que agora a matriz está no mesmo formato da "matrizcont"
matriz_BR_2017_semana=matriz_BR_2017_semana[,-151] #Tirando o star-date 2019-11-18 do Brasil, pois nos outros locais não se tem essa data.
matriz_BR_2017_semana=t(matriz_BR_2017_semana)
rownames(matriz_BR_2017_semana)<-("Brasil")
#####

# Vamos agora agrupar os dados do Brasil em meses (2008 até 2016)
library(readxl)
teste_brasil_completo <- read_excel("teste_brasil_completo.xlsx")

d1_Brasil=as.character(teste_brasil_completo$Country..area.or.territory)
d2_Brasil=as.character(teste_brasil_completo$Start.date)

myData_Brasil=as.data.frame(list('dim1'=teste_brasil_completo$Country..area.or.territory,
                                 'dim2'=teste_brasil_completo$Start.date,
                                 'values'=teste_brasil_completo$Total.number.of.influenza.positive.viruses))

paises_Brasil=sort(unique(as.character(myData_Brasil$dim1)))
datas_Brasil=sort(unique(as.character(myData_Brasil$dim2)))
values_Brasil=as.numeric(as.character(myData_Brasil$values))

myMatrix_Brasil=matrix(nrow=length(paises_Brasil),ncol=length(datas_Brasil))
colnames(myMatrix_Brasil)=datas_Brasil
rownames(myMatrix_Brasil)=paises_Brasil
for (k in 1:dim(myData_Brasil)[1]){
  i=d1_Brasil[k] #país
  j=d2_Brasil[k] #data
  myMatrix_Brasil[i,j]=values_Brasil[k]
  
}
myMatrix_Brasil[myMatrix_Brasil == 0] <- NA #trocar zeros por NA's
matriz_Brasil_semana=myMatrix_Brasil[,40:469] #Deixando com a data de 06/10/2008 até 26/12/2016
matriz_Brasil_semana=t(matriz_Brasil_semana)
rownames(matriz_Brasil_semana)<-("Brasil")
#

anos_matriz_Brasil=as.numeric(format(as.Date(colnames(matriz_Brasil_semana)), "%Y"))
meses_matriz_Brasil=as.numeric(format(as.Date(colnames(matriz_Brasil_semana)), "%m"))
totais_matriz_Brasil_2008_2016_meses=matrix(nrow=dim(matriz_Brasil_semana)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_matriz_Brasil_2008_2016_meses)<-rownames(matriz_Brasil_semana)
colnames(totais_matriz_Brasil_2008_2016_meses)<- c(1:99)
class(matriz_Brasil_semana) <- "numeric" # transformando a matriz de character para numeric

for (i in 1:1){
  totais_matriz_Brasil_2008_2016_meses[i,"1"]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==10&anos_matriz_Brasil==2008],na.rm=T)
  totais_matriz_Brasil_2008_2016_meses[i,"2"]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==11&anos_matriz_Brasil==2008],na.rm=T)
  totais_matriz_Brasil_2008_2016_meses[i,"3"]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==12&anos_matriz_Brasil==2008],na.rm=T)
}

for (i in 1:1){
  for (j in 1:12){
    totais_matriz_Brasil_2008_2016_meses[i,(j+3)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2009],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+15)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2010],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+27)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2011],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+39)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2012],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+51)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2013],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+63)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2014],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+75)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2015],na.rm=T)
    totais_matriz_Brasil_2008_2016_meses[i,(j+87)]<-sum(matriz_Brasil_semana[i,meses_matriz_Brasil==j&anos_matriz_Brasil==2016],na.rm=T)
  }  }
#####

# Vamos agora agrupar os dados do Brasil a partir de 2017 em meses
matriz_BR_2017_semana # dados dos número de casos do BR de janeiro de 2017 até novembro de 2019

anos_matriz_BR_2017_2019=as.numeric(format(as.Date(colnames(matriz_BR_2017_semana)), "%Y"))
meses_matriz_BR_2017_2019=as.numeric(format(as.Date(colnames(matriz_BR_2017_semana)), "%m"))
totais_matriz_BR_2017_2019_meses=matrix(nrow=dim(matriz_BR_2017_semana)[1],ncol=35) #criando a matriz 3*12-1
rownames(totais_matriz_BR_2017_2019_meses)<-rownames(matriz_BR_2017_semana)
colnames(totais_matriz_BR_2017_2019_meses)<- c(1:35)
class(matriz_BR_2017_semana) <- "numeric" # transformando a matriz de character para numeric

# 2017
for (i in 1:1){
  for (j in 1:12){
    totais_matriz_BR_2017_2019_meses[i,j]<-sum(matriz_BR_2017_semana[i,meses_matriz_BR_2017_2019==j&anos_matriz_BR_2017_2019==2017],na.rm=T)
  } }
# 2018
for (i in 1:1){
  for (j in 1:12){
    totais_matriz_BR_2017_2019_meses[i,(j+12)]<-sum(matriz_BR_2017_semana[i,meses_matriz_BR_2017_2019==j&anos_matriz_BR_2017_2019==2018],na.rm=T)
  } }
# 2019
for (i in 1:1){
  for (j in 1:11){
    totais_matriz_BR_2017_2019_meses[i,(j+24)]<-sum(matriz_BR_2017_semana[i,meses_matriz_BR_2017_2019==j&anos_matriz_BR_2017_2019==2019],na.rm=T)
  } }
##########################################################

#Matriz América do Norte (semanas)
teste_AmericaNorte <- read_excel("teste_AmericaNorte.xlsx")

matriz_AmericaNorte_2017_semana=as.matrix(teste_AmericaNorte,nrow=teste_AmericaNorte$Data)
matriz_AmericaNorte_2017_semana=matriz_AmericaNorte_2017_semana[,-1]
colnames(matriz_AmericaNorte_2017_semana)<-c("Canada","Bermuda","United States of America")
rownames(matriz_AmericaNorte_2017_semana)<-c("2017-01-02","2017-01-09","2017-01-16","2017-01-23",
                                             "2017-01-30","2017-02-06","2017-02-13","2017-02-20",
                                             "2017-02-27","2017-03-06","2017-03-13","2017-03-20",
                                             "2017-03-27","2017-04-03","2017-04-10","2017-04-17",
                                             "2017-04-24","2017-05-01","2017-05-08","2017-05-15",
                                             "2017-05-22","2017-05-29","2017-06-05","2017-06-12",
                                             "2017-06-19","2017-06-26","2017-07-03","2017-07-10",
                                             "2017-07-17","2017-07-24","2017-07-31","2017-08-07",
                                             "2017-08-14","2017-08-21","2017-08-28","2017-09-04",
                                             "2017-09-11","2017-09-18","2017-09-25","2017-10-02",
                                             "2017-10-09","2017-10-16","2017-10-23","2017-10-30",
                                             "2017-11-06","2017-11-13","2017-11-20","2017-11-27",
                                             "2017-12-04","2017-12-11","2017-12-18","2017-12-25",
                                             "2018-01-01","2018-01-08","2018-01-15","2018-01-22",
                                             "2018-01-29","2018-02-05","2018-02-12","2018-02-19",
                                             "2018-02-26","2018-03-05","2018-03-12","2018-03-19",
                                             "2018-03-26","2018-04-02","2018-04-09","2018-04-16",
                                             "2018-04-23","2018-04-30","2018-05-07","2018-05-14",
                                             "2018-05-21","2018-05-28","2018-06-04","2018-06-11",
                                             "2018-06-18","2018-06-25","2018-07-02","2018-07-09",
                                             "2018-07-16","2018-07-23","2018-07-30","2018-08-06",
                                             "2018-08-13","2018-08-20","2018-08-27","2018-09-03",
                                             "2018-09-10","2018-09-17","2018-09-24","2018-10-01",
                                             "2018-10-08","2018-10-15","2018-10-22","2018-10-29",
                                             "2018-11-05","2018-11-12","2018-11-19","2018-11-26",
                                             "2018-12-03","2018-12-10","2018-12-17","2018-12-24",
                                             "2018-12-31","2019-01-07","2019-01-14","2019-01-21",
                                             "2019-01-28","2019-02-04","2019-02-11","2019-02-18",
                                             "2019-02-25","2019-03-04","2019-03-11","2019-03-18",
                                             "2019-03-25","2019-04-01","2019-04-08","2019-04-15",
                                             "2019-04-22","2019-04-29","2019-05-06","2019-05-13",
                                             "2019-05-20","2019-05-27","2019-06-03","2019-06-10",
                                             "2019-06-17","2019-06-24","2019-07-01","2019-07-08",
                                             "2019-07-15","2019-07-22","2019-07-29","2019-08-05",
                                             "2019-08-12","2019-08-19","2019-08-26","2019-09-02",
                                             "2019-09-09","2019-09-16","2019-09-23","2019-09-30",
                                             "2019-10-07","2019-10-14","2019-10-21","2019-10-28",
                                             "2019-11-04","2019-11-11") #temos as datas do "start date"
matriz_AmericaNorte_2017_semana=t(matriz_AmericaNorte_2017_semana) #Vemos que agora a matriz está no mesmo formato da "matrizcont"
#####

# Vamos agora agrupar os dados da América do Norte em meses (2008 até 2016)
library(readxl)
teste_AmericaNorte_completo <- read_excel("teste_AmericaNorte_completo.xlsx") # Para esse banco utilizou-se o número de casos dos países das regiões "Temperate South America" e "Tropical South America".

d1_NorthAmerica=as.character(teste_AmericaNorte_completo$Country..area.or.territory)
d2_NorthAmerica=as.character(teste_AmericaNorte_completo$Start.date)

myData_NorthAmerica=as.data.frame(list('dim1'=teste_AmericaNorte_completo$Country..area.or.territory,
                                       'dim2'=teste_AmericaNorte_completo$Start.date,
                                       'values'=teste_AmericaNorte_completo$Total.number.of.influenza.positive.viruses))

paises_NorthAmerica=sort(unique(as.character(myData_NorthAmerica$dim1)))
datas_NorthAmerica=sort(unique(as.character(myData_NorthAmerica$dim2)))
values_NorthAmerica=as.numeric(as.character(myData_NorthAmerica$values))

myMatrix_NorthAmerica=matrix(nrow=length(paises_NorthAmerica),ncol=length(datas_NorthAmerica))
colnames(myMatrix_NorthAmerica)=datas_NorthAmerica
rownames(myMatrix_NorthAmerica)=paises_NorthAmerica
for (k in 1:dim(myData_NorthAmerica)[1]){
  i=d1_NorthAmerica[k] #país
  j=d2_NorthAmerica[k] #data
  myMatrix_NorthAmerica[i,j]=values_NorthAmerica[k]
  
}
myMatrix_NorthAmerica[myMatrix_NorthAmerica == 0] <- NA #trocar zeros por NA's
matriz_AmericaNorte_semana=myMatrix_NorthAmerica[,40:469] #Deixando com a data de 06/10/2008 até 26/12/2016
#

anos_matriz_AmericaNorte=as.numeric(format(as.Date(colnames(matriz_AmericaNorte_semana)), "%Y"))
meses_matriz_AmericaNorte=as.numeric(format(as.Date(colnames(matriz_AmericaNorte_semana)), "%m"))
totais_matriz_AmericaNorte_2008_2016_meses=matrix(nrow=dim(matriz_AmericaNorte_semana)[1],ncol=99) #criando a matriz 8*12+3
rownames(totais_matriz_AmericaNorte_2008_2016_meses)<-rownames(matriz_AmericaNorte_semana)
colnames(totais_matriz_AmericaNorte_2008_2016_meses)<- c(1:99)
class(matriz_AmericaNorte_semana) <- "numeric" # transformando a matriz de character para numeric

for (i in 1:3){
  totais_matriz_AmericaNorte_2008_2016_meses[i,"1"]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==10&anos_matriz_AmericaNorte==2008],na.rm=T)
  totais_matriz_AmericaNorte_2008_2016_meses[i,"2"]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==11&anos_matriz_AmericaNorte==2008],na.rm=T)
  totais_matriz_AmericaNorte_2008_2016_meses[i,"3"]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==12&anos_matriz_AmericaNorte==2008],na.rm=T)
}

for (i in 1:3){
  for (j in 1:12){
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+3)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2009],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+15)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2010],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+27)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2011],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+39)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2012],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+51)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2013],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+63)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2014],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+75)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2015],na.rm=T)
    totais_matriz_AmericaNorte_2008_2016_meses[i,(j+87)]<-sum(matriz_AmericaNorte_semana[i,meses_matriz_AmericaNorte==j&anos_matriz_AmericaNorte==2016],na.rm=T)
  }  }
#####

# Vamos agora agrupar os dados da América do Norte a partir de 2017 em meses
matriz_AmericaNorte_2017_semana # dados dos número de casos do BR de janeiro de 2017 até novembro de 2019

anos_matriz_AmericaNorte_2017_2019=as.numeric(format(as.Date(colnames(matriz_AmericaNorte_2017_semana)), "%Y"))
meses_matriz_AmericaNorte_2017_2019=as.numeric(format(as.Date(colnames(matriz_AmericaNorte_2017_semana)), "%m"))
totais_matriz_AmericaNorte_2017_2019_meses=matrix(nrow=dim(matriz_AmericaNorte_2017_semana)[1],ncol=35) #criando a matriz 3*12-1
rownames(totais_matriz_AmericaNorte_2017_2019_meses)<-rownames(matriz_AmericaNorte_2017_semana)
colnames(totais_matriz_AmericaNorte_2017_2019_meses)<- c(1:35)
class(matriz_AmericaNorte_2017_semana) <- "numeric" # transformando a matriz de character para numeric

# 2017
for (i in 1:3){
  for (j in 1:12){
    totais_matriz_AmericaNorte_2017_2019_meses[i,j]<-sum(matriz_AmericaNorte_2017_semana[i,meses_matriz_AmericaNorte_2017_2019==j&anos_matriz_AmericaNorte_2017_2019==2017],na.rm=T)
  } }
# 2018
for (i in 1:3){
  for (j in 1:12){
    totais_matriz_AmericaNorte_2017_2019_meses[i,(j+12)]<-sum(matriz_AmericaNorte_2017_semana[i,meses_matriz_AmericaNorte_2017_2019==j&anos_matriz_AmericaNorte_2017_2019==2018],na.rm=T)
  } }
# 2019
for (i in 1:3){
  for (j in 1:11){
    totais_matriz_AmericaNorte_2017_2019_meses[i,(j+24)]<-sum(matriz_AmericaNorte_2017_semana[i,meses_matriz_AmericaNorte_2017_2019==j&anos_matriz_AmericaNorte_2017_2019==2019],na.rm=T)
  } }
#########################################################################################

## 2) Agrupar as matrizes mensais em Regioes e nao mais em países (ou seja, somar e juntar número de casos dos EUA com os do Canadá e com os de Bermuda em cada mês) e nomear de Regiao das Américas, assim transformando a matriz (matrizpaisesnortemeses2008 (2008 até 2018) + totais_matriz_AmericaNorte_2017_2019_meses (2017 até 2019)) em apenas um vetor com número de casos mensal da Regiao das Américas (2008 até 2019). Fazer o mesmo para as demais 6 regioes: Brasil, América do Sul, América Central, Europa, South Asia e Western Pacific.

# Regiao das Americas
totais_matriz_AmericaNorte_2017_2019_meses # matriz com número de casos (janeiro de 2017 até novembro de 2019) de cada um dos países da Regiao das Américas
t(apply(totais_matriz_AmericaNorte_2017_2019_meses,2,sum)) # Somando o número de casos de todos os países juntos mensalmente e nomeando de Regiao das Américas

#totais_paisesnortemeses2008
totais_matriz_AmericaNorte_2008_2016_meses

apply(totais_matriz_AmericaNorte_2008_2016_meses[,1:99],2,sum) #Vamos selecionar dados desde 2008 até 2016, pois os dados de 2017 em diante vamos usar os da matriz "totais_matriz_AmericaNorte_2017_2019_meses"

totais_RegiaoAmericaNorte_2008_2019_meses=cbind(t(apply(totais_matriz_AmericaNorte_2008_2016_meses[,1:99],2,sum)),t(apply(totais_matriz_AmericaNorte_2017_2019_meses,2,sum))) # Matriz número de casos Regiao das Américas (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_RegiaoAmericaNorte_2008_2019_meses)<-"RegiaoAmericaNorte"
###

# Brasil
totais_matriz_BR_2017_2019_meses # matriz com número de casos (janeiro de 2017 até novembro de 2019) do Brasil

#totais_paisessulmeses2008["Brazil",] # número de casos no Brazil de janeiro de 2008 até junho de 2018 (126 colunas). - Essa matriz feita anteriormente tem menos número de casos quando comparada com a matriz "totais_matriz_BR_2017_2019_meses"
totais_matriz_Brasil_2008_2016_meses

totais_Brasil_2008_2019_meses=cbind(totais_matriz_Brasil_2008_2016_meses,totais_matriz_BR_2017_2019_meses) # Matriz número de casos Brasil (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_Brasil_2008_2019_meses)<-"Brasil"
###

# America do Sul
totais_mat_v_SouthAmerica_meses #outubro de 2008 até dezembro de 2016
totais_mat_v_SouthAmerica_valid_meses # janeiro de 2018 até novembro de 2019
totais_mat_v_SouthAmerica2017_meses #janeiro de 2017 até dezembro de 2017

t(apply(totais_mat_v_SouthAmerica_meses,2,sum))
t(apply(totais_mat_v_SouthAmerica_valid_meses,2,sum))
t(apply(totais_mat_v_SouthAmerica2017_meses,2,sum))

totais_RegiaoAmericaSul_2008_2019_meses=cbind(t(apply(totais_mat_v_SouthAmerica_meses,2,sum)),t(apply(totais_mat_v_SouthAmerica2017_meses,2,sum)),t(apply(totais_mat_v_SouthAmerica_valid_meses,2,sum))) # Matriz número de casos Regiao das América do Sul (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_RegiaoAmericaSul_2008_2019_meses)<-"RegiaoAmericaSul"
###

# America Central
totais_mat_v_CentralAmerica_meses #outubro de 2008 até dezembro de 2016
totais_mat_v_CentralAmerica_valid_meses # janeiro de 2018 até novembro de 2019
totais_mat_v_CentralAmerica2017_meses #janeiro de 2017 até dezembro de 2017

t(apply(totais_mat_v_CentralAmerica_meses,2,sum))
t(apply(totais_mat_v_CentralAmerica_valid_meses,2,sum))
t(apply(totais_mat_v_CentralAmerica2017_meses,2,sum))

totais_RegiaoAmericaCentral_2008_2019_meses=cbind(t(apply(totais_mat_v_CentralAmerica_meses,2,sum)),t(apply(totais_mat_v_CentralAmerica2017_meses,2,sum)),t(apply(totais_mat_v_CentralAmerica_valid_meses,2,sum))) # Matriz número de casos Regiao das América Central (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_RegiaoAmericaCentral_2008_2019_meses)<-"RegiaoAmericaCentral"
###

# Europa
totais_mat_v_ordenada_meses #outubro de 2008 até dezembro de 2016
totais_mat_v_europa_valid_meses # janeiro de 2018 até novembro de 2019
totais_mat_v_europa2017_meses #janeiro de 2017 até dezembro de 2017

t(apply(totais_mat_v_ordenada_meses,2,sum))
t(apply(totais_mat_v_europa_valid_meses,2,sum))
t(apply(totais_mat_v_europa2017_meses,2,sum))

totais_RegiaoEuropeia_2008_2019_meses=cbind(t(apply(totais_mat_v_ordenada_meses,2,sum)),t(apply(totais_mat_v_europa2017_meses,2,sum)),t(apply(totais_mat_v_europa_valid_meses,2,sum))) # Matriz número de casos Regiao Europeia (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_RegiaoEuropeia_2008_2019_meses)<-"RegiaoEuropeia"
###

# South Asia
totais_mat_v_SouthAsia_meses #outubro de 2008 até dezembro de 2016
totais_mat_v_SouthAsia_valid_meses # janeiro de 2018 até novembro de 2019
totais_mat_v_SouthAsia2017_meses #janeiro de 2017 até dezembro de 2017

t(apply(totais_mat_v_SouthAsia_meses,2,sum))
t(apply(totais_mat_v_SouthAsia_valid_meses,2,sum))
t(apply(totais_mat_v_SouthAsia2017_meses,2,sum))

totais_RegiaoSouthAsia_2008_2019_meses=cbind(t(apply(totais_mat_v_SouthAsia_meses,2,sum)),t(apply(totais_mat_v_SouthAsia2017_meses,2,sum)),t(apply(totais_mat_v_SouthAsia_valid_meses,2,sum))) # Matriz número de casos Regiao South Asia (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_RegiaoSouthAsia_2008_2019_meses)<-"RegiaoSouthAsia"
###

# Western Pacific
totais_mat_v_WesternPacific_meses #outubro de 2008 até dezembro de 2016
totais_mat_v_WesternPacific_valid_meses # janeiro de 2018 até novembro de 2019
totais_mat_v_WesternPacific2017_meses #janeiro de 2017 até dezembro de 2017

t(apply(totais_mat_v_WesternPacific_meses,2,sum))
t(apply(totais_mat_v_WesternPacific_valid_meses,2,sum))
t(apply(totais_mat_v_WesternPacific2017_meses,2,sum))

totais_RegiaoWesternPacific_2008_2019_meses=cbind(t(apply(totais_mat_v_WesternPacific_meses,2,sum)),t(apply(totais_mat_v_WesternPacific2017_meses,2,sum)),t(apply(totais_mat_v_WesternPacific_valid_meses,2,sum))) # Matriz número de casos Regiao Western Pacific (outubro de 2008 até novembro de 2019- 134 meses)
rownames(totais_RegiaoWesternPacific_2008_2019_meses)<-"RegiaoWesternPacific"
#########################################################################################

## 3) Fazer análise de regressao com defasagem através das matrizes dos números de casos das 7 regioes (todas regioes tem número de casos desde outubro de 2008 até novembro de 2019). Para isso fazer a análise de regressao com defasagens com os dados desde outubro de 2008 até dezembro de 2018 e guardar os dados de 2019 para que quando feita a previsao, comparar os verdadeiros valores de 2019 com os valores da previsao.
# 3.0) Separar dados em duas diferentes matrizes: uma que contenha dados desde outubro de 2008 até dezembro de 2018 e outra que contenha dados desde janeiro de 2019 até novembro de 2019 (além disso cada matriz deve contar as 7 regioes)

# Separando matrizes Regiao America Norte
totais_RegiaoAmericaNorte_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_RegiaoAmericaNorte_analise=totais_RegiaoAmericaNorte_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_RegiaoAmericaNorte_analise=t(totais_RegiaoAmericaNorte_analise)
rownames(totais_RegiaoAmericaNorte_analise)<-"RegiaoAmericaNorte"

totais_RegiaoAmericaNorte_2019=totais_RegiaoAmericaNorte_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_RegiaoAmericaNorte_2019=t(totais_RegiaoAmericaNorte_2019)
rownames(totais_RegiaoAmericaNorte_2019)<-"RegiaoAmericaNorte"
###

# Separando matrizes Brasil
totais_Brasil_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_Brasil_analise=totais_Brasil_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_Brasil_analise=t(totais_Brasil_analise)
rownames(totais_Brasil_analise)<-"Brasil"

totais_Brasil_2019=totais_Brasil_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_Brasil_2019=t(totais_Brasil_2019)
rownames(totais_Brasil_2019)<-"Brasil"
###

# Separando matrizes Regiao America Sul
totais_RegiaoAmericaSul_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_RegiaoAmericaSul_analise=totais_RegiaoAmericaSul_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_RegiaoAmericaSul_analise=t(totais_RegiaoAmericaSul_analise)
rownames(totais_RegiaoAmericaSul_analise)<-"RegiaoAmericaSul"

totais_RegiaoAmericaSul_2019=totais_RegiaoAmericaSul_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_RegiaoAmericaSul_2019=t(totais_RegiaoAmericaSul_2019)
rownames(totais_RegiaoAmericaSul_2019)<-"RegiaoAmericaSul"
###

# Separando matrizes Regiao America Central
totais_RegiaoAmericaCentral_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_RegiaoAmericaCentral_analise=totais_RegiaoAmericaCentral_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_RegiaoAmericaCentral_analise=t(totais_RegiaoAmericaCentral_analise)
rownames(totais_RegiaoAmericaCentral_analise)<-"RegiaoAmericaCentral"

totais_RegiaoAmericaCentral_2019=totais_RegiaoAmericaCentral_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_RegiaoAmericaCentral_2019=t(totais_RegiaoAmericaCentral_2019)
rownames(totais_RegiaoAmericaCentral_2019)<-"RegiaoAmericaCentral"
###

# Separando matrizes Regiao Europeia
totais_RegiaoEuropeia_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_RegiaoEuropeia_analise=totais_RegiaoEuropeia_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_RegiaoEuropeia_analise=t(totais_RegiaoEuropeia_analise)
rownames(totais_RegiaoEuropeia_analise)<-"RegiaoEuropeia"

totais_RegiaoEuropeia_2019=totais_RegiaoEuropeia_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_RegiaoEuropeia_2019=t(totais_RegiaoEuropeia_2019)
rownames(totais_RegiaoEuropeia_2019)<-"RegiaoEuropeia"
###

# Separando matrizes Regiao South Asia
totais_RegiaoSouthAsia_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_RegiaoSouthAsia_analise=totais_RegiaoSouthAsia_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_RegiaoSouthAsia_analise=t(totais_RegiaoSouthAsia_analise)
rownames(totais_RegiaoSouthAsia_analise)<-"RegiaoSouthAsia"

totais_RegiaoSouthAsia_2019=totais_RegiaoSouthAsia_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_RegiaoSouthAsia_2019=t(totais_RegiaoSouthAsia_2019)
rownames(totais_RegiaoSouthAsia_2019)<-"RegiaoSouthAsia"
###

# Separando matrizes Regiao Western Pacific
totais_RegiaoWesternPacific_2008_2019_meses # 134 meses (outubro de 2008 até novembro de 2019)

totais_RegiaoWesternPacific_analise=totais_RegiaoWesternPacific_2008_2019_meses[,1:123] #outubro de 2008 até dezembro de 2018- vai ser utilizada para fazer a análise
totais_RegiaoWesternPacific_analise=t(totais_RegiaoWesternPacific_analise)
rownames(totais_RegiaoWesternPacific_analise)<-"RegiaoWesternPacific"

totais_RegiaoWesternPacific_2019=totais_RegiaoWesternPacific_2008_2019_meses[,124:134] #janeiro de 2019 até novembro de 2019- vai ser utilizada para comparar com os dados da previsao
totais_RegiaoWesternPacific_2019=t(totais_RegiaoWesternPacific_2019)
rownames(totais_RegiaoWesternPacific_2019)<-"RegiaoWesternPacific"
###

# Matrizes finais
matriz_ncont_analise=rbind(totais_RegiaoAmericaNorte_analise,totais_Brasil_analise,totais_RegiaoAmericaSul_analise,totais_RegiaoAmericaCentral_analise,totais_RegiaoEuropeia_analise,totais_RegiaoSouthAsia_analise,totais_RegiaoWesternPacific_analise) #matriz com número de casos de outubro de 2008 até dezembro de 2018 das diferentes 7 regioes
matriz_ncont_2019=rbind(totais_RegiaoAmericaNorte_2019,totais_Brasil_2019,totais_RegiaoAmericaSul_2019,totais_RegiaoAmericaCentral_2019,totais_RegiaoEuropeia_2019,totais_RegiaoSouthAsia_2019,totais_RegiaoWesternPacific_2019)  # matriz com número de casos de janeiro de 2019 até novembro de 2019 das diferentes 7 regioes
##########################################

# 3.1) Montar 7 matrizes (uma para cada regiao), onde cada uma tem 12 defasagens.

# Defasagens Região America Norte
View(matriz_ncont_analise["RegiaoAmericaNorte",]) #matriz completa tamanho 123- outubro de 2008 até dezembro de 2018

RegiaoAmericaNorte_defasagem_12=rbind(matriz_ncont_analise["RegiaoAmericaNorte",13:123],matriz_ncont_analise["RegiaoAmericaNorte",12:122],matriz_ncont_analise["RegiaoAmericaNorte",11:121],matriz_ncont_analise["RegiaoAmericaNorte",10:120],matriz_ncont_analise["RegiaoAmericaNorte",9:119],matriz_ncont_analise["RegiaoAmericaNorte",8:118],matriz_ncont_analise["RegiaoAmericaNorte",7:117],matriz_ncont_analise["RegiaoAmericaNorte",6:116],matriz_ncont_analise["RegiaoAmericaNorte",5:115],matriz_ncont_analise["RegiaoAmericaNorte",4:114],matriz_ncont_analise["RegiaoAmericaNorte",3:113],matriz_ncont_analise["RegiaoAmericaNorte",2:112],matriz_ncont_analise["RegiaoAmericaNorte",1:111])
rownames(RegiaoAmericaNorte_defasagem_12)<-c("RegiaoAmericaNorte_At", "RegiaoAmericaNorte_At1", "RegiaoAmericaNorte_At2", "RegiaoAmericaNorte_At3", "RegiaoAmericaNorte_At4", "RegiaoAmericaNorte_At5", "RegiaoAmericaNorte_At6", "RegiaoAmericaNorte_At7", "RegiaoAmericaNorte_At8", "RegiaoAmericaNorte_At9", "RegiaoAmericaNorte_At10", "RegiaoAmericaNorte_At11", "RegiaoAmericaNorte_At12") #nomeando as linhas

View(RegiaoAmericaNorte_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###

# Defasagens Brasil
Brasil_defasagem_12=rbind(matriz_ncont_analise["Brasil",13:123],matriz_ncont_analise["Brasil",12:122],matriz_ncont_analise["Brasil",11:121],matriz_ncont_analise["Brasil",10:120],matriz_ncont_analise["Brasil",9:119],matriz_ncont_analise["Brasil",8:118],matriz_ncont_analise["Brasil",7:117],matriz_ncont_analise["Brasil",6:116],matriz_ncont_analise["Brasil",5:115],matriz_ncont_analise["Brasil",4:114],matriz_ncont_analise["Brasil",3:113],matriz_ncont_analise["Brasil",2:112],matriz_ncont_analise["Brasil",1:111])
rownames(Brasil_defasagem_12)<-c("Brasil_Bt", "Brasil_Bt1", "Brasil_Bt2", "Brasil_Bt3", "Brasil_Bt4", "Brasil_Bt5", "Brasil_Bt6", "Brasil_Bt7", "Brasil_Bt8", "Brasil_Bt9", "Brasil_Bt10", "Brasil_Bt11", "Brasil_Bt12") #nomeando as linhas

View(Brasil_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###

# Defasagens Regiao America Sul
RegiaoAmericaSul_defasagem_12=rbind(matriz_ncont_analise["RegiaoAmericaSul",13:123],matriz_ncont_analise["RegiaoAmericaSul",12:122],matriz_ncont_analise["RegiaoAmericaSul",11:121],matriz_ncont_analise["RegiaoAmericaSul",10:120],matriz_ncont_analise["RegiaoAmericaSul",9:119],matriz_ncont_analise["RegiaoAmericaSul",8:118],matriz_ncont_analise["RegiaoAmericaSul",7:117],matriz_ncont_analise["RegiaoAmericaSul",6:116],matriz_ncont_analise["RegiaoAmericaSul",5:115],matriz_ncont_analise["RegiaoAmericaSul",4:114],matriz_ncont_analise["RegiaoAmericaSul",3:113],matriz_ncont_analise["RegiaoAmericaSul",2:112],matriz_ncont_analise["RegiaoAmericaSul",1:111])
rownames(RegiaoAmericaSul_defasagem_12)<-c("RegiaoAmericaSul_St", "RegiaoAmericaSul_St1", "RegiaoAmericaSul_St2", "RegiaoAmericaSul_St3", "RegiaoAmericaSul_St4", "RegiaoAmericaSul_St5", "RegiaoAmericaSul_St6", "RegiaoAmericaSul_St7", "RegiaoAmericaSul_St8", "RegiaoAmericaSul_St9", "RegiaoAmericaSul_St10", "RegiaoAmericaSul_St11", "RegiaoAmericaSul_St12") #nomeando as linhas

View(RegiaoAmericaSul_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###

# Defasagens Regiao America Central
RegiaoAmericaCentral_defasagem_12=rbind(matriz_ncont_analise["RegiaoAmericaCentral",13:123],matriz_ncont_analise["RegiaoAmericaCentral",12:122],matriz_ncont_analise["RegiaoAmericaCentral",11:121],matriz_ncont_analise["RegiaoAmericaCentral",10:120],matriz_ncont_analise["RegiaoAmericaCentral",9:119],matriz_ncont_analise["RegiaoAmericaCentral",8:118],matriz_ncont_analise["RegiaoAmericaCentral",7:117],matriz_ncont_analise["RegiaoAmericaCentral",6:116],matriz_ncont_analise["RegiaoAmericaCentral",5:115],matriz_ncont_analise["RegiaoAmericaCentral",4:114],matriz_ncont_analise["RegiaoAmericaCentral",3:113],matriz_ncont_analise["RegiaoAmericaCentral",2:112],matriz_ncont_analise["RegiaoAmericaCentral",1:111])
rownames(RegiaoAmericaCentral_defasagem_12)<-c("RegiaoAmericaCentral_Ct", "RegiaoAmericaCentral_Ct1", "RegiaoAmericaCentral_Ct2", "RegiaoAmericaCentral_Ct3", "RegiaoAmericaCentral_Ct4", "RegiaoAmericaCentral_Ct5", "RegiaoAmericaCentral_Ct6", "RegiaoAmericaCentral_Ct7", "RegiaoAmericaCentral_Ct8", "RegiaoAmericaCentral_Ct9", "RegiaoAmericaCentral_Ct10", "RegiaoAmericaCentral_Ct11", "RegiaoAmericaCentral_Ct12") #nomeando as linhas

View(RegiaoAmericaCentral_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###

# Defasagens Regiao Europeia
RegiaoEuropeia_defasagem_12=rbind(matriz_ncont_analise["RegiaoEuropeia",13:123],matriz_ncont_analise["RegiaoEuropeia",12:122],matriz_ncont_analise["RegiaoEuropeia",11:121],matriz_ncont_analise["RegiaoEuropeia",10:120],matriz_ncont_analise["RegiaoEuropeia",9:119],matriz_ncont_analise["RegiaoEuropeia",8:118],matriz_ncont_analise["RegiaoEuropeia",7:117],matriz_ncont_analise["RegiaoEuropeia",6:116],matriz_ncont_analise["RegiaoEuropeia",5:115],matriz_ncont_analise["RegiaoEuropeia",4:114],matriz_ncont_analise["RegiaoEuropeia",3:113],matriz_ncont_analise["RegiaoEuropeia",2:112],matriz_ncont_analise["RegiaoEuropeia",1:111])
rownames(RegiaoEuropeia_defasagem_12)<-c("RegiaoEuropeia_Et", "RegiaoEuropeia_Et1", "RegiaoEuropeia_Et2", "RegiaoEuropeia_Et3", "RegiaoEuropeia_Et4", "RegiaoEuropeia_Et5", "RegiaoEuropeia_Et6", "RegiaoEuropeia_Et7", "RegiaoEuropeia_Et8", "RegiaoEuropeia_Et9", "RegiaoEuropeia_Et10", "RegiaoEuropeia_Et11", "RegiaoEuropeia_Et12") #nomeando as linhas

View(RegiaoEuropeia_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###

# Defasagens Regiao South Asia
RegiaoSouthAsia_defasagem_12=rbind(matriz_ncont_analise["RegiaoSouthAsia",13:123],matriz_ncont_analise["RegiaoSouthAsia",12:122],matriz_ncont_analise["RegiaoSouthAsia",11:121],matriz_ncont_analise["RegiaoSouthAsia",10:120],matriz_ncont_analise["RegiaoSouthAsia",9:119],matriz_ncont_analise["RegiaoSouthAsia",8:118],matriz_ncont_analise["RegiaoSouthAsia",7:117],matriz_ncont_analise["RegiaoSouthAsia",6:116],matriz_ncont_analise["RegiaoSouthAsia",5:115],matriz_ncont_analise["RegiaoSouthAsia",4:114],matriz_ncont_analise["RegiaoSouthAsia",3:113],matriz_ncont_analise["RegiaoSouthAsia",2:112],matriz_ncont_analise["RegiaoSouthAsia",1:111])
rownames(RegiaoSouthAsia_defasagem_12)<-c("RegiaoSouthAsia_st", "RegiaoSouthAsia_st1", "RegiaoSouthAsia_st2", "RegiaoSouthAsia_st3", "RegiaoSouthAsia_st4", "RegiaoSouthAsia_st5", "RegiaoSouthAsia_st6", "RegiaoSouthAsia_st7", "RegiaoSouthAsia_st8", "RegiaoSouthAsia_st9", "RegiaoSouthAsia_st10", "RegiaoSouthAsia_st11", "RegiaoSouthAsia_st12") #nomeando as linhas

View(RegiaoSouthAsia_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###

# Defasagens Regiao Western Pacific
RegiaoWesternPacific_defasagem_12=rbind(matriz_ncont_analise["RegiaoWesternPacific",13:123],matriz_ncont_analise["RegiaoWesternPacific",12:122],matriz_ncont_analise["RegiaoWesternPacific",11:121],matriz_ncont_analise["RegiaoWesternPacific",10:120],matriz_ncont_analise["RegiaoWesternPacific",9:119],matriz_ncont_analise["RegiaoWesternPacific",8:118],matriz_ncont_analise["RegiaoWesternPacific",7:117],matriz_ncont_analise["RegiaoWesternPacific",6:116],matriz_ncont_analise["RegiaoWesternPacific",5:115],matriz_ncont_analise["RegiaoWesternPacific",4:114],matriz_ncont_analise["RegiaoWesternPacific",3:113],matriz_ncont_analise["RegiaoWesternPacific",2:112],matriz_ncont_analise["RegiaoWesternPacific",1:111])
rownames(RegiaoWesternPacific_defasagem_12)<-c("RegiaoWesternPacific_Wt", "RegiaoWesternPacific_Wt1", "RegiaoWesternPacific_Wt2", "RegiaoWesternPacific_Wt3", "RegiaoWesternPacific_Wt4", "RegiaoWesternPacific_Wt5", "RegiaoWesternPacific_Wt6", "RegiaoWesternPacific_Wt7", "RegiaoWesternPacific_Wt8", "RegiaoWesternPacific_Wt9", "RegiaoWesternPacific_Wt10", "RegiaoWesternPacific_Wt11", "RegiaoWesternPacific_Wt12") #nomeando as linhas

View(RegiaoWesternPacific_defasagem_12)  # Lembrando que a matriz original tinha 123 meses (outubro de 2008 até dezembro de 2018) e essa matriz de intersecção tem 111 meses, onde at (outubro de 2009 até dezembro de 2018), at-1 (setembro de 2009 até novembro de 2018), at-2 (agosto de 2009 até outubro de 2018), at-3 (julho de 2009 até setembro de 2018), at-4 (junho de 2009 até agosto de 2018), at-5 (maio de 2009 até julho de 2018),  at-6 (abril de 2009 até junho de 2018), at-7 (marco de 2009 até maio de 2018), at-8 (fevereiro de 2009 até abril de 2018), at-9 (janeiro de 2009 até marco de 2018), at-10 (dezembro de 2008 até fevereiro de 2018), at-11 (novembro de 2008 até janeiro de 2018) e at-12 (outubro de 2008 até dezembro de 2017).
###############################

## Montando matriz incidência defasagens
matriz_ncasos_defasagens=rbind(RegiaoAmericaNorte_defasagem_12,Brasil_defasagem_12,RegiaoAmericaSul_defasagem_12,RegiaoAmericaCentral_defasagem_12,RegiaoEuropeia_defasagem_12,RegiaoSouthAsia_defasagem_12,RegiaoWesternPacific_defasagem_12)
matriz_ncasos_defasagens=t(matriz_ncasos_defasagens)
matriz_ncasos_defasagens=as.data.frame(matriz_ncasos_defasagens)
rownames(matriz_ncasos_defasagens)=1:111
matriz_ncasos_defasagens=as.data.frame(matriz_ncasos_defasagens)
attach(matriz_ncasos_defasagens)

## Montando matriz predição incidência
# Criando matrix_pred: Nessa matriz vamos preencher os dados de 2019 de acordo com a média do mês nos anos anteriores (exemplo, o número de casos de janeiro de 2019 da europa será a média do meses de janeiro da europa dos anos anteriores). Além disso vamos colocar os dados existentes de 2018 (que serão necessários por causa das defasagens), por exemplo, na coluna da variável "Brasil_Bt1" teremos as 11 linhas indo desde dezembro de 2018 até outubro de 2010 (pois é apenas 1 defasagem), já a variável "Brasil_Bt2" teremos as 11 linhas indo desde novembro de 2018 até setembro de 2010 (pois são 2 defasagens) e assim por diante.
matrix_pred=matrix(ncol=12, nrow=11)
colnames(matrix_pred)=c("Brasil_Bt1","RegiaoEuropeia_Et1","Brasil_Bt2","RegiaoEuropeia_Et2","RegiaoAmericaNorte_At2","Brasil_Bt3","RegiaoEuropeia_Et3","RegiaoAmericaCentral_Ct3","RegiaoAmericaSul_St3","RegiaoEuropeia_Et4","RegiaoAmericaNorte_At4","RegiaoWesternPacific_Wt7")
matrix_pred[1,"Brasil_Bt1"]=matriz_ncont_analise["Brasil",123] # dezembro de 2018 do Brasil
matrix_pred[1,"RegiaoEuropeia_Et1"]=matriz_ncont_analise["RegiaoEuropeia",123] #dezembro de 2018 da Regiao Europeia
matrix_pred[1:2,"Brasil_Bt2"]=matriz_ncont_analise["Brasil",122:123] # novembro e dezembro de 2018 do Brasil
matrix_pred[1:2,"RegiaoEuropeia_Et2"]=matriz_ncont_analise["RegiaoEuropeia",122:123] # novembro e dezembro de 2018 da Regiao Europeia
matrix_pred[1:2,"RegiaoAmericaNorte_At2"]=matriz_ncont_analise["RegiaoAmericaNorte",122:123] # novembro e dezembro de 2018 da Regiao America Norte
matrix_pred[1:3,"Brasil_Bt3"]=matriz_ncont_analise["Brasil",121:123] # outubro até dezembro de 2018 do Brasil
matrix_pred[1:3,"RegiaoEuropeia_Et3"]=matriz_ncont_analise["RegiaoEuropeia",121:123] # outubro até dezembro de 2018 da Regiao Europeia
matrix_pred[1:3,"RegiaoAmericaCentral_Ct3"]=matriz_ncont_analise["RegiaoAmericaCentral",121:123] # outubro até dezembro de 2018 da Regiao America Central
matrix_pred[1:3,"RegiaoAmericaSul_St3"]=matriz_ncont_analise["RegiaoAmericaSul",121:123] # outubro até dezembro de 2018 da Regiao America do Sul
matrix_pred[1:4,"RegiaoEuropeia_Et4"]=matriz_ncont_analise["RegiaoEuropeia",120:123] # setembro até dezembro de 2018 da Regiao Europeia
matrix_pred[1:4,"RegiaoAmericaNorte_At4"]=matriz_ncont_analise["RegiaoAmericaNorte",120:123] # setembro até dezembro de 2018 da Regiao America Norte
matrix_pred[1:7,"RegiaoWesternPacific_Wt7"]=matriz_ncont_analise["RegiaoWesternPacific",117:123] # junho até dezembro de 2018 da Regiao Western Pacific
# Vamos ver as médias de cada região em cada mês:
medias_janeiro=medias_fevereiro=medias_marco=medias_abril=medias_maio=medias_junho=medias_julho=medias_agosto=medias_setembro=medias_outubro=medias_novembro=medias_dezembro=numeric(7)
for (i in 1:7){ # média de cada região em janeiro, fevereiro,.., dezembro (2008 até 2018- mas não tem janeiro de 2008, logo média dos janeiros de 2009 até 2018)
  medias_janeiro[i]=mean(matriz_ncont_analise[i,c(4,16,28,40,52,64,76,88,100,112)]) # (2008 até 2018- mas não tem janeiro de 2008, logo média dos janeiros de 2009 até 2018)
  medias_fevereiro[i]=mean(matriz_ncont_analise[i,c(5,17,29,41,53,65,77,89,101,113)])
  medias_marco[i]=mean(matriz_ncont_analise[i,c(6,18,30,42,54,66,78,90,102,114)])
  medias_abril[i]=mean(matriz_ncont_analise[i,c(7,19,31,43,55,67,79,91,103,115)])
  medias_maio[i]=mean(matriz_ncont_analise[i,c(8,20,32,44,56,68,80,92,104,116)])
  medias_junho[i]=mean(matriz_ncont_analise[i,c(9,21,33,45,57,69,81,93,105,117)])
  medias_julho[i]=mean(matriz_ncont_analise[i,c(10,22,34,46,58,70,82,94,106,118)])
  medias_agosto[i]=mean(matriz_ncont_analise[i,c(11,23,35,47,59,71,83,95,107,119)])
  medias_setembro[i]=mean(matriz_ncont_analise[i,c(12,24,36,48,60,72,84,96,108,120)])
  medias_outubro[i]=mean(matriz_ncont_analise[i,c(1,13,25,37,49,61,73,85,97,109,121)])
  medias_novembro[i]=mean(matriz_ncont_analise[i,c(2,14,26,38,50,62,74,86,98,110,122)])
  medias_dezembro[i]=mean(matriz_ncont_analise[i,c(3,15,27,39,51,63,75,87,99,111,123)])
}
names(medias_janeiro)=names(medias_fevereiro)=names(medias_marco)=names(medias_abril)=names(medias_maio)=names(medias_junho)=names(medias_julho)=names(medias_agosto)=names(medias_setembro)=names(medias_outubro)=names(medias_novembro)=names(medias_dezembro)=rownames(matriz_ncont_analise)

# Agora vamos preencher os dados de 2019 de acordo com a média do mês nos anos anteriores:
matrix_pred[2:11,"Brasil_Bt1"]=c(medias_janeiro["Brasil"],medias_fevereiro["Brasil"],medias_marco["Brasil"],medias_abril["Brasil"],medias_maio["Brasil"],medias_junho["Brasil"],medias_julho["Brasil"],medias_agosto["Brasil"],medias_setembro["Brasil"],medias_outubro["Brasil"]) #completando os dados de 2019 do Brasil (cada mês de 2019 é feito de acordo com a média do Brasil desse mesmo mês nos anos anteriores, de 2008 até 2018). Ou seja, o valor do Brasil de janeiro de 2019 será a média dos janeiros desde 2008 até 2018 do Brasil
matrix_pred[2:11,"RegiaoEuropeia_Et1"]=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"],medias_abril["RegiaoEuropeia"],medias_maio["RegiaoEuropeia"],medias_junho["RegiaoEuropeia"],medias_julho["RegiaoEuropeia"],medias_agosto["RegiaoEuropeia"],medias_setembro["RegiaoEuropeia"],medias_outubro["RegiaoEuropeia"])
matrix_pred[3:11,"Brasil_Bt2"]=c(medias_janeiro["Brasil"],medias_fevereiro["Brasil"],medias_marco["Brasil"],medias_abril["Brasil"],medias_maio["Brasil"],medias_junho["Brasil"],medias_julho["Brasil"],medias_agosto["Brasil"],medias_setembro["Brasil"])
matrix_pred[3:11,"RegiaoEuropeia_Et2"]=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"],medias_abril["RegiaoEuropeia"],medias_maio["RegiaoEuropeia"],medias_junho["RegiaoEuropeia"],medias_julho["RegiaoEuropeia"],medias_agosto["RegiaoEuropeia"],medias_setembro["RegiaoEuropeia"])
matrix_pred[3:11,"RegiaoAmericaNorte_At2"]=c(medias_janeiro["RegiaoAmericaNorte"],medias_fevereiro["RegiaoAmericaNorte"],medias_marco["RegiaoAmericaNorte"],medias_abril["RegiaoAmericaNorte"],medias_maio["RegiaoAmericaNorte"],medias_junho["RegiaoAmericaNorte"],medias_julho["RegiaoAmericaNorte"],medias_agosto["RegiaoAmericaNorte"],medias_setembro["RegiaoAmericaNorte"])
matrix_pred[4:11,"Brasil_Bt3"]=c(medias_janeiro["Brasil"],medias_fevereiro["Brasil"],medias_marco["Brasil"],medias_abril["Brasil"],medias_maio["Brasil"],medias_junho["Brasil"],medias_julho["Brasil"],medias_agosto["Brasil"])
matrix_pred[4:11,"RegiaoEuropeia_Et3"]=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"],medias_abril["RegiaoEuropeia"],medias_maio["RegiaoEuropeia"],medias_junho["RegiaoEuropeia"],medias_julho["RegiaoEuropeia"],medias_agosto["RegiaoEuropeia"])
matrix_pred[4:11,"RegiaoAmericaCentral_Ct3"]=c(medias_janeiro["RegiaoAmericaCentral"],medias_fevereiro["RegiaoAmericaCentral"],medias_marco["RegiaoAmericaCentral"],medias_abril["RegiaoAmericaCentral"],medias_maio["RegiaoAmericaCentral"],medias_junho["RegiaoAmericaCentral"],medias_julho["RegiaoAmericaCentral"],medias_agosto["RegiaoAmericaCentral"])
matrix_pred[4:11,"RegiaoAmericaSul_St3"]=c(medias_janeiro["RegiaoAmericaSul"],medias_fevereiro["RegiaoAmericaSul"],medias_marco["RegiaoAmericaSul"],medias_abril["RegiaoAmericaSul"],medias_maio["RegiaoAmericaSul"],medias_junho["RegiaoAmericaSul"],medias_julho["RegiaoAmericaSul"],medias_agosto["RegiaoAmericaSul"])
matrix_pred[5:11,"RegiaoEuropeia_Et4"]=c(medias_janeiro["RegiaoEuropeia"],medias_fevereiro["RegiaoEuropeia"],medias_marco["RegiaoEuropeia"],medias_abril["RegiaoEuropeia"],medias_maio["RegiaoEuropeia"],medias_junho["RegiaoEuropeia"],medias_julho["RegiaoEuropeia"])
matrix_pred[5:11,"RegiaoAmericaNorte_At4"]=c(medias_janeiro["RegiaoAmericaNorte"],medias_fevereiro["RegiaoAmericaNorte"],medias_marco["RegiaoAmericaNorte"],medias_abril["RegiaoAmericaNorte"],medias_maio["RegiaoAmericaNorte"],medias_junho["RegiaoAmericaNorte"],medias_julho["RegiaoAmericaNorte"])
matrix_pred[8:11,"RegiaoWesternPacific_Wt7"]=c(medias_janeiro["RegiaoWesternPacific"],medias_fevereiro["RegiaoWesternPacific"],medias_marco["RegiaoWesternPacific"],medias_abril["RegiaoWesternPacific"])
matrix_pred

matriz_mediasBR_2008_2018=c(127,85.90909,67.45455,rep(c(111.3,133.9,333.3,551.5,559,559.8,348.3,168.5,142.6,127,85.90909,67.45455),10)) #seria de outubro de 2008 até dezembro de 2018, mas como temos que colocar na matriz com defasagens (ia até a defasagem 12, ams selecionamos as colunas até a defasagem 11), então perdemos
mediasBR_formatodefasagem=c(127,85.90909,67.45455,rep(c(111.3,133.9,333.3,551.5,559,559.8,348.3,168.5,142.6,127,85.90909,67.45455),9)) # outubro de 2009 até dezembro de 2018 (por causa das defasagens)

matriz_ncont_analise_mediasBR=cbind(matriz_mediasBR_2008_2018,t(matriz_ncont_analise))

## Montando matrizes com defasagens e com médias mensais do Brasil
vetor_=c(10,11,12,rep(1:12,10)) # Criando vetor dummy: outubro de 2008 até dezembro de 2018 (dados da análise)
abc=model.matrix(~as.factor(vetor_))
abc1=abc[,-1] # Vemos que não há o mês de janeiro nessa matrix pois na regressão janeiro é o mês de referência
colnames(abc1)=c("fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")
abc1 # Matriz que tem nas colunas as variáveis dummy e nas linhas as datas desde outubro de 2008 até dezembro 2018 (123 meses, ou seja, 123 linhas). As colunas dessa matriz são os meses de fevereiro até dezembro (não tem o mês de janeiro pois janeiro é o mês de referência). A ideia dessa matriz é indicar a qual mês pertence aquela observação, como por exemplo: os dados começam em outubro de 2008, então a primeira linha (primeira observação vai ter o valor 1 no mês outubro e 0 nos outros meses).

# Vamos começar juntando as matrizes "matriz_ncont_analise" e "abc1" e as matrizes "matriz_ncasos_defasagens" e "abc1"
matriz_ncont_analise_dummy=cbind(abc1,t(matriz_ncont_analise)) #juntando dummy que representa os meses com os números de casos
matriz_ncasos_defasagens_dummy=cbind(abc1[13:123,],matriz_ncasos_defasagens[,-c(1,13,26,27,39,40,52,53,65,66,78,79,91)]) # Como na matriz de defasagens o Brasil no tempo atual vai de outubro de 2009 até dezembro de 2018, então as variáveis categóricas que representam os meses vão também de outubro de 2009 até dezembro de 2018.

# Montando as matrizes
matriz_ncont_analise_mediasBR=cbind(matriz_mediasBR_2008_2018,t(matriz_ncont_analise))
matriz_ncasos_defasagens_mediasBR=cbind(mediasBR_formatodefasagem,matriz_ncasos_defasagens_dummy[,-c(1:11)]) # Como na matriz de defasagens o Brasil no tempo atual vai de outubro de 2009 até dezembro de 2018 (pois quando a matriz foi montada ia até a defasagem 12 e depois essa mesma foi retirada, indo até a defasagem 11), então a variável que representa as médias dos meses no Brasil vão também de outubro de 2009 até dezembro de 2018.
###############################

# FINAL FLU INCIDENCE MATRICES:
matriz_ncont_analise # matriz com número de casos de outubro de 2008 até dezembro de 2018 das diferentes 7 regioes - necessário para Granger Causalidade
matriz_ncont_2019 # matriz com número de casos de janeiro de 2019 até novembro de 2019 das diferentes 7 regioes - necessário para Granger Causalidade
matrix_pred # necessária para predicao
matriz_ncasos_defasagens_mediasBR # matriz usada nas modelagens stepwise 1, stepwise 2, lasso 5 e lasso 10

# Exporting Final Matrices:
#library('openxlsx')
#write.xlsx(t(matriz_ncont_analise), "matriz_ncont_analise.xlsx")
#write.xlsx(t(matriz_ncont_2019), "matriz_ncont_2019.xlsx")
#write.xlsx(matrix_pred, "matrix_pred.xlsx")
#write.xlsx(matriz_ncasos_defasagens_mediasBR, "matriz_ncasos_defasagens_mediasBR.xlsx")

##############################################################################################################################
######################## GENETIC DATABASE ##############################################################################
##############################################################################################################################

######################################
#               H1N1                 #
######################################

#ALL
data_div<-seq(from=as.Date("2008-10-01"), to=as.Date("2019-11-01"), by="month")
load("trim_sw_h1n1.div.RData")
load("trim_sw_h1n1.n.RData")
all_h1n1_trim_div<-trim_sw.div[166:299]
all_h1n1_trim_n<-trim_sw.n[166:299]
names(all_h1n1_trim_div)<-data_div
names(all_h1n1_trim_n)<-data_div


#ASIA
data_div<-seq(from=as.Date("2008-10-01"), to=as.Date("2019-09-01"), by="month")
load("trim_sw_h1n1_asia.div.RData")
load("trim_sw_h1n1_asia.n.RData")
asia_h1n1_trim_div<-trim_sw.div[106:237]
asia_h1n1_trim_n<-trim_sw.n[106:237]
names(asia_h1n1_trim_div)<-data_div
names(asia_h1n1_trim_n)<-data_div

#NORTH AMERICA
data_div<-seq(from=as.Date("2008-10-01"), to=as.Date("2019-11-01"), by="month")
load("trim_sw_h1n1_north_america.div.RData")
load("trim_sw_h1n1_north_america.n.RData")
north_america_h1n1_trim_div<-trim_sw.div[106:239]
north_america_h1n1_trim_n<-trim_sw.n[106:239]
names(north_america_h1n1_trim_div)<-data_div
names(north_america_h1n1_trim_n)<-data_div


######################################
#               H3N2                 #
######################################

#ALL
data_div<-seq(from=as.Date("2008-10-01"), to=as.Date("2019-11-01"), by="month")
load("trim_sw_h3n2.div.RData")
all_h3n2_trim_div<-trim_sw.div[214:347]
all_h3n2_trim_n<-trim_sw.n[214:347]
names(all_h3n2_trim_div)<-data_div
names(all_h3n2_trim_n)<-data_div

#ASIA
data_div<-seq(from=as.Date("2008-10-01"), to=as.Date("2019-09-01"), by="month")
load("trim_sw_h3n2_asia.div.RData")
asia_h3n2_trim_div<-trim_sw.div[178:309]
asia_h3n2_trim_n<-trim_sw.n[178:309]
names(asia_h3n2_trim_div)<-data_div
names(asia_h3n2_trim_n)<-data_div

#NORTH AMERICA
data_div<-seq(from=as.Date("2008-10-01"), to=as.Date("2019-11-01"), by="month")
load("trim_sw_h3n2_north_america.div.RData")
load("trim_sw_h3n2_north_america.n.RData")
north_america_h3n2_trim_div<-trim_sw.div[202:335]
north_america_h3n2_trim_n<-trim_sw.n[202:335]
names(north_america_h3n2_trim_div)<-data_div
names(north_america_h3n2_trim_n)<-data_div

#save(all_h1n1_trim_div,all_h1n1_trim_n,asia_h1n1_trim_div,asia_h1n1_trim_n,north_america_h1n1_trim_div,north_america_h1n1_trim_n, file="Div_h1n1_Aline.RData")
#save(all_h3n2_trim_div,all_h3n2_trim_n,asia_h3n2_trim_div,asia_h3n2_trim_n,north_america_h3n2_trim_div,north_america_h3n2_trim_n, file="Div_h3n2_Aline.RData")


# Dados genéticos globais (outubro de 2008 até novembro de 2019)
all_h1n1_trim_div # Essa matriz é dos dados genéticos gerais (AllH1N1)
all_h3n2_trim_div # Essa matriz é dos dados genéticos gerais (AllH3N2)
all_h1n1_trim_n # n é o tamanho de amostra de cada uma das observações do H1N1 no mundo - guardar ele, mas não usar nas análises no momento
all_h3n2_trim_n

# Dados genéticos North America (outubro de 2008 até novembro de 2019)
north_america_h1n1_trim_div # NorthAmericaH1N1
north_america_h3n2_trim_div # NorthAmericaH3N2
north_america_h1n1_trim_n
north_america_h3n2_trim_n

# Dados genéticos Asia (outubro de 2008 até setembro de 2019)
asia_h1n1_trim_div #AsiaH1N1 - tem poucos missings
asia_h3n2_trim_div #AsiaH3N2 - tem poucos missings
asia_h1n1_trim_n
asia_h3n2_trim_n
###################

# Preenchendo os missings com a média do mês anterior e o mês posterior
north_america_h1n1_trim_div[33]=mean(c(north_america_h1n1_trim_div[32],north_america_h1n1_trim_div[34]))

# Dados genéticos Asia (outubro de 2008 até setembro de 2019)
asia_h1n1_trim_div #AsiaH1N1 - tem poucos missings
asia_h3n2_trim_div #AsiaH3N2 - tem poucos missings
asia_h1n1_trim_n
asia_h3n2_trim_n

# Preenchendo os missings com a média do mês anterior e o mês posterior
# H1N1
asia_h1n1_trim_div # missing nas observações 132,129,128,117,96,95
asia_h1n1_trim_div[128:129]=mean(c(asia_h1n1_trim_div[127],asia_h1n1_trim_div[130]))
asia_h1n1_trim_div[117]=mean(c(asia_h1n1_trim_div[116],asia_h1n1_trim_div[118]))
asia_h1n1_trim_div[95:96]=mean(c(asia_h1n1_trim_div[94],asia_h1n1_trim_div[97]))

#H3N2
asia_h3n2_trim_div # missing nas observações 115,116 e 117
asia_h3n2_trim_div[115:117]=mean(c(asia_h3n2_trim_div[114],asia_h3n2_trim_div[118])) #como os 3 missings são um seguido do outro, então eles 3 vão ter a mesma média, que é a média entre a observação 114 e 118
###############################

# Montando matrizes diversidade genética:

# Dados genéticos
all_h1n1_trim_div # AllH1N1 - outubro de 2008 até novembro de 2019
all_h3n2_trim_div # AllH3N2 - outubro de 2008 até novembro de 2019
north_america_h1n1_trim_div # NorthAmericaH1N1 - outubro de 2008 até novembro de 2019
north_america_h3n2_trim_div # NorthAmericaH3N2 - outubro de 2008 até novembro de 2019
asia_h1n1_trim_div # AsiaH1N1 - outubro de 2008 até setembro de 2019
asia_h3n2_trim_div # AsiaH3N2 - outubro de 2008 até setembro de 2019
# Vemos que a intersecção das datas dos dados da genética e dos dados do número de casos é: outubro de 2008 até setembro de 2019. Assim os dados que serão usados para a análise serão de outubro de 2008 até dezembro de 2018 e os para previsão serão de janeiro de 2019 até setembro de 2019.

# Dados genéticos intersecção (outubro de 2008 até setembro de 2019)
AllH1=all_h1n1_trim_div[-c(133,134)]
AllH3=all_h3n2_trim_div[-c(133,134)]
NorthAmericaH1=north_america_h1n1_trim_div[-c(133,134)]
NorthAmericaH3=north_america_h3n2_trim_div[-c(133,134)]
AsiaH1=asia_h1n1_trim_div
AsiaH3=asia_h3n2_trim_div

# Juntando dados genéticos com número de casos
matriz_ncont_analise_genetica=cbind(t(matriz_ncont_analise),AllH1[1:123],AllH3[1:123],NorthAmericaH1[1:123],NorthAmericaH3[1:123],AsiaH1[1:123],AsiaH3[1:123])
colnames(matriz_ncont_analise_genetica)=c("RegiaoAmericaNorte","Brasil","RegiaoAmericaSul","RegiaoAmericaCentral", "RegiaoEuropeia", "RegiaoSouthAsia", "RegiaoWesternPacific","AllH1", "AllH3", "NorthAmericaH1", "NorthAmericaH3", "AsiaH1", "AsiaH3")
rownames(matriz_ncont_analise_genetica)=c(1:123)

matriz_ncont_2019_genetica=rbind(matriz_ncont_2019[,1:9],AllH1[124:132],AllH3[124:132],NorthAmericaH1[124:132],NorthAmericaH3[124:132],AsiaH1[124:132],AsiaH3[124:132])
rownames(matriz_ncont_2019_genetica)=c("RegiaoAmericaNorte","Brasil","RegiaoAmericaSul","RegiaoAmericaCentral", "RegiaoEuropeia", "RegiaoSouthAsia", "RegiaoWesternPacific","AllH1", "AllH3", "NorthAmericaH1", "NorthAmericaH3", "AsiaH1", "AsiaH3")

# Matrizes com número de casos positivos e genética
matriz_ncont_analise_genetica # outubro de 2008 até dezembro de 2018
matriz_ncont_2019_genetica # janeiro de 2019 até setembro de 2019

# Matriz com 6 defasagens:
matriz_ncasos_defasagens_genetica=matrix(ncol=80, nrow=117)
colnames(matriz_ncasos_defasagens_genetica)=c("Brasil_Bt","Brasil_Bt1","Brasil_Bt2","Brasil_Bt3","Brasil_Bt4","Brasil_Bt5","Brasil_Bt6","RegiaoEuropeia_Et1","RegiaoEuropeia_Et2","RegiaoEuropeia_Et3","RegiaoEuropeia_Et4","RegiaoEuropeia_Et5","RegiaoEuropeia_Et6","RegiaoAmericaNorte_At1","RegiaoAmericaNorte_At2","RegiaoAmericaNorte_At3","RegiaoAmericaNorte_At4","RegiaoAmericaNorte_At5","RegiaoAmericaNorte_At6","RegiaoAmericaSul_St1","RegiaoAmericaSul_St2","RegiaoAmericaSul_St3","RegiaoAmericaSul_St4","RegiaoAmericaSul_St5","RegiaoAmericaSul_St6","RegiaoAmericaCentral_Ct1","RegiaoAmericaCentral_Ct2","RegiaoAmericaCentral_Ct3","RegiaoAmericaCentral_Ct4","RegiaoAmericaCentral_Ct5","RegiaoAmericaCentral_Ct6","RegiaoSouthAsia_st1","RegiaoSouthAsia_st2","RegiaoSouthAsia_st3","RegiaoSouthAsia_st4","RegiaoSouthAsia_st5","RegiaoSouthAsia_st6","RegiaoWesternPacific_Wt1","RegiaoWesternPacific_Wt2","RegiaoWesternPacific_Wt3","RegiaoWesternPacific_Wt4","RegiaoWesternPacific_Wt5","RegiaoWesternPacific_Wt6","mediasBR","AllH1_1","AllH1_2","AllH1_3","AllH1_4","AllH1_5","AllH1_6","AllH3_1","AllH3_2","AllH3_3","AllH3_4","AllH3_5","AllH3_6","NorthAmericaH1_1","NorthAmericaH1_2","NorthAmericaH1_3","NorthAmericaH1_4","NorthAmericaH1_5","NorthAmericaH1_6","NorthAmericaH3_1","NorthAmericaH3_2","NorthAmericaH3_3","NorthAmericaH3_4","NorthAmericaH3_5","NorthAmericaH3_6","AsiaH1_1","AsiaH1_2","AsiaH1_3","AsiaH1_4","AsiaH1_5","AsiaH1_6","AsiaH3_1","AsiaH3_2","AsiaH3_3","AsiaH3_4","AsiaH3_5","AsiaH3_6")
for (i in 1:7){
  matriz_ncasos_defasagens_genetica[,i]=matriz_ncont_analise_genetica[(8-i):(124-i),"Brasil"] # Temos Bt (abril de 2009 até dezembro de 2018) até Bt6 (outubro de 2008 até junho de 2018)
}
for (i in 1:6){
  matriz_ncasos_defasagens_genetica[,i+7]=matriz_ncont_analise_genetica[(7-i):(123-i),"RegiaoEuropeia"] 
  matriz_ncasos_defasagens_genetica[,i+13]=matriz_ncont_analise_genetica[(7-i):(123-i),"RegiaoAmericaNorte"] 
  matriz_ncasos_defasagens_genetica[,i+19]=matriz_ncont_analise_genetica[(7-i):(123-i),"RegiaoAmericaSul"] 
  matriz_ncasos_defasagens_genetica[,i+25]=matriz_ncont_analise_genetica[(7-i):(123-i),"RegiaoAmericaCentral"] 
  matriz_ncasos_defasagens_genetica[,i+31]=matriz_ncont_analise_genetica[(7-i):(123-i),"RegiaoSouthAsia"] 
  matriz_ncasos_defasagens_genetica[,i+37]=matriz_ncont_analise_genetica[(7-i):(123-i),"RegiaoWesternPacific"] 
  matriz_ncasos_defasagens_genetica[,i+44]=matriz_ncont_analise_genetica[(7-i):(123-i),"AllH1"] 
  matriz_ncasos_defasagens_genetica[,i+50]=matriz_ncont_analise_genetica[(7-i):(123-i),"AllH3"] 
  matriz_ncasos_defasagens_genetica[,i+56]=matriz_ncont_analise_genetica[(7-i):(123-i),"NorthAmericaH1"] 
  matriz_ncasos_defasagens_genetica[,i+62]=matriz_ncont_analise_genetica[(7-i):(123-i),"NorthAmericaH3"] 
  matriz_ncasos_defasagens_genetica[,i+68]=matriz_ncont_analise_genetica[(7-i):(123-i),"AsiaH1"] 
  matriz_ncasos_defasagens_genetica[,i+74]=matriz_ncont_analise_genetica[(7-i):(123-i),"AsiaH3"] 
}
matriz_ncasos_defasagens_genetica[,"mediasBR"]=matriz_mediasBR_2008_2018[7:123] #abril de 2009 até dezembro de 2018 (mesmas datas do Brasil no tempo atual)

matriz_ncasos_defasagens_genetica=as.data.frame(matriz_ncasos_defasagens_genetica)
matriz_ncasos_defasagens_genetica
###############################

# FINAL GENETIC MATRICES:
matriz_ncont_analise_genetica # outubro de 2008 até dezembro de 2018
matriz_ncont_2019_genetica # janeiro de 2019 até setembro de 2019
matriz_ncasos_defasagens_genetica # Matriz com 6 defasagens usada nas modelagens stepwise 1, stepwise 2, lasso 5 e lasso 10

# Exporting Final Matrices:
#library('openxlsx')
#write.xlsx(matriz_ncont_analise_genetica, "matriz_ncont_analise_genetica.xlsx")
#write.xlsx(t(matriz_ncont_2019_genetica), "matriz_ncont_2019_genetica.xlsx")
#write.xlsx(matriz_ncasos_defasagens_genetica, "matriz_ncasos_defasagens_genetica.xlsx")
