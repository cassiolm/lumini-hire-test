## setando o diretorio documentos (onde coloquei o arquivo csv)
setwd("~/Documents")

##Instalação e uso das bibs
#install.packages('devtools')
library(devtools)
#install_github("ujjwalkarn/xda") ##necessaria para a funcao charSummary
library(xda)
#install.packages("mosaic")
library(mosaic)
#install.packages('lattice')
library(lattice)
library(ggplot2)
install.packages('plotly')
library(plotly)


## Import da base para variável base
base = read.csv('Microdados_Enem_2016.csv')

#Check nas primeiras linhas do dataset
head(base)

#resume todos os possíveis atributos de variáveis categóricas, gerando um resumo com bastante informação útil
charSummary(base)

## Dados de  Bivariância por sexo em cada estado
bivariate(base,'SG_UF_RESIDENCIA','TP_SEXO' )
#Interessante denotar que temos diversos estados e nos parece ter uma tendência em maior parte deles de um sexo majoritário

#Observa-se que a quantidade absoluta do sexo feminino é consideravelmente maior que o masculino
plot(base$TP_SEXO,main='Distribuicao da prova por sexo', xlab='Sexo',ylab='Distribuicao da amostra',col=('dark red'))

#É importante verificarmos qual a faixa etária do público alvo a fim de entender melhor o perfil dos candidatos
histogram(base$NU_IDADE,main = 'Distribuicao de Idades no Enem 2016', xlab = 'Idades', ylab= '% do Total',breaks= 5, col=c("seagreen"))
# Observa-se que a maior fatia das pessoas se encontra na faixa de maior que 10 a 30 anos

#para uma maior exatidão vamos observar este dado em tabela
summary(base$NU_IDADE)
#A média de idade está em 21.59, ou seja, exatamente na tendência denotada anteriormente

#----------------------------
#Sendo mais específico no seguinte problema: 
#Como é possível segmentar os inscritos de forma clara e objetiva 
#com o intuito de justificar investimentos em educação 
#destinados a certas parcelas de alunos?

#Podemos verificar como estão as notas por estado, como amostra pegaremos LC.
estados <-group_by(base, SG_UF_RESIDENCIA)
nota_por_estado <- summarize(estados, media = mean(NU_NOTA_LC, na.rm=TRUE))
nota_por_estado <- arrange(nota_por_estado,media)

## Com base em nossa amostra, as menores notas se concentram nos estados do PI, AM, AP e MA Respectivamente.
head(nota_por_estado)

##Teoria-> Devemos investir mais em Linguagem e códigos nos estados do PI, AM, AP e MA

#mas será que isto tem alguma correlação com trabalho, ou a falta dele?
estadostrab <-group_by(base, SG_UF_RESIDENCIA,Q026)
trab_por_estado <- summarize(estadostrab)
trab_por_estado <-arrange(trab_por_estado,Q026)
head(trab_por_estado)
# verificamos que não existe correlação direta entre ter que trabalhar e deficiência no segmento de estudo levantado.
tail(trab_por_estado) 

#Conclusoes de primeira análise: 
#1) É possível direcionar bastante das questões para o público feminino, visto que compõe a maior parte da amostra.
#2) A prova deve ter como faixa etária foco entre 18 e 22 anos, onde se concentra a maior parcela da amostra
#3) Os estados PI, AM, AP e MA possuem as piores notas em Linguagens e códigos, é necessário um reforço por parte dos orgãos educativos da região para uma evolução
#4) Não existe correlação entre baixa nota em LC e já estar no mercado de trabalho.