##Análise de dados_OpenDataSUS_SRAG
#Domínio público: https://opendatasus.saude.gov.br/dataset/bd-srag-2020
#SRAG 2020 - Banco de Dados de Síndrome Respiratória Aguda Grave - incluindo dados da COVID-19
#Vigilância de Síndrome Respiratória Aguda Grave (SRAG)

#Este script tem o objetivo de servir orientar uma análise objetiva e simples dos casos de SRAG através do R,
#dessa forma, pode ser ser acessado por qualquer pessoa e técnico (a) de serviços de vigilância epidemiológica

#Uma das potencialidades do R é que é possível abrir diversos tipos de arquivos nele. 
#formato .csv, .txt, .dta, etc., por outro lado, essa diversidade implica diferentes funções de importação
#isso torna o trabalho, as vezes, um pouco mais complicado.
#O pacote rio ajuda acabar com estes problemas. Com a função import(), 
#ele é capaz de detectar qual arquivo o usuário deseja abrir e seleciona a forma mais eficaz de acesso.

#O primeiro passo para usá-lo é instalar o pacote:

#Instalando o pacote 'rio'
install.packages("rio", dependencies = TRUE)

#Acima, instalamos o pacote com todas as dependências 
#(outros pacotes que são necessários para que ele funcione). 
#Em seguida, carregar qualquer arquivo de dados fica mais fácil, 
#basta usar a função import():

#outros pacotes serão utilizados ao longo da análise, por isso, é recomendado ativá-los.
#Carrega o pacote rio e os demais que serão usados nas análises:
library(rio)
library(dplyr)
library(lubridate)
library(tidyverse)
library(epiDisplay)

#Nesta análise será utilizada a base de SRAG com dados atualizados até 08-03-2021 disponível no openDatasus
#-----https://opendatasus.saude.gov.br/dataset/bd-srag-2020


#O próximo passo é importar o banco de dados.
#É importnte verificar o nome ao importar, se não estiver correto vai acusar erro:
srag <- import(file = "INFLUD-08-03-2021.csv")

#É recomendado preservar a base original e trabalhar com uma cópia, 
#assim, se por algum erro ao realizar uma função algma variável por danificada basta recarregar 
#o banco original já no R
srag2 <-srag

#OBJETIVO: QUAIS PASSOS SÃO IMPORTANTES PARA GERAR UM BOLETIM EPIDEMIOLÓGICO DE COMUNICAÇÃO COM A SOCIEDADE?

#NestE trabalho vamos ver alguns comandos úteis e pacotes do R para fazer uma 
#análise exploratória descritiva. Um passo inicial importante para uma análise descritiva adequada 
#é verificar os tipos de de variáveis disponíveis, como estão classificadas e quais farão parte da análise:

#Qualitativas
#Nominais
#Ordinais
#Quantitativas
#Discretas
#Contínuas

#só após esse passo os dados poderão ser resumidos em tabelas, gráficos e/ou medidas.

#verificando os dados
str(srag2)

#O base é nacional e possui os dados de todos os municípios, por isso faremos um filtro:
#1º pelo município
#2º pelas variáveis de interesse
#Município: Matias Barbosa-MG, código do IBGE: 3140803
#População estimada [2020]---14.548 pessoas
#População no último censo [2010]---13.435 pessoas
#Densidade demográfica [2010]---85,51 hab/km² 

#Fazendo o 1º filtro por código do município de residência do paciente 
#Perceba que é importante dar um segundo nome ao fazer o filtro, neste caso chamamos de srag.mb

srag.mb <- srag2 %>%
  filter(CO_MUN_RES == 314080)


#Dessa forma, o banco a ser analisado possui 80 observações e 154 variáveis
#A partir daqui é possível trabalhar apenas com os casos do município escolhido 
str(srag.mb)

#A partir disso, é possível identificar os municípios de notificação e os 
#códigos dos municípios de internação dos casos de SRAG de pacientes de Matias Barbosa
table(srag.mb$ID_MUNICIP)
table(srag.mb$CO_MUN_NOT)
table(srag.mb$ID_MUNICIP)/nrow(srag.mb)*100

#------os pacientes de srag de Matias Barbosa foram internados em 5 municípios:
#--Cataguases, Juiz de Fora, leopoldina, Matias Barbosa, Petrópolis e Santos Dumont

#Escolhendo as variáveis que entrarão nas análises:

#1-Data do preenchimento da ficha de notificação/DT_NOTIFIC
#2-Data de 1ºs sintomas/DT_SIN_PRI
#5-Unidade de Saúde/Código (CNES)/ID_UNIDADE OU/CO_UNI_NOT ok
#8-Sexo/CS_SEXO OK
#10-(ou) Idade/NU_IDADE_N ok
#11-Gestante/CS_GESTANT OK
#12-Raça/Cor/CS_RACA OK
#14-Escolaridade/CS_ESCOL_N OK
#15- Ocupação/PAC_COCBO ou PAC_DSCBO OK 
#20-Bairro/NM_BAIRRO ok
#27- Paciente tem histórico de viagem internacional até 14 dias antes do/HISTO_VGM
#36-Fatores de risco/ Puérpera/PUERPERA
#36-Fatores de risco/ Doença Cardiovascular Crônica/CARDIOPATI
#36-Fatores de risco/ Doença Hematológica Crônica/HEMATOLOGI
#36-Fatores de risco/ Síndrome de Down/SIND_DOWN
#36-Fatores de risco/Doença Hepática Crônica/HEPATICA
#36-Fatores de risco/Asma/ASMA
#36-Fatores de risco/ Diabetes mellitus/DIABETES
#36-Fatores de risco/ Doença Neurológica Crônica/NEUROLOGIC
#36-Fatores de risco/ Outra Pneumatopatia Crônica/PNEUMOPATI
#36-Fatores de risco/ Imunodeficiência/IMUNODEPRE
#36-Fatores de risco/ Doença Renal Crônica/RENAL
#36-Fatores de risco/ Obesidade/OBESIDADE
#36-Fatores de risco/ Obesidade (Descrição IMC)/OBES_IMC
#43-Data da internação por SRAG/DT_INTERNA
#47-Internado em UTI?/UTI
#48-Data da entrada na UTI/DT_ENTUTI
#50-Uso de suporte ventilatório?/SUPORT_VEN
#74-Evolução do caso/EVOLUCAO
#75-Data da alta ou óbito/DT_EVOLUCA


#Para trabalhar com as dstas é necessário transformar as datas de interger para Date:
#Datas escolhidas:

#1-Data do preenchimento da ficha de notificação/DT_NOTIFIC
#2-Data de 1ºs sintomas/DT_SIN_PRI
#43-Data da internação por SRAG/DT_INTERNA
#48-Data da entrada na UTI/DT_ENTUTI
#75-Data da alta ou óbito/DT_EVOLUCA

#Transformando de interger-->Date
srag.mb <- srag.mb %>%
  mutate(DT_ENTUTI = as.Date(DT_ENTUTI, format = "%d/%m/%Y"),
         DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
         DT_INTERNA = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
         DT_SIN_PRI = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
         DT_EVOLUCA = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
         DT_SAIDUTI = as.Date(DT_SAIDUTI, format = "%d/%m/%Y"))

#Conferindo:
table(srag.mb$DT_EVOLUCA)
#OBS: # As funções date() e as_date() assumem que a ordem segue o padrão da 
#língua inglesa: ano-mês-dia (ymd). Por isso, ao transformar troca-se a ordem na variável, 
#mas não se preocupe, no momento das operações algébricas tudo sairá como o esperado

#Transformando de character-->factor
srag.mb <- srag.mb %>%
  mutate( ID_MUNICIP = as.factor(ID_MUNICIP),
          CS_SEXO = as.factor(CS_SEXO),
          NU_IDADE_N = as.numeric(NU_IDADE_N),
          CS_GESTANT = as.factor(CS_GESTANT),
          CS_RACA = as.factor(CS_RACA),
          CS_ESCOL_N = as.factor(CS_ESCOL_N),
          HISTO_VGM = as.factor(HISTO_VGM),
          PUERPERA = as.factor(PUERPERA),
          CARDIOPATI = as.factor(CARDIOPATI),
          HEMATOLOGI = as.factor(HEMATOLOGI),
          SIND_DOWN = as.factor(SIND_DOWN),
          HEPATICA = as.factor(HEPATICA),
          ASMA = as.factor(ASMA),
          DIABETES = as.factor(DIABETES),
          NEUROLOGIC = as.factor(NEUROLOGIC),
          PNEUMOPATI = as.factor(PNEUMOPATI),
          IMUNODEPRE = as.factor(IMUNODEPRE),
          RENAL= as.factor(RENAL),
          OBESIDADE = as.factor(OBESIDADE),
          OBES_IMC = as.factor(OBES_IMC),
          UTI = as.factor(UTI),
          SUPORT_VEN = as.factor(SUPORT_VEN),
          EVOLUCAO = as.factor(EVOLUCAO))

#TESTANDO          
class(srag.mb$EVOLUCAO)
table(srag.mb$EVOLUCAO)

table(srag.mb$CLASSI_FIN)
table(srag.mb$CLASSI_OUT)

srag.mb <- srag.mb %>%
  mutate(CLASSI_FIN = as.factor(CLASSI_FIN),
         CLASSI_OUT = as.factor(CLASSI_OUT))

#Renomeando os lebels das variáveis 
srag.mb <- srag.mb %>%
  mutate(CS_SEXO = recode(CS_SEXO, `F` = "Feminino", `M` = "Masculino",`9` = "Ignorado"),
         CS_GESTANT = recode(CS_GESTANT, `2` = "2º Trimestre", `5` = "Não", `6` = "Não se aplica"),
         CS_RACA = recode(CS_RACA, `1` = "Branca", `2` = "Preta", `3` = "Amarela", `4` = "Parda", `9` = "Ignorado"),
         CS_ESCOL_N = recode(CS_ESCOL_N, `0` = "Sem escolaridade", `1` = "Fundamental 1º ciclo",
                             `2` = "Fundamental 2º ciclo", `3` = "Médio 1º ao 3º ano", `9` = "Ignorado"))

#Analisando a presença de NA
table(srag.mb$CS_SEXO, useNA = "always")
head(srag.mb$CS_SEXO,20)
table(srag.mb$CS_SEXO, srag.mb$CS_SEXO, useNA = "always")

table(srag.mb$CS_GESTANT, useNA = "always")
head(srag.mb$CS_GESTANT,20)
table(srag.mb$CS_GESTANT, srag.mb$CS_GESTANT, useNA = "always")

table(srag.mb$CS_RACA, useNA = "always")
head(srag.mb$CS_RACA,20)
table(srag.mb$CS_RACA, srag.mb$CS_RACA, useNA = "always")

table(srag.mb$CS_ESCOL_N, useNA = "always")
head(srag.mb$CS_ESCOL_N,20)
table(srag.mb$CS_ESCOL_N, srag.mb$CS_ESCOL_N, useNA = "always")

#Trocando NA para ignorado
srag.mb$CS_ESCOL_N <- srag.mb$CS_ESCOL_N %>%
  replace_na("Ignorado")


#Renomeando os lebels
srag.mb <- srag.mb %>%
  mutate(PUERPERA = recode(PUERPERA,`2` = "Não", `9` = "Ignorado"),
         HEMATOLOGI = recode(HEMATOLOGI,`1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         SIND_DOWN = recode(SIND_DOWN,`2` = "Não", `9` = "Ignorado"))

#Analisando a presença de NA
table(srag.mb$PUERPERA, useNA = "always")
head(srag.mb$PUERPERA,20)
table(srag.mb$PUERPERA, srag.mb$PUERPERA, useNA = "always")

#Trocando NA para Ignorado
srag.mb$PUERPERA <- srag.mb$PUERPERA %>%
  replace_na("Ignorado")


table(srag.mb$HEMATOLOGI, useNA = "always")
head(srag.mb$HEMATOLOGI,20)
table(srag.mb$HEMATOLOGI, srag.mb$HEMATOLOGI, useNA = "always")

#Trocando NA para Ignorado
srag.mb$HEMATOLOGI <- srag.mb$HEMATOLOGI %>%
  replace_na("Ignorado")

table(srag.mb$SIND_DOWN, useNA = "always")
head(srag.mb$SIND_DOWN,20)
table(srag.mb$SIND_DOWN, srag.mb$SIND_DOWN, useNA = "always")

#Trocando NA para Ignorado
srag.mb$SIND_DOWN <- srag.mb$SIND_DOWN %>%
  replace_na("Ignorado")


#Renomeando os lebels
srag.mb <- srag.mb %>%
  mutate(CARDIOPATI = recode(CARDIOPATI,`1` = "Sim", `2` = "Não"),
         HEPATICA = recode(HEPATICA,`1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         ASMA = recode(ASMA, `1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         DIABETES = recode(DIABETES, `1` = "Sim", `2` = "Não", `9` = "Ignorado"))

#Analisando a presença de NA
table(srag.mb$CARDIOPATI, useNA = "always")
head(srag.mb$CARDIOPATI,20)
table(srag.mb$CARDIOPATI, srag.mb$CARDIOPATI, useNA = "always")

#Trocando NA para Ignorado
srag.mb$CARDIOPATI <- srag.mb$CARDIOPATI %>%
  replace_na("Ignorado")

table(srag.mb$HEPATICA, useNA = "always")
head(srag.mb$HEPATICA,20)
table(srag.mb$HEPATICA, srag.mb$HEPATICA, useNA = "always")

#Trocando NA para Ignorado
srag.mb$HEPATICA <- srag.mb$HEPATICA %>%
  replace_na("Ignorado")

table(srag.mb$ASMA, useNA = "always")
head(srag.mb$ASMA,20)
table(srag.mb$ASMA, srag.mb$ASMA, useNA = "always")

#Trocando NA para Ignorado
srag.mb$ASMA <- srag.mb$ASMA %>%
  replace_na("Ignorado")


table(srag.mb$DIABETES, useNA = "always")
head(srag.mb$DIABETES,20)
table(srag.mb$DIABETES, srag.mb$DIABETES, useNA = "always")

#Trocando NA para Ignorado
srag.mb$DIABETES <- srag.mb$DIABETES %>%
  replace_na("Ignorado")


#Renomeando os lebels
srag.mb <- srag.mb %>%
  mutate(NEUROLOGIC = recode(NEUROLOGIC, `1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         PNEUMOPATI = recode(PNEUMOPATI, `1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         IMUNODEPRE = recode(IMUNODEPRE, `1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         RENAL = recode(RENAL, `1` = "Sim", `2` = "Não", `9` = "Ignorado"))

#ANALISANDO NA
table(srag.mb$NEUROLOGIC, useNA = "always")
head(srag.mb$NEUROLOGIC,20)
table(srag.mb$NEUROLOGIC, srag.mb$NEUROLOGIC, useNA = "always")

#Trocando NA para Ignorado
srag.mb$NEUROLOGIC <- srag.mb$NEUROLOGIC %>%
  replace_na("Ignorado")


table(srag.mb$PNEUMOPATI, useNA = "always")
head(srag.mb$PNEUMOPATI,20)
table(srag.mb$PNEUMOPATI, srag.mb$PNEUMOPATI, useNA = "always")

#Trocando NA para Ignorado
srag.mb$PNEUMOPATI <- srag.mb$PNEUMOPATI %>%
  replace_na("Ignorado")


table(srag.mb$IMUNODEPRE, useNA = "always")
head(srag.mb$IMUNODEPRE,20)
table(srag.mb$IMUNODEPRE, srag.mb$IMUNODEPRE, useNA = "always")

#Trocando NA para Ignorado
srag.mb$IMUNODEPRE<- srag.mb$IMUNODEPRE %>%
  replace_na("Ignorado")


table(srag.mb$RENAL, useNA = "always")
head(srag.mb$RENAL,20)
table(srag.mb$RENAL, srag.mb$RENAL, useNA = "always")

#Trocando NA para Ignorado
srag.mb$RENAL <- srag.mb$RENAL %>%
  replace_na("Ignorado")

table(srag.mb$SUPORT_VEN)

#renomeando os lebels
srag.mb <- srag.mb %>%
  mutate(OBESIDADE = recode(OBESIDADE, `1` = "Sim", `2` = "Não", `9` = "Ignorado"),
         UTI = recode(UTI, `1` = "Sim", `2` = "Não"),
         SUPORT_VEN = recode(SUPORT_VEN, `1` = "Sim invasivo", `2` = "Sim não invasivo", `3` = "Não", `9` = "Ignorado"),
         EVOLUCAO = recode(EVOLUCAO, `1` = "Cura", `2` = "Óbito", `9` = "Ignorado"))

table(srag.mb$OBESIDADE, useNA = "always")
head(srag.mb$OBESIDADE,20)
table(srag.mb$OBESIDADE, srag.mb$OBESIDADE, useNA = "always")

#Trocando NA para Ignorado
srag.mb$OBESIDADE <- srag.mb$OBESIDADE %>%
  replace_na("Ignorado")


table(srag.mb$UTI, useNA = "always")
head(srag.mb$UTI,20)
table(srag.mb$UTI, srag.mb$UTI, useNA = "always")

#Trocando NA para Ignorado
srag.mb$HEMATOLOGI <- srag.mb$HEMATOLOGI %>%
  replace_na("Ignorado")


table(srag.mb$SUPORT_VEN, useNA = "always")
head(srag.mb$SUPORT_VEN,20)
table(srag.mb$SUPORT_VEN, srag.mb$SUPORT_VEN, useNA = "always")


table(srag.mb$EVOLUCAO, useNA = "always")
head(srag.mb$EVOLUCAO,20)
table(srag.mb$EVOLUCAO, srag.mb$EVOLUCAO, useNA = "always")

#Mudando os NA para "Ignorado"
library(tidyr)

srag.mb$EVOLUCAO <- srag.mb$EVOLUCAO %>%
  replace_na("Ignorado")

#ps: lembrando que so vai mudar se a variável tiver como levels a categoria "Ignorado"


###########---------------------Manipular variável Idade-------------------------#############

#vamos criar a variável idade?
#Nem sempre os bancos nos fornecem a idade em ponto de ser 'trabalhada'
#Podemos criar a partir dos códigos que o banco nos fornece ou a partir das datas disponíveis:

################
# IDADE / DATAS
###############

#Pensado... se eu tenho um banco baseado em um questionário no qual não foi coletada 
#a idade em anos, #dias, meses... mAs tenho duas datas: NASCIMENTO e PREENCHIMENTO DO QUEST
#É possível construir a idade com base nas datas de internação/questionário e nascimento!

table(banco$DT_NASC[1:5], useNA = "always")
table(banco$DT_INTERNA[1:5], useNA = "always")

cbind(banco$DT_INTERNA[1:5], banco$DT_NASC[1:5])

#Neste caso podemos subtrair a data de internação da data de nascimento para construir a IDADE APROXIMADA


#Trocando o formato para date
#1º verifica a classificação da data
class(banco$DT_NASC)
class(banco$DT_INTERNA)

#2ºverifica a disposição dos elementos da data
table(banco$DT_NASC)

#Transforma com as.Date
banco <- banco %>%
  mutate(DT_NASC = as.Date(DT_NASC, format = "%d/%m/%Y"),
         DT_INTERNA = as.Date(DT_INTERNA, format = "%d/%m/%Y"))

#será que foi?
table(banco$DT_NASC)
class(banco$DT_NASC) #ok!

#Vamos extrair o ano da data de nascimento
#Para isso vamos utilizar o pacote lubridate que nos permite manipular datas (Aula 3!)

#install.packages("lubridate")

library(lubridate)
#ano de nascimento
banco <- banco %>%
  mutate(ano_nasc = epiyear(DT_NASC))

banco$ano_nasc[1:5]
summary(banco$ano_nasc)

#ano de internação
banco <- banco %>%
  mutate(ano_inter = epiyear(DT_INTERNA))

banco$ano_inter[1:5]
summary(banco$ano_inter)

#subtração dos dois anos para criar a idade aproximada em anos
#Porque APROXIMADA? ---> NEM TODOS COMPLETARAM ANIVERSSÁRIO ATÉ A DATA DA INTERNAÇÃO!

banco$idade_aprox <- (banco$ano_inter-banco$ano_nasc)

summary(banco$idade_aprox)

cbind(banco$ano_nasc[1:5], banco$ano_inter[1:5])


############################################
# IDADE / CÓDIGOS [SIH-SUS, SIVEP-GRIPE...]
###########################################

#Em bancos como SIH-SUS, SINAN, SIVEP-Gripe... a idade não é fornecidada em anos completos
#EX: SIH-SUS, cod-idade: 1=horas, 2=dias, 3= meses, 4=anos, 5=acima 100 anos

####SIVEP-GRIPE:

banco$NU_IDADE_N # idade 
banco$TP_IDADE # tipo da idade

summary(banco$NU_IDADE_N)
table(banco$TP_IDADE) 
#1-Dia
#2-Mês
#3-Ano

#Temos no banco 409 bebês que foram internados com SRAG

# quero criar uma variavel idade EM ANOS; Se tiver dias ou meses, receberá 0;
# pois nao comletou 1 ano


###--->IF ELSE: Conditional Element Selection
#Argumentos: teste, sim, não
#A estrutura condicional if/else é um recurso que indica quais instruções o sistema deve 
#processar de acordo com uma expressão booleana. Assim, o sistema testa se uma condição é verdadeira 
#e então executa comandos de acordo com esse resultado.


#Se o tipo da idade for 3 --> anos, nao precisamos fazer nada com idade
banco$idade_anos <- ifelse(banco$TP_IDADE == 3, banco$NU_IDADE_N, 0)

#Qual a condição a ser testada?
#se (TP_IDADE) tipo de idade == 3, 
#idade_anos recebe NU_IDADE_N (SIM), caso contrario recebe 0 (NÃO)

# resumo da variavel idade em anos - usando base do R
summary(banco$idade_anos) 
sd(banco$idade_anos) # desvio padrao

summary(banco$idade_aprox)

#Será que a idade em anos tem uma distribuição normal?
windows()
hist(banco$idade_anos)


# resumir a idade segundo sexo
by(banco$var.numeric, banco$var.categorico, funcao)

by(banco$idade_anos, banco$CS_SEXO, summary)

# resumo da idade segundo UTI
by(banco$idade_anos, banco$UTI, summary)


###########################################################
# CRIAR VARIÁVEL CATEGÓRICA A PARTIR DE VARIÁVEL NUMÉRICA #
##########################################################

#Há momentos em que uma variável, mesmo numérica, não tem impacto sempre positivo ou negativo 
#sobre outra variável. A idade, por exemplo, é uma característica quantificável dos indivíduos 
#que pode exercer influência sobre outras características de modo complexo. 
#Para indivíduos relativamente jovens, quanto maior a idade, em média, maior a
#escolaridade. Porém, em geral as pessoas um pouco mais velhas viveram sua
#juventude num contetxo em que o acesso à escola era menos comum e,
#por isso, a partir de uma certa idade/faixa, quanto maior a idade, em geral,
#menor será a escolaridade. 
#Nesses casos, pode ser mais produtivo converter essa variável numérica numa variável categórica 
#ainda na manipulação dos dados/banco. 
#Para isso é possível utilizar o comando cut(), usando como argumento a variável numérica, 
#um vetor com os valores de corte mínimo, intermediários e máximo, e, opcionalmente, um vetor
#com os rótulos (AQUINO, 2014).
#http://www.uesc.br/editora/livrosdigitais_20140513/r_cientistas.pdf


#--->cut: Convert Numeric to Factor
#cut divide Y em intervalos e codifica os valores de Y acordo com o intervalo que possui. 
#O intervalo mais à esquerda corresponde ao nível um, o próximo mais à esquerda ao nível dois...

summary(banco$IDADEANOS)
table(banco$IDADEANOS)


#Breaks = número de intervalos nos quais Y (variável/vetor) deve ser "cortado"
#Right = indica se os intervalos devem ser fechados à direita (e abertos à esquerda) ou vice-versa.

#RIGHT = FALSE ---> os intervalos serão fechados à esquerda e abertos à direita.
#right = true ---> os intervalos são abertos à esquerda e fechados à direita (x, y]


####------Intervalos na Reta Real-----####

#Intervalo aberto em a e aberto em b, ]a,b[ , {xЄR/a < x < b}
#Aberto à esquerda e aberto à direita

# <----------0------------0---------->
#           a             b




#Intervalo fechado em a e fechado em b, [a,b], {xЄR/a ≤ x ≤ b}
#Fechado à esquerda e fechado à direita


# <----------*------------*---------->
#           a             b





#Intervalo aberto em a e fechado em b, ]a,b], {xЄR/a < x ≤ b}
#Aberto à esquerda e fechado à direita ---> right = TRUE 

# <----------0------------*---------->
#           a             b




#Intervalo fechado em a e aberto em b, [a,b[, {xЄR/a ≤ x < b}
#Fechado à esquerda e aberto à direita ---> right = FALSE

# <----------*------------0---------->
#           a             b



#Faixa etária
#Fechado à esquerda e aberto à direita ---> right = FALSE:

#1º as faixas
banco <- banco %>%
  mutate(fx_etaria1 = cut(IDADEANOS, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                          right = FALSE))

table(banco$fx_etaria1)

banco <- banco %>%
  mutate(fx_etaria1 = cut(IDADEANOS, breaks=c(0,10,20,30,40,50,60,70,80,90,Inf), right = FALSE,
                          labels=c("0-9 anos","10-19 anos","20-29 anos","30-39 anos",
                                   "40-49 anos","50-59 anos",
                                   "60-69 anos","70-79 anos","80-89 anos","90 anos ou+")))

table(banco$fx_etaria1)



#Trabalhando com as datas:

#Qual o tempo enre 1º sintomas e internação?
#Qual o tempo de atraso entre 1º sintomas e a notificação?
#Essas são algumas perguntas que podem ser investigadas

#Datas que serão utilizadas:

#1-Data do preenchimento da ficha de notificação/DT_NOTIFIC
#2-Data de 1ºs sintomas/DT_SIN_PRI
#43-Data da internação por SRAG/DT_INTERNA
#48-Data da entrada na UTI/DT_ENTUTI
#75-Data da alta ou óbito/DT_EVOLUCAO


#Tempo de atraso utilizando os 1º sintomas
srag.mb$atraso <- as.numeric(srag.mb$DT_NOTIFIC-srag.mb$DT_SIN_PRI)
summary(srag.mb$atraso)
table(srag.mb$tempo)

table(srag.mb$DT_NOTIFIC, useNA = "always")
table(srag.mb$DT_NOTIFIC, srag.mb$DT_NOTIFIC, useNA = "always")

#Tempo entre 1º sintomas e internação na UTI
srag.mb$tempoateuti <- as.numeric(srag.mb$DT_ENTUTI-srag.mb$DT_SIN_PRI)
summary(srag.mb$tempoateuti)
table(srag.mb$tempoateuti)

#ÓTIMO, O R CALCULOU O TEMPO EXATAMENTE PARA OS 41 QUE FORAM INTERNADOS NA UTI!

#Tempo entre 1º sintomas e internação hospitalar
srag.mb$tempointernacao <- as.numeric(srag.mb$DT_INTERNA-srag.mb$DT_SIN_PRI)
summary(srag.mb$tempointernacao)
table(srag.mb$tempointernacao)

table(srag.mb$DT_INTERNA, useNA = "always")
table(srag.mb$DT_INTERNA, srag.mb$DT_INTERNA, useNA = "always")

#TEMPO DE PERMANÊNCIA A UTI
srag.mb$temponauti <- as.numeric(srag.mb$DT_SAIDUTI-srag.mb$DT_ENTUTI)
summary(srag.mb$temponauti)
table(srag.mb$temponauti)
hist(srag.mb$temponauti)

class(srag.mb$DT_ENTUTI)

#variáveis criadas:
table(srag.mb$atraso) #----> tempo entre 1º sintomas e a notificação
table(srag.mb$tempointernacao) #------> tempo entre 1º sintomas e a internação
table(srag.mb$tempoateuti) #-------> tempo entre 1º sintomas e a entrada na uti
table(srag.mb$temponauti) #-------> tempo de permanência na UTI

#HOSPITAS de internação;
table(srag.mb$ID_UNIDADE)

#CIDADES DE INTERNAÇÃO; (Município onde está localizada a Unidade Sentinela que realizou a notificação)
table(srag.mb$ID_MUNICIP)

#CURIOSIDADE------> SERÁ QUE COSNEGUIMOS IDENTIFICAR ESSES 80 CASOS OU BOA PARTE DELES NO SIHSUS?
#qual foi o gasto do município com internações em 2020?
#Para quem tiver interesse vale a pena conferir.

#Vamos analisar a signficância das variáveis selecionadas
#Ao final o comando "write.csv(tab.sem.ignorado, "tab.sem.ignorado.csv")" vai direcionar a tabela
#para o repositório que está sendo utilizado

tab.sem.ignorado<-tableStack(c(CS_SEXO, 
                               NU_IDADE_N,
                               fx_etaria,
                               CS_GESTANT,
                               CS_RACA,
                               CS_ESCOL_N,
                               HISTO_VGM,
                               PUERPERA,
                               CARDIOPATI,
                               HEMATOLOGI,
                               SIND_DOWN,
                               HEPATICA,
                               ASMA,
                               DIABETES,
                               NEUROLOGIC,
                               PNEUMOPATI,
                               IMUNODEPRE,
                               RENAL,
                               OBESIDADE,
                               UTI,
                               SUPORT_VEN,
                               temponauti,
                               tempointernacao,
                               tempoateuti,
                               atraso
                               
),by = EVOLUCAO, dataFrame = srag.mb, simulate.p.value =  T, na.rm = TRUE, 
total.colum = TRUE, percent = c ("column"))      
write.csv(tab.sem.ignorado, "tab.sem.ignorado.csv")

#criando outra variável e subset para retirar as 5 observações ignorado do banco
srag.mb.2<-droplevels(subset(srag.mb,EVOLUCAO!="Ignorado"))
levels(srag.mb.2$EVOLUCAO)

table(srag.mb.2$EVOLUCAO)
rm(srag.mb.2)

#Figura 1. Número absoluto de casos e óbitos, taxas de incidência e mortalidade
#habitantes e letalidade (%) por Covid-19
library(tableone)

variaveis <- c("CS_SEXO", 
               "NU_IDADE_N","fx_etaria","CS_GESTANT","CS_RACA","CS_ESCOL_N","PUERPERA",
               "CARDIOPATI","HEMATOLOGI","SIND_DOWN","HEPATICA","ASMA","DIABETES","NEUROLOGIC",
               "PNEUMOPATI","IMUNODEPRE","RENAL","OBESIDADE","UTI","SUPORT_VEN")

tabela<-CreateTableOne(vars = variaveis, data = srag.mb)
tabela
tabela$ContTable

#Para mostrar todas as categorias
tabela1<-print(tabela,showAllLevels=T)

#_______________________________

#Figura 2. Distribuição dos casos de srag, por semana epidemiológica de início de sintomas
#para isso vamos criar a semana epidemiológica com base na data dos 1º sintomas

srag.mb <- srag.mb %>%
  mutate(SemanaEpid = epiweek(as.Date(DT_SIN_PRI, format = "%d/%m/%y")))

srag.mb$SemanaEpid <- as.factor(srag.mb$SemanaEpid)

table(srag.mb$SemanaEpid)

#extraindo mês e ano 
srag.mb <- srag.mb %>%
  mutate(MES = month(DT_SIN_PRI))

table(srag.mb$MES)
srag.mb$MES <- as.factor(srag.mb$MES) # colocando MES como factor

#gráfico de casos segundo a semana epidemiológica
library(ggplot2)

windows()
ggplot(srag.mb, aes(x= SemanaEpid)) + 
  geom_bar(fill= "indianred4") +
  labs(x = "Semana Epidemiológica", y = "Casos de Srag", 
       title = "Distribuição dos casos de Srag",
       subtitle = "Matias Barbosa fevereiro de 2020 a janeiro 2021",
       caption = "Fonte: SINAN/Sivep gripe") + 
  theme_minimal(base_size = 22)

#-----> O PRIMEIRO CASO DE SRAG DO MUNICÍPIO FOI NOTIFICADO EM 23/02/2020

#-----> SEGUNDO O CALENDÁRIO EPIDEMIOLÓGICO DO SINAN DE 2020 O PRIMEIRO CASO BATE COM A 9º SEMANA,
#TAL COMO EVIDENCIA O PRIMEIRO CASO DA VARIÁVEL CRIADA SEMANA EPIDEMIOLÓGICA
#DA MESMA FORMA OS ÚLTIMOS 3 CASOS SEGUNDO A VARIÁVEL OCORREU NA SEMANA 53
#SE 53 = 27/12/2020	ATÉ 02/01/2021 ---> OS ULTIMOS 3 CASOS OCORREREM EM:
#30/12/2020 --- 31/12/2020 --- 01/01/2021

#OBS:o calendário epidemiológico de 2020 está disponível no material final 

#___________________________

#Figura 3.Tempo entre sintomas e data de notificação do caso, 
#e entre data do óbito e notificação do óbito por COVID e SRAG.

variaveis2 <- c("tempoateuti", "tempointernacao", "temponauti", "atraso")

tabela2<-CreateTableOne(vars = variaveis2, data = srag.mb)
tabela2
tabela2<-print(tabela2)
write.csv(tabela2, file = "tabela2.csv")

summary(srag.mb$tempoateuti)
ad.test(srag.mb$tempoateuti)

summary(srag.mb$tempointernacao)
ad.test(srag.mb$tempointernacao)

summary(srag.mb$temponauti)
ad.test(srag.mb$temponauti)

summary(srag.mb$atraso)
ad.test(srag.mb$atraso)

#Figura 4. Distribuição dos óbitos confirmados por COVID-19 por semana epidemiológica de ocorrência 
#para isso vamos faezr um subset com os óbitos do município

table(srag.mb.obito$EVOLUCAO) #--->23 óbitos

srag.mb.obito <-srag.mb %>%
  filter(EVOLUCAO == "Óbito")

#gráfico: 
#COR
#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

windows()
ggplot(srag.mb.obito, aes(x= SemanaEpid)) + 
  geom_bar(fill= "red3") +
  labs(x = "Semana Epidemiológica", y = "Óbito por Srag", 
       title = "Distribuição dos Óbitos de Srag",
       subtitle = "Matias Barbosa fevereiro de 2020 a janeiro 2021",
       caption = "Fonte: SINAN/Sivep gripe") +
  theme_minimal(base_size = 22)

table(srag.mb$DT_SIN_PRI)

#Figura 5. Casos de SRAG internados de Matias
#gráfico de evolução das nptificações segundo a semana epidemiológica


#Figura 9. Prevalência (%) de fatores de risco nos casos de óbito
variaveis3 <- c("CS_SEXO", 
                "NU_IDADE_N","fx_etaria","CS_GESTANT","CS_RACA","CS_ESCOL_N","PUERPERA",
                "CARDIOPATI","HEMATOLOGI","SIND_DOWN","HEPATICA","ASMA","DIABETES","NEUROLOGIC",
                "PNEUMOPATI","IMUNODEPRE","RENAL","OBESIDADE","UTI","SUPORT_VEN")

tabela3<-CreateTableOne(vars = variaveis3, data = srag.mb.obito)
tabela3

#Gráfico de coluna evolução segundo faixa etária
library(ggplot2)
theme_set(theme_bw())

windows()
ggplot(srag.mb) +
  aes(x = SemanaEpid, fill = fx_etaria) +
  geom_bar(position = "dodge") +
  scale_fill_hue()+
  labs(x = "Semana Epidemiológica", y = "Número de casos", title = "Casos de Srag: segundo faixa etária", 
       subtitle = "Matias Barbosa fev de 2020 a jan de 2021 ",
       caption = "Fonte: SINAN, Sivep gripe") +
  theme_bw(base_size = 22) +
  theme(legend.position= "bottom")

###-------> A paleta de cores não é bonita, mas é fácil de visualizar

#PACOTES USADOS:
library(ggplot2)
library(tableone)
library(rio)
library(dplyr)
library(lubridate)
library(tidyverse)
library(epiDisplay)
library(tidyr)
library(nortest)


_____________________________________________________________________________________________________________________________________

##salvando o R.data
analise_srag.mb <- tempfile(fileext = '.RData')
save(list = c('srag', 'srag.mb', 'srag2'),
     file = analise_srag.mb)
save(srag.mb, srag, file = "dados_srag.mb.Rdata")