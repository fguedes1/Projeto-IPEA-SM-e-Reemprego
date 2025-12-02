################################################################################
# NOME: TABELAS DE REGRESSAO
# AUTOR: FILLIPE
# DESCRICAO: CRIACAO DAS TABELAS DE REGRESSAO CONSOLIDADAS.
###############################################################################
library(kableExtra)
library(tidyverse)
#################### GRUPO DE CONTROLE 1 #######################################
Controle1_2010= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2010.RDs')
Controle1_2010_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2010_EF.RDs')

Controle1_2011_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2011_EF.RDs')
Controle1_2011= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2011.RDs')

Controle1_2012= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2012.RDs')
Controle1_2012_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2012_EF.RDs')

Controle1_2013= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2013.RDs')
Controle1_2013_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2013_EF.RDs')


#################### GRUPO DE CONTROLE 2 #######################################
Controle2_2010= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2010.RDs')
Controle2_2010_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2010_EF.RDs')

Controle2_2011= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2011.RDs')
Controle2_2011_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2011_EF.RDs')

Controle2_2012= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2012.RDs')
Controle2_2012_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2012_EF.RDs')

Controle2_2013= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2013.RDs')
Controle2_2013_EF= readRDS('E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2013_EF.RDs')



extra_rows = tribble(
  ~term, ~a, ~b, ~c, ~d, ~e, ~f, ~g, ~h,
  'Efeitos Fixos de Firma', 'Não', 'Não', 'Não', 'Não', 'Sim', 'Sim', 'Sim', 'Sim',
  'Controle de Tamanho do Firma', 'Sim', 'Sim', 'Sim', 'Sim', 'Não', 'Não', 'Não', 'Não',
  

)

A=modelsummary(models = list(
  '(2010)' = Controle1_2010, 
  '(2011)' = Controle1_2011, 
  '(2012)' = Controle1_2012, 
  '(2013)' = Controle1_2013, 
  '(2010)' = Controle1_2010_EF, 
  '(2011)' = Controle1_2011_EF, 
  '(2012)' = Controle1_2012_EF, 
  '(2013)' = Controle1_2013_EF
), 
gof_omit = c('^AIC|^BIC|^Log|^FE|^Std.|Pseudo|R2 Within'),
stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
coef_omit = '^totemp1|^educa|^tam|^fx|^part|^industria|^genero|^servicos|^construcao|^comercio',
title = 'Resultados de Regressão',
escape = FALSE, 
add_rows = extra_rows, 
output = "E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_GERAL.docx"
) 

B=modelsummary(models = list(
  '(2010)' = Controle2_2010, 
  '(2011)' = Controle2_2011, 
  '(2012)' = Controle2_2012, 
  '(2013)' = Controle2_2013, 
  '(2010)' = Controle2_2010_EF, 
  '(2011)' = Controle2_2011_EF, 
  '(2012)' = Controle2_2012_EF, 
  '(2013)' = Controle2_2013_EF
), 
gof_omit = c('^AIC|^BIC|^Log|^FE|^Std.|Pseudo|R2 Within'),
stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
coef_omit = '^totemp1|^educa|^tam|^fx|^part|^industria|^genero|^servicos|^construcao|^comercio',
title = 'Resultados de Regressão',
escape = FALSE, 
add_rows = extra_rows, 
output = "E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez2_GERAL.docx"
) 
