################################################################################
# NOME: DESCRITIVAS CONSOLIDADA
# AUTOR: FILLIPE
# DESCRICAO: JUNTANDO AS TABELAS DE ESTATISTICAS DESCRITIVAS
#################################################################################
library(modelsummary)
library(tidyverse)
# CARREGANDO AS TABELAS
descr_2010 = readRDS('E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2010.RDs')
descr_2011 = readRDS('E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2011.RDs')
descr_2012 = readRDS('E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2012.RDs')
descr_2013 = readRDS('E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2013.RDs')

cbind(descr_2010, descr_2011, descr_2012, descr_2013)

a=cbind(descr_2010, descr_2011, descr_2012, descr_2013)
a = as.data.frame(a)

# EXTRACAO DO No DE LINHAS POR CATEGORIA
vetor = substring(colnames(a), 36, 46)
vetor = vetor %>%
  str_replace('\\)','')

a = rbind(a,vetor)
rownames(a)[length(rownames(a))] = 'Observações'

# ARRUMANDO O NOME DAS COLUNAS
colnames(a) = substring(colnames(a), 21, 30)

# DECIMAIS
a["Observações",]= gsub('\\,', '.', a["Observações",])
a


saveRDS(a, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_CONSOLIDADA_AMOSTRA GERAL.RDs')
write.csv2(a, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_CONSOLIDADA_AMOSTRA GERAL.csv')

##############################
rm(list=ls())

# CARREGANDO AS TABELAS
descr_2010_tab2 = readRDS('E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL.RDs')
a = as.data.frame(descr_2010_tab2)

# EXTRACAO DO No DE LINHAS POR CATEGORIA
vetor = substring(colnames(a), 47, 60)
vetor = vetor %>%
  str_replace('\\)','')

a = rbind(a,vetor)
rownames(a)[length(rownames(a))] = 'Observações'

# ARRUMANDO O NOME DAS COLUNAS
colnames(a) = substring(colnames(a), 20, 38)

# DECIMAIS
a["Observações",]= gsub('\\,', '.', a["Observações",])

a

saveRDS(a, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_CONSOLIDADA_TABELA 2_AMOSTRA GERAL.RDs')
write.csv2(a, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_CONSOLIDADA_TABELA 2_AMOSTRA GERAL.csv')

