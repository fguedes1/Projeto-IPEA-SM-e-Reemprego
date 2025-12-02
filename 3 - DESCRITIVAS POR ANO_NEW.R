################################################################################
# NOME: DESCRITIVAS POR ANO AMOSTRA GERAL.R
# AUTOR: FILLIPE
# DESCRICAO: CRIACAO DAS TABELAS DE ESTATISTICAS DESCRITIVAS PARA A AMOSTRA PARA
# CADA UM DOS ANOS
################################################################################
library(qwraps2)
library(tidyverse)
memory.limit(size=10000000000)


# BAIXANDO A BASE
base_2010 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2010_LIMPA')
base_2011 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2011_LIMPA')
base_2012 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2012_LIMPA')
base_2013 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2013_LIMPA')

# Identificando a base
base_2010 = base_2010 %>%
  mutate(ano_base = 2010)

base_2011 = base_2011 %>%
  mutate(ano_base = 2011)

base_2012 = base_2012 %>%
  mutate(ano_base = 2012)

base_2013 = base_2013 %>%
  mutate(ano_base = 2013)

# JUNTANDO A BASE
consolidada = rbind(base_2010, base_2011, base_2012, base_2013)

rm(base_2010, base_2011, base_2012, base_2013)
gc()

# Removendo mes de desligamento igual a 99
consolidada = filter(consolidada, is.na(mes_deslig)|mes_deslig!=99)
nas=which(is.na(consolidada$var_y)) 


# Zerando as var dependentes dos individuos de controle 3
consolidada = consolidada %>%
  mutate(var_y = ifelse(Categoria == 'Grupo de Controle 3', NA, var_y))


# Removendo as linhas com var dependente NA para criar as descritivas
consolidada = filter(consolidada, !is.na(var_y))

# Substituindo algumas variáveis que contém missing por 0 para usar
# nas descritivas
consolidada <- consolidada %>%
  mutate(ab = if_else(is.na(ab), 0, ab), 
         var_y_clt_indeterminado = if_else(is.na(var_y_clt_indeterminado), 0, var_y_clt_indeterminado),
         var_y_firma = if_else(is.na(var_y_firma), 0, var_y_firma),
         var_y_clt_indeterminado_e_firma = if_else(is.na(var_y_clt_indeterminado_e_firma), 0, var_y_clt_indeterminado_e_firma), )


############## DESCRITIVAS
our_summary1 <-
  list(
    "Desligamentos" =
      list(
        "Desligados" = ~ round(mean(.data$var_y *100),2)
      ),
    "Período de Desligamento" =
      list(
        "Desligados Pós" = ~ round(mean(.data$deslig_pos *100),2),
        "Desligados Pré" = ~ round(mean(.data$deslig_pre *100),2)
      ),
    "Características do Desligamento" =
      list(
        "Desligados Iniciativa Firma" = ~ round(mean(.data$var_y_firma *100),2),
        "Desligados Contrato CLT Tempo Indeterminado" = ~ round(mean(.data$var_y_clt_indeterminado *100),2),
        "Desligados por Desaparecimento da Amostra" = ~ round(mean(.data$ab *100),2) 
        
        
      ))

descritivas=summary_table(consolidada,our_summary1 )
descritivas

# Juntando a coluna de categoria com periodo
consolidada = consolidada %>%
  mutate(categoria_periodo = paste(Categoria, year))

descritivas_agrupadas <- consolidada %>%
  group_by(categoria_periodo) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2010.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2010.csv')

