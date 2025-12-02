################################################################################
# NOME: DESCRITIVAS POR ANO AMOSTRA GERAL.R
# AUTOR: FILLIPE
# DESCRICAO: CRIACAO DAS TABELAS DE ESTATISTICAS DESCRITIVAS PARA A AMOSTRA PARA
# CADA UM DOS ANOS
################################################################################
library(qwraps2)
library(tidyverse)
memory.limit(size=10000000000)

################################################################################
############################### 2010 ###########################################
################################################################################

# BAIXANDO A BASE
base_2010 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2010_LIMPA')

# Retirando algumas colunas que nao vou usar para a base ficar mais leve
colunas_retirar = c("nat_jur", "totemp6", "totemp1", "RENO", "RENE", "RESE", "RSUL", "RECO",
                    "adm_pub", "agricultura", "outros", "ocup1", "ocup2", "ocup3", "ocup4",
                    "ocup5", "ocup6", "ocup7", "ocup8", "ocup9", "ocup10")

base_2010 = select(base_2010, select = -colunas_retirar)

# FAZENDO ALGUMAS VERIFICACOES 
amostrinha = base_2010[1:1000,]

amostrinha = amostrinha %>%
  select(pis, year, mes_adm, mes_deslig_atualizado, ab, var_y, deslig_pre, deslig_pos,
         causa_desli,var_y_firma, tipo_vinculo, var_y_clt_indeterminado, 
         var_y_clt_indeterminado_e_firma)


############ INFOS A SEREM USADAS 

# Removendo mes de desligamento igual a 99
base_2010 = filter(base_2010, is.na(mes_deslig)|mes_deslig!=99)
nas=which(is.na(base_2010$var_y)) 


# Removendo as linhas com var dependente NA para criar as descritivas
base_2010 = filter(base_2010, !is.na(var_y))

# Substituindo algumas variáveis que contém missing por 0 para usar
# nas descritivas
base_2010 <- base_2010 %>%
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

descritivas=summary_table(base_2010,our_summary1 )
descritivas

descritivas_agrupadas <- base_2010 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2010.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2010.csv')

rm(list=ls())
gc()

################################################################################
############################### 2011 ###########################################
################################################################################

# BAIXANDO A BASE
base_2011 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2011_LIMPA')

# Retirando algumas colunas que nao vou usar para a base ficar mais leve
colunas_retirar = c("nat_jur", "totemp6", "totemp1", "RENO", "RENE", "RESE", "RSUL", "RECO",
                    "adm_pub", "agricultura", "outros", "ocup1", "ocup2", "ocup3", "ocup4",
                    "ocup5", "ocup6", "ocup7", "ocup8", "ocup9", "ocup10")

base_2011 = select(base_2011, select = -colunas_retirar)

# Para ser um bienio (par de anos), tenho que eliminar as infos de t-1

############ ALGUMAS LIMPEZAS

# Removendo mes de desligamento igual a 99
base_2011 = filter(base_2011, is.na(mes_deslig)|mes_deslig!=99)
nas=which(is.na(base_2011$var_y)) 


# Removendo as linhas com var dependente NA para criar as descritivas
base_2011 = filter(base_2011, !is.na(var_y))


# Substituindo algumas variáveis que contém missing por 0 para usar
# nas descritivas
base_2011 <- base_2011 %>%
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


descritivas=summary_table(base_2011,our_summary1 )
descritivas

descritivas_agrupadas <- base_2011 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2011.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2011.csv')

rm(list=ls())
gc()

################################################################################
############################### 2012 ###########################################
################################################################################

# BAIXANDO A BASE
base_2012 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2012_LIMPA')

# Retirando algumas colunas que nao vou usar para a base ficar mais leve
colunas_retirar = c("nat_jur", "totemp6", "totemp1", "RENO", "RENE", "RESE", "RSUL", "RECO",
                    "adm_pub", "agricultura", "outros", "ocup1", "ocup2", "ocup3", "ocup4",
                    "ocup5", "ocup6", "ocup7", "ocup8", "ocup9", "ocup10")

base_2012 = select(base_2012, select = -colunas_retirar)

# Para ser um bienio (par de anos), tenho que eliminar as infos de t-1

############ ALGUMAS LIMPEZAS

# Removendo mes de desligamento igual a 99
base_2012 = filter(base_2012, is.na(mes_deslig)|mes_deslig!=99)
nas=which(is.na(base_2012$var_y)) 


# Removendo as linhas com var dependente NA para criar as descritivas
base_2012 = filter(base_2012, !is.na(var_y))

# Substituindo algumas variáveis que contém missing por 0 para usar
# nas descritivas
base_2012 <- base_2012 %>%
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


descritivas=summary_table(base_2012,our_summary1 )
descritivas

descritivas_agrupadas <- base_2012 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2012.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2012.csv')

rm(list=ls())
gc()

################################################################################
############################### 2013 ###########################################
################################################################################

# BAIXANDO A BASE
base_2013 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2013_LIMPA')

# Retirando algumas colunas que nao vou usar para a base ficar mais leve
colunas_retirar = c("nat_jur", "totemp6", "totemp1", "RENO", "RENE", "RESE", "RSUL", "RECO",
                    "adm_pub", "agricultura", "outros", "ocup1", "ocup2", "ocup3", "ocup4",
                    "ocup5", "ocup6", "ocup7", "ocup8", "ocup9", "ocup10")

base_2013 = select(base_2013, select = -colunas_retirar)

# Para ser um bienio (par de anos), tenho que eliminar as infos de t-1

############ ALGUMAS LIMPEZAS

# Removendo mes de desligamento igual a 99
base_2013 = filter(base_2013, is.na(mes_deslig)|mes_deslig!=99)
nas=which(is.na(base_2013$var_y)) 


# Removendo as linhas com var dependente NA para criar as descritivas
base_2013 = filter(base_2013, !is.na(var_y))

# Substituindo algumas variáveis que contém missing por 0 para usar
# nas descritivas
base_2013 <- base_2013 %>%
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



descritivas=summary_table(base_2013,our_summary1 )
descritivas

descritivas_agrupadas <- base_2013 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2013.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_AMOSTRA GERAL_2013.csv')

rm(list=ls())
gc()