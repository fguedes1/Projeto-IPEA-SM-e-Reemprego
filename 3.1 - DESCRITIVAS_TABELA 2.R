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


############ INFOS A SEREM USADAS 

# SOMENTE JANELA PRÉ
base_2010 = base_2010 %>%
  filter(year==1)

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

our_summary1 <-
  list(
    "Sexo" =
      list(
        "Masculino" = ~ round(mean(.data$genero*100),2), 
        "Feminino" = ~ round(mean(.data$Feminino*100),2)
      ),
    "Educação" =
      list(
        "0-4 anos de estudo" = ~ round(mean(.data$educa1*100),2), 
        "5-8 anos de estudo" = ~ round(mean(.data$educa2*100),2), 
        "9-11 anos de estudo" = ~ round(mean(.data$educa3*100),2), 
        "12 ou mais anos de estudo" = ~ round(mean(.data$educa4*100),2)
      ),
    "Faixa Idade" =
      list( "16-24 anos" = ~ round(mean(.data$fx_idade1*100),2), 
            "25-49 anos" = ~ round(mean(.data$fx_idade2*100),2), 
            "50 anos ou mais" = ~ round(mean(.data$fx_idade3*100),2)
      ),
    "Trabalho Parcial" =
      list( "Trabalhador Parcial" = ~ round(mean(.data$part_time*100),2)
      ),
    "Tempo no Emprego" =
      list( "0-6 meses" = ~ round(mean(.data$fx_temp_empr1*100),2), 
            ">6 meses-12 meses" = ~ round(mean(.data$fx_temp_empr2*100),2), 
            ">12 meses-18 meses" = ~ round(mean(.data$fx_temp_empr3*100),2), 
            ">18 meses" = ~ round(mean(.data$fx_temp_empr4*100),2)
      ),
    "Tamanho do Estabelecimento" =
      list( "0 a 9 empregados" = ~ round(mean(.data$tam_estab1*100),2), 
            "10 a 99 empregados" = ~ round(mean(.data$tam_estab2*100),2), 
            "100 a 499 empregados" = ~ round(mean(.data$tam_estab3*100),2), 
            "500 ou mais empregados" = ~ round(mean(.data$tam_estab4*100),2)
      ),
    "Setor" =
      list( "Indústria" = ~ round(mean(.data$industria*100),2), 
            "Serviços" = ~ round(mean(.data$servicos*100),2), 
            "Construção" = ~ round(mean(.data$construcao*100),2),
            "Comércio" = ~ round(mean(.data$comercio*100),2)
      ))

descritivas=summary_table(base_2010,our_summary1 )
descritivas

descritivas_agrupadas <- base_2010 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2010.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2010.csv')

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


############ INFOS A SEREM USADAS 

# SOMENTE JANELA PRÉ
base_2011 = base_2011 %>%
  filter(year==1)

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

our_summary1 <-
  list(
    "Sexo" =
      list(
        "Masculino" = ~ round(mean(.data$genero*100),2), 
        "Feminino" = ~ round(mean(.data$Feminino*100),2)
      ),
    "Educação" =
      list(
        "0-4 anos de estudo" = ~ round(mean(.data$educa1*100),2), 
        "5-8 anos de estudo" = ~ round(mean(.data$educa2*100),2), 
        "9-11 anos de estudo" = ~ round(mean(.data$educa3*100),2), 
        "12 ou mais anos de estudo" = ~ round(mean(.data$educa4*100),2)
      ),
    "Faixa Idade" =
      list( "16-24 anos" = ~ round(mean(.data$fx_idade1*100),2), 
            "25-49 anos" = ~ round(mean(.data$fx_idade2*100),2), 
            "50 anos ou mais" = ~ round(mean(.data$fx_idade3*100),2)
      ),
    "Trabalho Parcial" =
      list( "Trabalhador Parcial" = ~ round(mean(.data$part_time*100),2)
      ),
    "Tempo no Emprego" =
      list( "0-6 meses" = ~ round(mean(.data$fx_temp_empr1*100),2), 
            ">6 meses-12 meses" = ~ round(mean(.data$fx_temp_empr2*100),2), 
            ">12 meses-18 meses" = ~ round(mean(.data$fx_temp_empr3*100),2), 
            ">18 meses" = ~ round(mean(.data$fx_temp_empr4*100),2)
      ),
    "Tamanho do Estabelecimento" =
      list( "0 a 9 empregados" = ~ round(mean(.data$tam_estab1*100),2), 
            "10 a 99 empregados" = ~ round(mean(.data$tam_estab2*100),2), 
            "100 a 499 empregados" = ~ round(mean(.data$tam_estab3*100),2), 
            "500 ou mais empregados" = ~ round(mean(.data$tam_estab4*100),2)
      ),
    "Setor" =
      list( "Indústria" = ~ round(mean(.data$industria*100),2), 
            "Serviços" = ~ round(mean(.data$servicos*100),2), 
            "Construção" = ~ round(mean(.data$construcao*100),2),
            "Comércio" = ~ round(mean(.data$comercio*100),2)
      ))

descritivas=summary_table(base_2011,our_summary1 )
descritivas

descritivas_agrupadas <- base_2011 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2011.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2011.csv')

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


############ INFOS A SEREM USADAS 

# SOMENTE JANELA PRÉ
base_2012 = base_2012 %>%
  filter(year==1)

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

our_summary1 <-
  list(
    "Sexo" =
      list(
        "Masculino" = ~ round(mean(.data$genero*100),2), 
        "Feminino" = ~ round(mean(.data$Feminino*100),2)
      ),
    "Educação" =
      list(
        "0-4 anos de estudo" = ~ round(mean(.data$educa1*100),2), 
        "5-8 anos de estudo" = ~ round(mean(.data$educa2*100),2), 
        "9-11 anos de estudo" = ~ round(mean(.data$educa3*100),2), 
        "12 ou mais anos de estudo" = ~ round(mean(.data$educa4*100),2)
      ),
    "Faixa Idade" =
      list( "16-24 anos" = ~ round(mean(.data$fx_idade1*100),2), 
            "25-49 anos" = ~ round(mean(.data$fx_idade2*100),2), 
            "50 anos ou mais" = ~ round(mean(.data$fx_idade3*100),2)
      ),
    "Trabalho Parcial" =
      list( "Trabalhador Parcial" = ~ round(mean(.data$part_time*100),2)
      ),
    "Tempo no Emprego" =
      list( "0-6 meses" = ~ round(mean(.data$fx_temp_empr1*100),2), 
            ">6 meses-12 meses" = ~ round(mean(.data$fx_temp_empr2*100),2), 
            ">12 meses-18 meses" = ~ round(mean(.data$fx_temp_empr3*100),2), 
            ">18 meses" = ~ round(mean(.data$fx_temp_empr4*100),2)
      ),
    "Tamanho do Estabelecimento" =
      list( "0 a 9 empregados" = ~ round(mean(.data$tam_estab1*100),2), 
            "10 a 99 empregados" = ~ round(mean(.data$tam_estab2*100),2), 
            "100 a 499 empregados" = ~ round(mean(.data$tam_estab3*100),2), 
            "500 ou mais empregados" = ~ round(mean(.data$tam_estab4*100),2)
      ),
    "Setor" =
      list( "Indústria" = ~ round(mean(.data$industria*100),2), 
            "Serviços" = ~ round(mean(.data$servicos*100),2), 
            "Construção" = ~ round(mean(.data$construcao*100),2),
            "Comércio" = ~ round(mean(.data$comercio*100),2)
      ))

descritivas=summary_table(base_2012,our_summary1 )
descritivas

descritivas_agrupadas <- base_2012 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2012.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2012.csv')

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


############ INFOS A SEREM USADAS 

# SOMENTE JANELA PRÉ
base_2013 = base_2013 %>%
  filter(year==1)

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

our_summary1 <-
  list(
    "Sexo" =
      list(
        "Masculino" = ~ round(mean(.data$genero*100),2), 
        "Feminino" = ~ round(mean(.data$Feminino*100),2)
      ),
    "Educação" =
      list(
        "0-4 anos de estudo" = ~ round(mean(.data$educa1*100),2), 
        "5-8 anos de estudo" = ~ round(mean(.data$educa2*100),2), 
        "9-11 anos de estudo" = ~ round(mean(.data$educa3*100),2), 
        "12 ou mais anos de estudo" = ~ round(mean(.data$educa4*100),2)
      ),
    "Faixa Idade" =
      list( "16-24 anos" = ~ round(mean(.data$fx_idade1*100),2), 
            "25-49 anos" = ~ round(mean(.data$fx_idade2*100),2), 
            "50 anos ou mais" = ~ round(mean(.data$fx_idade3*100),2)
      ),
    "Trabalho Parcial" =
      list( "Trabalhador Parcial" = ~ round(mean(.data$part_time*100),2)
      ),
    "Tempo no Emprego" =
      list( "0-6 meses" = ~ round(mean(.data$fx_temp_empr1*100),2), 
            ">6 meses-12 meses" = ~ round(mean(.data$fx_temp_empr2*100),2), 
            ">12 meses-18 meses" = ~ round(mean(.data$fx_temp_empr3*100),2), 
            ">18 meses" = ~ round(mean(.data$fx_temp_empr4*100),2)
      ),
    "Tamanho do Estabelecimento" =
      list( "0 a 9 empregados" = ~ round(mean(.data$tam_estab1*100),2), 
            "10 a 99 empregados" = ~ round(mean(.data$tam_estab2*100),2), 
            "100 a 499 empregados" = ~ round(mean(.data$tam_estab3*100),2), 
            "500 ou mais empregados" = ~ round(mean(.data$tam_estab4*100),2)
      ),
    "Setor" =
      list( "Indústria" = ~ round(mean(.data$industria*100),2), 
            "Serviços" = ~ round(mean(.data$servicos*100),2), 
            "Construção" = ~ round(mean(.data$construcao*100),2),
            "Comércio" = ~ round(mean(.data$comercio*100),2)
      ))

descritivas=summary_table(base_2013,our_summary1 )
descritivas

descritivas_agrupadas <- base_2013 %>%
  group_by(Categoria) %>%
  summary_table(our_summary1)
descritivas_agrupadas

# SALVANDO AS TABELAS
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2013.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL_2013.csv')

rm(list=ls())
gc()

