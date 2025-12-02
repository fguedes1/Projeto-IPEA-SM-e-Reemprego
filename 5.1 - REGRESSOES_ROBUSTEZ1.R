####################################################################
# NOME: REGRESSOES
# AUTOR: FILLIPE
# DESCRIÇÃO: RODAGENS DAS REGRESSOES
###################################################################
library(tidyverse)
library(fixest)
library(modelsummary)

# Carregando as bases em RDs
base_2010 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2010_LIMPA')

############ ALGUMAS LIMPEZAS

# Removendo mes de desligamento igual a 99
base_2010 = filter(base_2010, is.na(mes_deslig)|mes_deslig!=99)

# ZERANDO A VAR DEPENDENTE DE QUEM DESAPARECEU DA AMOSTRA
base_2010 = base_2010 %>%
  mutate(ab = ifelse(is.na(ab), 0, ab))

base_2010 = base_2010 %>%
  mutate(var_y = ifelse(ab==1, NA, var_y))

########################## REGRESSOES
controle1_2010  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio, 
                           se = 'hetero',
                           subset =  c(base_2010$grupo_trat==1|base_2010$controle1==1),
                           data = base_2010)
saveRDS(controle1_2010, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2010.RDs')


controle1_2010_EF  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio|as.factor(cnpj), 
                           se = 'hetero',
                           subset =  c(base_2010$grupo_trat==1|base_2010$controle1==1),
                           data = base_2010)
saveRDS(controle1_2010_EF, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2010_EF.RDs')


# GRUPO DE CONTROLE 2
controle2_2010  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio, 
                           se = 'hetero',
                           subset =  c(base_2010$grupo_trat==1|base_2010$controle2==1),
                           data = base_2010)
saveRDS(controle2_2010, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2010.RDs')

controle2_2010_EF  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio|as.factor(cnpj), 
                           se = 'hetero',
                           subset =  c(base_2010$grupo_trat==1|base_2010$controle2==1),
                           data = base_2010)
saveRDS(controle2_2010_EF, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2010_EF.RDs')



rm(list=ls())
gc()
#######################################################################################
# Carregando as bases em RDs
base_2011 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2011_LIMPA')

############ ALGUMAS LIMPEZAS

# Removendo mes de desligamento igual a 99
base_2011 = filter(base_2011, is.na(mes_deslig)|mes_deslig!=99)

# ZERANDO A VAR DEPENDENTE DE QUEM DESAPARECEU DA AMOSTRA
base_2011 = base_2011 %>%
  mutate(ab = ifelse(is.na(ab), 0, ab))

base_2011 = base_2011 %>%
  mutate(var_y = ifelse(ab==1, NA, var_y))


########################## REGRESSOES
controle1_2011  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio, 
                           se = 'hetero',
                           subset =  c(base_2011$grupo_trat==1|base_2011$controle1==1),
                           data = base_2011)
saveRDS(controle1_2011, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2011.RDs')

controle1_2011_EF  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio|as.factor(cnpj), 
                           se = 'hetero',
                           subset =  c(base_2011$grupo_trat==1|base_2011$controle1==1),
                           data = base_2011)
saveRDS(controle1_2011_EF, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2011_EF.RDs')


# GRUPO DE CONTROLE 2
controle2_2011  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio, 
                           se = 'hetero',
                           subset =  c(base_2011$grupo_trat==1|base_2011$controle2==1),
                           data = base_2011)
saveRDS(controle2_2011, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2011.RDs')


controle2_2011_EF  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio|as.factor(cnpj), 
                           se = 'hetero',
                           subset =  c(base_2011$grupo_trat==1|base_2011$controle2==1),
                           data = base_2011)
saveRDS(controle2_2011_EF, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2011_EF.RDs')


rm(list=ls())
gc()
################################################################################
# Carregando as bases em RDs
base_2012 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2012_LIMPA')

############ ALGUMAS LIMPEZAS

# Removendo mes de desligamento igual a 99
base_2012 = filter(base_2012, is.na(mes_deslig)|mes_deslig!=99)

# ZERANDO A VAR DEPENDENTE DE QUEM DESAPARECEU DA AMOSTRA
base_2012 = base_2012 %>%
  mutate(ab = ifelse(is.na(ab), 0, ab))

base_2012 = base_2012 %>%
  mutate(var_y = ifelse(ab==1, NA, var_y))


########################## REGRESSOES
controle1_2012  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio, 
                           se = 'hetero',
                           subset =  c(base_2012$grupo_trat==1|base_2012$controle1==1),
                           data = base_2012)
saveRDS(controle1_2012, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2012.RDs')



controle1_2012_EF  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio|as.factor(cnpj), 
                           se = 'hetero',
                           subset =  c(base_2012$grupo_trat==1|base_2012$controle1==1),
                           data = base_2012)
saveRDS(controle1_2012_EF, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle1_2012_EF.RDs')


# GRUPO DE CONTROLE 2
controle2_2012  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio, 
                           se = 'hetero',
                           subset =  c(base_2012$grupo_trat==1|base_2012$controle2==1),
                           data = base_2012)
saveRDS(controle2_2012, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2012.RDs')


controle2_2012_EF  = feols(var_y~0+grupo_trat + periodo + trat_p + genero + educa2 +  educa3 + educa4 +
                             fx_temp_empr2 + fx_temp_empr3 + fx_temp_empr4 +fx_idade2 + fx_idade3 + 
                             part_time + 
                             industria + servicos + construcao+ comercio|as.factor(cnpj), 
                           se = 'hetero',
                           subset =  c(base_2012$grupo_trat==1|base_2012$controle2==1),
                           data = base_2012)
saveRDS(controle2_2012_EF, 'E:/salmin_reg2/R Sal Min/REGRESSÕES/GERAL/robustez1_Controle2_2012_EF.RDs')


rm(list=ls())
gc()
