###########################################################
# NOME: ALGUMAS LIMPEZAS.R
# AUTOR: FILLIPE GUEDES
# DESCRICAO: REALIZANDO ALGUMAS LIMPEZAS NA BASE DE DADOS. 
# ESTAS LIMPEZAS CONSISTEM EM CRIAR VARIAVEIS: CATEGORIA,
# FEMININO E FAIXA DE TEMPO NO EMPREGO, 
###########################################################

########################## 2010 ###############################
library(tidyverse)
# BAIXANDO A BASE
base_2010 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2010')

# Criando uma coluna para categorizar as observacoes
base_2010 = base_2010 %>%
  mutate(Categoria = if_else(grupo_trat==1, 'Grupo de Tratamento', 
                             if_else(controle1==1, 'Grupo de Controle 1', 
                                     if_else(controle2==1, 'Grupo de Controle 2', 
                                             if_else(controle3==1, 'Grupo de Controle 3', '')))))

base_2010 = base_2010%>%
  mutate(Categoria = as.factor(Categoria))

# Criando uma variavel para o genero feminino
base_2010 = base_2010 %>%
  mutate(Feminino = if_else(genero==0,1,0))

# Criando uma Variavel de Faixa de Tempo no Emprego
base_2010 = base_2010 %>%
  mutate(fx_temp_empr1 = ifelse(temp_empr<=6,1,0))

base_2010 = base_2010 %>%
  mutate(fx_temp_empr2 = ifelse(temp_empr>6 & temp_empr<=12,1,0))

base_2010 = base_2010 %>%
  mutate(fx_temp_empr3 = ifelse(temp_empr>12 & temp_empr<=18,1,0))

base_2010 = base_2010 %>%
  mutate(fx_temp_empr4 = ifelse(temp_empr> 18,1,0))

# Zerando as variaveis importantes que contem NA
base_2010$mes_adm[is.na(base_2010$mes_adm)] = 0


#################### NOVO
# Zerando a var dependente das pessoas que foram contratadas apos o mes 10 do periodo pre
base_2010=base_2010 %>%
  mutate(var_y = ifelse(year==1 & mes_adm > 10, NA, var_y), 
         var_y_firma  = ifelse(year==1 & mes_adm > 10, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==1 & mes_adm > 10,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==1 & mes_adm > 10,  NA,  var_y_clt_indeterminado_e_firma))





# Zerando a var dependente das pessoas que foram contratadas apos o mes 3
base_2010=base_2010 %>%
  mutate(var_y = ifelse(year==2 & mes_adm > 3, NA, var_y), 
         var_y_firma  = ifelse(year==2 & mes_adm > 3, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==2 & mes_adm > 3,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==2 & mes_adm > 3,  NA,  var_y_clt_indeterminado_e_firma))




# CHECAGENS
amostrinha = base_2010[1:1000,]
  
amostrinha_1 = amostrinha %>%
  select(pis,cnpj,concatenate2, year,mes_deslig_atualizado, var_y)

amostrinha_2 = amostrinha %>%
  select(pis,cnpj,concatenate2, year,mes_deslig_atualizado, var_y, deslig_pre, deslig_pos)

amostrinha_3 = amostrinha %>%
  select(pis,cnpj,concatenate2, year,mes_deslig_atualizado, var_y, causa_desli, var_y_firma)

saveRDS(base_2010, 'E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2010_LIMPA')
rm(base_2010)
gc()
########################## 2011 ###############################

# BAIXANDO A BASE
base_2011 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2011')
gc()
# Criando uma coluna para categorizar as observacoes
base_2011 = base_2011 %>%
  mutate(Categoria = if_else(grupo_trat==1, 'Grupo de Tratamento', 
                             if_else(controle1==1, 'Grupo de Controle 1', 
                                     if_else(controle2==1, 'Grupo de Controle 2', 
                                             if_else(controle3==1, 'Grupo de Controle 3', '')))))

base_2011 = base_2011%>%
  mutate(Categoria = as.factor(Categoria))

# Criando uma variavel para o genero feminino
base_2011 = base_2011 %>%
  mutate(Feminino = if_else(genero==0,1,0))

# Criando uma Variavel de Faixa de Tempo no Emprego
base_2011 = base_2011 %>%
  mutate(fx_temp_empr1 = ifelse(temp_empr<=6,1,0))

base_2011 = base_2011 %>%
  mutate(fx_temp_empr2 = ifelse(temp_empr>6 & temp_empr<=12,1,0))

base_2011 = base_2011 %>%
  mutate(fx_temp_empr3 = ifelse(temp_empr>12 & temp_empr<=18,1,0))

base_2011 = base_2011 %>%
  mutate(fx_temp_empr4 = ifelse(temp_empr> 18,1,0))

# Zerando as variaveis importantes que contem NA
base_2011$mes_adm[is.na(base_2011$mes_adm)] = 0


#################### NOVO
# Zerando a var dependente das pessoas que foram contratadas apos o mes 10 do periodo pre
base_2011=base_2011 %>%
  mutate(var_y = ifelse(year==1 & mes_adm > 10, NA, var_y), 
         var_y_firma  = ifelse(year==1 & mes_adm > 10, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==1 & mes_adm > 10,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==1 & mes_adm > 10,  NA,  var_y_clt_indeterminado_e_firma))

# Zerando a var dependente das pessoas que foram contratadas apos o mes 3
base_2011=base_2011 %>%
  mutate(var_y = ifelse(year==2 & mes_adm > 3, NA, var_y), 
         var_y_firma  = ifelse(year==2 & mes_adm > 3, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==2 & mes_adm > 3,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==2 & mes_adm > 3,  NA,  var_y_clt_indeterminado_e_firma))


saveRDS(base_2011, 'E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2011_LIMPA')
rm(base_2011)
gc()
########################## 2012 ###############################

# BAIXANDO A BASE
base_2012 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2012')

# Criando uma coluna para categorizar as observacoes
base_2012 = base_2012 %>%
  mutate(Categoria = if_else(grupo_trat==1, 'Grupo de Tratamento', 
                             if_else(controle1==1, 'Grupo de Controle 1', 
                                     if_else(controle2==1, 'Grupo de Controle 2', 
                                             if_else(controle3==1, 'Grupo de Controle 3', '')))))

base_2012 = base_2012%>%
  mutate(Categoria = as.factor(Categoria))

# Criando uma variavel para o genero feminino
base_2012 = base_2012 %>%
  mutate(Feminino = if_else(genero==0,1,0))

# Criando uma Variavel de Faixa de Tempo no Emprego
base_2012 = base_2012 %>%
  mutate(fx_temp_empr1 = ifelse(temp_empr<=6,1,0))

base_2012 = base_2012 %>%
  mutate(fx_temp_empr2 = ifelse(temp_empr>6 & temp_empr<=12,1,0))

base_2012 = base_2012 %>%
  mutate(fx_temp_empr3 = ifelse(temp_empr>12 & temp_empr<=18,1,0))

base_2012 = base_2012 %>%
  mutate(fx_temp_empr4 = ifelse(temp_empr> 18,1,0))

# Zerando as variaveis importantes que contem NA
base_2012$mes_adm[is.na(base_2012$mes_adm)] = 0


#################### NOVO
# Zerando a var dependente das pessoas que foram contratadas apos o mes 10 do periodo pre
base_2012=base_2012 %>%
  mutate(var_y = ifelse(year==1 & mes_adm > 10, NA, var_y), 
         var_y_firma  = ifelse(year==1 & mes_adm > 10, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==1 & mes_adm > 10,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==1 & mes_adm > 10,  NA,  var_y_clt_indeterminado_e_firma))

# Zerando a var dependente das pessoas que foram contratadas apos o mes 3
base_2012=base_2012 %>%
  mutate(var_y = ifelse(year==2 & mes_adm > 3, NA, var_y), 
         var_y_firma  = ifelse(year==2 & mes_adm > 3, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==2 & mes_adm > 3,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==2 & mes_adm > 3,  NA,  var_y_clt_indeterminado_e_firma))


saveRDS(base_2012, 'E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2012_LIMPA')
rm(base_2012)
gc()
########################## 2013 ###############################

# BAIXANDO A BASE
base_2013 = readRDS('E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2013')

# Criando uma coluna para categorizar as observacoes
base_2013 = base_2013 %>%
  mutate(Categoria = if_else(grupo_trat==1, 'Grupo de Tratamento', 
                             if_else(controle1==1, 'Grupo de Controle 1', 
                                     if_else(controle2==1, 'Grupo de Controle 2', 
                                             if_else(controle3==1, 'Grupo de Controle 3', '')))))

base_2013 = base_2013%>%
  mutate(Categoria = as.factor(Categoria))

# Criando uma variavel para o genero feminino
base_2013 = base_2013 %>%
  mutate(Feminino = if_else(genero==0,1,0))

# Criando uma Variavel de Faixa de Tempo no Emprego
base_2013 = base_2013 %>%
  mutate(fx_temp_empr1 = ifelse(temp_empr<=6,1,0))

base_2013 = base_2013 %>%
  mutate(fx_temp_empr2 = ifelse(temp_empr>6 & temp_empr<=12,1,0))

base_2013 = base_2013 %>%
  mutate(fx_temp_empr3 = ifelse(temp_empr>12 & temp_empr<=18,1,0))

base_2013 = base_2013 %>%
  mutate(fx_temp_empr4 = ifelse(temp_empr> 18,1,0))

# Zerando as variaveis importantes que contem NA
base_2013$mes_adm[is.na(base_2013$mes_adm)] = 0


#################### NOVO
# Zerando a var dependente das pessoas que foram contratadas apos o mes 10 do periodo pre
base_2013=base_2013 %>%
  mutate(var_y = ifelse(year==1 & mes_adm > 10, NA, var_y), 
         var_y_firma  = ifelse(year==1 & mes_adm > 10, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==1 & mes_adm > 10,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==1 & mes_adm > 10,  NA,  var_y_clt_indeterminado_e_firma))

# Zerando a var dependente das pessoas que foram contratadas apos o mes 3
base_2013=base_2013 %>%
  mutate(var_y = ifelse(year==2 & mes_adm > 3, NA, var_y), 
         var_y_firma  = ifelse(year==2 & mes_adm > 3, NA, var_y_firma),
         var_y_clt_indeterminado = ifelse(year==2 & mes_adm > 3,  NA, var_y_clt_indeterminado), 
         var_y_clt_indeterminado_e_firma = ifelse(year==2 & mes_adm > 3,  NA,  var_y_clt_indeterminado_e_firma))


saveRDS(base_2013, 'E:/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2013_LIMPA')