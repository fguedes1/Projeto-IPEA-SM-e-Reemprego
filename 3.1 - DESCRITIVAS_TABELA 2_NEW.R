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
saveRDS(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL.RDs')
write.csv2(descritivas_agrupadas, 'E:/salmin_reg2/R Sal Min/TABELAS - DESCRITIVAS/DESCRITIVAS_TABELA 2_AMOSTRA GERAL.csv')
