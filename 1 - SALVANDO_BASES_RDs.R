memory.limit(size = 200000000)
###########################################################
# NOME: SALVANDO BASES RDS.R
# AUTOR: FILLIPE GUEDES
# DESCRICAO: SALVANDO AS BASES DO STATA (ORIGINAL) PARA RDs
###########################################################
library(readstata13)

# CARREGANDO AS BASES EM FORMATO STATA
base_2010 = read.dta13('D:/Users/B207424813/Desktop/salmin_reg2/Bases/base1_2010_sm_ajustada.dta')
base_2011 = read.dta13('D:/Users/B207424813/Desktop/salmin_reg2/Bases/base1_2011_sm_ajustada.dta')
base_2012 = read.dta13('D:/Users/B207424813/Desktop/salmin_reg2/Bases/base1_2012_sm_ajustada.dta')
base_2013 = read.dta13('D:/Users/B207424813/Desktop/salmin_reg2/Bases/base1_2013_sm_ajustada.dta')

# Salvando as bases em RDs
saveRDS(base_2010, 'D:/Users/B207424813/Desktop/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2010')
saveRDS(base_2011, 'D:/Users/B207424813/Desktop/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2011')
saveRDS(base_2012, 'D:/Users/B207424813/Desktop/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2012')
saveRDS(base_2013, 'D:/Users/B207424813/Desktop/salmin_reg2/R Sal Min/BASES/GERAL/1-Consolidada_2013')


# Removendo as bases
rm(base_2010, base_2011, base_2012, base_2013)
gc()

