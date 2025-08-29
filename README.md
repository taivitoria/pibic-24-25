# pibic-24-25
repositorio para o pibic 2024 - 2025

códigos utilizado no projeto:

escores 2011

Lambda <- resultadopc$loadings
Psi <- diag(resultadopc$uniquenesses)
Psi_inv <- diag(1 / diag(Psi))
score.z<-NULL
score_z = scale(dadospadronizados2011, center = T,scale = T)
score_z[1,]
M <- solve(t(lambda_matriz) %*% Psi_inv %*% lambda_matriz)
matrix(Lambda,21,5) -> lambda_matriz
F_scores <- M %*% t(lambda_matriz) %*% Psi_inv %*% t(dadospadronizados2011)

F2011 <- t(F_scores)

dim(F_scores)



dim(M)
dim(lambda_matriz)
dim(Psi_inv)
dim(dadospadronizados)

escores 2022

Lambda_2022 <- resultado2022$loadings
Psi_2022 <- diag(resultado2022$uniquenesses)
Psi_inv_2022 <- diag(1 / diag(Psi_2022))
score.z_2022<-NULL
score_z_2022 = scale(dadospadronizados2022, center = T,scale = T)
score_z_2022[1,]
M_2 <- solve(t(lambda_matriz) %*% Psi_inv %*% lambda_matriz)
matrix(Lambda,21,5) -> lambda_matriz_2
F_scores_2 <- M_2 %*% t(Lambda_2022) %*% Psi_inv_2022 %*% t(dadospadronizados2022)
F2022 <- t(F_scores_2)

dim(dadospadronizados2022)
dim(M_2)
dim(Psi_inv_2022)
dim(Psi_2022)
dim(Lambda_2022)

escores comparados

saude2022 <- saude_2022_xlsx_dados_saude_ba_2022_1_

Lambda <- resultadopc$loadings
Psi <- diag(resultadopc$uniquenesses)
Psi_inv <- diag(1 / diag(Psi))
score.z<-NULL
score_z = scale(saude2022, center = T,scale = T)
score_z[1,]
M <- solve(t(lambda_matriz) %*% Psi_inv %*% lambda_matriz)
F_2022 <- M %*% t(lambda_matriz) %*% Psi_inv %*% t(saude2022)

F <- t(F_2022)

dim(F_2022)

matrix(Lambda,21,5) -> lambda_matriz

análise fatorial para o ano de 2011

install.packages("psych")
library(psych)

dadospadronizados2011 <- scale(dados_2011_comparada_4)

resultadopc <- principal(dadospadronizados2011, nfactors = 5, rotate = "varimax")
print(resultadopc$loadings)  # Cargas fatoriais após rotação
print(resultadopc$Vaccounted)  # Variância explicada
print(resultadopc$communality)  # Comunalidades

kmoresultado <- KMO(dadospadronizados2011)
print(kmoresultado)

escores_fatoriais <- resultadopc$scores
head(escores_fatoriais)

scree(dadospadronizados, factors = TRUE, pc = TRUE)

cor(dados_2011_comparada_4)

analise fatorial 2022

#modelo final

install.packages("psych")
library(psych)

dados_2022 <- dados_saude_ba_2022_5_dados_saude_ba_2022_2_

dadospadronizados2022 <- scale(dados_2022)

resultado2022 <- principal(dadospadronizados2022, nfactors = 5, rotate = "varimax")
print(resultado2022$loadings)  # Cargas fatoriais após rotação
print(resultado2022$Vaccounted)  # Variância explicada
print(resultado2022$communality)  # Comunalidades

kmo2022 <- KMO(dadospadronizados2022)
print(kmo2022)

escores2022 <- resultadopc$scores
head(escores2022)

scree(dadospadronizados2022, factors = TRUE, pc = TRUE)

cor(dados_2011_comparada_4)

help(principal)

mapas 2011

nrow(municipios)
nrow(F2011)

F2011_df <- as.data.frame(F2011)

base_final11 <- cbind(municipios, F2011_df)

novo_shapefile <- st_read("/cloud/project/BA_Municipios_2022.shp")

library(writexl)
write_xlsx(base_final11, "escores_fatoriais_2011.xlsx")

head(novo_shapefile)
head(base_final11)

mapa_F2011 <- merge(novo_shapefile, base_final11, by.x = "NM_MUN", by.y = "municipio")

attach(base_final11)

#mapa fator 1

ggplot(data = mapa_F2011) +
  geom_sf(aes(fill = V1), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-20, 20)) +  # <- padroniza a escala
  labs(title = "Cobertura Vacinal em 2011",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)
#mapa fator 2

ggplot(data = mapa_F2011) +
  geom_sf(aes(fill = mapa_F2011$V2), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-5, 5)) +  # <- padroniza a escala
  labs(title = "Estruturia Etária em 2011",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 3

ggplot(data = mapa_F2011) +
  geom_sf(aes(fill = mapa_F2011$V3), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-10, 10)) +  # <- padroniza a escala
  labs(title = "Mortalidade e Doenças Cardiovasculares em 2011",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 4
ggplot(data = mapa_F2011) +
  geom_sf(aes(fill = mapa_F2011$V4), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-5, 5)) +  # <- padroniza a escala
  labs(title = "Saúde Reprodutiva em 2011",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 5

ggplot(data = mapa_F2011) +
  geom_sf(aes(fill = mapa_F2011$V5), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-15, 15)) +  # <- padroniza a escala
  labs(title = "Mortalidade Neonatal e Pré-Natal em 2011",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

mapas 2022 

nrow(municipios)
nrow(F2022)

F2022_df <- as.data.frame(F2022)

base_final <- cbind(municipios, F2022_df)

library(writexl)
write_xlsx(base_final, "escores_fatoriais_2022.xlsx")

install.packages("writexl")

#mapa

# Carregar pacotes (se necessário)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(viridis)

#    sf: Leitura e manipulação de dados geográficos.
# ggplot2: Criação de gráficos de alta qualidade.
# ggthemes: Temas pré-definidos para gráficos, incluindo temas para mapas.
# viridis: Paletas de cores perceptualmente uniformes.

#Shapefile -> Carregue o shapefile do mapa da Bahia utilizando a função st_read()

#Dados de Saúde -> Carregue os dados de saúde em um dataframe.

novo_shapefile <- st_read("/cloud/project/BA_Municipios_2022.shp")

head(novo_shapefile)
head(base_final)

mapa_F2022 <- merge(novo_shapefile, base_final, by.x = "NM_MUN", by.y = "municipio")

#Merge: Combine o shapefile com os dados de saúde 
#utilizando a função merge(), especificando a coluna chave comum (por exemplo, "NM_MUN"): 

attach(base_final)


#Adicione uma camada geométrica para representar o mapa
#mapa fator 1

ggplot(data = mapa_F2022) +
  geom_sf(aes(fill = V1)) +
  scale_fill_gradient(low = "white", high = "darkred", name = "Fator 1") +
  labs(title = "Cobertura Vacinal em 2022") +
  theme_map()

#mapa2

ggplot(data = mapa_F2022) +
  geom_sf(aes(fill = V1), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-20, 20)) +
  labs(title = "Cobertura Vacinal em 2022",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa3

ggplot(data = mapa_F2022$V1) +
  geom_sf(aes(fill = V1), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-7, 7)) +  # <- padroniza a escala
  labs(title = "Cobertura Vacinal em 2022",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 2

ggplot(data = mapa_F2022) +
  geom_sf(aes(fill = mapa_F2022$V2), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-5, 5)) +  # <- padroniza a escala
  labs(title = "Estruturia Etária em 2022",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 3

ggplot(data = mapa_F2022) +
  geom_sf(aes(fill = mapa_F2022$V3), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-10, 10)) +  # <- padroniza a escala
  labs(title = "Mortalidade e Doenças Cardiovasculares em 2022",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 4

ggplot(data = mapa_F2022) +
  geom_sf(aes(fill = mapa_F2022$V4), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-5, 5)) +  # <- padroniza a escala
  labs(title = "Saúde Reprodutiva em 2022",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)

#mapa fator 5

ggplot(data = mapa_F2022) +
  geom_sf(aes(fill = mapa_F2022$V5), color = "black") +
  scale_fill_viridis(option = "magma", 
                     name = "Incidência",
                     limits = c(-15, 15)) +  # <- padroniza a escala
  labs(title = "Mortalidade Neonatal e Pré-Natal em 2022",
       fill = "Incidência") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed")) +
  coord_sf(datum = NA)


help(principal)
