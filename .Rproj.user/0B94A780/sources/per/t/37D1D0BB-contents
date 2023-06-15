
## Análise de Cluster

# Autor: Clayton Del Tedesco Júnior


# Objetivo: categorizar partes de uma imagem e por meio da indentificação da quantidade
# de clusters, saber se uma peça está ou não com rosca em suas cavidades


pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4", #função 'ade4' para matriz de distâncias em var. binárias
             "hexView", "jpeg", "imager") #leitura de imagens

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando imagem de peça com rosca
com_rosca <- load.image("sem_rosca1.jpg")
# Transformando imagem para escala de cinza
com_rosca = grayscale(com_rosca)
save.image(im = com_rosca, "com_rosca_gray.jpg")
# Transformando em um dataframe
com_rosca <- as.data.frame(com_rosca)

# Criando matriz onde cada elemento corresponde a um pixel da imagem
nlin = com_rosca[nrow(com_rosca),1]
ncoluna = com_rosca[nrow(com_rosca),2]
mat1 <- matrix(com_rosca[,3], nrow = nlin)
dim(mat1)

## As variáveis apresentam unidades de medida e amplitudes muito distintas

# Padronizando as variáveis

mat1_pad <- as.data.frame(scale(mat1))


## Todas as variáveis passam a ter média = 0 e desvio padrão = 1.

# Vamos iniciar com o esquema de aglomeração hierárquico

# Matriz de dissimilaridades
matriz_D <- mat1 %>% 
  dist(method = "euclidean")

matriz_D_pad <- mat1_pad %>% 
  dist(method = "euclidean")

# 1º Teste: Elaboração da clusterização hierárquica como "single linkage"
#cluster_hier_single <- agnes(x = matriz_D, method = "single")
#cluster_hier_single_pad <- agnes(x = matriz_D_pad, method = "single")

# Construção do dendrograma "single linkage"
#dev.off()
#fviz_dend(x = cluster_hier_single, show_labels = F)
#fviz_dend(x = cluster_hier_single_pad, show_labels = F)

## O método de encadeamento single linkage não permite uma clusterização útil
## Pode-se interpretar que as observações estão muito próximas umas das outras

# 2º Teste: Elaboração da clusterização hierárquica como "complete linkage"
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")
cluster_hier_complete_pad <- agnes(x = matriz_D_pad, method = "complete")

# Construção do dendrograma "complete linkage"
fviz_dend(x = cluster_hier_complete, show_labels = F)
fviz_dend(x = cluster_hier_complete_pad, show_labels = F)

## O método de encadeamento complete linkage melhora significativamente

# 3º Teste: Elaboração da clusterização hierárquica como "average linkage"
cluster_hier_average <- agnes(x = matriz_D, method = "average")
cluster_hier_average_pad <- agnes(x = matriz_D_pad, method = "average")

# Construção do dendrograma "average linkage"
fviz_dend(x = cluster_hier_average, show_labels = F)
fviz_dend(x = cluster_hier_average_pad, show_labels = F)

## Vamos optar pelo complete linkage (average cria clusters com menos observações)

# Dendrograma com visualização dos clusters (selecionando por "altura")
fviz_dend(x = cluster_hier_average,
          h = 4.5,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())

## Formam 3 clusters cortando o dendrograma em 5

# Vamos detalhar esse esquema hierárquico

coeficientes <- sort(cluster_hier_average$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(cluster_hier_average$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

## Portanto, vamos gerar uma variável indicando 4 clusters

mat1$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 3))
mat1_pad$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 3))

# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_child_mort <- aov(formula = child_mort ~ cluster_H,
                                data = pais_padronizado))

summary(anova_exports <- aov(formula = exports ~ cluster_H,
                             data = pais_padronizado))

summary(anova_health <- aov(formula = health ~ cluster_H,
                            data = pais_padronizado))

summary(anova_imports <- aov(formula = imports ~ cluster_H,
                             data = pais_padronizado))

summary(anova_income <- aov(formula = income ~ cluster_H,
                            data = pais_padronizado))

summary(anova_inflation <- aov(formula = inflation ~ cluster_H,
                               data = pais_padronizado))

summary(anova_lifeexpec <- aov(formula = life_expec ~ cluster_H,
                               data = pais_padronizado))

summary(anova_totalfer <- aov(formula = total_fer ~ cluster_H,
                              data = pais_padronizado))

summary(anova_gdpp <- aov(formula = gdpp ~ cluster_H,
                          data = pais_padronizado))

## Todas auxiliam na formação de pelo menos um cluster

# O que os cluster indicam? Vamos interpretar algumas variáveis médias:

análise <- group_by(mat1, cluster_H) %>%
  summarise(income = mean(income, na.rm = TRUE),
            gdpp = mean(gdpp, na.rm = TRUE),
            mort = mean(child_mort, na.rm = TRUE),
            health = mean(health, na.rm = TRUE),
            expec = mean(life_expec, na.rm = TRUE))

## Por exemplo: os países do cluster 1 e 4 apresentam: 
## Baixa renda média, baixo PIB per capita, 
## Elevada mortalidade infantil, baixa expectativa de vida
## Portanto, são os países em que deve haver ajuda para melhoria das condições
