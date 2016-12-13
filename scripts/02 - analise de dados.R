# limpando ambiente -------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

# carregando pacotes ------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(betapart)
library(vegan)

# carregando dados --------------------------------------------------------------------------------------------------------------------

seminarios <- read_tsv("data/tidy data/lista_de_presenca.xls")

# dados das palestras -----------------------------------------------------------------------------------------------------------------

dados <- seminarios %>% 
  mutate(palestra = paste0(data,mes,ano)) %>% 
  select(palestra, palestra:`tema #1`) %>% 
  distinct %>% 
  mutate(ordem = c(4, 11, 7, 1, 8, 5, 2, 9, 6, 3, 10)) %>% 
  arrange(ordem)

# criando matriz de presenca ausencia -------------------------------------------------------------------------------------------------

presencas <- seminarios %>% 
  filter(id != 5) %>% 
  mutate(palestra = paste0(data,mes,ano)) %>% 
  select(id, palestra) %>% 
  mutate(presenca = rep(1, nrow(.))) %>% 
  spread(key = id, value = presenca, fill = 0) %>% 
  left_join(select(dados, palestra, ordem), by = "palestra") %>% 
  arrange(ordem) %>% 
  select(-ordem)

# particionando beta ------------------------------------------------------------------------------------------------------------------

beta1 <- beta.multi(x = presencas[,-1], index.family = "sorensen")
set.seed(97353)
beta1_sample <- beta.sample(x = presencas[,-1], index.family = "sorensen", sites = 5, samples = 252)
beta_data <- cbind.data.frame(media = beta1_sample$mean.values, desvio = beta1_sample$sd.values) %>% 
  rownames_to_column(var = "componente")

fig1 <- ggplot(data = beta_data, mapping = aes(x = reorder(componente, -media), y = media, fill = componente)) +
  geom_bar(colour = "black", position = position_dodge(width = 0.5), stat = "identity") +
  geom_errorbar(mapping = aes(ymin = media - desvio, ymax = media + desvio), position = position_dodge(width = 0.5), 
                stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("yellow1", "deepskyblue1", "forestgreen")) +
  scale_y_continuous("Valor do Componente", breaks = seq(0, 1, by = 0.2), expand = c(0,0), limits = c(0,0.9)) + 
  scale_x_discrete(expression(paste("Componente da Diversidade ", beta)), labels = c("Soresen", "Turnover", "Nestedness")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12))
fig1

# sequencia temporal da composicao ----------------------------------------------------------------------------------------------------

disp1 <- betadisper(d = beta.pair(x = presencas[,-1], index.family = "sorensen")$beta.sor, group = presencas$palestra)
disp1_centroids <- data.frame(disp1$centroids) %>% 
  rownames_to_column(var = "palestra") %>% 
  left_join(select(dados, palestra, ordem), by = "palestra") %>% 
  arrange(ordem)

fig2 <- ggplot(data = disp1_centroids, mapping = aes(x = PCoA1, y = PCoA2, fill = ordem)) +
  geom_path(aes(colour = ordem), arrow = arrow(angle = 30, length = unit(0.2, "inches"))) +
  geom_point(shape = 21, size = 4, stroke = 1) +
  scale_fill_gradientn(colours = rev(heat.colors(11))) +
  scale_color_gradientn(colours = rev(heat.colors(11))) +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey80", colour = "black"))
fig2

# a origem do palestra explica composição? --------------------------------------------------------------------------------------------

set.seed(73842)
perm1 <- adonis(beta.pair(x = presencas[,-1], index.family = "sorensen")$beta.sor ~ dados$origem_palestrante)
perm1

# invertendo a matriz - analise no modo R ---------------------------------------------------------------------------------------------

invertida <- seminarios %>% 
  filter(id != 5) %>% 
  mutate(palestra = paste0(data,mes,ano)) %>% 
  select(id, palestra) %>% 
  mutate(presenca = rep(1, nrow(.))) %>% 
  spread(key = palestra, value = presenca, fill = 0) %>% 
  arrange(id)

# dados dos individuos ----------------------------------------------------------------------------------------------------------------

individuos <- seminarios %>% 
  filter(id != 5) %>% 
  select(id, formacao_ouvinte:laboratorio_ouvinte) %>% 
  distinct %>% 
  mutate(formacao_ouvinte = ifelse(is.na(formacao_ouvinte), "desconhecido", formacao_ouvinte),
         origem_ouvinte = ifelse(origem_ouvinte %in% c("PPGE", "Dept Ecologia"), "Interno", 
                                 ifelse(origem_ouvinte == "IB", "IB", "Externo")),
         instituicao_ouvinte = ifelse(is.na(instituicao_ouvinte), "desconhecido", instituicao_ouvinte),
         laboratorio_ouvinte = ifelse(is.na(laboratorio_ouvinte), "desconhecido", laboratorio_ouvinte)) %>% 
  arrange(id) %>% 
  mutate(formacao_ouvinte = forcats::fct_lump(f = formacao_ouvinte, prop = 0.1),
         instituicao_ouvinte = forcats::fct_lump(f = instituicao_ouvinte, prop = 0.1),
         laboratorio_ouvinte = forcats::fct_lump(f = laboratorio_ouvinte, prop = 0.05),
         origem_ouvinte = ifelse(is.na(origem_ouvinte), "Externo", origem_ouvinte),
         origem_ouvinte = factor(origem_ouvinte)) %>% 
  arrange(id)

# analisando em modo R ----------------------------------------------------------------------------------------------------------------

set.seed(7343)
perm2 <- adonis(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor ~ formacao_ouvinte, data = individuos)
perm2

set.seed(83743)
perm3 <- adonis(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor ~ instituicao_ouvinte, data = individuos)
perm3

set.seed(8343)
perm4 <- adonis(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor ~ laboratorio_ouvinte, data = individuos)
perm4

set.seed(2434)
perm5 <- adonis(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor ~ origem_ouvinte, data = individuos)
perm5

# diferencas entre individuos ---------------------------------------------------------------------------------------------------------

disp2 <- betadisper(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor, individuos$formacao_ouvinte)
disp2_ind <- data.frame(disp2$vectors) %>% 
  tbl_df %>% 
  mutate(formacao_ouvinte = disp2$group)

disp4 <- betadisper(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor, individuos$laboratorio_ouvinte)
disp4_ind <- data.frame(disp4$vectors) %>% 
  tbl_df %>% 
  mutate(laboratorio_ouvinte = disp4$group)

disp5 <- betadisper(beta.pair(x = invertida[,-1], index.family = "sorensen")$beta.sor, individuos$origem_ouvinte)
disp5_ind <- data.frame(disp5$vectors) %>% 
  tbl_df %>% 
  mutate(origem_ouvinte = disp5$group)

# hull para o ggplot2 -----------------------------------------------------------------------------------------------------------------

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# figuras -----------------------------------------------------------------------------------------------------------------------------

fig3 <- ggplot(data = disp2_ind, mapping = aes(x = PCoA1, y = PCoA2, fill = formacao_ouvinte)) +
  facet_wrap(~ formacao_ouvinte) + 
  geom_point(aes(shape = formacao_ouvinte)) +
  stat_chull(colour = "black", alpha = 0.3) +
  scale_fill_manual(values = rainbow(6)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))
fig3

fig4 <- ggplot(data = disp4_ind, mapping = aes(x = PCoA1, y = PCoA2, fill = laboratorio_ouvinte)) +
  facet_wrap(~ laboratorio_ouvinte) + 
  geom_point(aes(shape = laboratorio_ouvinte)) +
  stat_chull(colour = "black", alpha = 0.3) +
  scale_fill_manual(values = heat.colors(4)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))
fig4

fig5 <- ggplot(data = disp5_ind, mapping = aes(x = PCoA1, y = PCoA2, fill = origem_ouvinte)) +
  facet_wrap(~ origem_ouvinte) + 
  geom_point(aes(shape = origem_ouvinte), size = 4) +
  stat_chull(colour = "black", alpha = 0.3) +
  scale_fill_manual(values = heat.colors(3)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))
fig5

# ANINHAMENTO -------------------------------------------------------------------------------------------------------------------------

ordem_colunas <- rev(sort(colSums(presencas[,-1]))) %>% 
  data.frame() %>% 
  rownames_to_column(var = "id")
presencas <- presencas[,c("palestra", ordem_colunas$id)]
presencas <- presencas %>% 
  mutate(soma = rowSums(presencas[,-1])) %>% 
  arrange(desc(soma)) %>% 
  select(-soma)

fig8 <- Imagine(comm = presencas[,-1], order = FALSE, sitenames = rev(presencas$palestra), col=c('white','black'), fill = FALSE, xlab = "", ylab = "")

oecosimu(comm = presencas[,-1], nestfun = nestednodf, method = "r1", nsimul = 999)

# RDA ---------------------------------------------------------------------------------------------------------------------------------

invertida <- arrange(invertida, id)
individuos <- arrange(individuos, id)

rda1 <- capscale(invertida[,-1] ~ formacao_ouvinte + origem_ouvinte + laboratorio_ouvinte, distance = "jaccard", data = individuos)

fatores <- scores(rda1)$centroids %>% 
  data.frame %>% 
  rownames_to_column(var = "variavel")
fatores$variavel <- c("Doutorado", "Graduacao", "Mestrado", "PosDoc", "Professor", "Outros", "Externo", "IB", "Interno", "Desconhecido",
                      "Limnologia", "Vertebrados", "Outros")

fatores <- fatores %>% 
  mutate(tipo = rep(c("formacao", "origem", "laboratorio"), times = c(6, 3, 4)))

dias <- scores(rda1)$species %>% 
  data.frame %>% 
  rownames_to_column(var = "palestra") %>% 
  left_join(select(dados, palestra, ordem, palestrante), by = "palestra") %>% 
  arrange(ordem)

fig7 <- ggplot() +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.3) + 
  geom_text(data = fatores, mapping = aes(x = CAP1, y = CAP2, colour = tipo, label = variavel), fontface = 2, size = 5) +
  scale_colour_manual(values = c("black", "blue3", "forestgreen")) +
  geom_point(data = dias, mapping = aes(x = CAP1, y = CAP2, fill = ordem), colour = "black", shape = 21, size = 4) +
  scale_fill_gradientn(colours = rev(heat.colors(11))) + 
  geom_text(data = dias, mapping = aes(x = CAP1 - 0.15, y = CAP2, label = palestrante), colour = "black") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray90", colour = "black"))
fig7


