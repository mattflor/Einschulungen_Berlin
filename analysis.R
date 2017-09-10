library(tidyverse)

#' Daten einlesen
#' Quelle für die 
#' - Einschulungszahlen: https://twitter.com/tagesspiegel/status/906418940074655744
#' - Einwohnerzahlen: https://de.m.wikipedia.org/wiki/Liste_der_Bezirke_und_Ortsteile_Berlins
DF <- read_csv("data.csv") %>% 
  mutate(label = as.character(Einschulungen),
         Einschulungen_relativ = round(Einschulungen * 100000 / Einwohner),
         label_relativ = as.character(Einschulungen_relativ))

#' Tagesspiegel-Grafik reproduzieren:
p1 <- ggplot(DF, aes(x = reorder(Bezirk, Einschulungen), 
               y = Einschulungen, 
               label = label)) +
  geom_col(col = "orange", fill = "orange") +
  geom_text(size = 4, col = "white", 
            fontface = "bold",
            hjust = 1.2) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("Schülerzahlen",
          subtitle = "in der 1. Jahrgangsstufe der Grundschulen 2017/18") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13))
ggsave("schuelerzahlen.png", plot = p1, device = "png")

#' Schülerzahlen je 100.000 Einwohner:
p2 <- ggplot(DF, aes(x = reorder(Bezirk, Einschulungen_relativ), 
               y = Einschulungen_relativ, 
               label = label_relativ)) +
  geom_col(col = "orange", fill = "orange") +
  geom_text(size = 4, col = "white", 
            fontface = "bold",
            hjust = 1.2) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("Schülerzahlen je 100.000 Einwohner",
          subtitle = "in der 1. Jahrgangsstufe der Grundschulen 2017/18") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13))
ggsave("schuelerzahlen_je_100000_einwohner.png", plot = p2, device = "png")
        