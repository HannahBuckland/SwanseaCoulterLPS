# Script to plot AT and Aso-4 GSDs collected so far
# Only for IAVCEI talk need to tidy up workflow for publication

sieveCombo <- read.csv("data/sieve_combo.csv", header=TRUE)

sieveGSD <- sieveCombo[4:31,]

GSDInfo <- sieveCombo[1:3,]
GSDInfoLong <- GSDInfo %>%
  pivot_longer(cols = !label)

GSDInfoWide <- GSDInfoLong %>%
  pivot_wider(id_cols = name,
              names_from = label,
              values_from = value)

sieveGSDLong <- sieveGSD %>%
  pivot_longer(cols = !label) %>%
  mutate(value = as.numeric(value),
         label = as.numeric(label)) %>%
  rename(phi = label,
         perc = value)


GSDs_merge <- merge(sieveGSDLong,
                    GSDInfoWide,
                   by="name") %>%
  filter(eruption != "Unk")

ggplot(GSDs_merge) +
  geom_col(aes(x=phi,
               y=perc,
               group = name,
               fill = core)) +
  facet_grid(name~eruption)