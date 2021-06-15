# Figures

ggplot(d, aes(x = scale(NEP), fill = "springgreen4")) + geom_histogram(show.legend = FALSE, fill = "springgreen4")  + theme_minimal() +
  xlab("New Environmental Paradigm Scale (standardized)")
ggsave(filename = '~/Projects/SEGS_COVID/papers/ProEnvironmentalAttitudes/figures/NEP_Histogram.png',
       width = 6,
       height = 3,
       units = "in")
