# Internal color palette shared across all plot_* functions in this
# package, chosen to match the Shiny dashboard's visual identity (an
# institutional financial-market aesthetic: navy blue for primary data,
# crimson for risk/negative values, gold for highlighted reference points).
# Not exported -- these are implementation details, not part of the public
# API. Because they are defined in the package namespace, every function
# in R/ can use them directly without an explicit import.

brstocks_cor_azul    <- "#1E3A5F"
brstocks_cor_vermelho <- "#CC0000"
brstocks_cor_dourado  <- "#C9A227"
