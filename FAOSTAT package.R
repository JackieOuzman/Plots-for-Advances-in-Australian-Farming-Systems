
FAO

# FAOSTAT (https://www.faostat.fao.org) has Australian wheat area harvested 
# and production back to 1961 — consistent methodology, easy CSV download, good for long-run trends


install.packages("FAOSTAT")
library(FAOSTAT)
wheat_fao <- get_faostat_bulk(code = "QCL")  # crop production