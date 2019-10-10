library(tidyverse)

OTU_Table <- read_csv("Data/OTUtable_16S.csv", skip = 1:2)

OTU_Table

view(OTU_Table)

OTU_Table_Bacteria <- select(OTU_Table, -2, -3, -4, -6, -8, -10, -12, -14, -15, -17, -18)

OTU_Table_Bacteria

view(OTU_Table_Bacteria)


