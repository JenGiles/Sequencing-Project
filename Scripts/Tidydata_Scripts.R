library(tidyverse)

OTU_Table <- read_csv("Data/OTUtable_16S.csv", skip = 1:2)

OTU_Table

view(OTU_Table)

OTU_Table_Bacteria <- select(OTU_Table, -2, -3, -4, -6, -8, -10, -12, -14, -15, -17, -18)

OTU_Table_Bacteria

view(OTU_Table_Bacteria)

#OTU_Table_Bacteria %>% rename_at(vars(VAN001_S1:VAN107_S107),
                                # str_remove, "_.*")

OTU_Table_Renamed <- OTU_Table_Bacteria %>%
  rename(OTU = X1,
         Phylum = X5, Class = X7, Order = X9, Family = X11, Genus = X13, Species = X16,
         VAN001 = X19, VAN002 = X20, VAN003 = X21, VAN004 = X22, VAN005 = X23,
         VAN006 = X24, VAN007 = X25, VAN008 = X26, VAN009 = X27, VAN010 = X28, 
         VAN011 = X29, VAN012 = X30, VAN013 = X31, VAN014 = X32, VAN015 = X33,
         VAN016 = X34, VAN017 = X35, VAN018 = X36, VAN019 = X37, VAN020 = X38,
         VAN021 = X39, VAN022 = X40, VAN023 = X41, VAN024 = X42, VAN025 = X43,
         VAN026 = X44, VAN027 = X45, VAN028 = X46, VAN029 = X47, VAN030 = X48,
         VAN031 = X49, VAN032 = X50, VAN033 = X51, VAN034 = X52, VAN035 = X53,
         VAN036 = X54, VAN037 = X55, VAN038 = X56, VAN039 = X57, VAN040 = X58,
         VAN041 = X59, VAN042 = X60, VAN043 = X61, VAN044 = X62, VAN045 = X63,
         VAN046 = X64, VAN047 = X65, VAN048 = X66, VAN049 = X67, VAN050 = X68,
         VAN051 = X69, VAN052 = X70, VAN053 = X71, VAN054 = X72, VAN055 = X73,
         VAN056 = X74, VAN057 = X75, VAN058 = X76, VAN059 = X77, VAN060 = X78,
         VAN061 = X79, VAN062 = X80, VAN063 = X81, VAN064 = X82, VAN065 = X83,
         VAN066 = X84, VAN067 = X85, VAN068 = X86, VAN069 = X87, VAN070 = X88,
         VAN071 = X89, VAN072 = X90, VAN073 = X91, VAN074 = X92, VAN075 = X93,
         VAN076 = X94, VAN077 = X95, VAN078 = X96, VAN079 = X97, VAN080 = X98,
         VAN081 = X99, VAN082 = X100, VAN083 = X101, VAN084 = X102, VAN085 = X103,
         VAN086 = X104, VAN087 = X105, VAN088 = X106, VAN089 = X107, VAN090 = X108,
         VAN091 = X109, VAN092 = X110, VAN093 = X111, VAN094 = X112, VAN095 = X113,
         VAN096 = X114, VAN097 = X115, VAN098 = X116, VAN099 = X117, VAN100 = X118,
         VAN101 = X119, VAN102 = X120, VAN103 = X121, VAN104 = X122, VAN105 = X123,
         VAN106 = X124, VAN107 = X125)


view(OTU_Table_Renamed)         

OTU_Table_Filtered <- filter(OTU_Table_Renamed, OTU != "OTU")         

view(OTU_Table_Filtered)  

OTU_Table_Selected <- select(OTU_Table_Filtered, -Class, -Order, -Family, -Genus, -Species)

view(OTU_Table_Selected)

OTU_Table_Transpose <- OTU_Table_Selected %>%
  gather(key = "Sample_ID", value = "Counts", VAN001 : VAN107)

view(OTU_Table_Transpose)

Group_by_Phylum <- OTU_Table_Transpose %>%
  group_by(Phylum, Sample_ID)

view(Group_by_Phylum)


OTU_Table_Spread <- OTU_Table_Transpose %>%
  spread(Phylum, Counts)

view(OTU_Table_Spread)
