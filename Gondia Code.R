library(ona)
library(data.table)
library(dplyr)

data <- onaDownload("gondia", "achhatre", uname="achhatre", pass="Mehta@ac6")

data <- as.data.frame(data)

data <- read_excel("Master sheet_20_03_02.xlsx")
colnames(data) = gsub("/",".",colnames(data))

#data %>% select(matches("plot_code|tree_name|dbh|tree_others"))  

df =   data.table::melt(setDT(data), id=1:24,
                        measure=patterns("*tree_name$", "*tree_others$","dbh"),
                        value.name = c( "tree_name","tree_other","tree_d"))

colnames(df)[25] <- "tree_id"
df = df[rowSums(!is.na(df[,26:28]))>=1,]

df = df[order(df$part1.village_name,df$part2.plot_code,df$tree_id),]

df = df %>% filter(!is.na(tree_name))
df$tree_name <- ifelse(df$tree_name=="Other",df$tree_other,df$tree_name)


#### Basal Area

df$basal_area = (df$tree_d)^2/(4*pi)

BA <- df %>% 
  group_by(part1.village_name,part2.plot_code) %>%
  summarise(Basal_Area = sum(basal_area))

write.csv(BA,"Basal_Area.csv")


#### Species

species <- df %>%
              select(matches("village_name|plot_code|tree_id|tree_name|tree_other"))
species = species %>% filter(!is.na(tree_name))
species$tree_name <- ifelse(species$tree_name=="Other",species$tree_other,species$tree_name)


SD <- species %>% 
  group_by(part1.village_name,part2.plot_code) %>%
  summarise(species_count=n_distinct(tree_name))

write.csv(SD,"Species_Diversity.csv")

write.csv(left_join(SD,BA),"Gondia_SD_BA.csv")

### Relative density = n(given species)*100/n(all species)

shanon_diversity <- species %>% group_by(part1.village_name,tree_name) %>%
  summarise(ntree = n()) %>% group_by(part1.village_name) %>%
  mutate(ntree_vill = sum(ntree)) %>%
  mutate(rel_density = ntree/ntree_vill)
  
write.csv(shanon_diversity,"Shanon_Diversity.csv")


frequency <- species %>% group_by(part1.village_name,tree_name) %>% 
  summarise(n_quad = n_distinct(part2.plot_code)) %>% 
  mutate(freq = (n_quad*100)/30)

relative_frequency <- species %>% 
        group_by(part1.village_name,tree_name) %>%
        summarise(n_quad = n_distinct(part2.plot_code)) %>%
  group_by(part1.village_name) %>%
  mutate(nquad_vill = sum(n_quad)) %>%
  mutate(rel_frequency = 100*n_quad/nquad_vill) %>%
  mutate(freq = (n_quad*100)/30)
        

write.csv(relative_frequency,"Relative_Frequency.csv")

species %>% group_by(part1.village_name,tr)

### Dominanace

relative_dominanace <- df %>% 
  group_by(part1.village_name,tree_name) %>%
  summarise(species_basal_area_sq_m = sum(basal_area,na.rm=T)/10000) %>%
  mutate(dominance = species_basal_area_sq_m/9000) %>%
  group_by(part1.village_name) %>%
  mutate(dominance_vill = sum(dominance,na.rm=T)) %>%
  mutate(rel_dominance = 100*dominance/dominance_vill)

write.csv(relative_dominanace,"Relative_Dominance.csv")  


IVI = shanon_diversity %>% left_join(.,relative_frequency) %>%
  left_join(.,relative_dominanace)

write.csv(IVI,"IVI.csv")
