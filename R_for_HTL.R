### setwd and load packages ####################################################

setwd("~/Desktop/R_for_HTL/Data")

library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom) 

### step 1 FBS Calibration ####################################################

### Read faostat food supply files
Total_New <- read.csv("Total_New.csv", header=TRUE, stringsAsFactors = FALSE) # FBS 2010 - 2022
Total_Old <- read.csv("Total_Old.csv", header=TRUE, stringsAsFactors = FALSE) # FBS 1961 - 2009
Total_Old$Value[Total_Old$Value < 0 ] <- 0

### Get all the product items from Food Balances dataset
Total_New_items <- unique(Total_New[, c("Item", "Item.Code..FAO.")]) # FBS 2010 - 2022
Total_Old_items <- unique(Total_Old[, c("Item", "Item.Code..FAO.")]) # FBS 1961 - 2009

Total_items <- merge(Total_New_items, Total_Old_items, by.x="Item.Code..FAO.",by.y="Item.Code..FAO.", all.x=T, all.y=T)

item_code_map <- list(
  "2556" = 2552,  
  "2805" = 2807   
)

Total_Old_adjusted <- Total_Old %>%
  mutate(
    Item.Code..FAO. = as.character(Item.Code..FAO.),
    Item.Code..FAO. = case_when(
      Item.Code..FAO. %in% names(item_code_map) ~ 
        as.character(item_code_map[Item.Code..FAO.]),
      TRUE ~ Item.Code..FAO.
    ),
    Item.Code..FAO. = as.integer(Item.Code..FAO.)
  )

merged_data <- inner_join(
  Total_New %>% 
    rename(Value_New = Value) %>% 
    select(Area.Code..ISO3., Item.Code..FAO., Year, Value_New),
  Total_Old_adjusted %>% 
    rename(Value_Old = Value) %>% 
    select(Area.Code..ISO3., Item.Code..FAO., Year, Value_Old),
  by = c("Area.Code..ISO3.", "Item.Code..FAO.", "Year")
)

merged_annual <- merged_data %>%
  mutate(
    Annual_Factor = case_when(
      Value_Old == 0 & Value_New == 0 ~ 1,
      Value_Old == 0 & Value_New != 0 ~ NA_real_,
      Value_New == 0 & Value_Old != 0 ~ NA_real_,
      TRUE ~ Value_New / Value_Old
    )
  )

Adjustment_Factor <- merged_annual %>%
  group_by(Area.Code..ISO3., Item.Code..FAO.) %>%
  filter(!is.na(Annual_Factor)) %>% 
  summarise(
    n_years = n(),
    mean_Value_New = ifelse(n_years >= 1, mean(Value_New, na.rm = TRUE), NA_real_),
    mean_factor = ifelse(n_years >= 1, mean(Annual_Factor, na.rm = TRUE), NA_real_),
    sd_factor = ifelse(n_years >= 2, sd(Annual_Factor, na.rm = TRUE), NA_real_),
    cv_factor = ifelse(n_years >= 2, (sd_factor / mean_factor) * 100, NA_real_),
    .groups = "drop"
  )

### Output
#write.csv(Adjustment_Factor, 'Adjustment_Factor.csv', row.names= FALSE)

### FOR Animal Products
### Read faostat food supply files (1961 - 2009)
Animal_Products_Old <- read.csv("Animal Products_Old.csv", header=TRUE, stringsAsFactors = FALSE) 
colnames(Animal_Products_Old)

### Get all the animal product items from Food Balances dataset
Animal_items_Old <- unique(Animal_Products_Old[, c("Item", "Item.Code..FAO.")])

adjusted_Old_data <- Animal_Products_Old %>%
  left_join(
    Adjustment_Factor %>%
      select(Area.Code..ISO3., Item.Code..FAO., mean_factor),
    by = c("Area.Code..ISO3.", "Item.Code..FAO.")
  ) %>%
  mutate(
    Adjusted_Value = case_when(
      !is.na(mean_factor) ~ Value * mean_factor,
      TRUE ~ Value
    ),
    Adjustment_Type = case_when(
      !is.na(mean_factor) ~ "Factor Applied",
      TRUE ~ "No Adjustment"
    )
  )

### Output
#write.csv(adjusted_Old_data, "adjusted_animal_products_1961-2009.csv", row.names = FALSE)

### Read faostat food supply files (2010 - 2022)
Animal_Products_New <- read.csv("Animal Products_New.csv", header=TRUE, stringsAsFactors = FALSE) # FBS 2010 - 2022
colnames(Animal_Products_New)
Animal_Products_New <- Animal_Products_New[, !(colnames(Animal_Products_New) %in% "Note")]

adjusted_Old_data <- adjusted_Old_data[,c(1,2,3,4,5,6,7,8,9,10,11,16,13,14)]
names(adjusted_Old_data)[names(adjusted_Old_data) == "Adjusted_Value"] <- "Value"

Animal_Products <- rbind(Animal_Products_New, adjusted_Old_data)
colnames(Animal_Products)
### Get all the animal product items from Food Balances dataset
Animal_items <- unique(Animal_Products[, c("Item", "Item.Code..FAO.")])

### Exclude Meat, Aquatic Mammals (2768)
Animal_Products <- Animal_Products[Animal_Products$Item.Code..FAO. != 2768,]

### Get Aquatic Plants (2775)
Aquatic_Plants <- Animal_Products[Animal_Products$Item.Code..FAO. == 2775,]
### Output Aquatic Plants (2775)
#write.csv(Aquatic_Plants, 'Adjusted_Aquatic_Plants.csv', row.names= FALSE)

### Exclude Aquatic Plants (2775)
Animal_Products <- Animal_Products[Animal_Products$Item.Code..FAO. != 2775,]

### Get Fish, Body Oil & Fish, Liver Oil (2781 & 2782)
Fish_oil <- Animal_Products[Animal_Products$Item.Code..FAO. %in% c(2781,2782), ]
### Output Fish, Body Oil & Fish, Liver Oil (2781 & 2782)
#write.csv(Fish_oil, 'Adjusted_Fish_oil.csv', row.names= FALSE)

### Get aquatic products: Fish, Seafood
Fish_Food <- Animal_Products[Animal_Products$Item.Code..FAO. %in% c(2761,2762,2763,2764,2765,2766,2767,2769), ]
Fish_Food_items  <- unique(Fish_Food[, c("Item", "Item.Code..FAO.")])
### Output
#write.csv(Fish_Food, 'Adjusted_Fish_Food.csv', row.names= FALSE)

### Output
#write.csv(Animal_Products, 'Adjusted_Animal_Products.csv', row.names= FALSE)

### FOR Vegetal Products
### Read faostat food supply files (1961 - 2009)
Vegetal_Products_Old <- read.csv("Vegetal Products_Old.csv", header=TRUE, stringsAsFactors = FALSE)

### Get all the vegetal product items from Food Balances dataset
Vegetal_items_Old <- unique(Vegetal_Products_Old[, c("Item", "Item.Code..FAO.")])

item_code_map <- list(
  "2556" = 2552,  
  "2805" = 2807   
)

Vegetal_Old_adjusted <- Vegetal_Products_Old %>%
  mutate(
    Item.Code..FAO. = as.character(Item.Code..FAO.),
    Item.Code..FAO. = case_when(
      Item.Code..FAO. %in% names(item_code_map) ~ 
        as.character(item_code_map[Item.Code..FAO.]),
      TRUE ~ Item.Code..FAO.
    ),
    Item.Code..FAO. = as.integer(Item.Code..FAO.)
  )

adjusted_Old_data <-Vegetal_Old_adjusted %>%
  left_join(
    Adjustment_Factor %>%
      select(Area.Code..ISO3., Item.Code..FAO., mean_factor),
    by = c("Area.Code..ISO3.", "Item.Code..FAO.")
  ) %>%
  mutate(
    Adjusted_Value = case_when(
      !is.na(mean_factor) ~ Value * mean_factor,
      TRUE ~ Value
    ),
    Adjustment_Type = case_when(
      !is.na(mean_factor) ~ "Factor Applied",
      TRUE ~ "No Adjustment"
    )
  )

### Output
#write.csv(adjusted_Old_data, "adjusted_vegetal_products_1961-2009.csv", row.names = FALSE)

### Read faostat food supply files (2010 - 2022)
Vegetal_Products_New <- read.csv("Vegetal Products_New.csv", header=TRUE, stringsAsFactors = FALSE)
colnames(Vegetal_Products_New)
Vegetal_Products_New <- Vegetal_Products_New[, !(colnames(Vegetal_Products_New) %in% "Note")]

adjusted_Old_data <- adjusted_Old_data[,c(1,2,3,4,5,6,7,8,9,10,11,16,13,14)]
names(adjusted_Old_data)[names(adjusted_Old_data) == "Adjusted_Value"] <- "Value"

Aquatic_Plants <- read.csv("Adjusted_Aquatic_Plants.csv", header=TRUE, stringsAsFactors = FALSE)
colnames(Aquatic_Plants)

Vegetal_Products <- rbind(Vegetal_Products_New, adjusted_Old_data, Aquatic_Plants)
colnames(Vegetal_Products)

### Get all the vegetal product items from Food Balances dataset
Vegetal_items <- unique(Vegetal_Products[, c("Item", "Item.Code..FAO.")])

### Exclude Miscellaneous (2899)
Vegetal_Products <- Vegetal_Products[Vegetal_Products$Item.Code..FAO. != 2899,]
### Exclude Non-Food products (Alcohol, Non-Food: 2659)
Vegetal_Products <- Vegetal_Products[Vegetal_Products$Item.Code..FAO. != 2659,]

### Output
#write.csv(Vegetal_Products, 'Adjusted_Vegetal_Products.csv', row.names= FALSE)


### step 2 TL Assignment of Animal-source Foods ################################

### FOR livestock products
### Read faostat Supply Utilization Accounts (2010-) dataset
Livestock_Food <- read.csv("Livestock_Food.csv", header=TRUE, stringsAsFactors = FALSE)
### Get all the poultry meat items
poultry_meat <- Livestock_Food[Livestock_Food$Item.Code..FAO. %in% c(1058,1060,1061,1069,1073,1080), ]
poultry_meat_items <- data.frame(Item = unique(poultry_meat$Item))
### Output
#write.csv(poultry_meat_items, 'poultry_meat_items.csv', row.names= FALSE)

### Read livestock product TL
poultry_meat_items_TL <- read.csv("poultry_meat_items_TL.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge food item and food TL
poultry_meat_TL <- merge(poultry_meat, poultry_meat_items_TL, by.x="Item.Code..FAO.",by.y="Item.Code..FAO.", all.x=T, all.y=F)
### Output
#write.csv(poultry_meat_TL, 'poultry_meat_TL.csv', row.names= FALSE)

### Calculate the poultry meat TL for per country
TL_poultry <- ddply(poultry_meat_TL,.(Area.Code..ISO3.),
                    function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
TL_poultry$key <- paste(TL_poultry$Area.Code..ISO3.,  2734, sep="")
TL_poultry$Item <- "Poultry Meat"
poultry_COUNTRY_TL <- TL_poultry[,c(3,4,1,2)]
head(poultry_COUNTRY_TL)
names(poultry_COUNTRY_TL)[names(poultry_COUNTRY_TL) == "V1"] <- "TL"
names(poultry_COUNTRY_TL)[names(poultry_COUNTRY_TL) == "Area.Code..ISO3."] <- "ISO3_Code"
any(is.na(poultry_COUNTRY_TL$TL))
### Output
#write.csv(poultry_COUNTRY_TL, 'poultry_COUNTRY_TL.csv', row.names= FALSE)

### Get all the offal items
offals <- Livestock_Food[Livestock_Food$Item.Code..FAO. %in% c(868,878,948,978,1018,1036,1059,1074,1075,1081,1098,1128,1167), ]
offals_items <- data.frame(Item = unique(offals$Item))
### Output
#write.csv(offals_items, 'offals_items.csv', row.names= FALSE)

### Read livestock product TL
offals_items_TL <- read.csv("offals_items_TL.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge food item and food TL
offals_TL <- merge(offals, offals_items_TL, by.x="Item.Code..FAO.",by.y="Item.Code..FAO.", all.x=T, all.y=F)
### Output
#write.csv(offals_TL, 'offals_TL.csv', row.names= FALSE)

### Calculate the offal TL for per country
TL_offals <- ddply(offals_TL,.(Area.Code..ISO3.),
                   function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
TL_offals$key <- paste(TL_offals$Area.Code..ISO3., 2736, sep="")
TL_offals$Item <- "Offals, Edible"
offals_COUNTRY_TL <- TL_offals[,c(3,4,1,2)]
head(offals_COUNTRY_TL)
names(offals_COUNTRY_TL)[names(offals_COUNTRY_TL) == "V1"] <- "TL"
names(offals_COUNTRY_TL)[names(offals_COUNTRY_TL) == "Area.Code..ISO3."] <- "ISO3_Code"
any(is.na(offals_COUNTRY_TL$TL))
### Output
#write.csv(offals_COUNTRY_TL, 'offals_COUNTRY_TL.csv', row.names= FALSE)

### Get all the fat (animals, raw) items
fats <- Livestock_Food[Livestock_Food$Item.Code..FAO. %in% c(869,871,949,979,994,1019,1037,1040,1043,1065,1129,1160,1168,1221,1222,1225,1243), ]
fats_items <- data.frame(Item = unique(fats$Item))
### Output
#write.csv(fats_items, 'fats_items.csv', row.names= FALSE)

### Read livestock product TL
fats_items_TL <- read.csv("fats_items_TL.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge food item and food TL
fats_TL <- merge(fats, fats_items_TL, by.x="Item.Code..FAO.",by.y="Item.Code..FAO.", all.x=T, all.y=F)
### Output
#write.csv(fats_TL, 'fats_TL.csv', row.names= FALSE)

### Calculate the offal TL for per country
TL_fats <- ddply(fats_TL,.(Area.Code..ISO3.),
                 function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
TL_fats$key <- paste(TL_fats$Area.Code..ISO3., 2737, sep="")
TL_fats$Item <- "Fats, Animals, Raw"
fats_COUNTRY_TL <- TL_fats[,c(3,4,1,2)]
head(fats_COUNTRY_TL)
names(fats_COUNTRY_TL)[names(fats_COUNTRY_TL) == "V1"] <- "TL"
names(fats_COUNTRY_TL)[names(fats_COUNTRY_TL) == "Area.Code..ISO3."] <- "ISO3_Code"
any(is.na(fats_COUNTRY_TL$TL))
### Output
#write.csv(fats_COUNTRY_TL, 'fats_COUNTRY_TL.csv', row.names= FALSE)

### FOR aquatic products
### Read FAO FishStat database
Production_quantity <- read.csv("Global_production_quantity.csv", header=TRUE, stringsAsFactors = FALSE)
### Get production subset (Production quantity >= 100 && Year >=1961)
Production_sub1 <- Production_quantity[Production_quantity$VALUE >= 100,]
Production_sub2 <- Production_sub1[Production_sub1$PERIOD >= 1961,]

### Read species code
Fish_code <- read.csv("CL_FI_SPECIES_ITEM.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge species item and species code
Fish_code <- Fish_code[,c(1,5)]
Production_Code <- merge(Production_sub2, Fish_code, by.x="SPECIES.ALPHA_3_CODE",by.y="Alpha3_Code", all.x=T, all.y=F)

### Get production subset (Exclude inedible species)
Production_Code_sub1 <- Production_Code[Production_Code$IsscaapGrp_mapping <= 77,]
Production_Code_sub2 <- Production_Code_sub1[!(Production_Code_sub1$IsscaapGrp_mapping %in% c(73,61,62,63,64)), ]

### Read species TL (from fishbase, SeaAroundUs and paper)
Fish_code_tl <- read.csv("Fish_items_TL.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge species item and species TL
Production_TL <- merge(Production_Code_sub2, Fish_code_tl, by.x="SPECIES.ALPHA_3_CODE",by.y="ALPHA_3_CODE", all.x=T, all.y=F)
colnames(Production_TL)
any(is.na(Production_TL$Troph))

### Exclude forage fish
Production_TL_sub <- Production_TL[Production_TL$SPECIES.ALPHA_3_CODE != "VET",]

### Output
#write.csv(Production_TL_sub, 'Production_TL.csv', row.names= FALSE)

### Calculate the fish item TL for per country per year
Fish_TL <- ddply(Production_TL_sub,.(COUNTRY.UN_CODE, PERIOD, FOOD.ITEM),
                 function(x) sum(x$VALUE*x$Troph)/sum(x$VALUE),.progress="text")
colnames(Fish_TL)
any(is.na(Fish_TL$V1))

### Read country code
COUNTRY <- read.csv("COUNTRY_Code.csv", header=TRUE, stringsAsFactors = FALSE)
colnames(COUNTRY)
any(is.na(COUNTRY$UN_Code))
### Merge country code and country name
Fish_TL_COUNTRY <- merge(Fish_TL, COUNTRY, by.x="COUNTRY.UN_CODE",by.y="UN_Code", all.x=T, all.y=F)
colnames(Fish_TL_COUNTRY)
any(is.na(Fish_TL_COUNTRY$ISO3_Code))
### Output
#write.csv(Fish_TL_COUNTRY, 'Fish_TL_COUNTRY.csv', row.names= FALSE)

### define and merge food item code
mapping <- data.frame(  
  FOOD.ITEM = c("Freshwater fish", "Demersal fish", "Pelagic fish", "Marine fish, other", "Crustaceans", "Cephalopods", "Molluscs, other", "Aquatic animals, other"),  
  FOOD_ITEM_CODE = c("2761", "2762", "2763", "2764", "2765", "2766", "2767", "2769")   
)  
Fish_TL_COUNTRY <- Fish_TL_COUNTRY %>%  
  left_join(mapping, by = "FOOD.ITEM")
Fish_TL_COUNTRY$key <- paste(Fish_TL_COUNTRY$ISO3_Code, Fish_TL_COUNTRY$PERIOD, Fish_TL_COUNTRY$FOOD_ITEM_CODE, sep="")
Fish_COUNTRY_Year_TL <- Fish_TL_COUNTRY[,c(16,3,7,2,4)]
head(Fish_COUNTRY_Year_TL)
names(Fish_COUNTRY_Year_TL)[names(Fish_COUNTRY_Year_TL) == "V1"] <- "TL"
names(Fish_COUNTRY_Year_TL)[names(Fish_COUNTRY_Year_TL) == "PERIOD"] <- "Year"
names(Fish_COUNTRY_Year_TL)[names(Fish_COUNTRY_Year_TL) == "FOOD.ITEM"] <- "Item"
any(is.na(Fish_COUNTRY_Year_TL$TL))
### Output
#write.csv(Fish_COUNTRY_Year_TL, 'Fish_COUNTRY_Year_TL.csv', row.names= FALSE)


### step 3 Food TL Calculation #################################################

### FOR Animal Products 
### you have already got the Adjusted_Animal_Products.csv from step 1
#Animal_Products <- read.csv("Adjusted_Animal_Products.csv", header=TRUE, stringsAsFactors = FALSE)

### Get Animal Product subset1 (Exclude aquatic products: Fish, Seafood & Fish, Body Oil & Fish, Liver Oil)
Animal_Products_Sub1 <- Animal_Products[!(Animal_Products$Item.Code..FAO. %in% c(2761,2762,2763,2764,2765,2766,2767,2769,2781,2782)), ]
Animal_Products_Sub1_items  <- unique(Animal_Products_Sub1 [, c("Item", "Item.Code..FAO.")])

### Get Animal Product subset2 (Exclude Poultry Meat, Offals, Edible and Fats, Animals, Raw)
Animal_Products_Sub2 <- Animal_Products_Sub1[!(Animal_Products_Sub1$Item.Code..FAO. %in% c(2734,2736,2737)), ]
Animal_Products_Sub2_items  <- unique(Animal_Products_Sub2 [, c("Item", "Item.Code..FAO.")])
### Output
#write.csv(Animal_Products_Sub2_items, 'Animal_Products_Sub2_items.csv', row.names= FALSE)

### Read Animal Product TL (Exclude Poultry Meat, Offals, Edible and Fats, Animals, Raw)
Animal_Sub2_items_TL <- read.csv("Animal_Products_items_TL.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge food item and food TL
Animal_Sub2_TL <- merge(Animal_Products_Sub2, Animal_Sub2_items_TL, by.x="Item.Code..FAO.",by.y="Item.Code..FAO.", all.x=T, all.y=F)
colnames(Animal_Sub2_TL)
any(is.na(Animal_Sub2_TL$VALUE))
any(is.na(Animal_Sub2_TL$TL))

### Calculate the food consumption TL for per country per year
Animal_TL_sub2 <- ddply(Animal_Sub2_TL,.(Area.Code..ISO3.,Year),
                        function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
names(Animal_TL_sub2)[names(Animal_TL_sub2) == "V1"] <- "TL"
Animal_TL_sub2$key <- paste(Animal_TL_sub2$Area.Code..ISO3. ,Animal_TL_sub2$Year, sep="")
any(is.na(Animal_TL_sub2$TL))

### Calculate the food consumption for per country per year
Animal_Sub2_consumption <- ddply(Animal_Products_Sub2,.(Area.Code..ISO3.,Year),
                                 function(x) sum(x$Value),.progress="text")
names(Animal_Sub2_consumption)[names(Animal_Sub2_consumption) == "V1"] <- "consumption"
Animal_Sub2_consumption$key <- paste(Animal_Sub2_consumption$Area.Code..ISO3., Animal_Sub2_consumption$Year, sep="")
any(is.na(Animal_Sub2_consumption$consumption))

### merge food consumption and TL
Animal_Sub2_TL_consumption <- merge(Animal_TL_sub2, Animal_Sub2_consumption, by.x="key",by.y="key", all.x=T, all.y=F)
Animal_Sub2_TL_consumption$Item <- "Animal Product 1"
Animal_Sub2_TL_consumption <- Animal_Sub2_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Animal_Sub2_TL_consumption)
names(Animal_Sub2_TL_consumption)[names(Animal_Sub2_TL_consumption) == "Area.Code..ISO3..x"] <- "Area.ISO3"
names(Animal_Sub2_TL_consumption)[names(Animal_Sub2_TL_consumption) == "Year.x"] <- "Year"
any(is.na(Animal_Sub2_TL_consumption$TL))
any(is.na(Animal_Sub2_TL_consumption$consumption))

### Output
#write.csv(Animal_Sub2_TL_consumption, 'Adjusted_Animal_Sub2_TL_consumption.csv', row.names= FALSE)

### Get Animal Product subset3 (Include Poultry Meat, Offals, Edible and Fats, Animals, Raw)
Animal_Products_Sub3 <- Animal_Products_Sub1[Animal_Products_Sub1$Item.Code..FAO. %in% c(2734,2736,2737), ]
Animal_Products_Sub3_items  <- unique(Animal_Products_Sub3[, c("Item", "Item.Code..FAO.")])

### you have already got the fats_COUNTRY_TL.csv / offals_COUNTRY_TL.csv / poultry_COUNTRY_TL.csv from step 2
#poultry_COUNTRY_TL <- read.csv("poultry_COUNTRY_TL.csv", header=TRUE, stringsAsFactors = FALSE)
#offals_COUNTRY_TL <- read.csv("offals_COUNTRY_TL.csv", header=TRUE, stringsAsFactors = FALSE)
#fats_COUNTRY_TL <- read.csv("fats_COUNTRY_TL.csv", header=TRUE, stringsAsFactors = FALSE)
Animal_Sub3_items_TL <- rbind(poultry_COUNTRY_TL, offals_COUNTRY_TL, fats_COUNTRY_TL)
any(is.na(Animal_Sub3_items_TL$TL))

### merge food consumption and TL
colnames(Animal_Products_Sub3)
any(is.na(Animal_Products_Sub3$Value))
Animal_Products_Sub3$key <- paste(Animal_Products_Sub3$Area.Code..ISO3., Animal_Products_Sub3$Item.Code..FAO., sep="")
Animal_Products_Sub3 <- Animal_Products_Sub3[,c(3,4,7,8,10,11,12,15)]
Animal_Sub3_TL <- merge(Animal_Products_Sub3, Animal_Sub3_items_TL, by.x="key",by.y="key", all.x=T, all.y=F)
colnames(Animal_Sub3_TL)
any(is.na(Animal_Sub3_TL$TL))

### NA
Animal_NA_TL <- read.csv("Animal_NA_TL.csv", header=TRUE, stringsAsFactors = FALSE)
match_index <- match(Animal_Sub3_TL$Item.Code..FAO., Animal_NA_TL$Item.Code..FAO.)
na_indices <- is.na(Animal_Sub3_TL$TL)
Animal_Sub3_TL$TL[na_indices] <- Animal_NA_TL$TL[match_index[na_indices]]
any(is.na(Animal_Sub3_TL$TL))
### Output
#write.csv(Animal_Sub3_TL, 'Animal_Sub3_TL.csv', row.names= FALSE)

### Calculate the food consumption TL for per country per year
Animal_TL_sub3 <- ddply(Animal_Sub3_TL,.(Area.Code..ISO3.,Year),
                        function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
names(Animal_TL_sub3)[names(Animal_TL_sub3) == "V1"] <- "TL"
Animal_TL_sub3$key <- paste(Animal_TL_sub3$Area.Code..ISO3. ,Animal_TL_sub3$Year, sep="")
any(is.na(Animal_TL_sub3$TL))

### Calculate the food consumption for per country per year
Animal_Sub3_consumption <- ddply(Animal_Products_Sub3,.(Area.Code..ISO3.,Year),
                                 function(x) sum(x$Value),.progress="text")
names(Animal_Sub3_consumption)[names(Animal_Sub3_consumption) == "V1"] <- "consumption"
Animal_Sub3_consumption$key <- paste(Animal_Sub3_consumption$Area.Code..ISO3., Animal_Sub3_consumption$Year, sep="")

### merge food consumption and TL
Animal_Sub3_TL_consumption <- merge(Animal_TL_sub3, Animal_Sub3_consumption, by.x="key",by.y="key", all.x=T, all.y=F)
Animal_Sub3_TL_consumption$Item <- "Animal Product 2"
Animal_Sub3_TL_consumption <- Animal_Sub3_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Animal_Sub3_TL_consumption)
names(Animal_Sub3_TL_consumption)[names(Animal_Sub3_TL_consumption) == "Area.Code..ISO3..x"] <- "Area.ISO3"
names(Animal_Sub3_TL_consumption)[names(Animal_Sub3_TL_consumption) == "Year.x"] <- "Year"
Animal_Sub3_TL_consumption$TL[is.na(Animal_Sub3_TL_consumption$TL)] <- 0

### Output
#write.csv(Animal_Sub3_TL_consumption, 'Adjusted_Animal_Sub3_TL_consumption.csv', row.names= FALSE)

### Merge animal TL&consumption subset (subset2 + subset3, but exclude Aquatic Products)
any(is.na(Animal_Sub2_TL_consumption$TL))
any(is.na(Animal_Sub2_TL_consumption$consumption))
any(is.na(Animal_Sub3_TL_consumption$TL))
any(is.na(Animal_Sub3_TL_consumption$consumption))
Animal_TL_consumption <- rbind(Animal_Sub2_TL_consumption, Animal_Sub3_TL_consumption)

### Calculate the animal food consumption TL for per country per year
colnames(Animal_TL_consumption)
any(is.na(Animal_TL_consumption$TL))
any(is.na(Animal_TL_consumption$consumption))
Animal_TL <- ddply(Animal_TL_consumption,.(Area.ISO3,Year),
                   function(x) sum(x$consumption*x$TL)/sum(x$consumption),.progress="text")
names(Animal_TL)[names(Animal_TL) == "V1"] <- "TL"
colnames(Animal_TL)
any(is.na(Animal_TL$TL))
Animal_TL$key <- paste(Animal_TL$Area.ISO3, Animal_TL$Year, sep="")

### Calculate the food consumption for per country per year
Animal_consumption <- ddply(Animal_TL_consumption,.(Area.ISO3, Year),
                            function(x) sum(x$consumption),.progress="text")
names(Animal_consumption)[names(Animal_consumption) == "V1"] <- "consumption"
any(is.na(Animal_consumption$consumption))
Animal_consumption$key <- paste(Animal_consumption$Area.ISO3, Animal_consumption$Year, sep="")

### merge food consumption and TL
Livestock_TL_consumption <- merge(Animal_TL, Animal_consumption, by.x="key",by.y="key", all.x=T, all.y=F)
Livestock_TL_consumption$Item <- "Livestock Products"
Livestock_TL_consumption <- Livestock_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Livestock_TL_consumption)
names(Livestock_TL_consumption)[names(Livestock_TL_consumption) == "Area.ISO3.x"] <- "Area.ISO3"
names(Livestock_TL_consumption)[names(Livestock_TL_consumption) == "Year.x"] <- "Year"
any(is.na(Livestock_TL_consumption$TL))
any(is.na(Livestock_TL_consumption$consumption))

### Output
#write.csv(Livestock_TL_consumption, 'Adjusted_Livestock_TL_consumption.csv', row.names= FALSE)

### FOR Vegetal Products
### Include Aquatic Plants
### you have already got the Adjusted_Vegetal_Products.csv from step 1
#Vegetal_Products <- read.csv("Adjusted_Vegetal_Products.csv", header=TRUE, stringsAsFactors = FALSE)
any(is.na(Vegetal_Products$Value))
Vegetal_Products$TL <- 1
Vegetal_TL <- ddply(Vegetal_Products,.(Area.Code..ISO3.,Year),
                    function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
names(Vegetal_TL)[names(Vegetal_TL) == "V1"] <- "TL"
Vegetal_TL$key <- paste(Vegetal_TL$Area.Code..ISO3. ,Vegetal_TL$Year, sep="")
any(is.na(Vegetal_TL$TL))

### Calculate the food consumption for per country per year
Vegetal_consumption <- ddply(Vegetal_Products,.(Area.Code..ISO3.,Year),
                             function(x) sum(x$Value),.progress="text")
names(Vegetal_consumption)[names(Vegetal_consumption) == "V1"] <- "consumption"
Vegetal_consumption$key <- paste(Vegetal_consumption$Area.Code..ISO3., Vegetal_consumption$Year, sep="")
any(is.na(Vegetal_consumption$consumption))

### merge food consumption and TL
Vegetal_TL_consumption <- merge(Vegetal_TL, Vegetal_consumption, by.x="key",by.y="key", all.x=T, all.y=F)
Vegetal_TL_consumption$Item <- "Vegetal Products"
Vegetal_TL_consumption <- Vegetal_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Vegetal_TL_consumption)
names(Vegetal_TL_consumption)[names(Vegetal_TL_consumption) == "Area.Code..ISO3..x"] <- "Area.ISO3"
names(Vegetal_TL_consumption)[names(Vegetal_TL_consumption) == "Year.x"] <- "Year"
any(is.na(Vegetal_TL_consumption$TL))
any(is.na(Vegetal_TL_consumption$consumption))

### Output
#write.csv(Vegetal_TL_consumption, 'Adjusted_Vegetal_TL_consumption.csv', row.names= FALSE)

### FOR Aquatic Products 
### you have already got the Adjusted_Fish_oil.csv from step 1
#Fish_oil <- read.csv("Adjusted_Fish_oil.csv", header=TRUE, stringsAsFactors = FALSE)
### Read Animal Product TL
Animal_Sub2_items_TL <- read.csv("Animal_Products_items_TL.csv", header=TRUE, stringsAsFactors = FALSE)
### Merge food item and food TL
Fish_oil_TL <- merge(Fish_oil, Animal_Sub2_items_TL, by.x="Item.Code..FAO.",by.y="Item.Code..FAO.", all.x=T, all.y=F)
colnames(Fish_oil_TL)
any(is.na(Fish_oil_TL$VALUE))
any(is.na(Fish_oil_TL$TL))

### Calculate the food consumption TL for per country per year
TL_Fish_oil <- ddply(Fish_oil_TL,.(Area.Code..ISO3.,Year),
                     function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
names(TL_Fish_oil)[names(TL_Fish_oil) == "V1"] <- "TL"
TL_Fish_oil$key <- paste(TL_Fish_oil$Area.Code..ISO3. ,TL_Fish_oil$Year, sep="")
any(is.na(TL_Fish_oil$TL))

### Calculate the food consumption for per country per year
consumption_Fish_oil <- ddply(Fish_oil,.(Area.Code..ISO3.,Year),
                              function(x) sum(x$Value),.progress="text")
names(consumption_Fish_oil)[names(consumption_Fish_oil) == "V1"] <- "consumption"
consumption_Fish_oil$key <- paste(consumption_Fish_oil$Area.Code..ISO3., consumption_Fish_oil$Year, sep="")
any(is.na(consumption_Fish_oil$consumption))

### merge food consumption and TL
Fish_oil_TL_consumption <- merge(TL_Fish_oil, consumption_Fish_oil, by.x="key",by.y="key", all.x=T, all.y=F)
Fish_oil_TL_consumption$Item <- "Fish oil"
Fish_oil_TL_consumption <- Fish_oil_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Fish_oil_TL_consumption)
names(Fish_oil_TL_consumption)[names(Fish_oil_TL_consumption) == "Area.Code..ISO3..x"] <- "Area.ISO3"
names(Fish_oil_TL_consumption)[names(Fish_oil_TL_consumption) == "Year.x"] <- "Year"
any(is.na(Fish_oil_TL_consumption$TL))
any(is.na(Fish_oil_TL_consumption$consumption))
Fish_oil_TL_consumption$TL[is.na(Fish_oil_TL_consumption$TL)] <- 0

### Output
#write.csv(Fish_oil_TL_consumption, 'Adjusted_Fish_oil_TL_consumption.csv', row.names= FALSE)

### you have already got the Adjusted_Fish_Food.csv from step 1
#Fish_Food <- read.csv("Adjusted_Fish_Food.csv", header=TRUE, stringsAsFactors = FALSE)
Fish_Food$key <- paste(Fish_Food$Area.Code..ISO3., Fish_Food$Year, Fish_Food$Item.Code..FAO., sep="")

### you have already got the Fish_COUNTRY_Year_TL.csv from step 2
#Fish_COUNTRY_Year_TL <- read.csv("Fish_COUNTRY_Year_TL.csv", header=TRUE, stringsAsFactors = FALSE)
Fish_Food_TL <- merge(Fish_Food, Fish_COUNTRY_Year_TL, by.x="key",by.y="key", all.x=T, all.y=F)
colnames(Fish_Food_TL)
any(is.na(Fish_Food_TL$TL))
any(is.na(Fish_Food_TL$Value))

### NA
Fish_code_tl <- read.csv("Fish_items_sub_F.csv", header=TRUE, stringsAsFactors = FALSE)
colnames(Fish_code_tl)
Fish_TL_NA <- ddply(Fish_code_tl,.(FOOD.ITEM),
                    function(x) mean(x$Troph),.progress="text")
mapping <- data.frame(  
  FOOD.ITEM = c("Freshwater fish", "Demersal fish", "Pelagic fish", "Marine fish, other", "Crustaceans", "Cephalopods", "Molluscs, other", "Aquatic animals, other"),  
  FOOD_ITEM_CODE = c("2761", "2762", "2763", "2764", "2765", "2766", "2767", "2769")   
)  
Fish_TL_NA <- Fish_TL_NA %>%  
  left_join(mapping, by = "FOOD.ITEM")
colnames(Fish_TL_NA)
names(Fish_TL_NA)[names(Fish_TL_NA) == "V1"] <- "TL"
match_index <- match(Fish_Food_TL$Item.Code..FAO., Fish_TL_NA$FOOD_ITEM_CODE)
na_indices <- is.na(Fish_Food_TL$TL)
Fish_Food_TL$TL[na_indices] <- Fish_TL_NA$TL[match_index[na_indices]]
any(is.na(Fish_Food_TL$TL))

### Calculate the food consumption TL for per country per year
colnames(Fish_Food_TL)
TL_Fish <- ddply(Fish_Food_TL,.(Area.Code..ISO3.,Year.x),
                 function(x) sum(x$Value*x$TL)/sum(x$Value),.progress="text")
any(is.na(TL_Fish$V1))
colnames(TL_Fish)
names(TL_Fish)[names(TL_Fish) == "Year.x"] <- "Year"
names(TL_Fish)[names(TL_Fish) == "V1"] <- "TL"
TL_Fish$key <- paste(TL_Fish$Area.Code..ISO3. ,TL_Fish$Year, sep="")

### Calculate the food consumption for per country per year
consumption_Fish <- ddply(Fish_Food_TL,.(Area.Code..ISO3.,Year.x),
                          function(x) sum(x$Value),.progress="text")
names(consumption_Fish)[names(consumption_Fish) == "V1"] <- "consumption"
names(consumption_Fish)[names(consumption_Fish) == "Year.x"] <- "Year"
consumption_Fish$key <- paste(consumption_Fish$Area.Code..ISO3. ,consumption_Fish$Year, sep="")

### merge food consumption and TL
Fish_TL_consumption <- merge(TL_Fish, consumption_Fish, by.x="key",by.y="key", all.x=T, all.y=F)
Fish_TL_consumption$Item <- "Fish, Seafood"
Fish_TL_consumption<- Fish_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Fish_TL_consumption)
names(Fish_TL_consumption)[names(Fish_TL_consumption) == "Area.Code..ISO3..x"] <- "Area.ISO3"
names(Fish_TL_consumption)[names(Fish_TL_consumption) == "Year.x"] <- "Year"
Fish_TL_consumption$TL[is.na(Fish_TL_consumption$TL)] <- 0

### Output
#write.csv(Fish_TL_consumption, 'Adjusted_Fish_TL_consumption.csv', row.names= FALSE)

### Merge Aquatic Products TL&consumption subset (Fish, Seafood + fish oil)
#Fish_TL_consumption <- read.csv("Adjusted_Fish_TL_consumption.csv", header=TRUE, stringsAsFactors = FALSE)
any(is.na(Fish_TL_consumption$TL))
any(is.na(Fish_TL_consumption$consumption))
#Fish_oil_TL_consumption <- read.csv("Adjusted_Fish_oil_TL_consumption.csv", header=TRUE, stringsAsFactors = FALSE)
any(is.na(Fish_oil_TL_consumption$TL))
any(is.na(Fish_oil_TL_consumption$consumption))
Aquatic_TL_consumption <- rbind(Fish_TL_consumption, Fish_oil_TL_consumption)

### Calculate the animal food consumption TL for per country per year
any(is.na(Aquatic_TL_consumption$TL))
any(is.na(Aquatic_TL_consumption$consumption))
Aquatic_TL <- ddply(Aquatic_TL_consumption,.(Area.ISO3,Year),
                    function(x) sum(x$consumption*x$TL)/sum(x$consumption),.progress="text")
names(Aquatic_TL)[names(Aquatic_TL) == "V1"] <- "TL"
colnames(Aquatic_TL)
any(is.na(Aquatic_TL$TL))
Aquatic_TL$key <- paste(Aquatic_TL$Area.ISO3, Aquatic_TL$Year, sep="")

### Calculate the food consumption for per country per year
Aquatic_consumption <- ddply(Aquatic_TL_consumption,.(Area.ISO3, Year),
                             function(x) sum(x$consumption),.progress="text")
names(Aquatic_consumption)[names(Aquatic_consumption) == "V1"] <- "consumption"
any(is.na(Aquatic_consumption$consumption))
Aquatic_consumption$key <- paste(Aquatic_consumption$Area.ISO3, Aquatic_consumption$Year, sep="")

### merge food consumption and TL
Aquatic_TL_consumption <- merge(Aquatic_TL,Aquatic_consumption, by.x="key",by.y="key", all.x=T, all.y=F)
Aquatic_TL_consumption$Item <- "Aquatic Products"
Aquatic_TL_consumption <- Aquatic_TL_consumption[,c(1,8,2,3,7,4)]
colnames(Aquatic_TL_consumption)
names(Aquatic_TL_consumption)[names(Aquatic_TL_consumption) == "Area.ISO3.x"] <- "Area.ISO3"
names(Aquatic_TL_consumption)[names(Aquatic_TL_consumption) == "Year.x"] <- "Year"
any(is.na(Aquatic_TL_consumption$TL))
any(is.na(Aquatic_TL_consumption$consumption))
Aquatic_TL_consumption$TL[is.na(Aquatic_TL_consumption$TL)] <- 0

### Output
#write.csv(Aquatic_TL_consumption, 'Adjusted_Aquatic_TL_consumption.csv', row.names= FALSE)

### Merge food TL&consumption subset (Vegetal Products + Animal Products (Aquatic Products + Livestock Products))
#Vegetal_TL_consumption <- read.csv("Adjusted_Vegetal_TL_consumption.csv", header=TRUE, stringsAsFactors = FALSE)
#Aquatic_TL_consumption <- read.csv("Adjusted_Aquatic_TL_consumption.csv", header=TRUE, stringsAsFactors = FALSE)
#Livestock_TL_consumption <- read.csv("Adjusted_Livestock_TL_consumption.csv", header=TRUE, stringsAsFactors = FALSE)

### NA 
any(is.na(Aquatic_TL_consumption$TL))
any(is.na(Vegetal_TL_consumption$TL))
any(is.na(Livestock_TL_consumption$TL))
Food_TL_consumption <- rbind(Vegetal_TL_consumption, Livestock_TL_consumption, Aquatic_TL_consumption)

### Read population dataset
Pop <- read.csv("Population.csv", header=TRUE, stringsAsFactors = FALSE)
### merge food consumption, TL and population
Pop <- Pop[,c(3,4,10,11,12)]
Pop$key <- paste(Pop$Area.Code..ISO3. ,Pop$Year, sep="")
names(Pop)[names(Pop) == "Value"] <- "Pop"
Food_TL_consumption_pop <- merge(Food_TL_consumption, Pop, by.x="key",by.y="key", all.x=T, all.y=F)
any(is.na(Food_TL_consumption_pop$Pop))
colnames(Food_TL_consumption_pop)
Food_TL_consumption_pop <- Food_TL_consumption_pop[,c(1,2,3,8,4,5,6,10,11)]
names(Food_TL_consumption_pop)[names(Food_TL_consumption_pop) == "Year.x"] <- "Year"

### Calculate the total food consumption for per country per year (consumption * pop)
Food_TL_consumption_pop$tot_con <- Food_TL_consumption_pop$consumption*Food_TL_consumption_pop$Pop*1000

### Output
#write.csv(Food_TL_consumption_pop, 'Adjusted_Food_TL_consumption_pop.csv', row.names= FALSE)

### step 4 HTL Calculation ####################################################

### you have already got the Adjusted_Food_TL_consumption_pop.csv from step 3
#Food_TL_consumption_pop <- read.csv("Adjusted_Food_TL_consumption_pop.csv", header=TRUE, stringsAsFactors = FALSE)
### Calculate the HTL for per country per year
any(is.na(Food_TL_consumption$TL))
Food_HTL <- ddply(Food_TL_consumption,.(Area.ISO3,Year),
                  function(x) 1 + sum(x$consumption*x$TL)/sum(x$consumption),.progress="text")
names(Food_HTL)[names(Food_HTL) == "V1"] <- "HTL"
Food_HTL$key <- paste(Food_HTL$Area.ISO3, Food_HTL$Year, sep="")
colnames(Food_HTL)
any(is.na(Food_HTL$HTL))

### Calculate the food consumption for per country per year
Food_consumption <- ddply(Food_TL_consumption,.(Area.ISO3,Year),
                          function(x) sum(x$consumption),.progress="text")
names(Food_consumption)[names(Food_consumption) == "V1"] <- "consumption"
Food_consumption$key <- paste(Food_consumption$Area.ISO3, Food_consumption$Year, sep="")
any(is.na(Food_consumption$consumption))

### merge food consumption and HTL
HTL_consumption <- merge(Food_HTL, Food_consumption, by.x="key",by.y="key", all.x=T, all.y=F)
colnames(HTL_consumption)
HTL_consumption <- HTL_consumption[,c(1,2,3,7,4)]
colnames(HTL_consumption)
names(HTL_consumption)[names(HTL_consumption) == "Area.ISO3.x"] <- "Area.ISO3"
names(HTL_consumption)[names(HTL_consumption) == "Year.x"] <- "Year"
any(is.na(HTL_consumption$HTL))
any(is.na(HTL_consumption$consumption))

### Output
#write.csv(HTL_consumption, 'Adjusted_HTL_consumption.csv', row.names= FALSE)

### merge food consumption, HTL and population
#Pop <- read.csv("Population.csv", header=TRUE, stringsAsFactors = FALSE)
colnames(Pop)
Pop <- Pop[,c(3,4,10,11,12)]
Pop$key <- paste(Pop$Area.Code..ISO3. ,Pop$Year, sep="")
names(Pop)[names(Pop) == "Value"] <- "Pop"
HTL_consumption_pop <- merge(HTL_consumption, Pop, by.x="key",by.y="key", all.x=T, all.y=F)
any(is.na(HTL_consumption_pop$Pop))
colnames(HTL_consumption_pop)
HTL_consumption_pop <- HTL_consumption_pop[,c(1,2,7,3,4,5,9,10)]
names(HTL_consumption_pop)[names(HTL_consumption_pop) == "Year.x"] <- "Year"

### Calculate the total food consumption for per country per year (consumption * pop)
HTL_consumption_pop$tot_con <- HTL_consumption_pop$consumption*HTL_consumption_pop$Pop*1000

### Output
#write.csv(HTL_consumption_pop, 'Adjusted_HTL_consumption_pop.csv', row.names= FALSE)

### Calculate the world mean human trophic level (weighted by total food consumption)
WTL_Mean <- ddply(HTL_consumption_pop,.(Year),
                  function(x) weighted.mean(x$HTL,x$tot_con, na.rm = TRUE))
names(WTL_Mean)[names(WTL_Mean) == "V1"] <- "WTL"


### step 5 Decomposition of HTL Change #########################################

### you have already got the Adjusted_Food_TL_consumption_pop.csv from step 3
#Food_TL_consumption_pop <- read.csv("Adjusted_Food_TL_consumption_pop.csv", header=TRUE, stringsAsFactors = FALSE)
global_diet <- Food_TL_consumption_pop %>%
  group_by(Area.ISO3, Year) %>%
  mutate(
    total_consumption = sum(consumption),
    proportion = (consumption / total_consumption) * 100
  ) %>%
  ungroup()

global_diet_sum <- global_diet %>%
  group_by(Area.ISO3, Year) %>%
  summarise(total_pct = sum(proportion), .groups = "drop") %>%
  filter(abs(total_pct - 100) > 0.1)  

### Output
#write.csv(global_diet, 'Adjusted_global_diet.csv', row.names= FALSE)

global_Animal <- global_diet %>%
  filter(Item != "Vegetal Products") %>%
  group_by(Area.ISO3, Year) %>%
  summarise(Aniaml_pct = sum(proportion), 
            Aniaml_TL = sum(consumption * TL) / sum(consumption),
            .groups = "drop")

### Output
#write.csv(global_Animal, 'global_Animal.csv', row.names= FALSE)

### you have already got the Adjusted_HTL_consumption_pop.csv from step 4
#HTL_consumption_pop <- read.csv("Adjusted_HTL_consumption_pop.csv", header=TRUE, stringsAsFactors = FALSE)
global_Animal <- global_Animal %>%
  left_join(HTL_consumption_pop, by = c("Area.ISO3", "Year")) %>% 
  select(Area.ISO3, Area, Year, HTL, Animal_pct, Animal_TL) %>%
  mutate(
    Animal_pct = Animal_pct / 100
  )

df <- global_Animal %>%
  filter(!is.na(HTL), !is.na(Animal_pct), !is.na(Animal_TL))

df_models <- df %>%
  group_by(Area.ISO3) %>%
  filter(n() >= 10) %>% 
  summarise(
    n_year = n(),
    mean_pct = mean(Animal_pct, na.rm = TRUE),
    mean_TL = mean(Animal_TL, na.rm = TRUE),
    model_HTL = list(lm(HTL ~ Year)),
    model_pct = list(lm(Animal_pct ~ Year)),
    model_TL  = list(lm(Animal_TL ~ Year)),
    
    .groups = 'drop'
  )

df_slopes <- df_models %>%
  mutate(
    tidy_HTL = map(model_HTL, tidy),
    tidy_pct = map(model_pct, tidy),
    tidy_TL  = map(model_TL, tidy)
  ) %>%
  unnest(c(tidy_HTL, tidy_pct, tidy_TL), names_sep = "_") %>%
  filter(tidy_HTL_term == "Year") %>%
  select(
    Area.ISO3, mean_pct, mean_TL,
    slope_HTL = tidy_HTL_estimate, pval_HTL = tidy_HTL_p.value,
    slope_pct = tidy_pct_estimate, pval_pct = tidy_pct_p.value,
    slope_TL  = tidy_TL_estimate,  pval_TL  = tidy_TL_p.value
  )

# Decomposition based on derivatives
# d(HTL)/dt ≈ d(Animal_pct)/dt * (mean_TL - 1) + d(Animal_TL)/dt * mean_pct
df_decomp <- df_slopes %>%
  mutate(
    HTL_rate = slope_HTL * 10,
    pct_rate = slope_pct * 10,  
    TL_rate  = slope_TL * 10,
    
    # Dietary Structural Effect, DSE
    Effect_share = pct_rate * (mean_TL - 1),
    
    # Trophic Level Effect, TLE
    Effect_TL = TL_rate * mean_pct,
    
    Residual = HTL_rate - (Effect_share + Effect_TL)
  )

# Typology with Significance Threshold
country_classified <- df_decomp %>%
  mutate(
    HTL_Trend = case_when(
      pval_HTL >= 0.05 ~ "Stable", 
      HTL_rate > 0 & pval_HTL < 0.05 ~ "Increase",
      HTL_rate < 0 & pval_HTL < 0.05 ~ "Decrease"
    ),
    Dominant_Factor = case_when(
      abs(Effect_share) > abs(Effect_TL) ~ "Share-dominated",
      abs(Effect_share) < abs(Effect_TL) ~ "TL-dominated",
      TRUE ~ "Balanced"
    ),
    Quadrant = case_when(
      HTL_Trend == "Increase" & Dominant_Factor == "Share-dominated" & pval_pct < 0.05 ~ "Ⅰ. Animal consumption expansion",
      HTL_Trend == "Increase" & Dominant_Factor == "TL-dominated" & pval_TL < 0.05 ~ "Ⅲ. Food web upgrading",
      HTL_Trend == "Decrease" & Dominant_Factor == "Share-dominated" & pval_pct < 0.05 ~ "Ⅱ. Plant-based transition",
      HTL_Trend == "Decrease" & Dominant_Factor == "TL-dominated" & pval_TL < 0.05 ~ "Ⅳ. Food web downgrading",
      HTL_Trend != "Stable" ~ "Ⅴ. Other: mixed-driven",
      HTL_Trend == "Stable" & pval_pct < 0.05 & pval_TL < 0.05 ~ "Ⅵ. Offset",
      TRUE ~ "Ⅶ. Stagnant"
    )
  )

print(head(country_classified %>% select(Area.ISO3, HTL_rate, Quadrant)))