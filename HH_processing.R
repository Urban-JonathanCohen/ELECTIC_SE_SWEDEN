# Swedish-household
###########################
library(Hmisc)
library(psych)
library(dplyr)
library(randomForest)


options(max.print = 10000) 
setwd("./Household")



HH_DB <- read.csv("hut21_hushall.txt", header = TRUE, sep = ';' )
IN_DB <- read.csv("hut21_individ.txt", header = TRUE, sep = ';' )

IN_DB

#length(unique(IN_DB$LopnrHushall))
length(unique(HH_DB$LopnrHushall))


#IN_DB_all <- IN_DB
HH_DB_all <- HH_DB


#na.omit(HH_DB_all$U01)


#IN_DB <- na.omit(IN_DB_all)
#HH_DB <- HH_DB_all[!is.na(HH_DB_all$U01), ]

describe(HH_DB)


# #####################################
# # INDIVIDUAL 
# ##################################
# colnames(IN_DB)
# label(IN_DB$LopnrHushall)
# class(label(IN_DB$LopnrHushall))
# is.null(label(IN_DB$LopnrHushall))
# 
# label(IN_DB$AlderSlut) <- "Age"
# label(IN_DB$BORDNR) <- "Individual's order number in the household"
# label(IN_DB$BVXBARN) <- "Child or adult, 20 years old (Barn eller vuxen, 20 år)"
# label(IN_DB$Kommun) <- "Municipality"
# label(IN_DB$BKHF) <- "Household head"
# label(IN_DB$BKON) <- "Gender"
# label(IN_DB$LopnrHushall) <- "hh_ID"
# 
# 
# 
# label(IN_DB$LopnrHushall)
# is.null(label(IN_DB$LopnrHushall))
# 
# sapply(IN_DB, function(x) {
#   lbl <- label(x)
#   if (is.null(lbl)) {
#     return(TRUE)   # Returns TRUE if the label is NULL
#   } else {
#     return(lbl == "")  # Returns TRUE if the label is an empty string
#   }
# })
# 
# 
# names(IN_DB)[sapply(IN_DB, function(x) {
#   lbl <- label(x)
#   if (is.null(lbl)) {
#     return(TRUE)   # Returns TRUE if the label is NULL
#   } else {
#     return(lbl == "")  # Returns TRUE if the label is an empty string
#   }
# })
# ]
# 




#####################################
# HOUSEHOLD 
##################################


colnames(HH_DB)

names(HH_DB)[sapply(HH_DB, function(x) {
  lbl <- label(x)
  if (is.null(lbl)) {
    return(TRUE)   # Returns TRUE if the label is NULL
  } else {
    return(lbl == "")  # Returns TRUE if the label is an empty string
  }
})
]








label(HH_DB$kvikt) <- "Calibrated weight used to scale up to the population"
label(HH_DB$lillam) <- "Number of households participating in the respective stratum"
label(HH_DB$storan) <- "Number of households in the frame for the respective stratum"
label(HH_DB$LopnrHushall) <- "hh_ID"
label(HH_DB$HUT_del) <- "HH Survey part"

##################################################################
label(HH_DB$BALDZ) <- "Age of head of family"
label(HH_DB$BKONZ) <- "Gender of the household main"

summary(HH_DB$BALDZ) # age
summary(HH_DB$BKONZ) # gender


hh_clean        <- data.frame(hh_gender = HH_DB$BKONZ)

hh_clean <- hh_clean %>%
  mutate(hh_gender = case_when(
    hh_gender == 1 ~ 1,       # When BKONZ is 1, set as "man"
    hh_gender == 2 ~ 0        # When BKONZ is 2, set as "woman"
  ))


hh_clean$hh_gender <- factor(hh_clean$hh_gender,
                      levels = c(1, 0),
                      labels = c("man", "woman"))


HH_DB$BKON == HH_DB$BKONZ

label(hh_clean$hh_gender) <- "Gender of the household main"


hh_clean$hh_age <- HH_DB$BALDZ
hh_clean$weight <- HH_DB$kvikt          # Calibrated weight used to scale up to the population"
hh_clean$hh_in_stratum <- HH_DB$lillam  # <- "Number of households participating in the respective stratum"
hh_clean$hh_oin_frame <- HH_DB$storan   # <- "Number of households in the frame for the respective stratum"
hh_clean$HH_ID <- HH_DB$LopnrHushall    # <- "hh_ID"
hh_clean$survey_part <- HH_DB$HUT_del   # <- "Part"

unique(hh_clean$survey_part)

length(unique(hh_clean$weight))


##########3


##############################################################

label(HH_DB$BANTBARNZ)    <- "Number of children aged 0–19 years in the household"
label(HH_DB$BBARN03Z)     <- "Number of children aged 0–3 years in the household"
label(HH_DB$BBARN410Z)    <- "Number of children aged 4–10 years in the household"
label(HH_DB$BBARN1117Z)   <- "Number of children aged 11–17 years in the household"
label(HH_DB$BANTHUSMEDZ)  <- "Number of household members in the household"
label(HH_DB$BANTVUXZ)     <- "Number of adults in the household"



vars_hh_members <- c("BANTBARNZ", "BBARN03Z", "BBARN410Z", 
                     "BBARN1117Z", "BANTHUSMEDZ", "BANTVUXZ")

describe(HH_DB[, vars_hh_members])


hh_clean$kids_0_19 <- HH_DB$BANTBARNZ   # <- "Number of children aged 0–19 years in the household"
hh_clean$kids_0_3 <- HH_DB$BBARN03Z    # <- "Number of children aged 0–3 years in the household"
hh_clean$kids_4_10 <- HH_DB$BBARN410Z   # <- "Number of children aged 4–10 years in the household"
hh_clean$kids_11_17 <- HH_DB$BBARN1117Z  # <- "Number of children aged 11–17 years in the household"
hh_clean$total_members <- HH_DB$BANTHUSMEDZ # <- "Number of household members in the household"
hh_clean$total_adults <- HH_DB$BANTVUXZ    # <- "Number of adults in the household"





#########################################################
label(HH_DB$BEGSZ) <- "Household type"



hh_clean$hh_type <- factor(HH_DB$BKONZ,
                      levels = c(1, 2, 3, 4),
                      labels = c("Single with only one adult",
                                 "Cohabiting with only two adults",
                                 "Single with more than one adult",
                                 "Cohabiting with more than two adults"))




label(hh_clean$hh_type) <- "Household type"


summary(hh_clean$hh_type)


#####################################################################
label(HH_DB$BREGZ) <- "Regional division"

summary(HH_DB$BREGZ)

hh_clean$region <- factor(HH_DB$BREGZ,
                           levels = c(1, 2, 3, 4),
                           labels = c("Stockholm",
                                      "Gothenburg",
                                      "Munis with more than 75k",
                                      "Munis with less than 75k"))






label(hh_clean$region) <- "Regional division"

#####################################################################
hh_clean$living_area_sqm <- HH_DB$BOAREAZ
label(hh_clean$living_area_sqm) <- "Living area, primary residence, sqm"


summary(hh_clean$living_area_sqm)
wtd.mean(hh_clean$living_area_sqm, weights = hh_clean$living_area_sqm)


#####################################################################
hh_clean$q_rooms <-  HH_DB$BANTALRUMZ
label(hh_clean$q_rooms) <- "Number of rooms in the primary residence"




hh_clean$kok_type <- HH_DB$BKOKTYPZ


hh_clean$kok_type <- factor(hh_clean$kok_type,
                          levels = c(1, 2, 3, 4),
                          labels = c("Yes, kitchen",
                                     "Yes, kitchenette",
                                     "Yes, two or more kitchens",
                                     "No, neither kitchen nor kitchenette"))



summary(hh_clean$kok_type)

label(hh_clean$kok_type) <- "Type of kitchen in the primary residence"

#####################################################################

hh_clean$tenure <- HH_DB$BUPPLATZ




hh_clean$tenure <- factor(hh_clean$tenure,
                            levels = c(1, 2, 3, 4, 6),
                            labels = c("Ownership of a house or farm",
                                       "Housing cooperative",
                                       "Rental, first-hand",
                                       "Rental, second-hand",
                                       "Other"))


summary(hh_clean$tenure)
label(hh_clean$tenure) <- "Tenure form, indicates how the household occupies the primary residence"




#####################################################################

hh_clean$hh_stratum <- HH_DB$STRATUM

label(hh_clean$hh_stratum)    <- "Household stratum"


hh_clean$cons_weight <- HH_DB$CDISP04Z

label(hh_clean$cons_weight)     <- "Consumption weight, 2004 scale"


################################################################

hh_clean$disposable_inc <- HH_DB$CDISP04Z
label(hh_clean$disposable_inc)   <- "Disposable income of the household"



summary(hh_clean$disposable_inc)
wtd.mean(hh_clean$disposable_inc, weights = hh_clean$living_area_sqm)

#there are some negative values



hh_clean$total_expenditure <- HH_DB$U00
label(hh_clean$total_expenditure) <- "Total expenditure"

summary(hh_clean$total_expenditure)
wtd.mean(hh_clean$total_expenditure, weights = hh_clean$living_area_sqm)


#########################################################


hh_clean$a_food_n_alcohol <- HH_DB$U01
hh_clean$b_cloth_n_foot <- HH_DB$U03
hh_clean$c_furniture_mantain_home <- HH_DB$U05
hh_clean$d_transport <- HH_DB$U07
hh_clean$e_ict <- HH_DB$U08
hh_clean$f_recreation <- HH_DB$U09
hh_clean$g_personal_care <- HH_DB$U13



label(hh_clean$a_food_n_alcohol) <- "Food and non-alcoholic beverages"
label(hh_clean$b_cloth_n_foot) <- "Clothing and footwear"
label(hh_clean$c_furniture_mantain_home) <- "Furnishings, household equipment, and routine maintenance of the residence"
label(hh_clean$d_transport) <- "Transport"
label(hh_clean$e_ict) <- "Information and communication"
label(hh_clean$f_recreation) <- "Recreation, sports, culture, garden, and pets"
label(hh_clean$g_personal_care) <- "Personal care, social protection, and miscellaneous goods and services"


hh_clean$a1_food                <- HH_DB$U011
hh_clean$a2_cereal              <- HH_DB$U0111
hh_clean$a3_meat                <- HH_DB$U0112
hh_clean$a4_fish                <- HH_DB$U0113
hh_clean$a5_dary_egg_veg        <- HH_DB$U0114
hh_clean$a6_oil_butter          <- HH_DB$U0115
hh_clean$a7_fruit_nut           <- HH_DB$U0116
hh_clean$a8_green_veggie        <- HH_DB$U0117
hh_clean$a8.1_potato            <- HH_DB$U0117a
hh_clean$a8.2_veg_shroom        <- HH_DB$U0117b
hh_clean$a9_sugar_desserts      <- HH_DB$U0118
hh_clean$a9.1_goodies           <- HH_DB$U0118a
hh_clean$a9.2_sugar_jam_honey   <- HH_DB$U0118b
hh_clean$a10_snaks_ready        <- HH_DB$U0119
hh_clean$a10.1_snaks            <- HH_DB$U0119a
hh_clean$a10.2_ready_meal       <- HH_DB$U0119b
hh_clean$a11_sauce_spice_yeast  <- HH_DB$U0119c

  
  
  
label(hh_clean$a1_food) <- "Food"
label(hh_clean$a2_cereal) <- "Cereal, rice, or maize products"
label(hh_clean$a3_meat) <- "Meat products"
label(hh_clean$a4_fish) <- "Fish or seafood"
label(hh_clean$a5_dary_egg_veg) <- "Dairy products, eggs, or vegetarian alternatives"
label(hh_clean$a6_oil_butter) <- "Cooking oil, butter, margarine, and other fats"
label(hh_clean$a7_fruit_nut) <- "Fruits, berries, and nuts"
label(hh_clean$a8_green_veggie) <- "Vegetables, root vegetables, and vegetarian food"
label(hh_clean$a8.1_potato) <- "Potatoes and potato products"
label(hh_clean$a8.2_veg_shroom) <- "Vegetables, mushrooms, and vegetarian food"
label(hh_clean$a9_sugar_desserts) <- "Sugar, confectionery, and desserts"
label(hh_clean$a9.1_goodies) <- "Candy, chocolate, and ice cream"
label(hh_clean$a9.2_sugar_jam_honey) <- "Sugar, jam, marmalade, honey, and syrup"
label(hh_clean$a10_snaks_ready) <- "Snacks, ready meals, and other foods"
label(hh_clean$a10.1_snaks) <- "Snacks"
label(hh_clean$a10.2_ready_meal) <- "Ready meals"
label(hh_clean$a11_sauce_spice_yeast) <- "Sauces, spices, broth, yeast, baking powder, etc."


hh_clean$b1_clothing             <- HH_DB$U031
hh_clean$b2_footwear             <- HH_DB$U032
  
label(hh_clean$b1_clothing) <- "Clothing"
label(hh_clean$b2_footwear) <- "Footwear"



################## 
# Furniture
######################

hh_clean$c1_furniture_in_out       <- HH_DB$U051
hh_clean$c1.1_indoor               <- HH_DB$U05111
hh_clean$c1.1a_seating             <- HH_DB$U05111a
hh_clean$c1.1b_table               <- HH_DB$U05111b
hh_clean$c1.1c_bed                 <- HH_DB$U05111c
hh_clean$c1.1d_storage             <- HH_DB$U05111d
hh_clean$c1.1e_other               <- HH_DB$U05111e
hh_clean$c1.1f_small_painting      <- HH_DB$U0511x


label(hh_clean$c1_furniture_in_out)      <- "Furniture for indoor and outdoor use and smaller furnishings"
label(hh_clean$c1.1_indoor)       <- "Indoor furniture"
label(hh_clean$c1.1a_seating)     <- "Seating furniture"
label(hh_clean$c1.1b_table)       <- "Tables"
label(hh_clean$c1.1c_bed)         <- "Beds"
label(hh_clean$c1.1d_storage)     <- "Storage furniture like bookshelves, drawers, wardrobes"
label(hh_clean$c1.1e_other)       <- "Other indoor furniture"
label(hh_clean$c1.1f_small_painting) <- "Smaller furnishings like paintings, carpets, and lighting"


hh_clean$c1.2_furniture_repair             <- HH_DB$U0512
label(hh_clean$c1.2_furniture_repair) <- "Repair of furniture"





hh_clean$c1.3_outdoor_furni             <- HH_DB$U05112
label(hh_clean$c1.3_outdoor_furni) <- "Outdoor furniture"


hh_clean$c2_home_textiles             <- HH_DB$U052
hh_clean$c2.1_linen_tow             <- HH_DB$U052a


label(hh_clean$c2_home_textiles) <- "Home textiles"
label(hh_clean$c2.1_linen_tow) <- "Home textiles, such as bed linen, towels, curtains, and seat cushions"


hh_clean$c2.2_repair_hem_tex            <- HH_DB$U0522
label(hh_clean$c2.2_repair_hem_tex) <- "Repair of textiles"




#####################################################33

hh_clean$c3_white                  <- HH_DB$U053
hh_clean$c3.1_built_kok             <- HH_DB$U05311
hh_clean$c3.1a_refri               <- HH_DB$U05311a
hh_clean$c3.1b_stove               <- HH_DB$U05311b
hh_clean$c3.1c_dishwash            <- HH_DB$U05311c
hh_clean$c3.1d_other_fan           <- HH_DB$U05311d
hh_clean$c3.2_washing             <- HH_DB$U05312
hh_clean$c3.3_heater             <- HH_DB$U05313
hh_clean$c3.4_vacuum_clean             <- HH_DB$U05314 
hh_clean$c3.5_other             <- HH_DB$U05319
hh_clean$c3.6_smaller             <- HH_DB$U0532


label(hh_clean$c3_white)   <- "White goods and household appliances"
label(hh_clean$c3.1_built_kok) <- "Built-in kitchen equipment"
label(hh_clean$c3.1a_refri) <- "Refrigerator and freezer"
label(hh_clean$c3.1b_stove) <- "Stove and oven"
label(hh_clean$c3.1c_dishwash) <- "Dishwasher"
label(hh_clean$c3.1d_other_fan) <- "Other built-in kitchen equipment, e.g., fan and sink"
label(hh_clean$c3.2_washing) <- "Washing machine, tumble dryer"
label(hh_clean$c3.3_heater) <- "Heaters, fans, and air conditioning"
label(hh_clean$c3.4_vacuum_clean) <- "Vacuum cleaners and other household cleaning machines"
label(hh_clean$c3.5_other) <- "Other larger household equipment"
label(hh_clean$c3.6_smaller) <- "Smaller electrical household appliances, e.g., toaster, kettle, and iron"


hh_clean$c3.7_repair            <- HH_DB$U0533
label(hh_clean$c3.7_repair) <- "Repair of household appliances"




#############################################################


hh_clean$c4_glass_cutlery            <- HH_DB$U054
hh_clean$c4.1_glass             <- HH_DB$U054a
hh_clean$c4.2_glass_repair             <- HH_DB$U05404













label(hh_clean$c4_glass_cutlery) <- "Glass, crystal, ceramics, cutlery, porcelain, and other smaller kitchenware"
label(hh_clean$c4.1_glass) <- "Glass, crystal, ceramics, cutlery, porcelain, and other smaller kitchenware"
label(hh_clean$c4.2_glass_repair) <- "Repair of glass, porcelain, and cutlery"


hh_clean$c5_tools <-  HH_DB$U055
hh_clean$c5.1_motor_garden <-  HH_DB$U0551
hh_clean$c5.2_nonmoto <-  HH_DB$U0552
hh_clean$c5.3_tools_repair <-  HH_DB$U0553



label(hh_clean$c5_tools) <- "Tools"
label(hh_clean$c5.1_motor_garden) <- "Motor-driven tools and garden equipment"
label(hh_clean$c5.2_nonmoto) <- "Non-motorized tools, e.g., hammer, saw, and screwdriver"
label(hh_clean$c5.3_tools_repair) <- "Repair of tools"



hh_clean$c6_cleaning            <- HH_DB$U056
hh_clean$c6.1_cleaning            <- HH_DB$U0561
hh_clean$c6.1a_cleaning            <- HH_DB$U05611
hh_clean$c6.1b_cleaning            <- HH_DB$U05619


label(hh_clean$c6_cleaning) <- "Cleaning products, consumables, and household services"
label(hh_clean$c6.1_cleaning) <- "Cleaning products and consumables"
label(hh_clean$c6.1a_cleaning) <- "Cleaning products for the home, e.g., dishwashing liquid, detergent, dishcloths, and brushes"
label(hh_clean$c6.1b_cleaning) <- "Consumables for the home, e.g., paper towels, bags, candles, napkins, and matches"


#### label(hh_clean$U0562) is missing cleaning services and babysitting

#################################################

hh_clean$d1_vehicles <-    HH_DB$U071


hh_clean$d1.1_buy_car <-    HH_DB$U0711a
hh_clean$d1.2_buy_motorcycle <-    HH_DB$U0712
hh_clean$d1.3_buy_bike <-    HH_DB$U0713


label(hh_clean$d1_vehicles)    <- "Purchase of vehicles"
label(hh_clean$d1.1_buy_car)  <- "Purchase of car"
label(hh_clean$d1.2_buy_motorcycle)   <- "Purchase of motorcycle, moped, scooter, and similar"
label(hh_clean$d1.3_buy_bike)   <- "Purchase of bicycle, electric bicycle"


hh_clean$d2.4_rent_lease_car <-    HH_DB$U0712
hh_clean$d2.5_fuel <-    HH_DB$U0713


label(hh_clean$d2.4_rent_lease_car)  <- "Rental and leasing of car"
label(hh_clean$d2.5_fuel)   <- "Fuel"





##############################################
# label(hh_clean$e_ict) <- "Information and communication"




hh_clean$e1_phone             <- HH_DB$U0811  
hh_clean$e2_cell             <- HH_DB$U0812
hh_clean$e3_pc             <- HH_DB$U0813
hh_clean$e4_tv             <- HH_DB$U0814
hh_clean$e4.1_tv             <- HH_DB$U0814a
hh_clean$e4.2_dvd             <- HH_DB$U0814b
hh_clean$e5_hdrive             <- HH_DB$U0815
hh_clean$e6_other             <- HH_DB$U0819


label(hh_clean$e1_phone) <- "Purchase of landline phone and fax"
label(hh_clean$e2_cell) <- "Mobile phone purchased without a contract"
label(hh_clean$e3_pc) <- "Computer, tablet, printer, and accessories"
label(hh_clean$e4_tv) <- "TV, DVD player, speakers, etc."
label(hh_clean$e4.1_tv) <- "TV"
label(hh_clean$e4.2_dvd) <- "DVD player, speakers, digital box, etc."
label(hh_clean$e5_hdrive) <- "External hard drives, USB sticks, and blank discs"
label(hh_clean$e6_other) <- "Other home electronics and accessories"

########################################
 
hh_clean$e7_software             <- HH_DB$U082
hh_clean$e8_subscrip             <- HH_DB$U083
hh_clean$e8.1_phone             <- HH_DB$U0831
hh_clean$e8.2_cell              <- HH_DB$U0832
hh_clean$e8.3_tv             <- HH_DB$U08391
hh_clean$e8.4_digital             <- HH_DB$U08392
hh_clean$e8.4a_video             <- HH_DB$U08392a
hh_clean$e8.4b_music             <- HH_DB$U08392b  
hh_clean$e8.5_internet             <- HH_DB$U0833

  
label(hh_clean$e7_software) <- "Software and apps that are not games"
label(hh_clean$e8_subscrip) <- "Digital subscriptions"
label(hh_clean$e8.1_phone) <- "Landline phone subscription"
label(hh_clean$e8.2_cell) <- "Mobile phone subscription"
label(hh_clean$e8.3_tv) <- "TV subscription beyond the basic package"
label(hh_clean$e8.4_digital) <- "Digital services"
label(hh_clean$e8.4a_video) <- "Streaming services like Netflix, HBO, Viaplay"
label(hh_clean$e8.4b_music) <- "Digital music services like Spotify, Apple Music"
label(hh_clean$e8.5_internet) <- "Internet subscription"



########################################################################################
######### CULTURAL
####################################################


hh_clean$f1_recreation  <- HH_DB$U091
hh_clean$f1.1_camera       <- HH_DB$U0911
hh_clean$f1.1a_lens             <- HH_DB$U09113  
hh_clean$f1.2_large             <- HH_DB$U0912
hh_clean$f1.2a_motorhome             <- HH_DB$U09121
hh_clean$f1.2b_boat             <- HH_DB$U09123
hh_clean$f1.2b1_boat             <- HH_DB$U09123a
hh_clean$f1.2b2_canoes             <- HH_DB$U09123b
hh_clean$f1.3_horse             <- HH_DB$U09124
hh_clean$f1.4_large_other             <- HH_DB$U09129




label(hh_clean$f1_recreation) <- "Equipment for recreational activities"
label(hh_clean$f1.1_camera) <- "Cameras and accessories for photo and video"
label(hh_clean$f1.1a_lens) <- "Optical instruments, e.g., binoculars and microscopes"
label(hh_clean$f1.2_large) <- "Larger equipment for recreational activities"
label(hh_clean$f1.2a_motorhome) <- "Purchase of caravans, motorhomes, and trailers"
label(hh_clean$f1.2b_boat) <- "Boats, boat accessories, and water sports equipment"
label(hh_clean$f1.2b1_boat) <- "Purchase of boats, boat motors, and boat accessories"
label(hh_clean$f1.2b2_canoes) <- "Canoes, windsurfing boards, jet skis, and diving equipment"
label(hh_clean$f1.3_horse) <- "Horses and equipment for them"
label(hh_clean$f1.4_large_other) <- "Larger equipment for recreational activities, e.g., table tennis tables, billiard tables, above-ground pools, electric skateboards, snowmobiles"




hh_clean$f2_big_fun             <- HH_DB$U092
hh_clean$f2.1_egames             <- HH_DB$U09211
hh_clean$f2.2_toys             <- HH_DB$U09212
hh_clean$f2.3_party             <- HH_DB$U09213
hh_clean$f2.4_sport_camp             <- HH_DB$U0922

label(hh_clean$f2_big_fun) <- "Other equipment for recreational activities"
label(hh_clean$f2.1_egames) <- "Gaming computers, consoles, games, mobile games, accessories for and fees for games"
label(hh_clean$f2.2_toys) <- "Toys, games, and hobby collections"
label(hh_clean$f2.3_party) <- "Party decorations or holiday items"
label(hh_clean$f2.4_sport_camp) <- "Equipment for sports, camping, and outdoor activities"

hh_clean$f3_plantpet             <- HH_DB$U093
hh_clean$f3.1_plan             <- HH_DB$U0931
hh_clean$f3.2_pet             <- HH_DB$U0932

label(hh_clean$f3_plantpet) <- "Plants, flowers, and pets"
label(hh_clean$f3.1_plan) <- "Plants, flowers, and garden products"
label(hh_clean$f3.2_pet) <- "Pets, pet accessories, and pet food"



hh_clean$f4_music_film             <- HH_DB$U095
hh_clean$f4.1_instrument             <- HH_DB$U0951
hh_clean$f4.2_records             <- HH_DB$U0952


label(hh_clean$f4_music_film) <- "Musical instruments and film or music recordings"
label(hh_clean$f4.1_instrument) <- "Acoustic or electronic musical instruments"
label(hh_clean$f4.2_records) <- "Records with film or music"

hh_clean$f5_culture             <- HH_DB$U096
hh_clean$f5.1_theater             <- HH_DB$U0961
hh_clean$f5.2_musem             <- HH_DB$U0962
hh_clean$f5.3_photo             <- HH_DB$U0963
hh_clean$f5.4_dance_music             <- HH_DB$U0969


label(hh_clean$f5_culture) <- "Cultural services"
label(hh_clean$f5.1_theater) <- "Admission to theater, concert, circus, cinema"
label(hh_clean$f5.2_musem) <- "Admission to museums, exhibitions, and zoos"
label(hh_clean$f5.3_photo) <- "Photographer and photography services"
label(hh_clean$f5.4_dance_music) <- "Dance, singing, and music lessons"


hh_clean$f6_news             <- HH_DB$U097

label(hh_clean$f6_news) <- "Newspapers and books, including e-books and audiobooks"




############################################################
#label(hh_clean$g_personal_care) <- "Personal care, social protection, and miscellaneous goods and services"
#####################################################################

hh_clean$g1_kids_items             <- HH_DB$U13291
hh_clean$g1.1_kids             <- HH_DB$U13291a
hh_clean$g1.2_bags             <- HH_DB$U13291b

label(hh_clean$g1_kids_items) <- "Children's items and bags"
label(hh_clean$g1.1_kids) <- "Children's items"
label(hh_clean$g1.2_bags) <- "Bags"



hh_clean$g2_electric             <- HH_DB$U1311
hh_clean$g3_jewelry_watch             <- HH_DB$U1321


label(hh_clean$g2_electric) <- "Electrical devices for personal care"
label(hh_clean$g3_jewelry_watch) <- "Jewelry and watches"








#########################################
# Check
#############################
sapply(hh_clean, function(x) {
  lbl <- label(x)
  if (is.null(lbl)) {
    return(TRUE)   # Returns TRUE if the label is NULL
  } else {
    return(lbl == "")  # Returns TRUE if the label is an empty string
  }
})



names(hh_clean)[sapply(hh_clean, function(x) {
  lbl <- label(x)
  if (is.null(lbl)) {
    return(TRUE)   # Returns TRUE if the label is NULL
  } else {
    return(lbl == "")  # Returns TRUE if the label is an empty string
  }
})
]




#####################################################




hh_clean$CE_repair_tool     <-hh_clean$c5.3_tools_repair
hh_clean$CE_repair_glass   <-hh_clean$c4.2_glass_repair
hh_clean$CE_repair_white   <-hh_clean$c3.7_repair
hh_clean$CE_repair_textiles   <-hh_clean$c2.2_repair_hem_tex
hh_clean$CE_repair_furniture   <-hh_clean$c1.2_furniture_repair





describe(hh_clean)
describe(hh_clean$CE_repair_tool)
describe(hh_clean$CE_repair_glass)
describe(hh_clean$CE_repair_white)
describe(hh_clean$CE_repair_textiles)
describe(hh_clean$CE_repair_furniture)







hh_clean$CE_repair_tool_dummy         <- ifelse(as.numeric(hh_clean$CE_repair_tool)>0,1,0)
hh_clean$CE_repair_glass_dummy        <- ifelse(as.numeric(hh_clean$CE_repair_glass)>0,1,0)
hh_clean$CE_repair_white_dummy        <- ifelse(as.numeric(hh_clean$CE_repair_white)>0,1,0)
hh_clean$CE_repair_textiles_dummy     <- ifelse(as.numeric(hh_clean$CE_repair_textiles)>0,1,0)
hh_clean$CE_repair_furniture_dummy    <- ifelse(as.numeric(hh_clean$CE_repair_furniture)>0,1,0)

hh_clean$CE_sum_5    <-      hh_clean$CE_repair_tool +     hh_clean$CE_repair_glass    +   hh_clean$CE_repair_white    +     hh_clean$CE_repair_textiles    +    hh_clean$CE_repair_furniture


CE_VARS_DUMMY <- c('CE_repair_tool_dummy', 'CE_repair_glass_dummy', 'CE_repair_white_dummy', 'CE_repair_textiles_dummy', 'CE_repair_furniture_dummy', 'CE_sum_5')


describe(hh_clean[,CE_VARS_DUMMY])

describe(hh_clean)
describe(hh_clean$CE_repair_tool)
describe(hh_clean$CE_repair_glass)
describe(hh_clean$CE_repair_white)
describe(hh_clean$CE_repair_textiles)
describe(hh_clean$CE_repair_furniture)

describe(hh_clean)

hh_clean$g2_electric
hh_clean$CE_repair_tool
describe(na.omit(hh_clean))


na.omit(hh_clean$hh_gender)
describe(na.omit(hh_clean$disposable_inc))    

describe(na.omit(hh_clean$living_area_sqm))  






sapply(hh_clean, function(x) sum(is.na(x)))




hh_clean$expand <- round(hh_clean$weight/3,0)




############################################################################################
#place imputed
###################################################################################################
library(mice)



co_vars <- c("hh_gender", "hh_age",
             "kids_0_19", "kids_0_3", 
             "kids_4_10", "kids_11_17", "total_members", 
             "total_adults", "hh_type", "region", 
             "tenure", "hh_stratum", "total_expenditure")


to_impute <- c('disposable_inc','living_area_sqm','q_rooms','kok_type','cons_weight','HH_ID')

all <- c(co_vars , to_impute)

to_impute <- hh_clean[,all ]


pred_matrix <- quickpred(hh_clean, 
                         exclude = to_impute, 
                         include = co_vars)


imputed_data <- mice(
  to_impute,
  m = 1,
  method = 'pmm',
  seed = 123
)

imputed_data <- complete(imputed_data, 1)

describe(imputed_data)

# Step 1: Select only the columns you want to transfer from the imputed data
imputed_columns <- imputed_data[, c('HH_ID', 'disposable_inc', 'living_area_sqm', 'q_rooms', 'kok_type', 'cons_weight')]

# Step 2: Merge the imputed data with the original dataset (hh_clean_no_expand) by HH_ID
hh_clean_hat <- merge(hh_clean, imputed_data, by = 'HH_ID')

options(max.print = 10000)
describe(hh_clean_hat)


# Get the names of columns with the ".x" suffix
colnames_with_x <- grep("\\.x$", colnames(hh_clean_hat), value = TRUE)

# Remove the ".x" suffix from the column names
colnames(hh_clean_hat)[colnames(hh_clean_hat) %in% colnames_with_x] <- 
  gsub("\\.x$", "", colnames_with_x)

# Check the updated column names
colnames(hh_clean_hat)





# Step 3: Replace NA values in the original dataset with the imputed values for the selected columns
hh_clean_hat$disposable_inc <- ifelse(is.na(hh_clean_hat$disposable_inc), 
                                            hh_clean_hat$disposable_inc.y, 
                                            hh_clean_hat$disposable_inc)

hh_clean_hat$living_area_sqm <- ifelse(is.na(hh_clean_hat$living_area_sqm), 
                                       hh_clean_hat$living_area_sqm.y, 
                                       hh_clean_hat$living_area_sqm)

hh_clean_hat$q_rooms <- ifelse(is.na(hh_clean_hat$q_rooms), 
                               hh_clean_hat$q_rooms.y, 
                               hh_clean_hat$q_rooms)

hh_clean_hat$kok_type <- ifelse(is.na(hh_clean_hat$kok_type), 
                                hh_clean_hat$kok_type.y, 
                                hh_clean_hat$kok_type)

hh_clean_hat$cons_weight <- ifelse(is.na(hh_clean_hat$cons_weight), 
                                   hh_clean_hat$cons_weight.y, 
                                   hh_clean_hat$cons_weight)





# Step 4: Drop the extra columns (".y") created during the merge
hh_clean_hat <- hh_clean_hat[, !grepl("\\.y$", colnames(hh_clean_hat))]



describe(hh_clean_hat)



hh_clean_hat$b_cloth_n_foot
hh_clean_hat$CE_repair_furniture_dummy  
hh_clean_hat$CE_repair_tool             




############################################################################################################################


hh_clean <- hh_clean_hat

rm(hh_clean_hat, imputed_data, pred_matrix,to_impute)



#####################################################################################################################
hh_clean$survey_part <- as.factor(hh_clean$survey_part)
unique(hh_clean$survey_part)







############################################################################

describe(hh_clean)

hh_clean <- hh_clean %>%
  mutate(d_income_decile = ntile(disposable_inc, 10))


hh_clean <- hh_clean %>%
  mutate(expenditure_decile = ntile(total_expenditure, 10))



hh_clean <- hh_clean %>%
  mutate(d_income_decile5 = ntile(disposable_inc, 5))


hh_clean <- hh_clean %>%
  mutate(expenditure_decile5 = ntile(total_expenditure, 5))









describe(hh_clean)

hh_clean_BUT <- hh_clean %>%  filter(survey_part == "BUT")
hh_clean_DUT <- hh_clean %>%  filter(survey_part == "DUT")
hh_clean_LUT <- hh_clean %>%  filter(survey_part == "LUT")

describe(hh_clean_BUT)
describe(hh_clean_DUT)
describe(hh_clean_LUT)

hh_clean_BUT <- hh_clean_BUT[, colSums(is.na(hh_clean_BUT)) == 0]
hh_clean_DUT <- hh_clean_DUT[, colSums(is.na(hh_clean_DUT)) == 0]
hh_clean_LUT <- hh_clean_LUT[, colSums(is.na(hh_clean_LUT)) == 0]




##############################################################################







