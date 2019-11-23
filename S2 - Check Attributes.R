# check attributes of new SKUs
# - grammar of brands
# - grammar of companies
# - pair brand-company
# - grammar of segments
# - correctness of the segments
# - scent: grammar, sorting, extra spaces


## DICTIONARIES
dictPS.IMF = c(
  "Allergy Treatment",
  "Anti Reflux",
  "BFO",
  "BIF",
  "BPFO",
  "BPIF",
  "Digestive Comfort",
  "DR-NL",
  "Goat",
  "Gum Base",
  "Gum Plus",
  "Hypoallergenic",
  "Preterm",
  "Soy",
  "Soy / Goat"
)

dictPS.Foods = c(
  "CB-C",
  "Fruit",
  "Fruit Plus",
  "IMC",
  "IPC",
  "Juice",
  "Kefir",
  "Meal Components",
  "Meat Meal",
  "Non Dairy",
  "Other Dairy",
  "RTE Cereals",
  "Soups",
  "Tea",
  "Veggie Meal",
  "Water",
  "Yoghurt"
)

dictPS = c(dictPS.IMF, dictPS.Foods)

dictPS3.IMF = c("Base", "Plus", "Specials")
dictPS3.Foods = c("Cereal Biscuits",
                  "Dairy/desserts",
                  "Drinks",
                  "Fruits",
                  "Instant Cereals",
                  "RTE Cereals",
                  "Savoury Meal")

dictPS.DryFood = c("CB-C",
                   "IMC",
                   "IPC",
                   "RTE Cereals")

dictPS.WetFood = c("Fruit",
                   "Fruit Plus",
                   "Juice",
                   "Kefir",
                   "Meal Components",
                   "Meat Meal",
                   "Non Dairy",
                   "Other Dairy",
                   "Soups",
                   "Tea",
                   "Veggie Meal",
                   "Water",
                   "Yoghurt")

dictPS0 = c("AMN", "IMF", "Foods")

dictPS2 = c("IF", "FO", "Gum", "Dry Food", "Wet Food")

dictPS3 = c("AMN", dictPS3.IMF, dictPS3.Foods)

dictPackage = c("Can",
                "Carton",
                "Foiled box",
                "Glass",
                "Plastic",
                "Pouch",
                "Soft")

dictForm = c("Liquid", "Not Applicable", "Powder", "Pure", "Solid")








df = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201909/New SKUs - SEP19 - Sheet1.csv",
           check.names = TRUE)

df1 = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected.csv")
df1 = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected2.csv")




check.attributes(df)
