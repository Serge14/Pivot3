# check attributes of new SKUs
# - grammar of brands
# - grammar of companies
# - pair brand-company
# - grammar of segments
# - correctness of the segments
# - scent: grammar, sorting, extra spaces

addPS2 = function(df){
  
  #IF from 0 to 6, but actually from less than 6 months
  df[PS0 == "IMF" & as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) < 6 & 
       !stri_detect_fixed(Age, "Y"), 
     PS2.Temp := "IF"]
  
  # FO (6-12) starts from 6 to less than 12
  df[PS0 == "IMF" & 
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) >= 6 &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) < 12 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "FO"]
  
  # GUM 12+
  df[PS0 == "IMF" & as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) >= 12 &
       !stri_detect_fixed(Age, "Y"), 
     PS2.Temp := "Gum"]
  
  df[PS0 == "IMF" & stri_detect_fixed(Age, "Y"), 
     PS2.Temp := "Gum"]
  
  # Others are not stated
  df[PS0 == "IMF" & is.na(PS2), PS2.Temp := "N/S"]
  
  # Dry Food
  DryFoodSegments = c("Instant Cereals",
                      "Cereal Biscuits",
                      "Ready To Eat Cereals",
                      "Liquid Cereals")
  
  df[PS3 %in% DryFoodSegments, PS2.Temp := "Dry Food"]
  
  # Wet Food
  WetFoodSegments = c("Fruits",
                      "Drinks",
                      "Savoury Meal",
                      "Dairy/desserts")
  
  df[PS3 %in% WetFoodSegments, PS2.Temp := "Wet Food"]
  
  # AMN
  df[PS3 == "AMN", PS2.Temp := "AMN"]
  
  # All others
  df[is.na(PS2), PS2 := ""]
  
}

check.UOM.db = function(df) {
  
  names(df)[1] = "SKU"
  # Check the style
  
  if (df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
         Brand != "Private label", .N] > 0){
  print("Strange Size style:")
  print(df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
             Brand != "Private label",
           .(Size = unique(Size)),
           by = SKU])
  
  } else {print("Style size: OK")}
  
  cat("\n")
  print("Strange Size numbers:")
  dt = df[Brand != "Private label", .(Size = unique(Size)), by = SKU]
  dt[, Size.Value := stri_extract_all_regex(Size, "[0-9]*\\.?[0-9]+(?!\\*|x|X)")]
  dt[, Size.UOM := stri_extract_last_regex(Size, "[a-zA-Z]+")]
  
  if (dt[(Size.UOM == "Gr" & (Size.Value < 10 | Size.Value > 1000)) |
       (Size.UOM == "Kg" & (Size.Value < 0.1 | Size.Value > 1)) |
       (Size.UOM == "Ml" & (Size.Value < 50 | Size.Value > 1000)) |
       (Size.UOM == "Lt" & (Size.Value < 0.1 | Size.Value > 5)), .N] > 0) {
  
  print(dt[(Size.UOM == "Gr" & (Size.Value < 10 | Size.Value > 1000)) |
             (Size.UOM == "Kg" & (Size.Value < 0.1 | Size.Value > 1)) |
             (Size.UOM == "Ml" & (Size.Value < 50 | Size.Value > 1000)) |
             (Size.UOM == "Lt" & (Size.Value < 0.1 | Size.Value > 5)),
           Size, by = SKU])
  
  } else {print("Size numbers: OK")}
  
  rm(dt)
  
}

correct.UOM = function(text.string) {
  text.string.characters = stri_extract_last_regex(text.string, "[a-zA-Z]+")
  
  if (stri_detect_regex(text.string.characters, "^[gG]") == TRUE) {
    text.string = stri_c(stri_extract_last_regex(text.string, "[0-9]+"), "Gr")
    
  }
  
  return(text.string)
  
}


df = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201908/New SKUs - AUG19.csv",
           check.names = TRUE)

df1 = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected.csv")

check.attributes = function(df){

attributes = c("Brand",
               "SubBrand",
               "Age",
               "Scent",
               "Company",
               "Form",
               "Package")

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

dictPackage = c("Can",
                "Carton",
                "Foiled box",
                "Glass",
                "Plastic",
                "Pouch",
                "Soft")

dictForm = c("Liquid", "Not Applicable", "Powder", "Pure", "Solid")

# Pre-processing of values
# Form to title
df[ , (attributes) := lapply(.SD, function(x){stri_replace_all_regex(x, "\\s+", " ")}), 
    .SDcols = attributes]

df[ , (attributes) := lapply(.SD, function(x){stri_trans_totitle(x)}), 
    .SDcols = attributes]

print("Grammar...")

for (i in attributes) {
  # Size - separate function
  # Age - ?
  # Scent
  
  if (i %in% c("Brand", "SubBrand", "Company", "Age")) {
    list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% df1[, get(i)]])]
    
  } else if (i == "PS") {
    list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPS])]
    
  } else if (i == "Form") {
    list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictForm])]
    
  } else if (i == "Package") {
    list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPackage])]
  }
  
  if (length(list1) > 0) {
    print(paste0("Suspicious ", i, "s:"))
    print(list1)
    cat("\n")
    print("SKUs:")
    print(df[get(i) %in% list1, get(i), by = SKU])
    
  } else {
    print(paste0("Grammar of ", i, ": OK"))
    cat("\n")
    
  }
}

# Company-Brand
print("Pair Company-Brand")
list1 = df[, unique(paste0(Brand, "-", Company))][
  !(df[, unique(paste0(Brand, "-", Company))] %in% 
      df1[, unique(paste0(Brand, "-", Company))])]

if (length(list1) > 0) {
  print(paste0("Suspicious Brand-Company piar(s)"))
  print(list1)
  cat("\n")
  print("SKUs:")
  print(df[paste0(Brand, "-", Company) %in% list1, 
           .(Brand, Company), by = SKU])
  
} else {
  print(paste0("Brand-Company: OK"))
  cat("\n")
  
}

# Scent: grammar, sorting, extra spaces
df[, Scent := stri_replace_all_regex(Scent, "\\s*-\\s*", "-")]

list1 = df[, unique(unlist(stri_split_fixed(Scent, "-")))]
dictScent = df1[, unique(unlist(stri_split_fixed(Scent, "-")))]

list1 = list1[!(list1 %in% dictScent)]

if (length(list1) > 0) {
  print(paste0("Suspicious Scent(s)"))
  print(list1)
  cat("\n")
  print("SKUs:")
  print(df[stri_detect_regex(Scent, paste(list1, collapse = "|")),
           .(Brand, Scent), by = SKU])
  
} else {
  print(paste0("Scent: OK"))
  cat("\n")
  
}

df[, Scent := sapply(stri_split_fixed(Scent, "-"), function(x) 
  paste(sort(x), collapse = "-"))]

# Check correct segments

print("Checking correct segments PS0, PS2, PS3...")

# PS0
if(df[(PS == "AMN" & PS0 != "AMN") |
      (PS %in% dictPS.IMF
      & PS0 != "IMF") |
  (PS %in% dictPS.Foods & PS0 != "Foods"), .N] > 0) {
  print("Suspicious PS0 or PS:")
  print(df[(PS == "AMN" & PS0 != "AMN") |
             (PS %in% dictPS.IMF
           & PS0 != "IMF") |
             (PS %in% dictPS.Foods & PS0 != "Foods"),
           SKU, by = .(PS0, PS)])
  
} else {print("PS0: OK")}

cat("\n")

# PS2
df = addPS2(df)
if (df[PS2 != PS2.Temp, .N] > 0) {
  print("Suspicious PS2 or PS:")
  print(df[PS2 != PS2.Temp, SKU, by = .(PS2, PS2.Temp, PS)])
  
} else {print("PS2: OK")}


# PS3
if(df[(PS == "AMN" & PS3 != "AMN") |
      (PS %in% dictPS3.IMF
       & PS0 != "IMF") |
      (PS %in% dictPS3.Foods & PS0 != "Foods"), .N] > 0) {
  print("Suspicious PS3 or PS:")
  print(df[(PS == "AMN" & PS3 != "AMN") |
             (PS %in% dictPS3.IMF
              & PS0 != "IMF") |
             (PS %in% dictPS3.Foods & PS0 != "Foods"),
           SKU, by = .(PS3, PS)])
  
} else {print("PS3: OK")}

# Size
check.UOM.db(df)

# Clean
df[, PS2.Temp := NULL]

}








df1[ , Scent := lapply(.SD, function(x){stri_replace_all_regex(x, "\\s+", " ")}), 
    .SDcols = "Scent"]

df1[ , Scent := lapply(.SD, function(x){stri_trans_totitle(x)}), 
    .SDcols = "Scent"]

df1[ , Scent := lapply(.SD, function(x){stri_trim_both(x)}), 
    .SDcols = "Scent"]

fwrite(df1, "/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected2.csv")
