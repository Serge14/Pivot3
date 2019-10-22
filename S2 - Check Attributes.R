# check attributes of new SKUs
# - grammar of brands
# - grammar of companies
# - pair brand-company
# - grammar of segments
# - correctness of the segments
# - scent: grammar, sorting, extra spaces


check.UOM.db = function(df) {
  
  names(df)[1] = "SKU"
  # Check the style
  print("Strange Size style:")
  print(df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
             Brand != "Private label",
           .(Size = unique(Size)),
           by = SKU])
  
  cat("\n")
  print("Strange Size numbers:")
  dt = df[Brand != "Private label", .(Size = unique(Size)), by = SKU]
  dt[, Size.Value := stri_extract_all_regex(Size, "[0-9]*\\.?[0-9]+(?!\\*|x|X)")]
  dt[, Size.UOM := stri_extract_last_regex(Size, "[a-zA-Z]+")]
  
  print(dt[(Size.UOM == "Gr" & (Size.Value < 10 | Size.Value > 1000)) |
             (Size.UOM == "Kg" & (Size.Value < 0.1 | Size.Value > 1)) |
             (Size.UOM == "Ml" & (Size.Value < 50 | Size.Value > 1000)) |
             (Size.UOM == "Lt" & (Size.Value < 0.1 | Size.Value > 5)),
           Size, by = SKU])
  
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


attributes = c("Brand",
               "SubBrand",
               "Size",
               "Age",
               "Scent",
               "Company",
               "PS",
               "Form",
               "Package")

dictPS = c(
  "Allergy Treatment",
  "AMN",
  "DR-NL",
  "Anti Reflux",
  "BFO",
  "Hypoallergenic",
  "BIF",
  "BPFO",
  "BPIF",
  "CB-C",
  "Fruit",
  "Digestive Comfort",
  "IMC",
  "Fruit Plus",
  "Goat",
  "Gum Base",
  "Preterm",
  "RTE Cereals",
  "IPC",
  "Meat Meal",
  "Veggie Meal",
  "Non Dairy",
  "Other Dairy",
  "Soy",
  "Soups",
  "Juice",
  "Tea",
  "Water",
  "Yoghurt",
  "Kefir",
  "Gum Plus",
  "Meal Components"
)

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
list1 = df[, unique(unlist(stri_split_fixed(Scent, "-")))]
dictScent = df1[, unique(unlist(stri_split_fixed(Scent, "-")))]

list1 = !(list1 %in% dictScent)

if (length(list1) > 0) {
  print(paste0("Suspicious Scent(s)"))
  print(list1)
  cat("\n")
  print("SKUs:")
  print(df[list1, ##### THINK!!!!
           .(Brand, Company), by = SKU])
  
} else {
  print(paste0("Brand-Company: OK"))
  cat("\n")
  
}


















df1[ , Scent := lapply(.SD, function(x){stri_replace_all_regex(x, "\\s+", " ")}), 
    .SDcols = "Scent"]

df1[ , Scent := lapply(.SD, function(x){stri_trans_totitle(x)}), 
    .SDcols = "Scent"]

df1[ , Scent := lapply(.SD, function(x){stri_trim_both(x)}), 
    .SDcols = "Scent"]

fwrite(df1, "/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected2.csv")
