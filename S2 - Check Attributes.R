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

df1 = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201907/df.corrected.csv")

list1 = df[, unique(Brand)][!(df[, unique(Brand) %in% df1[, Brand]])]
attributes = c("Brand", "SubBrand", 
               "Size", "Age",
               "Scent", "Company", 
               "PS0", "PS3", "PS2", "PS",
               "Form", "Package")

print("Grammar...")

  for (i in attributes) {
    
    if (i == "PS") {
      dictTemp = c("Allergy Treatment", "AMN", "DR-NL",
                 "Anti Reflux", "BFO", "Hypoallergenic",
                 "BIF", "BPFO", "BPIF", "CB-C",
                 "Fruit", "Digestive Comfort","IMC",
                 "Fruit Plus",  "Goat", "Gum Base",
                 "Preterm", "RTE Cereals", "IPC",
                 "Meat Meal", "Veggie Meal", "Non Dairy",
                 "Other Dairy", "Soy", "Soups", "Juice",
                 "Tea", "Water", "Yoghurt", "Kefir",
                 "Gum Plus", "Meal Components")
      
      if (all(df[, unique(PS)] %in% dictTemp)) {
        print("PS: OK")
        
      } else {
        list1 = df[, unique(PS)][!(df[, unique(PS)] %in% dictTemp)]
        
        print(paste0("Suspicious segments:"))
        print(list1)
        cat("\n")
        
        print("SKUs:")
        print(df[(PS %in% list1), PS, by = SKU])
        
      }
      
    }
    
    if (i == "Form") {
      dictTemp = c()
      
      if (all(df[, unique(PS)] %in% dictTemp)) {
        print("PS: OK")
        
      } else {
        list1 = df[, unique(PS)][!(df[, unique(PS)] %in% dictTemp)]
        
        print(paste0("Suspicious segments:"))
        print(list1)
        cat("\n")
        
        print("SKUs:")
        print(df[(PS %in% list1), PS, by = SKU])
        
      }
      
    }
    
    if (i == "Package") {
      dictTemp = c("Allergy Treatment", "AMN", "DR-NL",
                   "Anti Reflux", "BFO", "Hypoallergenic",
                   "BIF", "BPFO", "BPIF", "CB-C",
                   "Fruit", "Digestive Comfort","IMC",
                   "Fruit Plus",  "Goat", "Gum Base",
                   "Preterm", "RTE Cereals", "IPC",
                   "Meat Meal", "Veggie Meal", "Non Dairy",
                   "Other Dairy", "Soy", "Soups", "Juice",
                   "Tea", "Water", "Yoghurt", "Kefir",
                   "Gum Plus", "Meal Components")
      
      if (all(df[, unique(PS)] %in% dictTemp)) {
        print("PS: OK")
        
      } else {
        list1 = df[, unique(PS)][!(df[, unique(PS)] %in% dictTemp)]
        
        print(paste0("Suspicious segments:"))
        print(list1)
        cat("\n")
        
        print("SKUs:")
        print(df[(PS %in% list1), PS, by = SKU])
        
      }
      
    }
    
    
    # Form, Package
    # Size - separate function
    # Age - ?
    # Scent
    
    if (i %in% c("Brand", "SubBrand", "Company", "Age"))
    
    list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% df1[, get(i)]])]
    
    if (length(list1) > 0) {
      print(paste0("New ", i, "s:"))
      print(list1)
      cat("\n")
      print("New SKUs:")
      print(df[get(i) %in% list1, get(i), by = SKU])
      
    } else {
      print(paste0(i, ": OK"))
      
    }
  }



df[, unique(Brand)][!df[, unique(Brand) %in% df1[,unique(Brand)]]]
