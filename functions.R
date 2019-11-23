

### ACIDIFIED

add.acidified = function(df) {
  df[PS == "Digestive Comfort", Acidified := "Non-Acidified"]
  df[PS == "Digestive Comfort" &
       (
         stri_detect_fixed(tolower(SKU), "bifid", ignore.case = TRUE) |
           stri_detect_fixed(tolower(SKU), "kisl", ignore.case = TRUE)
       ),
     Acidified := "Acidified"]
  
  return(df)
  
}


addAge2 = function(df) {
  # Remove spaces
  df[, SKU.Temp := stri_replace_all_fixed(SKU, " ", "")]
  df[, SKU.Temp := stri_replace_all_fixed(SKU.Temp, "ML", "XX")]
  df[, SKU.Temp := stri_sub(SKU.Temp, -12)]
  
  #d-d
  df[, Age2 := stri_extract_last_regex(SKU.Temp, "[0-9]{1,2}-[0-9]{1,2}$")]
  
  #d[MES]-d[MES]
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp,
                                     "[0-9]{1,2}[M(?=ES).]*-[0-9]{1,2}[M(?=ES).]*$")]
  
  #Years
  # df[is.na(Age2),
  #    Age2 := str_extract(SKU.Temp, "[0-9]{1,2}[M(?=ES)\\.]*[GL]*-[1-9]{1,2}[M(?=ES).]*[GL]*$")][]
  
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp,
                                     "[0-9]{1,2}[M(?=ES)\\.GL]*-[1-9]{1,2}[GL]*$")]
  
  #SdDN
  # df[is.na(Age2),
  #    Age2 := str_extract(SKU.Temp, "[S]{0,1}[0-9]{1,2}[D(?=N).]*$")]
  
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp, "[S]{0,1}[0-9]{1,2}DN?\\.?$")]
  
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp, "[S]{0,1}[0-9]{1,2}DN?\\.?")]
  
  #SdMES
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp, "[S]{0,1}[0-9]{1,2}\\.*[0-9]*[M(?=ES)]*\\.?$")]
  
  #SdM in the end
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp, "[S]{0,1}[0-9]{1,2}[M]")]
  
  #SdY in the end
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp, "[S]{0,1}[1-9]{1}[GL]")]
  
  #d in the end
  df[is.na(Age2),
     Age2 := stri_extract_last_regex(SKU.Temp, "(?<=/)[1-9]{1,2}[(?=OLD)]*$")]
  
  
  # Delete dots, but not in the digits
  df[, Age2 := stri_replace_all_regex(Age2, "((?<![0-9])\\.)|(\\.(?![0-9]))", "")]
  
  # Delete unnecessary symbols
  df[, Age2 := stri_replace_all_regex(Age2, "[/MESN]", "")]
  df[, Age2 := stri_replace_all_regex(Age2, "=OLD", "")]
  
  ### Transform Ages to months
  
  # Assign NA to Age
  df[, Age := NA]
  df$Age = as.character(df$Age)
  
  # Only month is mentioned
  df[stri_detect_regex(Age2, "^[0-9]+\\.*[0-9]*[M]?$"),
     Age := paste0(stri_extract_first_regex(Age2, "^[0-9]+\\.*[0-9]*"), "+")]
  
  # From the 1st day
  df[stri_detect_regex(Age2, "^[0-9]{1}[D]$"), Age := "0+"]
  
  # From N years
  df[stri_detect_regex(Age2, "^[0-9]{1}[GL]$"),
     Age := paste0(as.numeric(str_extract(Age2, "^[0-9]+")) * 12, "+")]
  
  # Months & hyphen, letter M is optionally
  df[stri_detect_regex(Age2, "^[0-9]+[M]*-[0-9]*[M]*$"), Age := Age2]
  
  # Range with years
  df[stri_detect_regex(Age2, "-") &
       is.na(Age), Age := mapply(convertYtoM, Age2)]
  
  # 1-3L
  df[stri_detect_regex(Age2, "1-3L?$"), Age := "12-36"]
  
  # Assign NA to the rest
  df[is.na(Age), Age := "NA"]
  
  # Remove unnecessary columns
  df[, Age2 := NULL]
  df[, SKU.Temp := NULL]
  
  # Add exclusions
  # df1 = fread("dictAgeExceptions.csv")
  # SKUAgeExceptions = df1[, SKU]
  # df[SKU %in% SKUAgeExceptions][df1, on = c(SKU = "SKU"), Age := i.Age]
  df[dictAgeExceptions, on = "SKU", Age := i.Age]
}

addDates = function(df) {
  # Dates
  dictMonth = data.table(
    Month = c(
      "JAN",
      "FEB",
      "MAR",
      "APR",
      "MAY",
      "JUN",
      "JUL",
      "AUG",
      "SEP",
      "OCT",
      "NOV",
      "DEC"
    ),
    Mnb = 1:12
  )
  
  df[, Ynb := stri_extract_first_regex(Period, "[0-9]+")]
  df[, Mnb.temp := stri_extract_first_regex(Period, "[A-Za-z]+")]
  
  df[dictMonth, on = .(Mnb.temp = Month), Mnb := i.Mnb]
  df[, Mnb.temp := NULL]
  
}

add_EC_AC = function(df) {
  # Extrapolation & correction
  
  # Extrapolation coefficients
  df[dictEC, on = c("Channel", "PS3"), EC := i.EC]
  df[PS == "Digestive Comfort" & Channel == "MT", EC := 1.1]
  df[PS == "Digestive Comfort" & Channel == "PHARMA", EC := 1.1]
  df[PS == "Hypoallergenic" & Channel == "MT", EC := 1.1]
  df[PS == "Hypoallergenic" & Channel == "PHARMA", EC := 1.1]
  df[is.na(EC), .N]
  
  # Additional corrections
  
  df[, AC := 1]
  
  # Old problem, Pharma * 1.3
  df[Ynb == 2016 & Mnb %in% c(3:9) &
       Channel == "PHARMA" &
       PS3 != "Specials",
     AC := 1.3]
  
  df[Ynb == 2017 & Mnb %in% c(7:12) & Channel == "PHARMA" &
       PS == "Digestive Comfort",
     AC := 1.3] # was 1.2
  
  df[Ynb == 2018 & Mnb %in% c(1:6) & Channel == "PHARMA" &
       PS == "Digestive Comfort",
     AC := 1.3] # was 1.2
  
  df[Ynb == 2019 & Mnb %in% c(1:2) & Channel == "PHARMA" &
       PS == "Digestive Comfort",
     AC := 0.89]
  
  df[Ynb == 2017 & Mnb %in% c(11:12) &
       PS == "Hypoallergenic",
     AC := 1.2]
  
  df[Ynb == 2018 & Mnb %in% c(1:9) &
       PS == "Hypoallergenic",
     AC := 1.2]
  
  df[Ynb >= 2019 &
       PS == "Hypoallergenic",
     AC := 0.95]
  
  df[Ynb == 2017 & Channel == "PHARMA" &
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic",
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(1, 2, 3, 5) & Channel == "PHARMA" &
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic",
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(1, 2, 3, 5) &
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic",
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(10, 11, 12) &
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic",
     AC := 0.95]
  
  df[Ynb >= 2019 &
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic",
     AC := 0.95]
  
  df[Ynb == 2018 & Mnb %in% c(3, 5, 6) & Channel == "PHARMA" &
       (PS3 == "Base" | PS3 == "Plus"),
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb == 12 & Channel == "PHARMA" &
       (PS3 == "Base" | PS3 == "Plus"),
     AC := 0.985]
  
  df[Ynb >= 2019 & Channel == "PHARMA" &
       (PS3 == "Base" | PS3 == "Plus"),
     AC := 0.985]
  
  df[Ynb == 2019 & Mnb >= 5 & Channel == "PHARMA" &
       (PS3 == "Fruits" | PS3 == "Savoury Meal"),
     AC := 1.2]
  
  # Pharma lost 300 & 600 Gr SKUs, had to increase sales of old 400 & 800 Gr
  df[Ynb == 2019 & Mnb == 5 & Channel == "PHARMA" &
       (
         SKU == "Nutrilon_Komfort 1_400Gr_0-6_N/S_IMF_IF_Specials_Digestive Comfort_Nutricia" |
           SKU == "Nutrilon_Komfort 1_800Gr_0-6_N/S_IMF_IF_Specials_Digestive Comfort_Nutricia" |
           SKU == "Nutrilon_Komfort 2_400Gr_6-12_N/S_IMF_FO_Specials_Digestive Comfort_Nutricia"
       ),
     AC := 1.4]
  
}


addForm = function(df) {
  df[stri_detect_fixed(SKU, "PYURE"), PRODUCT.FORM := "Pure"]
  df[, Form := stri_trans_totitle(PRODUCT.FORM)]
  # df[, PRODUCT.FORM := NULL]
  df[Form == "Not Applicable", Form := NA]
  
}

# Add package

add.package = function(df) {
  if (!("SKU2" %in% names(df))) {
    df[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
  }
  
  df[(PS3 == "Fruits" |
        PS3 == "Savoury Meal") & (is.na(Package) | Package == ""),
     .N,
     by = .(Package)]
  
  pouch = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201906/Pouch.csv")
  pouch[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
  
  df[(PS3 == "Fruits" |
        PS3 == "Savoury Meal") & (is.na(Package) | Package == ""),
     Package := pouch[.SD, Package, on = "SKU2"]]
  df[(PS3 == "Fruits" |
        PS3 == "Savoury Meal") & (is.na(Package) | Package == ""),
     Package := "Glass"]
  
  df[, SKU2 := NULL]
  
  return(df)
  
}



add.price.segments = function(df) {
  # Price Segments local
  df[.(PS0 = "IMF", dictPriceSegments[Segment == "IMF"]),
     on = c(Brand = "Brand", PS0 = "Segment"),
     PriceSegment := i.PriceSegment]
  
  df[.(PS2 = "Dry Food", dictPriceSegments[Segment == "Dry Food"]),
     on = c(Brand = "Brand", PS2 = "Segment"),
     PriceSegment := i.PriceSegment]
  
  dictPriceSegments[Segment == "Puree", Segment := "Fruits"]
  df[.(PS3 = "Fruits", dictPriceSegments[Segment == "Fruits"]),
     on = c(Brand = "Brand", PS3 = "Segment"),
     PriceSegment := i.PriceSegment]
  
  dictPriceSegments[Segment == "Fruits", Segment := "Savoury Meal"]
  df[.(PS3 = "Savoury Meal", dictPriceSegments[Segment == "Savoury Meal"]),
     on = c(Brand = "Brand", PS3 = "Segment"),
     PriceSegment := i.PriceSegment]
  
  # Price Segments global
  df[.(PS0 = "IMF", dictPriceSegments[Segment == "IMF"]),
     on = c(Brand = "Brand", PS0 = "Segment"),
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  df[.(PS2 = "Dry Food", dictPriceSegments[Segment == "Dry Food"]),
     on = c(Brand = "Brand", PS2 = "Segment"),
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  # dictPriceSegments[Segment == "Fruits", Segment := "Savoury Meal"]
  df[.(PS3 = "Savoury Meal", dictPriceSegments[Segment == "Savoury Meal"]),
     on = c(Brand = "Brand", PS3 = "Segment"),
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  dictPriceSegments[Segment == "Savoury Meal", Segment := "Fruits"]
  df[.(PS3 = "Fruits", dictPriceSegments[Segment == "Fruits"]),
     on = c(Brand = "Brand", PS3 = "Segment"),
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  # print("Price Segments (global, local):")
  # print(df[is.na(PriceSegment), .N])
  # print(df[is.na(GlobalPriceSegment), .N])
  
}

addPS2 = function(df) {
  #IF from 0 to 6, but actually from less than 6 months
  df[PS0 == "IMF" &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) < 6 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "IF"]
  
  # FO (6-12) starts from 6 to less than 12
  df[PS0 == "IMF" &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) >= 6 &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) < 12 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "FO"]
  
  # GUM 12+
  df[PS0 == "IMF" &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) >= 12 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "Gum"]
  
  df[PS0 == "IMF" & stri_detect_fixed(Age, "Y"),
     PS2.Temp := "Gum"]
  
  # Others are not stated
  df[PS0 == "IMF" & is.na(PS2.Temp), PS2.Temp := "N/S"]
  
  # Dry Food
  df[PS %in% dictPS.DryFood, PS2.Temp := "Dry Food"]
  
  # Wet Food
  df[PS %in% dictPS.WetFood, PS2.Temp := "Wet Food"]
  
  # AMN
  df[PS == "AMN", PS2.Temp := "AMN"]
  
  # All others
  df[is.na(PS2.Temp), PS2.Temp := ""]
  
}

# Add Scent2 & Type
### Scent2 for PHARMA

add.scent.pharma = function(df) {
  dictScent = fread("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/dictScent.csv")
  dictScent = dictScent[PS3 == "Instant Cereals"]
  
  # Puree & Cereals
  # df[dictScent, on = c("Scent", "PS3"),
  #    `:=`(Scent2 = i.Scent2, ScentType = i.Type)]
  
  df[PS3 == "Instant Cereals" & Channel == "PHARMA",
     c("Scent2", "ScentType") := dictScent[.SD, .(Scent2, Type), on = "Scent"]]
  
  if (df[(PS3 == "Instant Cereals" &
          (is.na(Scent2) | Scent2 == "")) &
         Brand != "Private label" &
         Channel == "PHARMA", .N] > 0) {
    print("Scent dictionary needs to be updated by scents for these SKUs:")
    
    print(df[PS3 == "Instant Cereals" &
               (is.na(Scent2) | Scent2 == ""),
             Scent,
             by = SKU])
  }
  
  return(df)
  
}



addSize = function(df) {
  # Size
  df[, Size := stri_extract_first_regex(SKU, "[0-9]+[GML]+")]
  df[, Size := stri_replace_first_fixed(Size, "G", "Gr")]
  df[, Size := stri_replace_first_fixed(Size, "ML", "Ml")]
  
}

check.attributes = function(df) {
  attributes = c("Brand",
                 "SubBrand",
                 "Age",
                 "Scent",
                 "Company",
                 "Form",
                 "Package")
  
  # Pre-processing of values
  # Form to title
  df[, (attributes) := lapply(.SD, function(x) {
    stri_replace_all_regex(x, "\\s+", " ")
  }),
  .SDcols = attributes]
  
  df[, (attributes) := lapply(.SD, function(x) {
    stri_trans_totitle(x)
  }),
  .SDcols = attributes]
  
  # print("Grammar...")
  
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
      cat("\n")
      print(paste0("Suspicious ", i, "s:"))
      cat("\n")
      print(list1)
      cat("\n")
      print("SKUs:")
      print(df[get(i) %in% list1, get(i), by = SKU])
      cat("\n")
      
    } else {
      print(paste0("Grammar of ", i, ": OK"))
      
    }
  }
  
  # Company-Brand
  # print("Pair Company-Brand")
  list1 = df[, unique(paste0(Brand, "-", Company))][!(df[, unique(paste0(Brand, "-", Company))] %in%
                                                        df1[, unique(paste0(Brand, "-", Company))])]
  
  if (length(list1) > 0) {
    cat("\n")
    print(paste0("Suspicious Brand-Company piar(s):"))
    cat("\n")
    print(list1)
    cat("\n")
    print("SKUs:")
    print(df[paste0(Brand, "-", Company) %in% list1,
             .(Brand, Company), by = SKU])
    cat("\n")
    
  } else {
    print(paste0("Brand-Company: OK"))
    
  }
  
  # Scent: grammar, sorting, extra spaces
  df[, Scent := stri_replace_all_regex(Scent, "\\s*-\\s*", "-")]
  
  list1 = df[, unique(unlist(stri_split_fixed(Scent, "-")))]
  dictScent = df1[, unique(unlist(stri_split_fixed(Scent, "-")))]
  
  list1 = list1[!(list1 %in% dictScent)]
  
  if (length(list1) > 0) {
    cat("\n")
    print(paste0("Suspicious Scent(s):"))
    cat("\n")
    print(list1)
    cat("\n")
    print("SKUs:")
    print(df[stri_detect_regex(Scent, paste(list1, collapse = "|")),
             .(Brand, Scent), by = SKU])
    cat("\n")
    
  } else {
    print(paste0("Scent: OK"))
    
  }
  
  df[, Scent := sapply(stri_split_fixed(Scent, "-"), function(x)
    paste(sort(x), collapse = "-"))]
  
  # Check correct segments
  
  # print("Checking correct segments PS0, PS2, PS3...")
  
  # PS0
  if (df[(PS == "AMN" & PS0 != "AMN") |
         (PS %in% dictPS.IMF
          & PS0 != "IMF") |
         (PS %in% dictPS.Foods & PS0 != "Foods") |
         (!(PS0 %in% dictPS0)), .N] > 0) {
    cat("\n")
    print("Suspicious PS0 or PS:")
    cat("\n")
    print(df[(PS == "AMN" & PS0 != "AMN") |
               (PS %in% dictPS.IMF
                & PS0 != "IMF") |
               (PS %in% dictPS.Foods & PS0 != "Foods") |
               (!(PS0 %in% dictPS0)),
             SKU, by = .(PS0, PS)])
    cat("\n")
    
  } else {
    print("PS0: OK")
  }
  
  # cat("\n")
  
  # PS2
  df = addPS2(df)
  if (df[(PS2 != PS2.Temp) |
         (!(PS2 %in% dictPS2)), .N] > 0) {
    cat("\n")
    print("Suspicious PS2 or PS:")
    cat("\n")
    print(df[(PS2 != PS2.Temp) |
               (!(PS2 %in% dictPS2)),
             SKU,
             by = .(PS2, PS2.Temp, PS)])
    cat("\n")
    
  } else {
    print("PS2: OK")
  }
  
  # PS3
  if (df[(PS == "AMN" & PS3 != "AMN") |
         (PS %in% dictPS3.IMF
          & PS0 != "IMF") |
         (PS %in% dictPS3.Foods & PS0 != "Foods")  |
         (!(PS3 %in% dictPS3)), .N] > 0) {
    cat("\n")
    print("Suspicious PS3 or PS:")
    cat("\n")
    print(df[(PS == "AMN" & PS3 != "AMN") |
               (PS %in% dictPS3.IMF
                & PS0 != "IMF") |
               (PS %in% dictPS3.Foods & PS0 != "Foods") |
               (!(PS3 %in% dictPS3)),
             SKU, by = .(PS3, PS)])
    cat("\n")
    
  } else {
    print("PS3: OK")
  }
  
  # Size
  check.UOM.db(df)
  
  # Clean
  df[, PS2.Temp := NULL]
  
}

check.UOM.db = function(df) {
  names(df)[1] = "SKU"
  # Check the style
  
  if (df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
         Brand != "Private label", .N] > 0) {
    cat("\n")
    print("Strange Size style:")
    print(df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
               Brand != "Private label",
             .(Size = unique(Size)),
             by = SKU])
    
  } else {
    print("Style size: OK")
  }
  
  # cat("\n")
  # print("Strange Size numbers:")
  dt = df[Brand != "Private label", .(Size = unique(Size)), by = SKU]
  dt[, Size.Value := stri_extract_all_regex(Size, "[0-9]*\\.?[0-9]+(?!\\*|x|X)")]
  dt[, Size.UOM := stri_extract_last_regex(Size, "[a-zA-Z]+")]
  
  if (dt[(Size.UOM == "Gr" &
          (Size.Value < 10 | Size.Value > 1000)) |
         (Size.UOM == "Kg" & (Size.Value < 0.1 | Size.Value > 1)) |
         (Size.UOM == "Ml" &
          (Size.Value < 50 | Size.Value > 1000)) |
         (Size.UOM == "Lt" &
          (Size.Value < 0.1 | Size.Value > 5)), .N] > 0) {
    cat("\n")
    print("Strange size numbers:")
    print(dt[(Size.UOM == "Gr" &
                (Size.Value < 10 | Size.Value > 1000)) |
               (Size.UOM == "Kg" &
                  (Size.Value < 0.1 | Size.Value > 1)) |
               (Size.UOM == "Ml" &
                  (Size.Value < 50 | Size.Value > 1000)) |
               (Size.UOM == "Lt" &
                  (Size.Value < 0.1 | Size.Value > 5)),
             Size, by = SKU])
    
  } else {
    print("Size numbers: OK")
  }
  
  rm(dt)
  
}

convert.regions = function(df) {
  df[Region == "KYIV RA&OM" |
       Region == "CENTER RA&OM", Region := "Center"]
  df[Region == "NORTH-EAST RA&OM" | Region == "SOUTH-EAST RA&OM" |
       Region == "EAST RA&OM", Region := "East"]
  df[Region == "SOUTH RA&OM", Region := "South"]
  df[Region == "WEST RA&OM", Region := "West"]
  
  return(df)
  
}


convertYtoM = function(string) {
  string = stri_split_fixed(string, "-")
  if (stri_detect_regex(string[[1]][1], "[GL]")) {
    string[[1]][1] = as.numeric(stri_extract_first_regex(string[[1]][1],
                                                         "^[0-9]+")) * 12
  }
  if (stri_detect_regex(string[[1]][2], "[GL]")) {
    string[[1]][2] = as.numeric(stri_extract_first_regex(string[[1]][2],
                                                         "^[0-9]+")) * 12
  }
  string = paste0(string[[1]][1], "-", string[[1]][2])
  return(string)
}

correct.UOM = function(text.string) {
  text.string.characters = stri_extract_last_regex(text.string, "[a-zA-Z]+")
  
  if (stri_detect_regex(text.string.characters, "^[gG]") == TRUE) {
    text.string = stri_c(stri_extract_last_regex(text.string, "[0-9]+"), "Gr")
    
  }
  
  return(text.string)
  
}


correct.segments = function(df) {
  df[SKU == "NUTRILON 2 P400GJT GIPPOALL/MOL/6M",
     `:=`(PS = "Hypoallergenic",
          PS3 = "Specials")]
  
  df[SKU == "NUTRILON/1 PRONU+ P400GJT GIPPOAL/MOL/1D",
     `:=`(PS = "Hypoallergenic",
          PS3 = "Specials")]
  
  df[SKU == "NUTRILON/1 PRONUTRA+P800GJC M/GIPOAL/0-6",
     `:=`(PS = "Hypoallergenic",
          PS3 = "Specials")]
  
  df[SKU == "NUTRILON/A-REFL.P400GJT MOL/SIND.SRYG/1D",
     `:=`(PS = "Anti Reflux")]
  
  df[SKU == "Similac_Nizkolaktoznyi_375Gr_0+_N/S_IMF_IF_Base_BIF_Abbott Laboratories",
     `:=`(PS = "Digestive Comfort",
          PS3 = "Specials")]
  
  df[SKU == "Semper_Lemolac_650Gr_0-6_N/S_Foods_Wet Food_Specials_Anti Reflux_Hero Ag",
     `:=`(PS0 = "IMF", PS2 = "IF")]
  
  df[SKU == "Similac_Nizkolaktoznyi_375Gr_0+_N/S_IMF_IF_Specials_Hypoallergenic_Abbott Laboratories",
     `:=`(PS = "DR-NL")]
  
  df[SKU == "Gerber_Do Re Mi_350Gr_12-36_N/S_IMF_Gum_Base_Gum Base_Nestle",
     `:=`(PS0 = "IMF", PS = "Gum Base")]
  
}


export.new.skus = function(df, df1) {
  df = initial.processing(df, df1)
  
  if (df[is.na(Brand), .N] > 0) {
    df = df[is.na(Brand)]
    
    # Add attributes
    
    # Brand - Company
    df[dictCompanyBrand, on = c(BRAND = "NielsenBrand"), Brand := i.RTRIBrand]
    df[dictCompanyBrand, on = c(Brand = "RTRIBrand"), Company := i.RTRICompany]
    
    # df[is.na(Brand), .N]
    # df[is.na(Company), .N]
    
    if (df[is.na(Brand), .N] > 0) {
      df[, Brand := stri_trans_totitle(BRAND)]
    }
    if (df[is.na(Company), .N] > 0) {
      df[, Company := stri_trans_totitle(BRAND.OWNER)]
    }
    
    df = addSize(df)
    df = addForm(df)
    df = addAge2(df)
    
    # Until addAge2 is not re-written
    df[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
    
    # PS0, PS2, PS3
    df[dictSegments,
       on = c(DANONE.SUB.SEGMENT = "NielsenSegment"),
       `:=`(PS = i.PS,
            PS3 = i.PS3,
            PS0 = i.PS0)] # this is wrong due to PS2
    
    df = addPS2(df)
    df[, PS2 := PS2.Temp]
    
    df[SKU.Matrix[, .N, by = .(Brand, Company)],
       on = "Brand",
       Company := i.Company]
    
    df = df[, unique(SKU2),
            by = .(
              SKU,
              Brand,
              SubBrand,
              Size,
              Age,
              Scent,
              Company,
              PS0,
              PS3,
              PS2,
              PS,
              Form,
              BRAND,
              BRAND.OWNER,
              DANONE.SEGMENT,
              DANONE.SUB.SEGMENT,
              PRODUCT.FORM,
              TYPE...BABY.PRODUCT,
              PRODUCT.BASE,
              Package
            )][order(Brand)]
    write.csv(df, "coding.csv", row.names = FALSE)
    print("New SKUs have been exported.")
    print("Check coding.csv file in the current dirrectory:")
    print(getwd())
    
  } else {
    print("There are no new SKUs")
  }
}

get.pharma.data = function(reported.year, reported.month) {
  df = fread("/home/sergiy/Documents/Work/Nutricia/1/Data/df.csv")
  df[, Package := NA]
  
  df = df[Ynb == max(reported.year) &
            Mnb == reported.month & Channel == "PHARMA",
          .(Volume = sum(Volume),
            Value = sum(Value)),
          by = .(
            SKU,
            Ynb,
            Mnb,
            Brand,
            SubBrand,
            Size,
            Age,
            Scent,
            Scent2,
            ScentType,
            Company,
            PS0,
            PS2,
            PS3,
            PS,
            Form,
            Package,
            Channel,
            Region
          )]
  
  return(df)
  
}


incoming.files.validation = function(df, df1) {
  # Check files dimensions
  if (all(dim(df) != dim(df1)) == TRUE) {
    print("Dimensions: Files have different dimensions, please check files content.")
    
  } else {
    print("Dimensions: OK")
  }
  
  # Column names of volume file
  reported.year.length = length(reported.year)
  
  if (reported.year.length > 1) {
    reported.year = c(rep(reported.year[1:(reported.year.length - 1)], each = 12),
                      rep(reported.year[reported.year.length], length(reported.month)))
    
  }
  
  if (length(reported.month) > 1) {
    reported.month = c(rep(1:12, (reported.year.length - 1)), reported.month)
    
  }
  
  expected.periods =  paste0(toupper(month.abb[reported.month]),
                             ".", reported.year)
  
  # The check of the column names of volume file
  if (all(names(df) == c(expected.fields, expected.periods)) == TRUE) {
    print("Column names of Volume file: OK")
    
  } else if (all(names(df) %in% c(expected.fields, expected.periods) == TRUE)) {
    print("Column names of Volume file: OK, but the order is different!")
    
  }
  
  # The check of the column names of value file
  if (all(names(df1) == c(expected.fields, expected.periods)) == TRUE) {
    print("Column names of Value file: OK")
    
  } else if (all(names(df1) %in% c(expected.fields, expected.periods) == TRUE)) {
    print("Column names of Value file: OK, but the order is different!")
    
  }
  
  
  # Column names identity
  if (!all(names(df) == names(df1)) == TRUE) {
    print("Files columns are different, please check files content.")
    
  } else {
    print("Column names identical: OK")
  }
  
  expected.types = c(types, rep("numeric", length(expected.periods)))
  
  if (all(as.vector(sapply(df, class)) == expected.types)) {
    print("Column types of Volume file: OK")
    
  } else {
    print(paste0(
      "Volume file: types of ",
      names(df)[!(as.vector(sapply(df, class)) == expected.types)],
      " don't mact the expected ones."
    ))
  }
  
  if (all(as.vector(sapply(df1, class)) == expected.types)) {
    print("Column types of Value file: OK")
    
  } else {
    print(paste0(
      "Value file: types of ",
      names(df1)[!(as.vector(sapply(df1, class)) == expected.types)],
      " don't mact the expected ones."
    ))
  }
  
  # Check SKUs in both files
  
  if (all(df[, SKU] == df1[, SKU])) {
    print("SKUs are identical: OK")
    
  } else {
    print("SKUs in volume file are different from the SKUs in Value file.")
  }
  
}

### Process data for coding

initial.processing = function(df, df1, details) {
  if (!(details %in% c("unique", "detailed"))) {
    stop("Incorrect input parameters, check spelling unique/detailed.")
  }
  
  # Assign already existing attributes
  cols = c(
    "SKU",
    "BRAND",
    "BRAND.OWNER",
    "DANONE.SEGMENT",
    "DANONE.SUB.SEGMENT",
    "PRODUCT.FORM",
    "TYPE...BABY.PRODUCT",
    "PRODUCT.BASE",
    "Region"
  )
  
  df = melt.data.table(df, id.vars = cols)
  df1 = melt.data.table(df1, id.vars = cols)
  
  if (all(names(df) == names(df1)) == TRUE) {
    names(df)[11] = "Volume"
    df[df1, on = c("SKU", "variable", "Region", "DANONE.SUB.SEGMENT"),
       Value := i.value]
    
    df[is.na(Volume) | Volume == "", Volume := 0]
    df[is.na(Value) | Value == "", Value := 0]
    
    df = df[(Volume + Value) > 0]
    
    names(df)[10] = "Period"
    
    # if (details == "unique") {
    # df = df[, .N,
    #         by = .(
    #           SKU,
    #           BRAND,
    #           BRAND.OWNER,
    #           DANONE.SEGMENT,
    #           DANONE.SUB.SEGMENT,
    #           PRODUCT.FORM,
    #           TYPE...BABY.PRODUCT,
    #           PRODUCT.BASE
    #         )]
    # } else {
    #   df = df[, .N,
    #           by = .(
    #             SKU,
    #             BRAND,
    #             BRAND.OWNER,
    #             DANONE.SEGMENT,
    #             DANONE.SUB.SEGMENT,
    #             PRODUCT.FORM,
    #             TYPE...BABY.PRODUCT,
    #             PRODUCT.BASE,
    #             Region,
    #             Period,
    #             Volume,
    #             Value
    #           )]
    #
    # }
    
    df[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
    
    # for private label
    
    df[SKU == "OTHER ITEMS PRIVATE LABEL",
       c(
         "Brand",
         "SubBrand",
         "Size",
         "Age",
         "Scent",
         "Scent2",
         "ScentType",
         "PS0",
         "PS2",
         "PS3",
         "PS",
         "Form",
         "Package",
         "Company"
       ) := SKU.Matrix[.SD, .(
         Brand,
         SubBrand,
         Size,
         Age,
         Scent,
         Scent2,
         ScentType,
         PS0,
         PS2,
         PS3,
         PS,
         Form,
         Package,
         Company
       ),
       on = c("SKU2", "DANONE.SUB.SEGMENT")]]
    
    if (df[SKU == "OTHER ITEMS PRIVATE LABEL" &
           is.na(Brand), .N] > 0) {
      print("Add Private Label line to the dictionary ans repeat operation.")
      df[SKU == "OTHER ITEMS PRIVATE LABEL" & is.na(Brand)]
      
    } else {
      df[is.na(Brand),
         c(
           "Brand",
           "SubBrand",
           "Size",
           "Age",
           "Scent",
           "Scent2",
           "ScentType",
           "PS0",
           "PS2",
           "PS3",
           "PS",
           "Form",
           "Package",
           "Company"
         ) := SKU.Matrix[.SD, .(
           Brand,
           SubBrand,
           Size,
           Age,
           Scent,
           Scent2,
           ScentType,
           PS0,
           PS2,
           PS3,
           PS,
           Form,
           Package,
           Company
         ),
         on = c("SKU2")]]
      
      return(df)
    }
  }
  
}

update.sku.matrix = function(SKU.Matrix, new.skus) {
  if (!("SKU2" %in% names(SKU.Matrix))) {
    SKU.Matrix[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
  }
  
  if (!("SKU2" %in% names(new.skus))) {
    new.skus[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
  }
  
  if (any(new.skus[, SKU2] %in% SKU.Matrix[, SKU2])) {
    print("These rows of new file are different from the existing:")
    print(fsetdiff(new.skus, SKU.Matrix))
    cat("\n")
    
    print("These rows of old file are different from the new one:")
    print(fsetdiff(SKU.Matrix, new.skus))
    cat("\n")
    
    print("This columns and rows are different:")
    print(setdiff(
      fsetdiff(SKU.Matrix, new.skus),
      fsetdiff(new.skus, SKU.Matrix)
    ))
    
  } else {
    if (all(names(SKU.Matrix) == names(new.skus))) {
      fwrite(SKU.Matrix, "SKU.Matrix_old.csv", row.names = FALSE)
      SKU.Matrix = rbindlist(list(SKU.Matrix, new.skus))
      fwrite(SKU.Matrix, "SKU.Matrix.csv", row.names = FALSE)
      print("SKU matrix file is updated.")
      
    } else {
      print("Column names do not match, check them and try again.")
      
    }
  }
}
