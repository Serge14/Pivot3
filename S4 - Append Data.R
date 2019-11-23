# Read files
# transform
# Add attributes
# check if there are missing values
# merge with historical data (check if all necessary is available)

source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/dictionaries.R")


setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201909")

df = fread("N_Y2019M09_Volume.csv", check.names = TRUE, na.strings = "NA")
df1 = fread("N_Y2019M09_Value.csv", check.names = TRUE, na.strings = "NA")

df = initial.processing(df, df1, "detailed")
df = convert.regions(df)

if (df[is.na(Brand), .N] > 0) {
  print("Some attributes are missing, check the file...")
  cat("\n")
  print(df[is.na(Brand)])
  
} else {
  # Some data may be incorrect, needs to be corrected
  df = correct.segments(df)
  
  df = addDates(df)
  
  df[is.na(Volume) | Volume == "", Volume := 0]
  df[is.na(Value) | Value == "", Value := 0]
  df = df[(Value + Volume) > 0]
  
  df[, `:=`(Volume = Volume*1000,
            Value = Value*1000)]
  
  df = df[, .(Volume = sum(Volume),
              Value = sum(Value)), 
          by = .(SKU, Ynb, Mnb, 
                 Brand, SubBrand, Size, Age, 
                 Scent, Scent2, ScentType, Company, 
                 PS0, PS3, PS2, PS, Form, Package, Region)]
  
  df[, Channel := "MT"]
  
  ## Checks
  df[is.na(Age) | Age == "" | Age == "NA", unique(SKU), by = PS3]
  df[is.na(Form) | Form == "" | Form == "NA", unique(SKU), by = PS3]
  
  df[is.na(PS0) | PS0 == "" | PS0 == "NA", unique(SKU), by = PS3]
  
  df[is.na(Form) | Form == "", Form := "Not Applicable"]
  
  # df[is.na(PriceSegment) & (PS2 == "Dry Food" | PS0 == "IMF" |
  #                             (PS3 == "Fruits" | PS3 == "Savoury Meal")), 
  # unique(Brand), by = PS]
  df[is.na(PS2) | PS2 == "" | PS2 == "NA", unique(SKU), by = PS3]
  
  # Merge with RTRI data, pharma channel

  df1 = get.pharma.data(reported.year, reported.month)
  
  if (length(names(df)) == length(names(df1))){
    
    setcolorder(df, names(df1))
    
    if (all(names(df) == (names(df1)))){
      df = rbindlist(list(df, df1))  
    
      # Add price Segments
      df = add.price.segments(df)
      
      # Check Price Segments
      df[(is.na(PriceSegment) | PriceSegment == "") & (PS2 == "Dry Food" | PS0 == "IMF" |
                                                         (PS3 == "Fruits" | PS3 == "Savoury Meal")), 
         unique(Brand), by = PS]
      
      df[(is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
           (PS2 == "Dry Food" | PS0 == "IMF" | (PS3 == "Fruits" | PS3 == "Savoury Meal")), 
         unique(Brand), by = PS]
      
      df[PS0 == "IMF" & Ynb == 2019, .(Sales=sum(Volume), Price = sum(Value)/sum(Volume)), 
         by = .(Brand, PriceSegment, GlobalPriceSegment)][order(Price)]
      
      df[PS2 == "Dry Food", .(Sales=sum(Volume), Price = sum(Value)/sum(Volume)), 
         by = .(Brand, PriceSegment, GlobalPriceSegment)][order(Price)]
      
      df[PS3 == "Fruits" | PS3 == "Savoury Meal", .(Sales=sum(Volume)/1000, 
                                                    Price = sum(Value)/sum(Volume)), 
         by = .(Brand, PriceSegment, GlobalPriceSegment)][order(Price)]
      
      # Acidified
      df = add.acidified(df)
      
      # Extrapolate (check 1.2 in Puree)
      df = add_EC_AC(df)
      df[, `:=`(VolumeC = Volume*EC*AC,
                ValueC = Value*EC*AC)]
      
      df = add.package(df)
      df = add.scent.pharma(df)
      
      # Merge with historical data
      
      print("Select corrected file of the previous period:")
      df1 = fread(file.choose())
      # df1 = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected2.csv")
      
      # check column names
      if ("SKU2" %in% names(df1)) {df1[, SKU2 := NULL]}
      
        setcolorder(df, names(df1))
        df = rbindlist(list(df1, df))  
      
      df = df[Volume + Value > 0]
      
      
      setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201909")
      fwrite(df, "df_test.csv", row.names = FALSE)
      
      } else {
      print("MT and Pharma files names mismatch, check column names")
      
    }
    
  } else {
    print("MT and Pharma files names mismatch, check column names")
    
  }
  
}




