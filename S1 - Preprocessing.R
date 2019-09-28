# Stage 1
# Data preprocessing and export SKUs for coding

library(data.table)
library(stringi)

source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/functions.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/dictionaries.R")


### Load files and check the structure
# Volume and value sales must be numeric

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201908")

df = fread("N_Y2019M08_Volume.csv", check.names = TRUE, na.strings = "NA")
df1 = fread("N_Y2019M08_Value.csv", check.names = TRUE, na.strings = "NA")
SKU.Matrix = fread("SKU.Matrix.csv", check.names = TRUE, na.strings = "NA")

expected.fields = c("Region", "SKU", "BRAND", "BRAND.OWNER", "DANONE.SEGMENT",
                    "DANONE.SUB.SEGMENT", "PRODUCT.FORM", "TYPE...BABY.PRODUCT",
                    "PRODUCT.BASE")

types = c(rep("character", 4), "logical", rep("character", 4))

reported.year = c(2019)
reported.month = 8 # mention all month of the last year; if mmore than ine year is reported then all months of the previous years will be included automatically




# Dimensions
# Column names of volume file
# Column names of value file
# Column names identical
# Column types of volume file
# Column types of value file

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
  if (all(names(df) == c(expected.fields, expected.periods)) == TRUE) {
    print("Column names of Volume file: OK")
    
  } else if (all(names(df) %in% c(expected.fields, expected.periods) == TRUE)) {
    print("Column names of Volume file: OK, but the order is different!")
    
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

# Validate files
incoming.files.validation(df, df1)

### Process data for coding

# Assign already existing attributes

df = df[, .N,
   by = .(SKU, BRAND, BRAND.OWNER, DANONE.SEGMENT, DANONE.SUB.SEGMENT, 
          PRODUCT.FORM, TYPE...BABY.PRODUCT, PRODUCT.BASE)]

df[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
  
  # for private label
  
  df[SKU == "OTHER ITEMS PRIVATE LABEL",
     c("Brand", "SubBrand",
       "Size", "Age", "Scent", "Scent2", "ScentType",
       "PS0", "PS2", "PS3", "PS",
       "Form", "Package",
       "Company") := SKU.Matrix[.SD, .(Brand, SubBrand,
                                       Size, Age, Scent, Scent2, ScentType,
                                       PS0, PS2, PS3, PS,
                                       Form, Package,
                                       Company), 
                                on = c("SKU2", "DANONE.SUB.SEGMENT")]]
  
  if (df[SKU == "OTHER ITEMS PRIVATE LABEL" & is.na(Brand), .N] > 0) {
    
    print("Add Private Label line to the dictionary ans repeat operation.")
    df[SKU == "OTHER ITEMS PRIVATE LABEL" & is.na(Brand)]
    
  }
  
  df[is.na(Brand),
     c("Brand", "SubBrand",
       "Size", "Age", "Scent", "Scent2", "ScentType",
       "PS0", "PS2", "PS3", "PS",
       "Form", "Package",
       "Company") := SKU.Matrix[.SD, .(Brand, SubBrand,
                                       Size, Age, Scent, Scent2, ScentType,
                                       PS0, PS2, PS3, PS,
                                       Form, Package,
                                       Company), 
                                on = c("SKU2")]]
  

  df.existing = df[!is.na(Brand)]
  df = df[is.na(Brand)]
  
# Brand - Company
df[dictCompanyBrand, on = c(BRAND = "NielsenBrand"), Brand := i.RTRIBrand]
df[dictCompanyBrand, on = c(Brand = "RTRIBrand"), Company := i.RTRICompany]

df[is.na(Brand), .N]
df[is.na(Company), .N]

if (df[is.na(Brand), .N] > 0) {df[, Brand := str_to_title(BRAND)]}
if (df[is.na(Company), .N] > 0) {df[, Company := str_to_title(BRAND.OWNER)]}

# SubBrand
#df = addSubBrand(df)
# df[, SubBrand := NA]

df = addSize(df)
df = addForm(df)
df = addAge2(df)

# PS0, PS2, PS3
df[dictSegments, 
   on = c(DANONE.SUB.SEGMENT = "NielsenSegment"), 
   `:=`(PS = i.PS, PS3 = i.PS3, PS0 = i.PS0)] # this is wrong due to PS2
df = addPS2(df)

### EXPORT FOR CODING
df[, SKU2 := str_trim(str_replace_all(SKU, "\\s+", " "))]


df[SKU.Matrix[, .N, by = .(Brand, Company)], on = "Brand", Company := i.Company]

a=df[, unique(SKU2),
     by = .(SKU, Brand, SubBrand, Size, Age, Scent, Company, 
            PS0, PS3, PS2, PS, Form,
            BRAND, BRAND.OWNER, DANONE.SEGMENT, DANONE.SUB.SEGMENT, 
            PRODUCT.FORM, TYPE...BABY.PRODUCT, PRODUCT.BASE, Package)][order(Brand)]
write.csv(a, "coding.csv", row.names = FALSE)
