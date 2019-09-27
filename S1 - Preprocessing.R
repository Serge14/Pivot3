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

# Dimensions
# Column names of volume file
# Column names of value file
# Column names identical
# Column types of volume file
# Column types of value file

# Check files dimensions
if (all(dim(df) != dim(df1)) == TRUE) {
  print("Dimensions: Files have different dimensions, please check files content.")
  
} else {
  print("Dimensions: OK")
}

# Column names of volume file



# Column names identity
if (!all(names(df) == names(df1)) == TRUE) {
  print("Files columns are different, please check files content.")
  
} else {
  print("Column names identical: OK")
}

# Check the volume file
if (is.numeric(df[, dim(df)[2]]) == TRUE) {
  print("Volume file seems to be OK. Check the structure visually.")
  
} else {
  print("Volume sales might be not numeric. Check the structure visually.")
  
}

cat("\n")
str(df)

# Check the value file
if (is.numeric(df1[, dim(df)[2]]) == TRUE) {
  print("Value file seems to be OK. Check the structure visually.")
  
} else {
  print("Value sales might be not numeric. Check the structure visually.")
  
}

cat("\n")
str(df1
)

reported.year = 2019
reported.month = 8

paste0(toupper(month.abb[reported.month]), ".", reported.year)


df[, SKU2 := str_trim(str_replace_all(SKU, "\\s+", " "))]
  
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
  


df[Region == "KYIV RA&OM" | Region == "CENTER RA&OM", Region := "Center"]
df[Region == "NORTH-EAST RA&OM" | Region == "SOUTH-EAST RA&OM" |
     Region == "EAST RA&OM", Region := "East"]
df[Region == "SOUTH RA&OM", Region := "South"]
df[Region == "WEST RA&OM", Region := "West"]

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