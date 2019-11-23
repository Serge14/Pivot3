# Stage 1
# Data preprocessing and export SKUs for coding

library(data.table)
library(stringi)

# source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/functions.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/dictionaries.R")


### Load files and check the structure
# Volume and value sales must be numeric

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201909")

df = fread("N_Y2019M09_Volume.csv", check.names = TRUE, na.strings = "NA")
df1 = fread("N_Y2019M09_Value.csv", check.names = TRUE, na.strings = "NA")
SKU.Matrix = fread("SKU.Matrix.csv", check.names = TRUE, na.strings = "NA")

expected.fields = c("Region", "SKU", "BRAND", "BRAND.OWNER", "DANONE.SEGMENT",
                    "DANONE.SUB.SEGMENT", "PRODUCT.FORM", "TYPE...BABY.PRODUCT",
                    "PRODUCT.BASE")

types = c(rep("character", 4), "logical", rep("character", 4))

reported.year = c(2019)
reported.month = 9 # mention all month of the last year; if more than one year is reported then all months of the previous years will be included automatically

# Dimensions
# Column names of volume file
# Column names of value file
# Column names identical
# Column types of volume file
# Column types of value file



# Validate files
incoming.files.validation(df, df1)

# Export new skus for coding
export.new.skus(df, df1)
