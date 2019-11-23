# open both files
# check if the new records are present in the original file
# check if the all attributes are exactly the same
# show records that are have differences
# add records that are new

SKU.Matrix = fread("SKU.Matrix.csv", check.names = TRUE)
new.skus = fread("new.skus.csv", check.names = TRUE)

update.sku.matrix(SKU.Matrix, new.skus)