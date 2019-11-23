library(data.table)
library(stringi)
df = fread("/home/sergiy/Documents/Temp/scents.csv")

define.scent.type = function(a, df) {
  v = c(0, 0)
b = unlist(stri_split_fixed(a, "-"))

all(b %in% df$V2)

df[V2 %in% b, .N, by = V3]

v[1] = df[V2 %in% b, .N, by = V3][V3 == "Fruit", N]
v[2] = df[V2 %in% b, .N, by = V3][V3 == "Grain", N]

c1 = df[V3 == "Fruit"][V2 %in% b, V2]
c2 = df[V3 == "Grain"][V2 %in% b, V2]

if (v[1] >=1 & v[1]<=2){Scent2 = stri_c(sort(c1), collapse = "-")}

if (v[1] == 0) {Scent2 = "Without"}
if (v[1] > 2) {Scent2 = "Multifruit"}

if (v[2] >=1 & v[2]<=2){Type = stri_c(sort(c2), collapse = "-")}

if (v[2] == 0) {Type = "Without"}
if (v[2] > 2) {Type = "Multigrain"}

return(list(Scent2, Type))

}
