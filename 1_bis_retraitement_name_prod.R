
#Code de retraitement des noms de produits 


# sort
(table(data$name_product))
#sort(unique(data$name_product))

data$name_product2 = tolower(data$name_product)
#sort(unique(data$name_product2))

data$name_product2[data$name_product2 %in% c("aba thai", "aba thia")] = "aba thai"
data$name_product2[data$name_product2 %in% c("ag-killer 003", "ag killer 003")] = "ag killer003"
data$name_product2[data$name_product2 %in% c("andolis", "andolis 120b", "andols")] = "andolis"
data$name_product2[data$name_product2 %in% c("anhead 12 gr")] = "anhead"
data$name_product2[data$name_product2 %in% c("anvil 5sc")] = "anvil"
data$name_product2[data$name_product2 %in% c("appasa", "appasa 600ec", "apsara")] = "appasa"
data$name_product2[data$name_product2 %in% c("assail", "assail (thnam kchorng)")] = "assail"
data$name_product2[data$name_product2 %in% c("bam beak kreab", "bam beak kreab smao", "bambeak kreab smao")] = "bam beak kreab"
data$name_product2[data$name_product2 %in% c("basa", "basa 50ec", "basa50ec", "bassatiga", "bassatigi", "basstigi")] = "bassa"
data$name_product2[data$name_product2 %in% c("bassatiga", "bassatigi", "basstigi")] = "bassatiga"
data$name_product2[data$name_product2 %in% c("bimgold 270wp")] = "bimgold"
data$name_product2[data$name_product2 %in% c("bom beak kreab", "bom beak krob")] = "bom beak kreab"
data$name_product2[data$name_product2 %in% c("carstivin", "cartisvin", "castivin", "catisvin")] = "cartisvin"
data$name_product2[data$name_product2 %in% c("catanil", "cantanil")] = "cantanil"
data$name_product2[data$name_product2 %in% c("choang 36ec")] = "choang"
data$name_product2[data$name_product2 %in% c("dang kov chhor", "dang kov moy", "dang kov moy roy", "dang kov moyroy", "dong kov mov sjlek","dangkov totov","dangkov mouylean")] = "doung kov"
data$name_product2[data$name_product2 %in% c("fast grow", "fast growth")] = "fast grow"
data$name_product2[data$name_product2 %in% c("fast growth", "fast grow")] = "fast growth"
data$name_product2[data$name_product2 %in% c("inthrey dek", "inthrey kamnach", "inthrey kanach")] = "inthrey dek"
data$name_product2[data$name_product2 %in% c("kamkakh", "kamkha")] = "kamkakh"
data$name_product2[data$name_product2 %in% c("katana", "katana 20wg")] = "katana 20wg"
data$name_product2[data$name_product2 %in% c("lem lèp", "lep lèp")] = "lem lep"
data$name_product2[data$name_product2 %in% c("meco 60 ec", "meco 60ec")] = "meco"
data$name_product2[data$name_product2 %in% c("michelle62ec", "michelle", "missel")] = "michelle"
data$name_product2[data$name_product2 %in% c("nano3.6ec", "katana 20wg", "nano3.6ec0")] = "katana"
data$name_product2[data$name_product2 %in% c("nano 3.6ec", "katana 20wg", "nano 3.6ec0")] = "katana"
data$name_product2[data$name_product2 %in% c("nida", "nida 500wp")] = "nida"
data$name_product2[data$name_product2 %in% c("pa to", "pa-to")] = "pa-to"
data$name_product2[data$name_product2 %in% c("prevanthorn", "privanthorn")] = "prevanthorn"
data$name_product2[data$name_product2 %in% c("reasgant 120ec", "reasgant", "reasgant 3.6ec", "reasgant3.6ec")] = "reasgant"
data$name_product2[data$name_product2 %in% c("sdach chhok", "sdach dang kov 99", "sdach dang kov")] = "sdach dang kov"
data$name_product2[data$name_product2 %in% c("sdach pikhaet nhi chhmol", "sdach pikheat nhi chhmol")] = "sdach pikhaet nhi chhmol"
data$name_product2[data$name_product2 %in% c("sofia 300")] = "sofia"
data$name_product2[data$name_product2 %in% c("srov khlang88", "srov khlang 88")] = "srov khlang 88"
data$name_product2[data$name_product2 %in% c("supergold 300wp", "sunderl and supergold")] = "supergold"
data$name_product2[data$name_product2 %in% c("super aco 600 ec","super aco 600ec","aco 600sl","super eco 600ec","superaco600ec" ,"supper aco 600ec")] = "super eco"
data$name_product2[data$name_product2 %in% c("tita", "tita 250wg")] = "tita"
data$name_product2[data$name_product2 %in% c("visel", "visell")] = "visell"
data$name_product2[data$name_product2 %in% c("xevelo 120ec", "xevelo120ec")] = "xevelo"
data$name_product2[data$name_product2 %in% c("yekmam", "yekmum")] = "yekmam"
data$name_product2[data$name_product2 %in% c("zen-cyp", "zeb-cyp")] = "zeb-cyp"


# rename made from RShiny: 

data$name_product2[data$name_product2 %in% c( "butin 550 ec", "butin 550ec" )] =  "butin"
data$name_product2[data$name_product2 %in% c( "forji 40 ec","forji 40ec" )] =  "forji"
data$name_product2[data$name_product2 %in% c( "michelle 62 ec", "michelle 62 ec" )] =  "michelle"
data$name_product2[data$name_product2 %in% c( "reasganr" , "reasgant",  "reasgant 3.3ec" ,  "reasgant 3.6 ec" )] =  "reasgant"
data$name_product2[data$name_product2 %in% c(  "xevelo 120b", "xevelo" )] =  "xevelo"
data$name_product2[data$name_product2 %in% c( "taco", "taco 600ec" )] =  "taco"
