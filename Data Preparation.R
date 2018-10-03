library(mlr)
library(data.table)
library(plyr)
library(dplyr)

zomato = read.csv("zomato.csv")
zomato = as.data.table(zomato)


countries = read.csv("Country-Code.csv")

# Scale cost by currency
zomato[,Average.Cost.for.two.Std := as.vector(scale(Average.Cost.for.two)), by=Currency]

# Reorder columns and remove original cost column
zomato = zomato[,c(1:11,22,12:21)]
zomato = zomato[,Average.Cost.for.two := NULL]

# Create new binary variables for cuisine type (1-C encoding)
splitdat = do.call("rbind", strsplit(as.character(zomato$Cuisines), ","))
unique.cuisines = as.vector(unique(as.vector(trimws(splitdat))))
current.names = as.vector(names(zomato))

new.names = as.vector(matrix(c(current.names,unique.cuisines))) 

for(i in 1:length(unique.cuisines)){
  zomato[,paste(i)] = rep(0,nrow(zomato))
}

names(zomato) = new.names

for(i in 1:length(unique.cuisines)){
zomato[,unique.cuisines[i]] = ifelse(grepl(unique.cuisines[i],zomato$Cuisines),1,0)
}


# Aggregate new variables 
zomato = zomato[, `:=`(European = French+European+Italian+German+
                                  Mediterranean+Greek+Spanish+Irish+
                                  British+Portuguese+Scottish+Belgian,
                       Asian = Japanese+Chinese+Asian+Filipino+Korean+
                               Vietnamese+Thai+Singaporean+Malaysian+
                               Indonesian+Taiwanese+Cantonese+Malay+
                               Peranakan+Sunda,
                       `South American` = Brazilian+Mexican+Peruvian+
                                          `Latin American`+Caribbean+
                                          Cuban+`South American`+Argentine+
                                          Mineira,
                       Indian = Indian+Goan+`South Indian`+`North Indian`+
                                Rajasthani+Mughlai+Mithai+Maharashtrian+
                                `Modern Indian`+Biryani+Parsi+Kashmiri+
                                Bengali+Awadhi+Lucknowi+Gujarati+Oriya+
                                Hyderabadi+Bihari+Kerala+Andhra+Malwani+
                                Assamese+Naga+Chettinad+Mangalorean,
                       American = American+Hawaiian+`New American`+Cajun+Canadian,
                       `Middle Eastern` = `Middle Eastern`+Arabian+Lebanese+
                                          Turkish+Pakistani+Persian+Burmese+Tibetan+
                                          Afghani+Nepalese+Iranian+Izgara+Armenian+Durban,
                       Other = `Bar Food`+International+Steak+BBQ+Southern+Breakfast+
                               Sandwich+`Modern Australian`+African+`North Eastern`+`Tex-Mex`+
                               `Cuisine Varies`+`Charcoal Grill`+`Asian Fusion`+Kiwi+Contemporary+
                               Ramen+Grill+`Sri Lankan`+Kebab+`World Cuisine`+`Turkish Pizza`+
                               `Restaurant Cafe`+Southwestern+Diner+Teriyaki+Vegetarian+
                               Fusion+Deli+`Fish and Chips`+`Dim Sum`+Curry+`South African`+
                               Bí_rek+`Soul Food`+Moroccan+Dí_ner+`Pub Food`+Tapas+Western+
                               Australian+Sushi+Salad,
                       Desserts = Desserts+`Ice Cream`+Patisserie,
                       `Fast Food` = `Fast Food`+`Gourmet Fast Food`,
                       Beverages = Beverages+Juices+`Coffee and Tea`+Tea+`Drinks Only`+`Bubble Tea`)]

zomato = zomato[, `:=`(French=NULL,Italian=NULL,German=NULL,
                    Mediterranean=NULL,Greek=NULL,Spanish=NULL,Irish=NULL,
                    British=NULL,Portuguese=NULL,Scottish=NULL,Belgian=NULL,
                    Japanese=NULL,Chinese=NULL,Filipino=NULL,Korean=NULL,
                    Vietnamese=NULL,Thai=NULL,Singaporean=NULL,Malaysian=NULL,
                    Indonesian=NULL,Taiwanese=NULL,Cantonese=NULL,Malay=NULL,
                    Peranakan=NULL,Sunda=NULL,
                    Brazilian=NULL,Mexican=NULL,Peruvian=NULL,
                    `Latin American`=NULL,Caribbean=NULL,
                    Cuban=NULL,Argentine=NULL,Mineira=NULL,
                    Goan=NULL,`South Indian`=NULL,`North Indian`=NULL,
                    Rajasthani=NULL,Mughlai=NULL,Mithai=NULL,Maharashtrian=NULL,
                    `Modern Indian`=NULL,Biryani=NULL,Parsi=NULL,Kashmiri=NULL,
                    Bengali=NULL,Awadhi=NULL,Lucknowi=NULL,Gujarati=NULL,Oriya=NULL,
                    Hyderabadi=NULL,Bihari=NULL,Kerala=NULL,Andhra=NULL,Malwani=NULL,
                    Assamese=NULL,Naga=NULL,Chettinad=NULL,Mangalorean=NULL,
                    American=NULL,Hawaiian=NULL,`New American`=NULL,Cajun=NULL,Canadian=NULL,
                    `Middle Eastern`=NULL,Arabian=NULL,Lebanese=NULL,
                    Turkish=NULL,Pakistani=NULL,Persian=NULL,Burmese=NULL,Tibetan=NULL,
                    Afghani=NULL,Nepalese=NULL,Iranian=NULL,Izgara=NULL,Armenian=NULL,Durban=NULL,
                    `Bar Food`=NULL,International=NULL,Steak=NULL,BBQ=NULL,Southern=NULL,Breakfast=NULL,
                    Sandwich=NULL,`Modern Australian`=NULL,African=NULL,`North Eastern`=NULL,`Tex-Mex`=NULL,
                    `Cuisine Varies`=NULL,`Charcoal Grill`=NULL,`Asian Fusion`=NULL,Kiwi=NULL,Contemporary=NULL,
                    Ramen=NULL,Grill=NULL,`Sri Lankan`=NULL,Kebab=NULL,`World Cuisine`=NULL,`Turkish Pizza`=NULL,
                    `Restaurant Cafe`=NULL,Southwestern=NULL,Diner=NULL,Teriyaki=NULL,Vegetarian=NULL,
                    Fusion=NULL,Deli=NULL,`Fish and Chips`=NULL,`Dim Sum`=NULL,Curry=NULL,`South African`=NULL,
                    Bí_rek=NULL,`Soul Food`=NULL,Moroccan=NULL,Dí_ner=NULL,`Pub Food`=NULL,Tapas=NULL,
                    Western=NULL,Australian=NULL,Sushi=NULL,`Ice Cream`=NULL,Patisserie=NULL,
                    `Gourmet Fast Food`=NULL,Salad=NULL,Juices=NULL,`Coffee and Tea`=NULL,
                    Tea=NULL,`Drinks Only`=NULL,`Bubble Tea`=NULL)]


# Make variables binary again
for(j in names(zomato)[-c(1:21)]){
  set(zomato, i= which(zomato[[j]]>1), j= j, value= 1)
}   

# Map country code to country
zomato = merge(zomato,countries,by = "Country.Code")
zomato = zomato[,Country.Code := NULL]

# Remove remaining unnecessary variables
zomato = zomato[,c("Restaurant.ID","Restaurant.Name","Address",
                   "Locality","Locality.Verbose","Cuisines",
                   "Is.delivering.now","Switch.to.order.menu",
                   "City","Currency","Rating.color"):=NULL]

################### josh additions ###########################
#Converting yes/no cols to numeric
zomato$Has.Table.booking <- revalue(zomato$Has.Table.booking, c("No"=0, "Yes"=1))
zomato$Has.Online.delivery <- revalue(zomato$Has.Online.delivery, c("No"=0, "Yes"=1))

#Aggregating country into continent(maybe leave india on its own?)
Europe <- c("United Kingdom")
North_America <- c("United States","Canada")
Asia <- c("India","Indonesia","Singapore","Sri Lanka","UAE","Phillipines","Qatar")
Oceania <- c("Australia", "New Zealand")
ROW <- c("Brazil","South Africa", "Turkey")

zomato$continent = zomato$Country

levels(zomato$continent)[levels(zomato$continent) %in% Europe] = "Europe"
levels(zomato$continent)[levels(zomato$continent) %in% North_America] = "North America"
levels(zomato$continent)[levels(zomato$continent) %in% Asia] = "Asia"
levels(zomato$continent)[levels(zomato$continent) %in% Oceania] = "Oceania"
levels(zomato$continent)[levels(zomato$continent) %in% ROW] = "Rest of World"

zomato$Country<-NULL

#converting continent to binary
setDT(zomato)[, c(levels(zomato$continent), "continent") := 
            c(lapply(levels(continent), function(x) as.integer(x == continent)), .(NULL))]

#creating new variable for range of cuisine

zomato$Cuisine_Range = rowSums(zomato[,10:27])

# aggregate cuisine range
zomato$Cuisine_Range = as.factor(zomato$Cuisine_Range)
levels(zomato$Cuisine_Range)[levels(zomato$Cuisine_Range) %in% c("5","6","7","8")] = ">4"


# filter data
zomato = zomato[Rating.text != "Not rated"] %>% droplevels()

#==================== phase 2 prep ===================#


zomato = read.csv("zomato.csv")
zomato = as.data.table(zomato)

countries = read.csv("Country-Code.csv")


# Scale cost by currency
zomato[,Average.Cost.for.two.Std := as.vector(scale(Average.Cost.for.two)), by=Currency]


# Create new binary variables for cuisine type (1-C encoding)
splitdat = do.call("rbind", strsplit(as.character(zomato$Cuisines), ","))
unique.cuisines = as.vector(unique(as.vector(trimws(splitdat))))
current.names = as.vector(names(zomato))

new.names = as.vector(matrix(c(current.names,unique.cuisines))) 

for(i in 1:length(unique.cuisines)){
  zomato[,paste(i)] = rep(0,nrow(zomato))
}

names(zomato) = new.names

for(i in 1:length(unique.cuisines)){
  zomato[,unique.cuisines[i]] = ifelse(grepl(unique.cuisines[i],zomato$Cuisines),1,0)
}


# Aggregate new variables 
zomato = zomato[, `:=`(European = French+European+Italian+German+
                         Mediterranean+Greek+Spanish+Irish+
                         British+Portuguese+Scottish+Belgian,
                       Asian = Japanese+Chinese+Asian+Filipino+Korean+
                         Vietnamese+Thai+Singaporean+Malaysian+
                         Indonesian+Taiwanese+Cantonese+Malay+
                         Peranakan+Sunda,
                       `South American` = Brazilian+Mexican+Peruvian+
                         `Latin American`+Caribbean+
                         Cuban+`South American`+Argentine+
                         Mineira,
                       Indian = Indian+Goan+`South Indian`+`North Indian`+
                         Rajasthani+Mughlai+Mithai+Maharashtrian+
                         `Modern Indian`+Biryani+Parsi+Kashmiri+
                         Bengali+Awadhi+Lucknowi+Gujarati+Oriya+
                         Hyderabadi+Bihari+Kerala+Andhra+Malwani+
                         Assamese+Naga+Chettinad+Mangalorean,
                       American = American+Hawaiian+`New American`+Cajun+Canadian,
                       `Middle Eastern` = `Middle Eastern`+Arabian+Lebanese+
                         Turkish+Pakistani+Persian+Burmese+Tibetan+
                         Afghani+Nepalese+Iranian+Izgara+Armenian+Durban,
                       Other = `Bar Food`+International+Steak+BBQ+Southern+Breakfast+
                         Sandwich+`Modern Australian`+African+`North Eastern`+`Tex-Mex`+
                         `Cuisine Varies`+`Charcoal Grill`+`Asian Fusion`+Kiwi+Contemporary+
                         Ramen+Grill+`Sri Lankan`+Kebab+`World Cuisine`+`Turkish Pizza`+
                         `Restaurant Cafe`+Southwestern+Diner+Teriyaki+Vegetarian+
                         Fusion+Deli+`Fish and Chips`+`Dim Sum`+Curry+`South African`+
                         Bí_rek+`Soul Food`+Moroccan+Dí_ner+`Pub Food`+Tapas+Western+
                         Australian+Sushi+Salad,
                       Desserts = Desserts+`Ice Cream`+Patisserie,
                       `Fast Food` = `Fast Food`+`Gourmet Fast Food`,
                       Beverages = Beverages+Juices+`Coffee and Tea`+Tea+`Drinks Only`+`Bubble Tea`)]

zomato = zomato[, `:=`(French=NULL,Italian=NULL,German=NULL,
                       Mediterranean=NULL,Greek=NULL,Spanish=NULL,Irish=NULL,
                       British=NULL,Portuguese=NULL,Scottish=NULL,Belgian=NULL,
                       Japanese=NULL,Chinese=NULL,Filipino=NULL,Korean=NULL,
                       Vietnamese=NULL,Thai=NULL,Singaporean=NULL,Malaysian=NULL,
                       Indonesian=NULL,Taiwanese=NULL,Cantonese=NULL,Malay=NULL,
                       Peranakan=NULL,Sunda=NULL,
                       Brazilian=NULL,Mexican=NULL,Peruvian=NULL,
                       `Latin American`=NULL,Caribbean=NULL,
                       Cuban=NULL,Argentine=NULL,Mineira=NULL,
                       Goan=NULL,`South Indian`=NULL,`North Indian`=NULL,
                       Rajasthani=NULL,Mughlai=NULL,Mithai=NULL,Maharashtrian=NULL,
                       `Modern Indian`=NULL,Biryani=NULL,Parsi=NULL,Kashmiri=NULL,
                       Bengali=NULL,Awadhi=NULL,Lucknowi=NULL,Gujarati=NULL,Oriya=NULL,
                       Hyderabadi=NULL,Bihari=NULL,Kerala=NULL,Andhra=NULL,Malwani=NULL,
                       Assamese=NULL,Naga=NULL,Chettinad=NULL,Mangalorean=NULL,
                       American=NULL,Hawaiian=NULL,`New American`=NULL,Cajun=NULL,Canadian=NULL,
                       `Middle Eastern`=NULL,Arabian=NULL,Lebanese=NULL,
                       Turkish=NULL,Pakistani=NULL,Persian=NULL,Burmese=NULL,Tibetan=NULL,
                       Afghani=NULL,Nepalese=NULL,Iranian=NULL,Izgara=NULL,Armenian=NULL,Durban=NULL,
                       `Bar Food`=NULL,International=NULL,Steak=NULL,BBQ=NULL,Southern=NULL,Breakfast=NULL,
                       Sandwich=NULL,`Modern Australian`=NULL,African=NULL,`North Eastern`=NULL,`Tex-Mex`=NULL,
                       `Cuisine Varies`=NULL,`Charcoal Grill`=NULL,`Asian Fusion`=NULL,Kiwi=NULL,Contemporary=NULL,
                       Ramen=NULL,Grill=NULL,`Sri Lankan`=NULL,Kebab=NULL,`World Cuisine`=NULL,`Turkish Pizza`=NULL,
                       `Restaurant Cafe`=NULL,Southwestern=NULL,Diner=NULL,Teriyaki=NULL,Vegetarian=NULL,
                       Fusion=NULL,Deli=NULL,`Fish and Chips`=NULL,`Dim Sum`=NULL,Curry=NULL,`South African`=NULL,
                       Bí_rek=NULL,`Soul Food`=NULL,Moroccan=NULL,Dí_ner=NULL,`Pub Food`=NULL,Tapas=NULL,
                       Western=NULL,Australian=NULL,Sushi=NULL,`Ice Cream`=NULL,Patisserie=NULL,
                       `Gourmet Fast Food`=NULL,Salad=NULL,Juices=NULL,`Coffee and Tea`=NULL,
                       Tea=NULL,`Drinks Only`=NULL,`Bubble Tea`=NULL)]


# Make variables binary again
for(j in names(zomato)[-c(1:21)]){
  set(zomato, i= which(zomato[[j]]>1), j= j, value= 1)
}   

# Map country code to country
zomato = merge(zomato,countries,by = "Country.Code")
zomato = zomato[,Country.Code := NULL]

# Remove remaining unnecessary variables
zomato = zomato[,c("Restaurant.ID","Restaurant.Name","Address",
                   "Locality","Locality.Verbose","Cuisines",
                   "Is.delivering.now","Switch.to.order.menu",
                   "City","Currency","Rating.color"):=NULL]


#Aggregating country into continent(maybe leave india on its own?)
Europe <- c("United Kingdom")
North_America <- c("United States","Canada")
Asia <- c("India","Indonesia","Singapore","Sri Lanka","UAE","Phillipines","Qatar")
Oceania <- c("Australia", "New Zealand")
ROW <- c("Brazil","South Africa", "Turkey")

zomato$continent <- case_when(
  zomato$Country %in% Europe ~ "Europe",
  zomato$Country %in% North_America ~ "North America",
  zomato$Country %in% Asia ~ "Asia",
  zomato$Country %in% Oceania ~ "Oceania",
  zomato$Country %in% ROW ~ "Rest of World"
)

zomato$continent<-as.factor(zomato$continent)
zomato = zomato[,"Country":=NULL]

#returning cuisine to a single column of factors (for visualisation)
zomato$cuisine<-max.col(zomato[,11:28])
zomato$cuisine<-zomato$cuisine%>%factor()

zomato$cuisine<-revalue(zomato$cuisine, c("1"=names(zomato[,11]),
                                          "2"=names(zomato[,12]),
                                          "3"=names(zomato[,13]),
                                          "4"=names(zomato[,14]),
                                          "5"=names(zomato[,15]),
                                          "6"=names(zomato[,16]),
                                          "7"=names(zomato[,17]),
                                          "8"=names(zomato[,18]),
                                          "9"=names(zomato[,19]),
                                          "10"=names(zomato[,20]),
                                          "11"=names(zomato[,21]),
                                          "12"=names(zomato[,22]),
                                          "13"=names(zomato[,23]),
                                          "14"=names(zomato[,24]),
                                          "15"=names(zomato[,25]),
                                          "16"=names(zomato[,26]),
                                          "17"=names(zomato[,27]),
                                          "18"=names(zomato[,28])))

#creating new variable for range of cuisine

zomato$Cuisine_Range = rowSums(zomato[,11:28])

# aggregate cuisine range
zomato$Cuisine_Range = as.factor(zomato$Cuisine_Range)
levels(zomato$Cuisine_Range)[levels(zomato$Cuisine_Range) %in% c("5","6","7","8")] = ">4"


#removing binary columns
zomato = zomato[,c("Seafood","Asian","European",
                   "Cafe","Fast Food","Bakery","Pizza","Desserts","Other",
                   "Beverages","Burger","Indian","Finger Food","Healthy Food",
                   "Continental","Street Food","Raw Meats","South American"):=NULL]

#Unnecessary columns
zomato = zomato[,c("Longitude", "Latitude", "Aggregate.rating", "Price.range", "Average.Cost.for.two") := NULL]

#saveRDS(zomato,file="zomato_phase2.rds")
