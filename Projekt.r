#5000 adat legyen
#data2 <- read.csv("combined1.csv")
#combined2 <- data2[sample(nrow(data), 5000), ]
#write.csv(combined2, file = "combined2.csv")
#data <- read.csv("combined2.csv")
#így választottuk ki a kisseb adatbázist, mivel a Combined1.csv tartalmazta a rengeteg értéket.

data=read.csv("https://raw.githubusercontent.com/vmajber/R/master/combined2.csv")

################# Map plotting - hitelfelvételek számának megoszlása államok szerint############
install.packages("usmap")
library(usmap)
library(ggplot2)
library(dplyr)

allam_db <- count(data, addr_state)
mapbase <- data.frame("state" = allam_db$addr_state, "Hitel_db" = allam_db$n)
gdp2018<- read.csv("gdp2018.csv")
gdp2018 <- data.frame(gdp2018)

plot_usmap(data = mapbase , regions = "state", values = "Hitel_db", lines = "white") + 
  scale_fill_continuous(low = "lightblue", high = "darkblue", name = "db", label = scales::comma ) + 
  theme(legend.position = "right") +
  labs(title = "Hitelfelvételek számának megoszlása az államokban")

#Összességében jól látható, hogy amelyik államokban magasabb a GDP, ott többször történt hitelfelvétel


################# Államok szerint a hitelöszegek mértéke ####################
ggplot(data, aes(x = addr_state, y = sum(loan_amnt/1000000000), stat = "bin")) + 
  geom_histogram(stat = "identity", binwidth=50, fill= "lightblue") +
  scale_y_continuous(name = "Hitelösszeg", label = scales::comma) +
  scale_x_discrete(name = "Államok") +
  ggtitle("Hitelösszegek nagysága millárdban \n államok szerint") +
  theme_light() +
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Kaliforniában, New Yorkban, és Texasban kiugróan magas volt a felvett hitelösszegek nagysága
# 2018-ban ennek a három államnak volt a legmagasabb a GDP-je, tehát itt is megfigyelhető ez az összefüggés

################ Hitelbesorolások eloszlása ellenőrzöttség tekintetében #############

ggplot(data, aes(x=verification_status)) +
  geom_bar(aes(fill=grade), position = "dodge") +
  ylab("Darab") +
  xlab("Ellenőrzöttségi státusz") + 
  ggtitle("Hitelbesorolások eloszlása \n Ellenőrzöttségi státusz szerint")

ggplot(data, aes(x=verification_status)) +
  geom_bar(aes(fill=grade), position = "fill") +
  ylab("Megoszlás") +
  xlab("Ellenőrzöttségi státusz") + 
  ggtitle("Hitelbesorolások eloszlása \n Ellenőrzöttségi státusz szerint")

# az ábrásól leolvasható, hogy a nem ellenőrzött hitelek inkább A, B, C besorolást kaptak, 
#míg az ellenőrzötteknél már nagyobb arányban jelennek meg az E, F, G besorolások is
# D besorolás megoszlása körübelül azonos

############### Kamat és hitelösszeg kapcsolata ##################
summary(data$rate) #min:0.0531 max:0.2459
summary(data$loan_amnt) #min:500 max:40000
model <- lm ( loan_amnt~rate , data ) 
summary ( model )

ggplot(data, aes(loan_amnt, rate)) +
  geom_jitter(alpha=0.1) +
  geom_smooth(method = "lm") +
  scale_x_continuous(name= "Hitelösszeg") +
  scale_y_continuous(name= "Kamat") +
  ggtitle("Kamat és hitelösszeg kapcsolata")

# A kamat és a hitelösszeg között enyhe pozitív kapcsolat figyelhető meg
# Akik nagyobb hitelt vettek fel, álalában magasabb kamatra számolhattak


################ Hitelfelvételek céljainak megoszlása ##############

ggplot(data, aes(x=purpose)) +
  geom_bar(fill="purple") + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_x_discrete(name= "Cél") +
  scale_y_continuous(name= "Darab") +
  ggtitle("Hitelfelvétel céljainak megoszlása")
# kimagaslóan magas a hitel rendezésére felvett hitelek száma.
# ez azért van, mert jelen esetben kedvezőbb kamatra számíthattak, és így anyagilag jobban jártak


################ Hitelfelvételek céljainak ~ besorolás ##############

ggplot(data, aes(purpose)) +
  geom_bar(aes(fill=purpose)) +
  facet_grid(.~grade) + 
  scale_x_discrete(name = "Cél") +
  scale_y_continuous(name= "Darab") +
  theme_light() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ggtitle("Hitelfelvételek céljainak megoszlása a besorolás szerint")
# összességében megállapítható, hogy a legtöbben A-D-ig kptak besorolást

install.packages("MASS")
library(MASS)

tbl <- table(data$grade, data$purpose)
tbl
chisq.test(tbl)
# mivel a p érték nagyon kicsi, ezért elvetem a H0-t, vagyis ez a két változó szignifikáns kapcsolatban van


#hitelbesorolás-kamat
ggplot(data,aes(grade, rate))+
  geom_jitter(alpha=0.1) +
  geom_smooth(method = "lm") +
  theme_minimal()+
  labs(title="A hitelkamat mértéke a hitelbesorolások függvényében",
       y="Hitelkamat",
       x="Hitelbesorolás")

#Pozitív kapcsolat figyelhető meg a hitelkamat mértéke és a hitelbesorolások között
#Legtöbben a kisebb kamatozású A, B, és C besorolású hiteleket választották



#hitelbesorolások eloszlása
ggplot(data,aes(grade))+
  geom_histogram(stat="count",fill="darkred")+
  theme_minimal()+
  labs(title="Hitelbesorolások eloszlása",
       y="Darab",
       x="Hitelbesorolás")
#A hitelbesorolások jobbra elnyúló eloszlásúak
#legtöbbet a B és C kategóriájúakból adtak el, az F és G kategóriásokból van a legkevesebb



#hitelösszeg-részlet
ggplot(data,aes(loan_amnt,installment))+
  geom_jitter(alpha=0.7,aes(col=grade))+
  theme_minimal()+
  labs(title="A hitelösszegek és a részletek kapcsolata",
       y="Részlet",
       x="Hitelösszeg")
## A hitelösszeg és a részlet között pozitív kapcsolat figyelhető meg
#Aki több hitelt vett fel, nagyobb részletekben fizette vissza



#home_ownership - term
ggplot(data,aes(home_ownership))+
  geom_histogram(stat="count",fill="darkblue")+
  facet_grid(.~term)+
  theme_minimal()+
  labs(title="A hitelfelvevők háztulajdonlási viszonyainak megoszlása a felvett hitel futamideje szerint",
       y="Darab",
       x="Háztulajdonlási viszony")
#Az emberek többsége 36 hónapos futamidőre vett hitel
#nagy részük bérelt ingatlanban él, vagy jelzálog van a házán.


#verification_status - loan_status
ggplot(data,aes(verification_status,loan_status))+
  geom_jitter(alpha=0.3)+
  theme_minimal()+
  labs(title="Az ellenőrzöttségi státusz és a hitel státuszának kapcsolata",
       y="Hitel státusza",
       x="Ellenőrzöttség")


# Az ábrán megfigyelhető, hogy nincs szoros kapcsolat a hitel státusza és az ellenőrzöttség között
#A hitelek nagy része a teljesen kifizetett, vagy a folyamatban lévő kategóriába tartozik




######Becslés a Besorolásra#######

#encode-olt adatbázis betöltése
data <- read.csv('https://raw.githubusercontent.com/vmajber/R/master/encoded.csv?fbclid=IwAR0vHbIJT0OXMqYrEBMJ5EdzEPn3c_y0PdOFAVfet65PiJ6_5xxNrVYN_04')
data = dplyr::select_if(data, is.numeric)
data<-data[-c(1,2)]
data <- na.omit(data)
data

data$grade <- factor(data$grade)

## Az adatbázis beolvasása után kiszűrjük a szöveget tartalmazó oszlopokat ##
## Ezt követően töröljük az első két oszlopot, amelyek nem szükségesek az összefüggések vizsgálatához ##
## Töröljük továbbá a hiányzó értékeket tartalmazó sorokat ##
## A vizsgálat tárgyát képező grade (hitelbesorolás) változót factor típusként értelmezzük ##


set.seed(100)
trainingRows <- sample(1:nrow(data), 0.7*nrow(data))
training <- data[trainingRows, ]
test <- data[-trainingRows, ]

## Létrehozzuk a tanuló-, és tesztadatokat ##

library(nnet)
multin <- multinom(grade ~ ., data=training) # multinom Model
summary (multin)

## Felépítjük a multinomiális modell alapját fókuszban a grade(hitelbesorolás) változóval ##


predicted_scores <- predict (multin, test, "probs")
predicted_class <- predict (multin, test)

## Előrejelzést végzünk a tesztadatokon ##


table(predicted_class, test$grade)

mean(as.character(predicted_class) != as.character(test$grade))

## Megalkotjuk az confusionMatrixot és kiszámítjuk a félreosztályozási hibát ##