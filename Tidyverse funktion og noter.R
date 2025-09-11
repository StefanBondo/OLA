library(nycflights13)

View(flights)

library(tidyverse)

#Filter funktion gør så man filtere på de forskellige variable. Først skrver sit dataframe og så hvilket kolonne man vil fremklade 
filter(flights, month == 1, day == 1)
#Hvis man har flere kolonner som man godt vil kalde så kan man benytte %in%, og så c() hvor man så skriver kolonnerne man vil kalde
filter(flights, month %in% c(11,12,6))

#Arrange gør så man arranger efter ønske. Her kalder jeg at day 1 skal komme først 
arrange(flights, day)

#Her kalder jeg at det er dag 31 som skal komme først. Altså vi starte bagfra
arrange(flights, desc(day))

#select er svarende til subset. Man skriver først sit data.frame og så efter skriver hvilke kolonner man gerne vil have ud
select(flights, month, day)

#her gør jeg det omvendt så jeg tager alle kolonner bortset fra month og day
select(flights, -month, -day)

#her får jeg alle koloner fra day til time_hour smidt ud
select(flights, -(day:time_hour))

#mutate gør så man tilføjer ny kolonne til sit dataframe samt inkluder en beregning i sin kode
(mutate(flights,
       gain = dep_delay - arr_delay, #formel for at beregne differencen mellem forsinkelse på afgang og ankomst
       hours = air_time / 60, #Formel for at antal minutter i luften divideret med 60 så det bliver timer
       gain_per_hour = gain/hours, #ingen ide hvad den her formel betyder
       test = "" #den laver en ny kolonne men uden noget data i felterne
       )
#summarise bruges til opsummere en variabel/kolone herunder fx gennemsnit. Under funktionen skal man lave en mini "vektor" hvor mean funktion bliver brugt
# i det her tilgælde er der nogle NA værdier og derfor skal vi sige til R at den skal ignorere dem 
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#Denne filter funktion kan være brugbar til når man skal finde NA værdier i specifikke kolonner
filter(flights, is.na(dep_delay))

#Group_by funtion laver grupper fra ens dataframe
flights_by_month <- group_by(flights, month) # I det her eksempel har vi opdelt flyene efter hvilken måned de fløj - altså måned 1,2,3,4 - og laver om til vektor

#Herefter benytter vi summarise at finde gennemsnit på dep_delay i de forskellige måneder
summarise(flights_by_month, delay = mean(dep_delay, na.rm = TRUE))
month delay
<int> <dbl>
  1     1 10.0 
2     2 10.8 
3     3 13.2 
4     4 13.9 
5     5 13.0 
6     6 20.8 
7     7 21.7 
8     8 12.6 
9     9  6.72
10    10  6.24
11    11  5.44
12    12 16.6 

# Denne summarise funktion samler også group_by funktion i sig, så man kan gøre det hele i et trin
summarise(group_by(flights, month, day), delay = mean(dep_delay, na.rm = TRUE))

# I denne summarise funkton inkluderer også filter funktion så vi tager kun de flyver hvor DL er carrier, hvor vi efterfølgende indeler dem i grupper i måneder og så finder gennemsnit i hver måned
summarise(group_by(filter(flights, carrier == "DL"), month, day), delay = mean(dep_delay, na.rm = TRUE)). #Dette smart det samler alt kode i en funktion men det kan være svært at aflæse

# I det her eksempl er koden brækket ned i mindre dele så det er nemere at overskue
flights_dl <- filter(flights, carrier == "DL") #Først filterer vi så vi kun får rækker der har DL som carrier
flights_dl_groups <- group_by(flights_dl, month, day) #Så grupperer vi dem efter måned de fløj
summarise(flights_dl_groups, delay = mean(dep_delay, na.rm = TRUE)) #Til sidst beregner vi gennemsnit i hver måned                      

#Man kan også udlede vektorerne og så benytte %>% da denne funktion tager outputtet fra den tidligere funktion og bruger som input til den næste funktion
filter(flights, carrier == "DL") %>% # Vi bruger først filter til finde rækkerne med DL som carrier. I den første linje skriver dog hvad datasæt vi skal bruge 
  group_by(month, day) %>% #her skriver vi bare hvad vi skal gruppere på, da den allerede ved, hvilke fly det er fra filter funktioen,
  summarise(delay = mean(dep_delay, na.rm = TRUE)) #til sidst inputter vi gennemsnit formel

flights %>% #man kan også skrive datasættet allerede her og så ved den hvilket datasæt den skal bruge igennem hele funktionen
filter(carrier == "DL") %>% 
  group_by(month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))

month   day   delay
<int> <int>   <dbl>
  1     1     1 -0.0625
2     1     2  3.97  
3     1     3  6.93  
4     1     4 -0.912 
5     1     5  5.05  
6     1     6 -1.45  
7     1     7  1.60  
8     1     8 -1.04  
9     1     9 -0.612 
10     1    10 -3.31 