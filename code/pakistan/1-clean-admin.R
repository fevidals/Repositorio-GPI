#--------------------------------------------------------------------------------------------------------------
# all loaded packages should come here
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
  library(lubridate)
})

#--------------------------------------------------------------------------------------------------------------
# read raw data
#--------------------------------------------------------------------------------------------------------------
pak_unit <- readRDS("data/out/pak-unit-clean.RDS")
pak_admin_n17 <- read_dta("data/in/pakistan/07_Processed Data/b_Second Stage Processing/Police Administrative Data/Crime Incident Data (FIRs)/Nankana_2017.dta")
pak_admin_n18 <- read_dta("data/in/pakistan/07_Processed Data/b_Second Stage Processing/Police Administrative Data/Crime Incident Data (FIRs)/Nankana_2018.dta")
pak_admin_n19 <- read_dta("data/in/pakistan/07_Processed Data/b_Second Stage Processing/Police Administrative Data/Crime Incident Data (FIRs)/Nankana_2019.dta")
pak_admin_s17 <- read_dta("data/in/pakistan/07_Processed Data/b_Second Stage Processing/Police Administrative Data/Crime Incident Data (FIRs)/Sheikhupura_2017.dta")
pak_admin_s18 <- read_dta("data/in/pakistan/07_Processed Data/b_Second Stage Processing/Police Administrative Data/Crime Incident Data (FIRs)/Sheikhupura_2018.dta")
pak_admin_s19 <- read_dta("data/in/pakistan/07_Processed Data/b_Second Stage Processing/Police Administrative Data/Crime Incident Data (FIRs)/Sheikhupura_2019.dta")

# basic cleaning
pak_admin_s18 <- 
  pak_admin_s18 %>% 
  rename(
    Position = پوزیشن,
    TotalZimiyaat = ٹوٹلضمنیات,
    UnknownAccused = نامعلومملزمان,
    Accused = ملزمان,
    Witnesses = گواہان,
    Date = تاریخوقت,
    RateNumber = رپٹنمبر,
    Policestation = تھانہ,
    CrimeCode = کیفیتجرم,
    Informer = ناماطلاعدہندہ,
    FIRNumber = FIRنمبر,
    Number = نمبر) %>% 
  mutate(
    Mouza = if_else(Mouza == "", Moaza, Mouza),
    Policestation = if_else(Policestation == "اے ڈویژن", "A Division" , Policestation),
    Policestation = if_else(Policestation == "بھکھی", "Bhikhi", Policestation),
    Policestation = if_else(Policestation == "بی ڈویژن", "B Division" , Policestation),
    Policestation = if_else(Policestation == "سٹی فاروق   آباد", "City Farooqabad" , Policestation),
    Policestation = if_else(Policestation == "سٹی مرید کے", "City Mureedke" , Policestation),
    Policestation = if_else(Policestation == "فیکٹری ایریا", "Factory Area" , Policestation),
    Policestation = if_else(Policestation == "فیروز والہ", "Feroz wala" , Policestation),
    Policestation = if_else(Policestation == "ہائوسنگ کالونی", "Housing Colony" , Policestation),
    Policestation = if_else(Policestation == "خانقاں ڈوگراں", "Khanqah Dograh" , Policestation),
    Policestation = if_else(Policestation == "مانا  نوالہ", "Mananwala", Policestation),
    Policestation = if_else(Policestation == "نارنگ", "Narang", Policestation),
    Policestation = if_else(Policestation == "صدر فاروق آباد", "Sadar Farooqabad" , Policestation),
    Policestation = if_else(Policestation == "صدر مرید کے", "Sadar Mureedke" , Policestation),
    Policestation = if_else(Policestation == "صدر شیخوپورہ", "Sadar Sheikhupura", Policestation),
    Policestation = if_else(Policestation == "صفدر آباد", "Safdarabad", Policestation),
    Policestation = if_else(Policestation == "شرقپور شریف", "Sharaqpur Shareef", Policestation),
    Policestation = if_else(Policestation == "سٹیفاروقآباد", "Cityfarooqabad", Policestation),
    Policestation = if_else(Policestation == "مانانوالہ", "Mananwala", Policestation))

pak_admin_s19 <- 
  pak_admin_s19 %>% 
  rename(
    Position = پوزیشن,
    TotalZimiyaat = ٹوٹلضمنیات,
    UnknownAccused = نامعلومملزمان,
    Accused = ملزمان,
    Witnesses = گواہان,
    Date = تاریخوقت,
    RateNumber = رپٹنمبر,
    Policestation = تھانہ,
    CrimeCode = کیفیتجرم,
    Informer = ناماطلاعدہندہ,
    FIRNumber = FIRنمبر,
    Number = نمبر) %>% 
  mutate(
    Mouza = if_else(Mouza == "", Moaza, Mouza),
    Policestation = if_else(Policestation == "اے ڈویژن", "A Division" , Policestation),
    Policestation = if_else(Policestation == "بھکھی", "Bhikhi", Policestation),
    Policestation = if_else(Policestation == "بی ڈویژن", "B Division" , Policestation),
    Policestation = if_else(Policestation == "سٹی فاروق   آباد", "City Farooqabad" , Policestation),
    Policestation = if_else(Policestation == "سٹی مرید کے", "City Mureedke" , Policestation),
    Policestation = if_else(Policestation == "فیکٹری ایریا", "Factory Area" , Policestation),
    Policestation = if_else(Policestation == "فیروز والہ", "Feroz wala" , Policestation),
    Policestation = if_else(Policestation == "ہائوسنگ کالونی", "Housing Colony" , Policestation),
    Policestation = if_else(Policestation == "خانقاں ڈوگراں", "Khanqah Dograh" , Policestation),
    Policestation = if_else(Policestation == "مانا  نوالہ", "Mananwala", Policestation),
    Policestation = if_else(Policestation == "نارنگ", "Narang", Policestation),
    Policestation = if_else(Policestation == "صدر فاروق آباد", "Sadar Farooqabad" , Policestation),
    Policestation = if_else(Policestation == "صدر مرید کے", "Sadar Mureedke" , Policestation),
    Policestation = if_else(Policestation == "صدر شیخوپورہ", "Sadar Sheikhupura", Policestation),
    Policestation = if_else(Policestation == "صفدر آباد", "Safdarabad", Policestation),
    Policestation = if_else(Policestation == "شرقپور شریف", "Sharaqpur Shareef", Policestation),
    Policestation = if_else(Policestation == "سٹیفاروقآباد", "Cityfarooqabad", Policestation),
    Policestation = if_else(Policestation == "مانانوالہ", "Mananwala", Policestation),
    Policestation = if_else(Policestation == "" & Beat != "", "Sharaqpur Shareef", Policestation))

pak_admin_s <- 
  pak_admin_s17 %>% 
  bind_rows(pak_admin_s18) %>% 
  bind_rows(pak_admin_s19) %>% 
  select(Policestation, Date, Beat, CrimeCode) %>% 
  mutate(
    Policestation = if_else(Policestation == "سٹیفاروقآباد", "Cityfarooqabad" ,Policestation),
    Policestation = if_else(Policestation == "مانانوالہ", "Mananwala", Policestation),
    Policestation = str_replace_all(Policestation, "\\s+", ""),
    Policestation = str_to_sentence(Policestation),
    Beat = str_replace_all(Beat, "\\s+", ""),
    Beat = str_to_sentence(Beat),
    Beat = if_else(Beat %in% c("13Bhikhiroad", "Bhikhiroad","Habibcolony","28Habibcolony","20Churigaran") & Policestation=="Adivision", "3Bustibalochan", Beat),
    Beat = if_else(Beat %in% c("Qilasheikhupura","Datashahjamalroad","Landabazar","Mobilemarkeet","17Landabazar","19Mobilemarkeet") & Policestation=="Adivision", "3Bustibalochan", Beat),
    Beat = if_else(Beat %in% c("Motorcyclemarkeet", "Sargodharoad","38Sargodharoad","Shareefplaza","Lahoreroad") & Policestation=="Adivision", "3Bustibalochan", Beat),
    Beat = if_else(Beat %in% c("Alipark", "Capripark","Qazipark","32Alipark") & Policestation=="Adivision", "6Sultanpura", Beat),
    Beat = if_else(Beat %in% c("Battichowk","29Battichowk","Sabzimandi","26Sabzimandi","Sharqpurchowk","21Jughiyanmachine") & Policestation=="Adivision", "7Peerbahaarshah", Beat),
    Beat = if_else(Beat %in% c("Sharqpurroad","Sharqpoorroad","Tasveermehalroad","36Sharqpoorroad")& Policestation=="Adivision", "7Peerbahaarshah", Beat),
    Beat = if_else(Beat %in% c("Civilhospital","Civillines","23Civilquartersroad","Ghangroad","15Ghangroad","Sadiqyacolony","16Sadiqyacolony") & Policestation=="Adivision", "1Civillines", Beat),
    Beat = if_else(Beat %in% c("Shamiroad" ,"Stadiumpark","Zaraifarm","24Stadiumpark","27Civilhospital") & Policestation=="Adivision", "1Civillines", Beat),
    Beat = if_else(Beat %in% c("10Arianwala","Faisalabadroad","Arianwala","Gulstanpark","Liaqatschool","Rafaqatabad") & Policestation=="Adivision", "4Mohallaahmadpura", Beat),
    Beat = if_else(Beat %in% c("Hanifpark","Jameeltown","37Jameeltown","Highwaycolony") & Policestation=="Adivision", "8Mohallarehmanpura", Beat),
    Beat = if_else(Beat %in% c("Iqbalpark","14Iqbalpark","Jahangiraabad","Jinnahpark","Taxistand") & Policestation=="Adivision", "2Jinnahpark", Beat),
    Beat = if_else(Beat %in% c("Khalidroad","Rasoolpura","Spchowk","Tariqroad","11Tariqroad","Damoanaroad","25Damoanaroad") & Policestation=="Adivision", "5Khalidroad", Beat),
    Beat = if_else(Beat == "7.Jandialaroad", "6.Jandialaroad", Beat),
    Beat = if_else(Beat == "1Civillines" & Policestation=="Bdivision", "2.Civilline", Beat),
    Beat = if_else(Beat %in% c("Bahoman","Bahooman", "Baoman","4Chakrasala"), "Bahomaan", Beat),
    Beat = if_else(Beat %in% c("Bahrianwala","Bahriawala", "Barianwala", "Bariawala","Bariwala", "Briarawala") & Policestation=="Bhikhi", "Bhahriawala", Beat),
    Beat = if_else(Beat == "Bhi" & Policestation=="Bhikhi", "Bhikhi", Beat),
    Beat = if_else(Beat %in% c("Bahoman","Bahooman", "Baoman"), "Bahomaan", Beat),
    Beat = if_else(Beat %in% c("Bahrianwala","Bahriawala", "Barianwala", "Bariawala","Bariwala", "Briarawala") & Policestation=="Bhikhi", "Bhahriawala", Beat),
    Beat = if_else(Beat %in% c("Kariawala","Kharianwala","Khariawala","Khariawala") & Policestation=="Bhikhi", "Khariwala", Beat),
    Beat = if_else(Beat %in% c("Mehmowali","Mianathada","Nawakot","Nawalkot","Nawankot","Sikandarabad") & Policestation=="Bhikhi", "Mahmowala", Beat),
    Beat = if_else(Beat == "Ayawirkan" & Policestation=="Bhikhi", "Kadlathi", Beat),
    Beat = if_else(Beat == "Gulahwatwan" & Policestation=="Bhikhi", "Farozwattowan", Beat),
    Beat = if_else(Beat %in% c("Chakock","Chakok") & Policestation=="Bhikhi", "Malowal", Beat),
    Beat = if_else(Beat == "Jevanpura" & Policestation=="Bhikhi", "Jiwanpura", Beat),
    Beat = if_else(Beat == "1    Shahabad", "Shahabad", Beat),
    Beat = if_else(Beat == "9.Wandaladialshah" & Policestation == "Ferozwala", "8.Wandaladialshah", Beat),
    Beat = if_else(Beat == "Bhulaybanewal" & Policestation == "Ferozwala", "3.Kachehriferozwala", Beat),
    Beat = if_else(Beat %in% c("Alapur","Alapurmuzakotlibaryar","Alapursaidanmouzarajpura") & Policestation=="Narang", "Alapursaidan", Beat),
    Beat = if_else(Beat %in% c("Hatcherkotlimiyani","Hatchermouzakotrajpotan") & Policestation=="Narang", "Hatcher", Beat),
    Beat = if_else(Beat == "Bhianwala" & Policestation=="Sadarmureedke" & Policestation=="Sadarmureedke", "Bhiyanwala", Beat),
    Beat = if_else(Beat %in% c("Mubarakabad","Zafarpark")  & Policestation=="Sadarmureedke", "Mubarkabad", Beat),
    Beat = if_else(Beat %in% c("Nangalsahdan", "Nangalisah","Nangalisah","Shahzadtown")  & Policestation=="Sadarmureedke", "Nangalsahdhan", Beat),
    Beat = if_else(Beat %in% c("Khatyalavirkan","Rakhbaolimannoabad") & Policestation=="Sadarmureedke", "Sekhumkhatyalavirkan", Beat),
    Beat = if_else(Beat == "2.Bhooyewaal" & Policestation=="Sharaqpurshareef", "2.Bhoywal", Beat),
    Beat = if_else(Beat == "3.Ghaazipur" & Policestation=="Sharaqpurshareef", "3.Gazipur", Beat),
    Beat = if_else(Beat == "5.Taridaywali" & Policestation=="Sharaqpurshareef", "5.Traidywali", Beat),
    Beat = if_else(Beat == "6.Dhaamkay" & Policestation=="Sharaqpurshareef", "6.Dhamky", Beat),
    Beat = if_else(Beat == "7.Fatowala" & Policestation=="Sharaqpurshareef", "7.Fatuwala", Beat),
    Beat = if_else(Beat == "9.Marhbhangoowan" & Policestation=="Sharaqpurshareef", "9.Marhbhangwa", Beat),
    Policestation = if_else(Policestation=="سٹیفاروقآباد", "Cityfarooqabad", Policestation),
    Policestation = if_else(Policestation=="مانانوالہ", "Mananwala", Policestation),
    Policestation = if_else(Policestation=="" & Beat!="", "Bhikhi", Policestation)) %>% 
  filter(Policestation!="" & Beat!="") %>% 
  select(
    beat = Beat,
    date = Date,
    policestation = Policestation,
    crimecode = CrimeCode)

pak_admin_n <- 
  pak_admin_n17 %>% 
  bind_rows(pak_admin_n18) %>% 
  bind_rows(pak_admin_n19) %>% 
  mutate(
    policestation = if_else(policestation == "سٹیسانگلہہل", "Citysanglahill", policestation),
    policestation = if_else(policestation == "صدرسانگلہہل", "Sadarsanglahill", policestation),
    policestation = if_else(policestation == "صدر سانگلہ ہل", "Sadar Sangla Hill", policestation),
    policestation = if_else(policestation == "بڑا گھر", "Baraghar", policestation),
    policestation = if_else(policestation == "سٹی سانگلہ ہل", "City Sangla Hill", policestation),
    policestation = if_else(policestation == "سٹی شاہکوٹ", "City Shahkot", policestation),
    policestation = if_else(policestation == "سٹی ننکانہ", "City Nankana", policestation),
    policestation = if_else(policestation == "سید والہ", "Syedwala", policestation),
    policestation = if_else(policestation == "صدر شاہکوٹ", "Sadar Shahkot", policestation),
    policestation = if_else(policestation == "صدر ننکانہ", "Sadar Nankana", policestation),
    policestation = if_else(policestation == "فیض آباد", "Faizabad", policestation),
    policestation = if_else(policestation == "مانگٹانوالہ", "Mangtanwala", policestation),
    policestation = if_else(policestation == "واربرٹن", "Warburton", policestation),
    
    policestation = str_replace_all(policestation, "\\s+", ""),
    policestation = str_to_lower(policestation),
    policestation = str_to_sentence(policestation),
    
    beat = if_else(beat == "", Beat, beat),
    beat = str_replace_all(beat, "\\s+", ""),
    beat = str_to_sentence(beat),
    beat = if_else(beat == "Baraghar","1-Qasbabarrghar", beat),
    beat = if_else(beat %in% c("Barkhurdarkichauki","Bacheki","Demoanakalan"),"3-Barkhurdar", beat),
    beat = if_else(beat == "Kotnandar","4-Kotnaamdar", beat),
    beat = if_else(beat == "Cheendpur","2-Chandpur", beat),
    beat = if_else(beat == "Baleela","2-Moh.Balila", beat),
    beat = if_else(beat == "Kyarasahib","3-Moh.Kyarasahib", beat),
    beat = if_else(beat == "Muhallahkyarasahib","3-Moh.Kyarasahib", beat),
    beat = if_else(beat == "Muhallahbaleela","2-Moh.Balila", beat),
    beat = if_else(beat == "Housingcolony","1-Housingcolony", beat),
    beat = if_else(beat == "Purananankana","4-Purananankana", beat),
    beat = if_else(beat == "Ghalamandisangla", "1-Qasbasangla", beat),
    beat = if_else(beat == "Chokhianwala","2-Chokhianwala", beat), 
    beat = if_else(beat == "Mirzapur","1-Mirzapour", beat), 
    beat = if_else(beat == "Munianwala","2-Muniyanwala", beat), 
    beat = if_else(beat == "Nawankot","4-Nawankot", beat), 
    beat = if_else(beat == "Qasbafaizabad","5-Qasbafaizabad", beat), 
    beat = if_else(beat == "Saleempurkacha","1-Saleempurkacha", beat), 
    beat = if_else(beat == "Zaildarbaroad","3-Chakno.50Towana,Darbarroad", beat), 
    beat = if_else(beat %in% c(".Jislani","Jaslani"), "2.Jislani", beat), 
    beat = if_else(beat %in% c("1-Housingcolony","1.Marrblochan","3.Badhomali") & policestation=="Citysanglahill", "", beat), 
    beat = if_else(beat %in% c("1-Islamneghar") & policestation=="Cityshahkot", "", beat), 
    beat = if_else(beat %in% c("2-Mouhallahiqbalpura","3-Mouhallahahmadabad") & policestation=="Sadarsanglahill", "", beat), 
    beat = if_else(beat %in% c("3-Barkhurdar","4-Chakno.87Meerpur","4-Kotnaamdar") & policestation=="Sadarshahkot", "", beat), 
    beat = if_else(beat %in% c("4-Purananankana") & policestation=="Citysanglahill", "", beat), 
    beat = if_else(beat %in% c("Abianwala","Abianwalazail"), "6-Abianwala", beat), 
    beat = if_else(beat %in% c("Alijudge","6-Kothakim"), "5-Alijajbanglabeat", beat), 
    beat = if_else(beat %in% c("Badumalhi"), "3.Badhomali", beat), 
    beat = if_else(beat %in% c("Borala"), "5-Borala", beat), 
    beat = if_else(beat %in% c("Budhazail"), "8-Budhabeat", beat), 
    beat = if_else(beat %in% c("Chak17Karyalzail","Chowk17Karyal","Karyal17Zail"), "2-Chak.No.17", beat), 
    beat = if_else(beat %in% c("Chak5Zail","Chowki5Chakzail"), "1-Chak.5", beat), 
    beat = if_else(beat %in% c("Cheemkaysaiyan"), "6-Chemekysahiyanbeat", beat), 
    beat = if_else(beat %in% c("Chokiwazirpur","Wazirpur","Colonyshaukatabad"), "4-Wazirpur", beat), 
    beat = if_else(beat %in% c("Chowkimuhammadpura"), "4-Muhmmadpura", beat), 
    beat = if_else(beat %in% c("Dharwali"), "2-Dahrowali", beat), 
    beat = if_else(beat %in% c("Fareedabad"), "3-Fareedabadbeat", beat), 
    beat = if_else(beat %in% c("Fatahdarya"), "1.Fatehdarya", beat), 
    beat = if_else(beat %in% c("Hallasyedan","Bongilalo Thathafatehchand "), "4-Halasyedanbeat", beat), 
    beat = if_else(beat %in% c("Imampurnakudarchak89","Chak90"), "2-Chakno.89Imampurnakodar", beat), 
    beat = if_else(beat %in% c("Islamnagar"), "1-Islamneghar", beat), 
    beat = if_else(beat %in% c("Jeewanpura", "Merajcolony", "3-Marajcolony, Jiwanpura "), "3-Marajcolony,Jiwanpura", beat), 
    beat = if_else(beat %in% c("Khaipwali","Khaipwalizail"), "4-Khepwaliabeat", beat), 
    beat = if_else(beat %in% c("Kotlakahloon","Katiaanwali"), "6-Kotlakahloon", beat), 
    beat = if_else(beat %in% c("Machrala","Kothussain"), "3.Machrala", beat), 
    beat = if_else(beat %in% c("Mahrpur","Ththapadhyar","Hebokybala/Mambar"), "2-Meharpurbeat", beat), 
    beat = if_else(beat %in% c("Marhbalochan"), "1.Marrblochan", beat), 
    beat = if_else(beat %in% c("Marrar","5.Marr","4.Marr"), "5.Mararh", beat), 
    beat = if_else(beat %in% c("Meeranpur"), "4.Miranpur", beat), 
    beat = if_else(beat %in% c("Morkhunda","Bongaangalan"), "3-Morekhunda", beat), 
    beat = if_else(beat %in% c("Pairaywali","Pairaywalizail"), "3-Perewalibeat", beat), 
    beat = if_else(beat %in% c("Panvan"), "3-Panwaan", beat), 
    beat = if_else(beat %in% c("Pindorian"), "2.Pandorian", beat), 
    beat = if_else(beat %in% c("Sathiali","5.Sathyali"), "4.Sathiali", beat), 
    beat = if_else(beat %in% c("Syedwala"), "1-Qasbasyedwalabeat", beat), 
    beat = if_else(beat %in% c("Takhtuwala","Takhtuwalazail"), "5-Takhtowali", beat), 
    beat = if_else(beat %in% c("Thattaesa","Thattaesazail"), "7-Thathaesa", beat), 
    beat = if_else(beat %in% c("Warburton"), "5.Qasbawarburton", beat), 
    beat = if_else(str_detect(beat, "Larhka"), "5-Lorrka", beat),
    beat = if_else(str_detect(beat, "5-Unioncouncillarhaka"), "5-Lorrka", beat),
    beat = if_else(str_detect(beat, "5-Larhka"), "5-Lorrka", beat),
    beat = if_else(str_detect(beat, "3-Marajcolony,"), "3-Marajcolony,Jiwanpura", beat),
    beat = if_else(str_detect(beat, "Bongilalo"), "4-Halasyedanbeat", beat),
    beat = if_else(str_detect(beat, "Chak8"), "1-Chakno.82Rasoolpurarrian", beat),
    beat = if_else(str_detect(beat, "Mirpurchak"), "4-Chakno.87Meerpur", beat),
    
    policestation = if_else(policestation=="سٹیننکانہ", "Citynankana", policestation),
    policestation = if_else(policestation=="صدرسانگلہہل", "Sadarsanglahill", policestation),
    policestation = if_else(policestation=="صدرننکانہ", "Sadarnankana", policestation),
    
    beat = str_replace_all(beat, "\\s+", ""),
    beat = str_to_lower(beat),
    beat = str_to_sentence(beat)) %>% 
  select(beat, date, policestation, crimecode) 

pak_admin <- 
  bind_rows("Nankana" = pak_admin_n, "Sheikhupura" = pak_admin_s, .id = "district") %>% 
  mutate(
    crime = case_when(
      grepl(c("349|350|351|352|353|354|355|356|357|358|186|149|148|147|392|393|394"),crimecode) ~ "Assault or Criminal Force",
      grepl(c("324"),crimecode) ~ "Attempted Murder",
      grepl(c("302|396|322"),crimecode) ~ "Murder",
      grepl(c("337|336|334|332"),crimecode) ~ "Hurt",
      grepl(c("359|360|361|362|363|364|365|366|367|368|369|370|374|342|341"),crimecode) ~ "Kidnapping/Abduction",
      grepl(c("381A"),crimecode) ~ "Vehicle theft",
      grepl(c("458|457|380|456|454|412|411|382|381|457|379|378|380|381397|395|391|447|448|453|456|462|399|402|452"),crimecode) ~ "Theft/Criminal Trespass or Dacoity",
      grepl(c("386|385|384"),crimecode) ~ "Extortion",
      grepl(c("471|420|468|471|419|418|417|489"),crimecode) ~ "Fraud and Counterfeit/Forgery",
      grepl(c("141|142|144|145|146|147|148|149|150|151|153|154|155|156|157|158|159"), crimecode) ~ "Rioting/Unlawful Assembly",
      grepl(c("295|296|297|298"),crimecode) ~ "Offence relating to religion",
      grepl(c("354|366|375|377|498|509|274|275|338|376"),crimecode) ~ "Gender Violence and Harrassment",
      grepl(c("493|494|495|496|497"),crimecode) ~ "Offences related to marriage",
      grepl(c("CNSA|PHO|3/4/4/79|510|Drug"), crimecode) ~ "Drug and Intoxication related Offences",
      grepl(c("506|507"),crimecode) ~ "Criminal Intimidation/Threatening",
      grepl(c("AO"),crimecode) ~ "Arms Ordinance violation"),
    crime = ifelse(is.na(crime) & !is.na(crimecode),"Other Crimes",crime))

pak_admin_m <- 
  pak_admin %>% 
  mutate(
    # the following classifications were borrowed from the team's code
    aother_num_nonviolent = if_else(crime == "Arms Ordinance violation", 1, 0),
    aaggassault_num = if_else(crime == "Assault or Criminal Force", 1, 0),
    aaggassault_num = if_else(crime == "Attempted Murder", 1, aaggassault_num),
    aother_num_nonviolent = if_else(crime == "Criminal Intimidation/Threatening", 1, aother_num_nonviolent),
    aother_num_nonviolent = if_else(crime == "Drug and Intoxication related Offences", 1, aother_num_nonviolent),
    aother_num_nonviolent = if_else(crime == "Extortion", 1, aother_num_nonviolent),
    aother_num_nonviolent = if_else(crime == "Fraud and Counterfeit/Forgery", 1, aother_num_nonviolent),
    asexual_num = if_else(crime == "Gender Violence and Harrassment" , 1, 0),
    asimpleassault_num = if_else(crime == "Hurt", 1, 0),
    aother_num_violent = if_else(crime == "Kidnapping/Abduction", 1, 0),
    amurder_num = if_else(crime == "Murder", 1, 0),
    aother_num_nonviolent = if_else(crime == "Offence relating to religion", 1, aother_num_nonviolent),
    adomestic_phys_num = if_else(crime == "Offences related to marriage", 1, 0),
    aother_num = if_else(crime == "Other Crimes", 1, 0),
    asimpleassault_num = if_else(crime == "Rioting/Unlawful Assembly", 1, asimpleassault_num),
    aarmedrob_num = if_else(crime == "Theft/Criminal Trespass or Dacoity", 1, 0),
    aburglary_num = if_else(crime == "Vehicle theft", 1, 0)) %>% 
  mutate(date_clean = parse_date_time(date, orders = c("dmY_HMp", "mdY_HM", "dmY_HM", "dmy", "Ymd_HMS"))) %>% 
  filter(!is.na(date_clean)) %>% 
  mutate(wave = if_else(date_clean >= as.Date("2019-03-01"), "endline", "baseline"),
         n_days_wave = case_when(
           wave == "baseline" ~ as.numeric(as.Date("2019-03-01") - as.Date("2017-01-01")),
           wave == "endline" ~ as.numeric(as.Date("2019-11-12") - as.Date("2019-03-01")),
           TRUE ~ NA_real_)) %>% 
  filter(!is.na(wave)) %>% 
  mutate(stations = policestation, beats = beat) %>% 
  group_by(policestation, beat, wave) %>%
  summarise(
    aarmedrob_num = sum(aarmedrob_num, na.rm = TRUE) / n_days_wave,
    aaggassault_num = sum(aaggassault_num, na.rm = TRUE) / n_days_wave,
    asimpleassault_num = sum(asimpleassault_num, na.rm = TRUE) / n_days_wave,
    asexual_num = sum(asexual_num, na.rm = TRUE) / n_days_wave,
    adomestic_phys_num = sum(adomestic_phys_num, na.rm = TRUE) / n_days_wave,
    amurder_num = sum(amurder_num, na.rm = TRUE) / n_days_wave,
    aother_num_violent = sum(aother_num_violent, na.rm = TRUE) / n_days_wave,
    aburglary_num = sum(aburglary_num, na.rm = TRUE) / n_days_wave,
    aother_num_nonviolent = sum(aother_num_nonviolent, na.rm = TRUE) / n_days_wave) %>% 
  ungroup() %>%
  filter(!is.na(wave)) %>% 
  distinct(policestation, beat, wave, .keep_all = TRUE) %>%
  pivot_wider(names_from = wave, values_from = aarmedrob_num:aother_num_nonviolent, id_cols = c(policestation, beat)) %>% 
  rename_at(vars(ends_with("_endline")), ~str_replace(., "_endline", "")) %>% 
  rename(stations = policestation, beats = beat) %>% 
  right_join(pak_unit %>% filter(!is.na(Z_common)), by = c("stations", "beats")) %>% 
  select(
    Z_common,
    Z_alt,
    Z_control,
    Z,
    Z_multistage_assignment_prob,
    stations, 
    beats,
    
    aarmedrob_num,
    aaggassault_num,
    asimpleassault_num,
    asexual_num,
    adomestic_phys_num,
    amurder_num,
    aother_num_violent,
    aburglary_num,
    aother_num_nonviolent,
    
    aarmedrob_num_baseline,
    aaggassault_num_baseline,
    asimpleassault_num_baseline,
    asexual_num_baseline,
    adomestic_phys_num_baseline,
    amurder_num_baseline,
    aother_num_violent_baseline,
    aburglary_num_baseline,
    aother_num_nonviolent_baseline) 

saveRDS(pak_admin_m,  file = "data/out/pak-admin-clean.RDS")
