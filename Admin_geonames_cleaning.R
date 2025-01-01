
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(tidyr)
library(writexl)

# Read and clean geonames Admin data 
Admin <- readxl::read_excel("C:/Users/TOURE/Documents/REPOSITORIES/admin/AFRO_admin _data_vf.xlsx", sheet = "Admin") 

# Admin <- Admin |>
#   (/(df) if (!"NameSettlement" %in% names(df)) mutate(df, NameSettlement = df$Village) else df)()

# Add Country column if missing
if (!"Country" %in% names(Admin)) {
  Admin <- Admin %>% mutate(Country = "GHA")
}

Admin <- Admin %>%
  mutate(Province =toupper(Province),
         District =toupper(District)) |> 
  mutate(
    Country = case_when(
      Country == "GAM" ~ "GAMBIA",
      Country == "GHA" ~ "GHANA",
      Country == "ALG" ~ "ALGERIA",
      Country == "ETH" ~ "ETHIOPIA",
      Country == "ANG" ~ "ANGOLA",
      Country == "BEN" ~ "BENIN",
      Country == "BUI" ~ "BURUNDI",
      Country == "BURKINA_FASO" ~ "BURKINA FASO",
      Country == "BFA" ~ "BURKINA FASO",
      Country == "BWA" ~ "BOTSWANA",
      Country == "CHD" ~ "CHAD",
      Country == "CAE" ~ "CAMEROON",
      Country == "CIV" ~ "COTE D IVOIRE",
      Country == "COG" ~ "CONGO",
      Country == "GUI" ~ "GUINEA",
      Country == "KEN" ~ "KENYA",
      Country == "LIB" ~ "LIBERIA",
      Country == "MAL" ~ "MALI",
      Country == "MAU" ~ "MAURITANIA",
      Country == "MDG" ~ "MADAGASCAR",
      Country == "MLW" ~ "MALAWI",
      Country == "MOZ" ~ "MOZAMBIQUE",
      Country == "NIE" ~ "NIGERIA",
      Country == "NIG" ~ "NIGER",
      Country == "RCA" ~ "CENTRAL AFRICAN REPUBLIC",
      Country == "RDC" ~ "DEMOCRATIC REPUBLIC OF THE CONGO",
      Country == "RWA" ~ "RWANDA",
      Country == "SEN" ~ "SENEGAL",
      Country == "SIL" ~ "SIERRA LEONE",
      Country == "SSUD" ~ "SOUTH SUDAN",
      Country == "TNZ" ~ "UNITED REPUBLIC OF TANZANIA",
      Country == "TOG" ~ "TOGO",
      Country == "UGA" ~ "UGANDA",
      Country == "ZIM" ~ "ZIMBABWE",
      Country == "ZMB" ~ "ZAMBIA",
      TRUE ~ Country
    ),
    District = case_when(
      Country	=="NIGER"&	District	=="AGADEZ COMMUNE" ~	"AGADEZ",
      Country	=="NIGER"&	District	=="DIFFA COMMUNE" ~	"DIFFA",
      Country	=="NIGER"&	District	=="MAINÉ SOROA" ~	"MAINE SOROA",
      Country	=="NIGER"&	District	=="DOGON-DOUTCHI" ~	"DOGON DOUTCHI",
      Country	=="NIGER"&	District	=="FALMEYE" ~	"FALMEY",
      Country	=="NIGER"&	District	=="TAHOUA COMMUNE" ~	"TAHOUA COM",
      Country	=="NIGER"&	District	=="TAHOUA DÉPARTEMENT" ~	"TAHOUA DEP",
      Country	=="NIGER"&	District	=="MATAMAYE" ~	"MATAMÈYE",
      Country	=="NIGER"&	District	=="TIBIRI (DOUTCHI)" ~	"TIBIRI",
      Country	=="NIGER"&	District	=="NIAMEY  I" ~	"NIAMEY 1",
      Country	=="NIGER"&	District	=="NIAMEY  II" ~	"NIAMEY 2",
      Country	=="NIGER"&	District	=="NIAMEY  III" ~	"NIAMEY 3",
      Country	=="NIGER"&	District	=="NIAMEY  IV" ~	"NIAMEY 4",
      Country	=="NIGER"&	District	=="NIAMEY  V" ~	"NIAMEY 5",
      Country	=="NIGER"&	District	=="TAHOUA VILLE" ~	"TAHOUA COM",
      Country	=="NIGER"&	District	=="BALLAYARA" ~	"BALLEYARA",
      Country	=="NIGER"&	District	=="GOTHEYE" ~	"GOTHÈYE",
      Country	=="NIGER"&	District	=="OULLAM" ~	"OUALLAM",
      Country	=="NIGER"&	District	=="TILLABÉRY" ~	"DS TILLABERI",
      Country	=="NIGER"&	District	=="BELBÉDJI" ~	"BELBEDJI",
      Country	=="NIGER"&	District	=="TCHIROZÃ‰RINE" ~	"TCHIROZÉRINE",
      Country	=="CAMEROON"&	District	=="BIYEM_ASSI" ~	"BIYEM ASSI",
      Country	=="CAMEROON"&	District	=="CITE_VERTE" ~	"CITE VERTE",
      Country	=="CAMEROON"&	District	=="ELIG_MFOMO" ~	"ELIG MFOMO",
      Country	=="CAMEROON"&	District	=="NANGA_EBOKO" ~	"NANGA EBOKO",
      Country	=="CAMEROON"&	District	=="NGOG_MAPUBI" ~	"NGOG MAPUBI",
      Country	=="CAMEROON"&	District	=="ABONG_MBANG" ~	"ABONG MBANG",
      Country	=="CAMEROON"&	District	=="BETARE_OYA" ~	"BETARE OYA",
      Country	=="CAMEROON"&	District	=="GAROUA-BOULAI" ~	"GAROUA BOULAI",
      Country	=="CAMEROON"&	District	=="NGUELEMENDOUGA" ~	"NGUELEMENDOUKA",
      Country	=="CAMEROON"&	District	=="KAR_HAY" ~	"KAR HAY",
      Country	=="CAMEROON"&	District	=="MAROUA1" ~	"MAROUA 1",
      Country	=="CAMEROON"&	District	=="MAROUA2" ~	"MAROUA 2",
      Country	=="CAMEROON"&	District	=="MAROUA3" ~	"MAROUA 3",
      Country	=="CAMEROON"&	District	=="CITE_DES_PALMIERS" ~	"CITE PALMIERS",
      Country	=="CAMEROON"&	District	=="NJOMBE_PENJA" ~	"NJOMBE PENJA",
      Country	=="CAMEROON"&	District	=="NEWBELL" ~	"NEW BELL",
      Country	=="CAMEROON"&	District	=="BAMENDA 3" ~	"BAMENDA",
      Country	=="CAMEROON"&	District	=="BAMENDA III" ~	"BAMENDA",
      Country	=="CAMEROON"&	District	=="KUMBOEAST" ~	"KUMBO EAST",
      Country	=="CAMEROON"&	District	=="KUMBOWEST" ~	"KUMBO WEST",
      Country	=="CAMEROON"&	District	=="GAROUA I" ~	"GAROUA 1",
      Country	=="CAMEROON"&	District	=="GAROUA II" ~	"GAROUA 2",
      Country	=="CAMEROON"&	District	=="GASHIGA" ~	"GASCHIGA",
      Country	=="CAMEROON"&	District	=="MALANTOUEN" ~	"MALENTOUEN",
      Country	=="CAMEROON"&	District	=="PENKAMICHEL" ~	"PENKA MICHEL",
      Country	=="CAMEROON"&	District	=="EKONDO_TITI" ~	"EKONDO TITI",
      Country	=="CAMEROON"&	District	=="EYUMOJOCK" ~	"EYUMODJOCK",
      Country	=="CAMEROON"&	District	=="KUMBA NORD" ~	"KUMBA",
      Country	=="CAMEROON"&	District	=="KUMBA SUD" ~	"KUMBA",
      Country	=="CAMEROON"&	District	=="KUMBA NORTH" ~	"KUMBA",
      Country	=="CAMEROON"&	District	=="KUMBA SOUTH" ~	"KUMBA",
      Country	=="CHAD"&	District	=="OUM-HADJER" ~	"OUM HADJER",
      Country	=="CHAD"&	District	=="KOUBA OLANGA5" ~	"KOUBA OLANGA",
      Country	=="CHAD"&	District	=="BAILLI" ~	"BA ILLI",
      Country	=="CHAD"&	District	=="MOURDI" ~	"MOURDI DJONA",
      Country	=="CHAD"&	District	=="OUNIANGA" ~	"OUNIANGA KEBIR",
      Country	=="CHAD"&	District	=="LAOKASSI" ~	"LAOKASSY",
      Country	=="CHAD"&	District	=="NDJAMENA CENTRE" ~	"N'DJAMENA CENTRE",
      Country	=="CHAD"&	District	=="NDJAMENA EST" ~	"N'DJAMENA EST",
      Country	=="CHAD"&	District	=="NDJAMENA NORD" ~	"N'DJAMENA NORD",
      Country	=="CHAD"&	District	=="NDJAMENA SUD" ~	"N'DJAMENA SUD",
      Country	=="CHAD"&	District	=="AMADAM" ~	"AM DAM",
      Country	=="CHAD"&	District	=="AMTIMAN" ~	"AM TIMAN",
      Country	=="CHAD"&	District	=="MANGUEIGNE" ~	"HARAZE MANGUEIGNE",
      Country	=="CHAD"&	District	=="GOZ-BEIDA" ~	"GOZ BEIDA",
      Country	=="CHAD"&	District	=="N’TIONA" ~	"N'TIONA",
      Country	=="CHAD"&	District	=="DS CHADDRA" ~	"CHADRA",
      Country	=="CHAD"&	District	=="DS MOUSSORO" ~	"MOUSSORO",
      Country	=="CHAD"&	District	=="DJEDA" ~	"DJEDDA",
      Country	=="CHAD"&	District	=="OUMHADJER" ~	"OUM HADJER",
      Country	=="CHAD"&	District	=="BAGASOLA" ~	"BAGASSOLA",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="BABOUA" ~	"BABOUA-ABBA",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="BOUAR" ~	"BOUAR-BAORO",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="CARNOT" ~	"CARNOT-GADZI",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="BOCARANGA" ~	"BOCARANGA-KOUI",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="BOGUILA" ~	"NANGA-BOGUILA",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="BOZOUM" ~	"BOZOUM-BOSSEMPTELE",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="GRIMARI" ~	"KOUANGO-GRIMARI",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="ALINDAO" ~	"ALINDAO-MINGALA",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="KEMBE" ~	"KEMBE-SATEMA",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="MOBAYE" ~	"MOBAYE-ZANGBA",
      Country	=="CENTRAL AFRICAN REPUBLIC"&	District	=="OUANGO" ~	"OUANGO-GAMBO",
      Country	== "GAMBIA" & District	== "NIAMINA DANKUNKU" ~	"NIAMNA DANKUNKU",
      Country	== "GAMBIA" &	District	== "SABAKH SANJAL" ~	"SABACH",
      Country	== "GAMBIA" &	District	== "FULLADU EAST" ~	"BASSE",
      Country	== "GAMBIA" &
        District	== "LOWER FULLADU WEST" ~	"CENTRAL RIVER Province",
      Country	== "GHANA" &
        District	== "ASANTE MAMPONG" ~	"ASANTE-MAMPONG",
      Country	== "GHANA" &
        District	== "ASOKORE MAMPONG" ~	"ASOKORE-MAMPONG",
      Country	== "GHANA" &
        District	== "ABUAKWA SOUTH" ~	"EAST AKIM - ABUAKWA SOUTH",
      Country	== "GHANA" &
        District	== "LOWER MANYA-KROBO" ~	"LOWER-MANYA-KROBO",
      Country	== "GHANA" &
        District	== "TWIFO ATI MORKWA" ~	"TWIFO ATI-MORKWA",
      Country	== "ANGOLA" &	District	== "N'HARÃŠA" ~	"NHAREA",
      Country	== "ANGOLA" &	District	== "NGONGUEMBO" ~	"GONGUEMBO",
      Country	== "ANGOLA" &	District	== "NÃ“QUI" ~	"NOQUI",
      Country	== "ANGOLA" &	District	== "PANGO-ALUQUEM" ~	"PANGO ALUQUEM",
      Country	== "ANGOLA" &
        District	== "TÃ”MBUA (EX. PORTO ALEXANDRE)" ~	"TOMBUA",
      Country	== "ANGOLA" &	District	== "UCUMA" ~	"UKUMA",
      Country	== "ANGOLA" &	District	== "UÃGE" ~	"UIGE",
      Country	== "ANGOLA" &	District	== "XÃ-MUTEBA" ~	"XA MUTEBA",
      Country	== "ANGOLA" &	District	== "NZETU" ~	"NZETO",
      Country	== "ANGOLA" &	District	== "CELA (EX. UACU-CUNGO)" ~	"CELA",
      Country	== "ANGOLA" &
        District	== "OMBADJA (EX. CUAMATO)" ~	"OMBADJA",
      Country	== "ANGOLA" &
        District	== "TCHICALA TCHOLOHANGA" ~	"TCHIKALA-TCHOLOHAN",
      Country	== "ANGOLA" &
        District	== "BUNDAS" ~	"LUMBALA NGUIMBO (BUNDAS)",
      Country	== "ANGOLA" &	District	== "AMBOIM (EX. GABELA)" ~	"AMBOIM",
      Country	== "ANGOLA" &	District	== "AMBUÃLA" ~	"AMBUILA",
      Country	== "ANGOLA" &	District	== "BAÃA FARTA" ~	"BAIA FARTA",
      Country	== "ANGOLA" &
        District	== "BUENGAS (EX. NOVA ESPERANÃ‡A)" ~	"BUENGAS",
      Country	== "ANGOLA" &	District	== "BULA-ATUMBA" ~	"BULA ATUMBA",
      Country	== "ANGOLA" &	District	== "QUIUABA-N'ZOGI" ~	"KIWABA NZOGI",
      Country	== "ANGOLA" &	District	== "SAMBA CAJÃš" ~	"SAMBA CAJU",
      Country	== "ANGOLA" &	District	== "SELES (EX. UCU SELES)" ~	"SELES",
      Country	== "ANGOLA" &	District	== "SUMBE (EX. NGUNZA)" ~	"SUMBE",
      Country	== "ANGOLA" &	District	== "CAMEIA" ~	"LUMEJE (CAMEIA)",
      Country	== "ANGOLA" &
        District	== "CATABOLA (EX. NOVA SINTRA)" ~	"CATABOLA",
      Country	== "ANGOLA" &	District	== "LÃ‰UA" ~	"LEUA",
      Country	== "ANGOLA" &	District	== "LIBOLO (EX. CALULO)" ~	"LIBOLO",
      Country	== "ANGOLA" &	District	== "LÃ“VUA" ~	"LOVUA",
      Country	== "ANGOLA" &
        District	== "BUNDAS-LUMBALA-NGUIMBO" ~	"LUMBALA NGUIMBO (BUNDAS)",
      Country	== "ANGOLA" &	District	== "CAÃLA" ~	"CAALA",
      Country	== "ANGOLA" &
        District	== "CACONGO (EX. LÃ‚NDANA)" ~	"CACONGO",
      Country	== "ANGOLA" &	District	== "DANDE (CAXITO)" ~	"DANDE",
      Country	== "ANGOLA" &
        District	== "DEMBOS-QUIBAXE" ~	"DEMBOS (QUIBAXE)",
      Country	== "ANGOLA" &	District	== "GAMBOS (EX. CHIANGE)" ~	"GAMBOS",
      Country	== "ANGOLA" &
        District	== "CUNDA-DIA-BAZE" ~	"KUNDA-DIA-BAZE",
      Country	== "ANGOLA" &	District	== "CUNHINGA (VOUGA)" ~	"CUNHINGA",
      Country	== "ANGOLA" &
        District	== "MUCABA (EX. QUINZALA)" ~	"MUCABA",
      Country	== "ANGOLA" &	District	== "MUCARI" ~	"CACULAMA (MUCARI)",
      Country	== "ANGOLA" &
        District	== "TCHIKALA TCHOLOHANG" ~	"TCHIKALA-TCHOLOHAN",
      Country	== "ANGOLA" &	District	== "CUROCA (EX. ONCOCUA)" ~	"CUROCA",
      Country	== "ANGOLA" &
        District	== "MILUNGA (SANTA CRUZ)" ~	"MILUNGA",
      Country	== "ANGOLA" &	District	== "LUENA" ~	"MOXICO (LUENA)",
      Country	== "NIGER" &	District	== "AGUIÃ‰" ~	"AGUIÉ",
      Country	== "NIGE" &	District	== "TCHIROZÃ‰RINE" ~	"TCHIROZÉRINE",
      Country	== "NIGER" &	District == "TÃ‰RA" ~	"TERA",
      Country	== "NIGER" &	District	== "GOURÃ‰" ~	"GOURÉ",
      Country	== "NIGER" &	District	== "IFÃ‰ROUANE" ~	"IFÉROUANE",
      Country	== "NIGER" &	District	== "ILLÃ‰LA" ~	"ILLÉLA",
      Country	== "NIGER" &	District	== "MATAMÃˆYE" ~	"MATAMEYE",
      Country	== "NIGER" &	District	== "FILINGUÃ‰" ~	"FILINGUE",
      Country	== "NIGER" &	District	== "KANTCHÃ‰" ~	"KANTCHÉ",
      Country	== "NIGER" &	District	== "GOTHÃˆYE" ~	"GOTHÈYE",
      Country	== "NIGER" &	District	== "JINJA CITY‰" ~	"JINJA",
      Country	== "NIGER" &	District	== "MBALE CITY" ~	"MBALE",
      Country	== "MAURITANIA" &	District	== "RIYADH" ~	"RIYAD",
      Country	== "MAURITANIA" &	District	== "LEKSEIBE" ~	"LEXEIBA",
      Country	== "MAURITANIA" &	District	== "WOMPOU" ~	"WOMPO",
      Country	== "MAURITANIA" &
        District	== "ADEL BAGHROU" ~	"ADEL BEGROU",
      Country	== "MAURITANIA" &	District	== "AKJOUJET" ~	"AKJOUJT",
      Country	== "MAURITANIA" &	District	== "BABABE" ~	"BABABÉ",
      Country	== "MAURITANIA" &
        District	== "BIR OUMGREINE" ~	"BIR MOGHREN",
      Country	== "MAURITANIA" &	District	== "BOGHE" ~	"BOGHÉ",
      Country	== "MAURITANIA" &	District	== "BARKEOL" ~	"BARKÉOLE",
      Country	== "MAURITANIA" &	District	== "CHINGUITTI" ~	"CHINGUITTY",
      Country	== "MAURITANIA" &	District	== "D_HAR" ~	"D'HAR",
      Country	== "MAURITANIA" &	District	== "BOUTILIMITT" ~	"BOUTILIMIT",
      Country	== "MAURITANIA" &	District	== "F_DERIK" ~	"F'DERICK",
      Country	== "MAURITANIA" &	District	== "GUERROU" ~	"GUÉRROU",
      Country	== "MAURITANIA" &	District	== "KANKOUSSA" ~	"KANKOSSA",
      Country	== "MAURITANIA" &	District	== "KOBENNI" ~	"KOBENI",
      Country	== "MAURITANIA" &	District	== "M_BAGNE" ~	"M'BAGNE",
      Country	== "MAURITANIA" &	District	== "M_BOUT" ~	"M'BOUT",
      Country	== "MAURITANIA" &
        District	== "MAGHTA LEHJAR" ~	"MAGTA LAHJAR",
      Country	== "MAURITANIA" &	District	== "MOUDJRIA" ~	"MOUDJÉRIA",
      Country	== "MAURITANIA" &	District	== "NEMA" ~	"NÉMA",
      Country	== "MAURITANIA" &	District	== "OUAD-NAGA" ~	"OUAD NAGA",
      Country	== "MAURITANIA" &	District	== "R_KIZ" ~	"R'KIZ",
      Country	== "MAURITANIA" &	District	== "SEILIBABY" ~	"SELIBABY",
      Country	== "MAURITANIA" &	District	== "TAMCHEKETT" ~	"TAMCHAKET",
      Country	== "MAURITANIA" &
        District	== "TEVRAGH ZEINE" ~	"TEVRAGH ZEINA",
      Country	== "MAURITANIA" &	District	== "TICHITT" ~	"TICHIT",
      Country	== "MAURITANIA" &	District	== "TIMBEDRA" ~	"TIMBÉDRA",
      Country	== "MAURITANIA" &	District	== "ZOUERATE" ~	"ZOUÉRAT",
      Country	== "MOZAMBIQUE" &	District	== "CHIÃšRE" ~	"CHIÚRE",
      Country	== "MOZAMBIQUE" &	District	== "MARÃVIA" ~	"MARÁVIA",
      Country	== "MOZAMBIQUE" &	District	== "MAÃšA" ~	"MAUA",
      Country	== "MOZAMBIQUE" &
        District	== "ALTO MOLÃ“CUÃˆ" ~	"ALTO MOLOCUE",
      Country	== "MOZAMBIQUE" &	District	== "ANGÃ“NIA" ~	"ANGONIA",
      Country	== "MOZAMBIQUE" &
        District	== "MOCÃMBOA DA PRAIA" ~	"MACIMBOA DA PRAI",
      Country	== "MOZAMBIQUE" &	District	== "MÃGOÃˆ" ~	"MÁGOÈ",
      Country	== "MOZAMBIQUE" &	District	== "GURUÃ‰" ~	"GURUE",
      Country	== "MOZAMBIQUE" &	District	== "GILÃ‰" ~	"GILÉ",
      Country	== "MOZAMBIQUE" &	District	== "NGAÃšMA" ~	"NGAÚMA",
      Country	== "ALGERIA" & District	== "EPSP ADRAR" ~	"Adrar",
      Country	== "ALGERIA" & District	== "EPSP AOULEF" ~	"Aoulef",
      Country	== "ALGERIA" &
        District	== "EPSP BADJI MOKHTAR" ~	"Bordj Badji Mokhtar",
      Country	== "ALGERIA" & District	== "EPSP REGGANE" ~	"Reggana",
      Country	== "ALGERIA" & District	== "EPSP TIMIMOUN" ~	"Timimmoun",
      Country	== "ALGERIA" & District	== "EPSP TINERKOUK" ~	"Tinerkouk",
      Country	== "ALGERIA" & District	== "EPSP ABADLA" ~	"Abadla",
      Country	== "ALGERIA" & District	== "EPSP BECHAR" ~	"Bechar",
      Country	== "ALGERIA" &
        District	== "EPSP BENI ABBES" ~	"Benni Abbes",
      Country	== "ALGERIA" &
        District	== "EPSP BENI OUNIF" ~	"Beni Ounif",
      Country	== "ALGERIA" & District	== "EPSP KERZAZ" ~	"Kerzaz",
      Country	== "ALGERIA" & District	== "EPSP KERZAZ" ~	"Kerzaz",
      Country	== "ALGERIA" & District	== "EPSP TABELBALA" ~	"Tabelbala",
      Country	== "ALGERIA" & District	== "EPSP TAGHIT" ~	"Taghit",
      Country	== "ALGERIA" & District	== "EPSP BREZINA" ~	"Breizina",
      Country	== "ALGERIA" & District	== "EPSP CHELLALA" ~	"Chellala",
      Country	== "ALGERIA" & District	== "EPSP EL BAYADH" ~	"El Baydh",
      Country	== "ALGERIA" & District	== "EPSP KHEITER" ~	"Kheiter",
      Country	== "ALGERIA" & District	== "EPSP DEBILA" ~	"Debila",
      Country	== "ALGERIA" & District	== "EPSP DEBILA" ~	"Debila",
      Country	== "ALGERIA" & District	== "EPSP DJEMAA" ~	"Djemaa",
      Country	== "ALGERIA" &
        District	== "EPSP EL MEGHAIER" ~	"El Meghaeir",
      Country	== "ALGERIA" &
        District	== "EPSP EL MEGHAIER" ~	"El Meghaeir",
      Country	== "ALGERIA" & District	== "EPSP EL OUED" ~	"El Oued",
      Country	== "ALGERIA" & District	== "EPSP EL OUED" ~	"El Oued",
      Country	== "ALGERIA" & District	== "EPSP GUEMAR" ~	"Guemar",
      Country	== "ALGERIA" & District	== "EPSP GUEMAR" ~	"Guemar",
      Country	== "ALGERIA" &
        District	== "EPSP TALEB LARBI" ~	"Taleb Arby",
      Country	== "ALGERIA" &
        District	== "EPSP TALEB LARBI" ~	"Taleb Arby",
      Country	== "ALGERIA" & District	== "EPSP BERIANE" ~	"Berriane",
      Country	== "ALGERIA" & District	== "EPSP EL MENEA" ~	"El Menea",
      Country	== "ALGERIA" & District	== "EPSP GUERRARA" ~	"Guerrara",
      Country	== "ALGERIA" & District	== "EPSP METLILI" ~	"Metlili",
      Country	== "ALGERIA" &
        District	== "EPSP BORDJ OMAR IDRISS" ~	"Borj Omar Idriss",
      Country	== "ALGERIA" &
        District	== "EPSP BORDJ-EL-HAOUESS" ~	"Borj El Haoues",
      Country	== "ALGERIA" & District	== "EPSP DEBDEB" ~	"Deb Deb",
      Country	== "ALGERIA" & District	== "EPSP DJANET" ~	"Djanet",
      Country	== "ALGERIA" & District	== "EPSP ILLIZI" ~	"Illizi",
      Country	== "ALGERIA" & District	== "EPSP IN AMENAS" ~	"In Amenas",
      Country	== "ALGERIA" & District	== "EPSP AIN SEFRA" ~	"Ain Sefra",
      Country	== "ALGERIA" & District	== "EPSP MECHERIA" ~	"Mecheria",
      Country	== "ALGERIA" &
        District	== "EPSP MEKMEN BENAMER" ~	"Mekmen benamer",
      Country	== "ALGERIA" & District	== "EPSP NAAMA" ~	"Naama",
      Country	== "ALGERIA" & District	== "EPSP EL BORMA" ~	"El Borma",
      Country	== "ALGERIA" &
        District	== "EPSP EL HADJIRA" ~	"El Hadjira",
      Country	== "ALGERIA" &
        District	== "EPSP HASSI MESSAOUD" ~	"Hassi Messaoud",
      Country	== "ALGERIA" & District	== "EPSP OUARGLA" ~	"Ouargla",
      Country	== "ALGERIA" & District	== "EPSP TOUGGOURT" ~	"Touggourt",
      Country	== "ALGERIA" &
        District	== "EPSP ABALESSA (SILET)" ~	"Abalessa",
      Country	== "ALGERIA" &
        District	== "EPSP IN GUEZZAM" ~	"In Guezzam",
      Country	== "ALGERIA" & District	== "EPSP IN MGUEL" ~	"In Amgueul",
      Country	== "ALGERIA" & District	== "EPSP IN SALAH" ~	"In Salah",
      Country	== "ALGERIA" &
        District	== "EPSP TAMENRASSET" ~	"Tamanrasset",
      Country	== "ALGERIA" & District	== "EPSP TAZROUK" ~	"Tazrouk",
      Country	== "ALGERIA" &
        District	== "EPSP TIN ZAOUATINE" ~	"Tin Zaouatine",
      Country	== "ALGERIA" &
        District	== "EPSP OUM EL ASSEL" ~	"Oum El Assel",
      Country	== "ALGERIA" & District	== "EPSP TINDOUF" ~	"Tindouf",
      Country	== "ETHIOPIA" &	District	== "Abiy Adi" ~	"Abi Adi Town",
      Country	== "ETHIOPIA" &	District	== "Adet" ~	"Naeder Adet",
      Country	== "ETHIOPIA" &	District	== "Adwa Town" ~	"Adwa Town",
      Country	== "ETHIOPIA" &	District	== "Adwa Zuria" ~	"Adwa",
      Country	== "ETHIOPIA" &	District	== "Ahiferom" ~	"Aheferom",
      Country	== "ETHIOPIA" &	District	== "Axum Town" ~	"Axum Town",
      Country	== "ETHIOPIA" &
        District	== "Laelay Maichew" ~	"Laelay Maychew",
      Country	== "ETHIOPIA" &
        District	== "Tahitay Maichew" ~	"Tahtay Mayechew",
      Country	== "ETHIOPIA" &
        District	== "Tankua Milash" ~	"Tanqua Abergele",
      Country	== "ETHIOPIA" &	District	== "Adigrat" ~	"Adigrat Town",
      Country	== "ETHIOPIA" &	District	== "Atsbi" ~	"Atsbi Wenberta",
      Country	== "ETHIOPIA" &
        District	== "Ganta Afeshum" ~	"Ganta Afeshum",
      Country	== "ETHIOPIA" &	District	== "Gulomekeda" ~	"Gulo Mekeda",
      Country	== "ETHIOPIA" &	District	== "Hawzen" ~	"Hawzen",
      Country	== "ETHIOPIA" &
        District	== "Kilte Awlaelo" ~	"Kelete Awelallo",
      Country	== "ETHIOPIA" &
        District	== "Tsaeda Emba" ~	"Saesie Tsaedamba",
      Country	== "ETHIOPIA" &	District	== "Wukro" ~	"Wukro Town",
      Country	== "ETHIOPIA" &	District	== "Adi Haki" ~	"Adhaki",
      Country	== "ETHIOPIA" &	District	== "Ayder" ~	"Ayder",
      Country	== "ETHIOPIA" &	District	== "Hawolti" ~	"Hawelti",
      Country	== "ETHIOPIA" &	District	== "Quiha" ~	"Kuha",
      Country	== "ETHIOPIA" &	District	== "Asgede" ~	"Tsegede (Tigray)",
      Country	== "ETHIOPIA" &
        District	== "Seyemti Adiyabo" ~	"Laelay Adiabo",
      Country	== "ETHIOPIA" &	District	== "Shire Town" ~	"Sheraro Town",
      Country	== "ETHIOPIA" &
        District	== "Tahitay Koraro" ~	"Tahtay Koraro",
      Country	== "ETHIOPIA" &	District	== "Tsimbla" ~	"Asgede Tsimbila",
      Country	== "ETHIOPIA" &	District	== "Degua Temben" ~	"Dega Temben",
      Country	== "ETHIOPIA" &	District	== "Enderta" ~	"Enderta",
      Country	== "ETHIOPIA" &	District	== "Samre" ~	"Saharti Samre",
      Country	== "ETHIOPIA" &	District	== "Endamekoni" ~	"Endamehoni",
      Country	== "ETHIOPIA" &	District	== "Maichew Town" ~	"Maychew Town",
      Country	== "ETHIOPIA" &	District	== "Raya Azebo" ~	"Raya Azebo",
      Country	== "CAMEROON" &	District	== "MOZOGO" ~	"MOZONGO",
      Country	== "CAMEROON" &
        District	== "CITE DES PALMIERS" ~	"CITE PALMIERS",
      Country	== "GUINEA" &	District	== "NZEREKORE" ~	"N'ZÉRÉKORÉ",
      Country	== "GUINEA" &	District	== "N'ZÃ‰RÃ‰KORÃ‰" ~	"N'ZÉRÉKORÉ",
      Country	== "COTE D IVOIRE" &
        District	== "ADJAME_PLATEAU_ATTECOUBE" ~	"ADJAME-PLATEAU-ATTECOUBE",
      Country	== "COTE D IVOIRE" &
        District	== "TREICHVILLE_MARCORY" ~	"TREICHVILLE-MARCORY",
      Country	== "COTE D IVOIRE" &
        District	== "PORT-BOUET-VRIDI" ~	"PORT BOUET-VRIDI",
      Country	== "COTE D IVOIRE" &
        District	== "BOUAKE-SUD" ~	"BOUAKE SUD",
      Country	== "COTE D IVOIRE" &	District	== "SAN-PEDRO" ~	"SAN PEDRO",
      Country	== "COTE D IVOIRE" &
        District	== "YOPOUGON-EST" ~	"YOPOUGON EST",
      Country	== "COTE D IVOIRE" &
        District	== "YOPOUGON-OUEST SONGON" ~	"YOPOUGON OUEST-SONGON",
      Country	== "COTE D IVOIRE" &
        District	== "KOUASSI KOUASSIKRO" ~	"KOUASSI-KOUASSIKRO",
      Country	== "COTE D IVOIRE" &
        District	== "COCODY BINGERVILLE" ~	"COCODY-BINGERVILLE",
      Country	== "COTE D IVOIRE" &	District	== "GAGNOA1" ~	"GAGNOA 1",
      Country	== "COTE D IVOIRE" &	District	== "M'BENGUE" ~	"MBENGUE",
      Country	== "COTE D IVOIRE" &
        District	== "BOUAKE-SUD" ~	"BOUAKE SUD",
      Country	== "COTE D IVOIRE" &	District	== "GAGNOA2" ~	"GAGNOA 2",
      Country	== "COTE D IVOIRE" &
        District	== "GRAND_LAHOU" ~	"GRAND-LAHOU",
      Country	== "COTE D IVOIRE" &
        District	== "YAKASSE_ATTOBROU" ~	"YAKASSE-ATTOBROU",
      Country	== "COTE D IVOIRE" &
        District	== "KOUASSI KOUASSIKRO" ~	"KOUASSI-KOUASSIKRO",
      Country	== "COTE D IVOIRE" &
        District	== "GRAND_BASSAM" ~	"GRAND-BASSAM",
      Country	== "COTE D IVOIRE" &
        District	== "ZOUAN_HOUNIEN" ~	"ZOUAN-HOUNIEN",
      Country	== "COTE D IVOIRE" &
        District	== "YOPOUGON-EST" ~	"YOPOUGON EST",
      Country	== "COTE D IVOIRE" &
        District	== "YOPOUGON-OUEST SONGON" ~	"YOPOUGON OUEST-SONGON",
      Country	== "COTE D IVOIRE" &
        District	== "COCODY BINGERVILLE" ~	"COCODY-BINGERVILLE",
      Country	== "COTE D IVOIRE" &
        District	== "PORT-BOUET-VRIDI" ~	"PORT BOUET-VRIDI",
      Country	== "COTE D IVOIRE" &	District	== "SAN-PEDRO" ~	"SAN PEDRO",
      Country == "COTE D IVOIRE" &
        District == "SAN-PEDRO" ~ "SAN PEDRO",
      Country	 == "GUINEA" &	District	 == "NZEREKORE"	 ~ "N'ZEREKORE",
      Country	== "GUINEA" &	District	== "DUBREKA" ~	"DUBRÉKA",
      Country	== "GUINEA" &	District	== "GUECKEDOU" ~	"GUECKÉDOU",
      Country	== "GUINEA" &	District	== "LELOUMA" ~	"LÉLOUMA",
      Country	== "GUINEA" &	District	== "BOKE" ~	"BOKÉ",
      Country	== "GUINEA" &	District	== "KEROUANE" ~	"KÉROUANE",
      Country	== "GUINEA" &	District	== "FORECARIAH" ~	"FORÉCARIAH",
      Country	== "GUINEA" &	District	== "TELIMELE" ~	"TÉLIMÉLÉ",
      Country	== "GUINEA" &	District	== "LABE" ~	"LABÉ",
      Country	== "GUINEA" &	District	== "N'ZEREKORE" ~	"N'ZÉRÉKORÉ",
      Country	== "GUINEA" &	District	== "NZEREKORE" ~	"N'ZÉRÉKORÉ",
      Country	== "GUINEA" &	District	== "TOUGUE" ~	"TOUGUÉ",
      Country == "ANGOLA" &
        District == "TCHIKALA TCHOLOHANG" ~ "TCHIKALA TCHOLOHANGA",
      Country == "ANGOLA" &
        District == "NGOLA KILUANGE" ~ "NGOLA QUILUANGE",
      Country == "ANGOLA" &
        District == "CACULAMA" ~ "CACULAMA (MUCARI)",
      Country == "ANGOLA" & District == "BUNDAS" ~ "BUENGAS",
      Country == "ANGOLA" &
        District == "LUMBALA NGUIMBO" ~ "LUMBALA NGUIMBO (BUNDAS)",
      Country == "BENIN" & District == "KARIMAMA" ~ "KARMAMA",
      Country == "BENIN" & District == "BOUKOUMBE" ~ "BOUKOMBE",
      Country == "BENIN" & District == "DASSA" ~ "DASSA-ZOUNME",
      Country == "BENIN" & District == "DJAKOTOMEY" ~ "DJAKOTOME",
      Country == "BENIN" & District == "ZA KPOTA" ~ "ZA-KPOTA",
      Country == "BENIN" & District == "PORTO-NOVO 1" ~ "PORTO-NOVO 1",
      Country == "BENIN" & District == "Cotonou I" ~ "COTONOU 1",
      Country == "BENIN" &
        District == "Abomey-Calavi 1" ~ "ABOMEY-CALAVI 1",
      Country	== "BENIN" &	District	== "GODOMEY" ~	"ABOMEY-CALAVI 1",
      Country	== "BENIN" &	District	== "COTONOU I" ~	"COTONOU 1",
      Country	== "BENIN" &	District	== "COTONOU II" ~	"COTONOU 2",
      Country	== "BENIN" &	District	== "COTONOU III" ~	"COTONOU 3",
      Country	== "BENIN" &	District	== "COTONOU IV" ~	"COTONOU 4",
      Country	== "BENIN" &	District	== "COTONOU V" ~	"COTONOU 5",
      Country	== "BENIN" &	District	== "COTONOU VI" ~	"COTONOU 6",
      Country	== "BENIN" &	District	== "SEME-PODJI" ~	"SEME-KPODJI",
      Country	== "BENIN" &	District	== "PORTO-NOVO" ~	"PORTO-NOVO 1",
      Country == "BURKINA FASO" &
        District == "NONGR-MASSOM" ~ "NONGR MASSOM",
      Country == "BURKINA FASO" &
        District == "SIGH-NOGHIN" ~ "SIG NOGHIN",
      Country == "BURKINA FASO" & District == "NDOROLA" ~ "N'DOROLA",
      Country == "BURKINA FASO" & District == "PO" ~ "PÔ",
      Country == "BURUNDI" & District == "GATERANYI" ~ "GITERANYI",
      Country == "BOTSWANA" & District == "GHANZI" ~ "GANTSI",
      Country == "BOTSWANA" &
        District == "GREATER FRANCISTOWN" ~ "FRANCISTOWN",
      Country == "BOTSWANA" & District == "KANYE" ~ "KANYE/MOSHUPA",
      Country == "BOTSWANA" &
        District == "KGALAGADI NORTH" ~ "KGALAGADI",
      Country == "BOTSWANA" & District == "SEROWE" ~ "SEROWE/PALAPYE",
      Country	== "BOTSWANA" &	District	== "CHARLESHILL" ~	"GANTSI",
      Country	== "BOTSWANA" &	District	== "KGALAGADI SOUTH" ~	"KGALAGADI",
      Country	== "BOTSWANA" &	District	== "MOSHUPA" ~	"KANYE/MOSHUPA",
      Country	== "BOTSWANA" &	District	== "PALAPYE" ~	"SEROWE/PALAPYE",
      Country	 == "CHAD" &	District	 == "BA-ILLI"	 ~ "BA ILLI",
      Country	 == "CHAD" &	District	 == "PONT_CAROL"	 ~ "PONT CAROL",
      Country	 == "CHAD" &
        District	 == "HARAZE_MANGUEIGNE"	 ~ "HARAZE MANGUEIGNE",
      Country	 == "CHAD" &	District	 == "AM_TIMAN"	 ~ "AM TIMAN",
      Country	 == "CHAD" &
        District	 == "NDJAMENA_9AR"	 ~ "N'DJAMENA SUD",
      Country	 == "CHAD" &	District	 == "BA_ILLI"	 ~ "BA ILLI",
      Country	 == "CHAD" &	District	 == "OUM_HADJER"	 ~ "OUM HADJER",
      Country	 == "CHAD" &	District	 == "RIG_RIG"	 ~ "RIG RIG",
      Country	== "CHAD" &
        District	== "N'DJAMENA-CENTRE" ~	"N'DJAMENA CENTRE",
      Country	== "CHAD" &	District	== "N'DJAMENA-SUD" ~	"N'DJAMENA SUD",
      Country	== "CHAD" &	District	== "N'DJAMENA-EST" ~	"N'DJAMENA EST",
      Country	== "CHAD" &	District	== "N'DJAMENA-NORD" ~	"N'DJAMENA NORD",
      Country	== "CHAD" &	District	== "HADJER-HADID" ~	"OUM HADJER",
      Country	== "CHAD" &	District	== "TINE" ~	"BILTINE",
      Country	== "CHAD" &	District	== "MICHEMERE" ~	"MICHEMIRE",
      Country	== "CHAD" &	District	== "MOUNDOU EST" ~	"MOUNDOU",
      Country	== "CHAD" &	District	== "MOUNDOU CENTRE" ~	"MOUNDOU",
      Country	== "CHAD" &	District	== "NDJAMENA-NORD" ~	"N'DJAMENA NORD",
      Country	== "CHAD" &	District	== "GOUNOUGAYA" ~	"GOUNOU GAYA",
      Country	== "CHAD" &	District	== "NDJAMENA-SUD" ~	"N'DJAMENA SUD",
      Country	== "CHAD" &	District	== "BARDAÏ" ~	"BARDAI",
      Country	== "CHAD" &	District	== "MOUNDOU OUEST" ~	"MOUNDOU",
      Country	== "CHAD" &
        District	== "9E ARRONDISSEMENT" ~	"N'DJAMENA SUD",
      Country	== "CHAD" &
        District	== "NDJAMENA-CENTRE" ~	"N'DJAMENA CENTRE",
      Country	== "CHAD" &	District	== "NDJAMENA-EST" ~	"N'DJAMENA EST",
      Country	 == "CHAD" &	District	 == "BIOBE"	 ~ "BIOBE SINGAKO",
      Country	 == "CHAD" &	District	 == "GOZ_BEIDA"	 ~ "GOZ BEIDA",
      Country	 == "CHAD" &	District	 == "KOUKOU"	 ~ "KOUKOU ANGARANA",
      Country	 == "CHAD" &	District	 == "NOUKOU"	 ~ "NOKOU",
      Country	 == "CHAD" &	District	 == "NTIONA"	 ~ "N'TIONA",
      Country	 == "CHAD" &	District	 == "RIG-RIG"	 ~ "RIG RIG",
      Country	 == "CHAD" &	District	 == "GUELO"	 ~ "GUELAO",
      Country	 == "CHAD" &
        District	 == "NDJAMENA_CENTRE"	 ~ "N'DJAMENA CENTRE",
      Country	 == "CHAD" &
        District	 == "NDJAMENA_EST"	 ~ "N'DJAMENA EST",
      Country	 == "CHAD" &	District	 == "BEBIDJA"	 ~ "BEBEDJIA",
      Country	 == "CHAD" &
        District	 == "NDJAMENA_NORD"	 ~ "N'DJAMENA NORD",
      Country	 == "CHAD" &
        District	 == "NDJAMENA_SUD"	 ~ "N'DJAMENA SUD",
      Country	 == "CHAD" &	District	 == "AM-TIMAN"	 ~ "AM TIMAN",
      Country	 == "CHAD" &	District	 == "BAKTCHORO"	 ~ "BAKCTCHORO",
      Country	 == "CHAD" &	District	 == "BAGA SOLA"	 ~ "BAGASSOLA",
      Country	 == "CONGO" &	District	 == "GOMA TSE-TSE"	 ~ "GOMA TSETSE",
      Country	 == "CONGO" &
        District	 == "KINKALA BOKO"	 ~ "KINKALA-BOKO",
      Country	 == "CONGO" &	District	 == "SEMBE"	 ~ "SEMBE SOUANKE",
      Country	 == "CONGO" &
        District	 == "MADINGO-NKAYES"	 ~ "MADINGO KAYES-ZAMBI",
      Country	 == "CONGO" &
        District	 == "KIMONGO"	 ~ "KIMONGO LONDELA KAYES",
      Country	 == "CONGO" &
        District	 == "IGNIE NGABE"	 ~ "IGNIE NGABE MAYAMA",
      Country	 == "CONGO" &	District	 == "LOUTÉTÉ"	 ~ "LOUTETE",
      Country	 == "CONGO" &	District	 == "MAKÉLÉKÉLÉ"	 ~ "MAKELEKELE",
      Country	 == "CONGO" &	District	 == "OUENZÉ"	 ~ "OUENZE",
      Country	 == "CONGO" &	District	 == "TIÉ-TIÉ"	 ~ "TIE-TIE",
      Country	 == "CONGO" &	District	 == "GOMA TSÉTSÉ"	 ~ "GOMA TSETSE",
      Country	 == "CONGO" &	District	 == "KINTÉLÉ"	 ~ "KINTELE",
      Country	 == "LIBERIA" &
        District	== "BUCHANAN District" ~	"BUCHANAN",
      Country	== "LIBERIA" &
        District	== "GOLA KONNEH District" ~	"GOLAKONNEH",
      Country	== "LIBERIA" &	District	== "PORKPA District" ~	"PORKPA",
      Country	== "LIBERIA" &
        District	== "BARCLAYVILLE District" ~	"BARCLAYVILLE",
      Country	== "LIBERIA" &	District	== "SALAYEA District" ~	"SALAYEA",
      Country	== "LIBERIA" &
        District	== "VOINJAMA District" ~	"VOINJAMA",
      Country	== "LIBERIA" &	District	== "ZORZOR District" ~	"ZORZOR",
      Country	== "LIBERIA" &	District	== "KAKATA District" ~	"KAKATA",
      Country	== "LIBERIA" &	District	== "BUSHROD District" ~	"BUSHROD",
      Country	== "LIBERIA" &
        District	== "ST PAUL RIVER District" ~	"ST. PAUL RIVER",
      Country	== "LIBERIA" &	District	== "TODEE District" ~	"TODEE",
      Country	== "LIBERIA" &	District	== "BOPOLU District" ~	"BOPOLU",
      Country	== "LIBERIA" &	District	== "GBARMA District" ~	"GBARMA",
      Country	== "LIBERIA" &	District	== "GARWULA District" ~	"GARWULA",
      Country	== "LIBERIA" &	District	== "TEWOR District" ~	"TEWOR",
      Country	== "LIBERIA" &	District	== "CAVALLA District" ~	"CAVALLA",
      Country	== "LIBERIA" &	District	== "TCHIEN District" ~	"TCHIEN",
      Country	== "LIBERIA" &	District	== "JRAOH District" ~	"JRAOH",
      Country	== "LIBERIA" &	District	== "FOYA District" ~	"FOYA",
      Country	== "LIBERIA" &
        District	== "MAMBAH - KABA District" ~	"MAMBAH-KABA",
      Country	== "LIBERIA" &
        District	== "KARLUWAY 2 District" ~	"KARLUWAY 2",
      Country	== "LIBERIA" &	District	== "PLEEBO District" ~	"PLEEBO",
      Country	== "LIBERIA" &
        District	== "CAREYSBURG District" ~	"CAREYSBURG",
      Country	== "LIBERIA" &
        District	== "GBEHLAY - GEH District" ~	"GBEHLAY-GEH",
      Country	== "LIBERIA" &
        District	== "SANNIQUELLEH MAHN District" ~	"SANNIQUELLEH MAHN",
      Country	== "LIBERIA" &
        District	== "CENTRAL C District" ~	"CENTRAL C",
      Country	== "LIBERIA" &	District	== "TIMBO District" ~	"TIMBO",
      Country	== "LIBERIA" &	District	== "CHEDEPO District" ~	"CHEDEPO",
      Country	== "LIBERIA" &	District	== "GBEAPO District" ~	"GBEAPO",
      Country	== "LIBERIA" &	District	== "BUTAW District" ~	"BUTAW",
      Country	== "LIBERIA" &
        District	== "GREENVILLE District" ~	"GREENVILLE",
      Country	== "LIBERIA" &	District	== "SANOYEA" ~	"SANOYEAH",
      Country	== "LIBERIA" & District	== "BELLEH District" ~	"BELLEH",
      Country	== "LIBERIA" &	District	== "BOKOMU District" ~	"BOKOMU",
      Country	== "LIBERIA" &	District	== "KONGBA District" ~	"KONGBA",
      Country	== "LIBERIA" &
        District	== "CAMPWOOD District" ~	"CAMP WOOD",
      Country	== "LIBERIA" &
        District	== "District # 3 A&B" ~	"District # 3A & 3B",
      Country	== "LIBERIA" &
        District	== "District # 3 C" ~	"District # 3C",
      Country	== "LIBERIA" &
        District	== "OWENSGROVE District" ~	"OWENSGROVE",
      Country	== "LIBERIA" &
        District	== "COMMONWEALTH - C District" ~	"COMMONWEALTH-C",
      Country	== "LIBERIA" &	District	== "B-HAI District" ~	"B'HAI",
      Country	== "LIBERIA" & District	== "GBAO District" ~	"GBAO",
      Country	== "LIBERIA" & District	== "KONOBO District" ~	"KONOBO",
      Country	== "LIBERIA" & District	== "PUTU District" ~	"PUTU",
      Country	== "LIBERIA" & District	== "BUAH District" ~	"BUAH",
      Country	== "LIBERIA" & District	== "DORBOR District" ~	"DORBOR",
      Country	== "LIBERIA" & District	== "TREHN District" ~	"TREHN",
      Country	== "LIBERIA" &
        District	== "KOLAHUN District" ~	"KOLAHUN",
      Country	== "LIBERIA" & District	== "VAHUN District" ~	"VAHUN",
      Country	== "LIBERIA" &
        District	== "FIRESTONE District" ~	"FIRESTONE",
      Country	== "LIBERIA" & District	== "GIBI District" ~	"GIBI",
      Country	== "LIBERIA" &
        District	== "BARROBO FARJAH District" ~	"BAROBO FARJAH",
      Country	== "LIBERIA" &
        District	== "BARROBO WHOJAH District" ~	"BARROBO WHOJAH",
      Country	== "LIBERIA" & District	== "HARPER District" ~	"HARPER",
      Country	== "LIBERIA" &
        District	== "KARLUWAY 1 District" ~	"KARLUWAY 1",
      Country	== "LIBERIA" &
        District	== "SACLEPEA-MAH" ~	"SACLEPEA-MAHN",
      Country	== "LIBERIA" &
        District	== "YARWIN MEHNSONNOH" ~	"YARWEIN MEHNSOHNNEH",
      Country	== "LIBERIA" & District	== "POTUPO District" ~	"POTUPO",
      Country	== "LIBERIA" & District	== "SARBO District" ~	"SARBO",
      Country	== "LIBERIA" & District	== "TIENPO District" ~	"TIENPO",
      Country	== "LIBERIA" & District	== "WEBBO District" ~	"WEBBO",
      Country	== "LIBERIA" &
        District	== "DOEDIAN District" ~	"DOEDIAN",
      Country	== "LIBERIA" & District	== "JOE RIVER District" ~	"JOE",
      Country	== "LIBERIA" & District	== "YARNIE District" ~	"YARNIE",
      Country	== "LIBERIA" &
        District	== "DUGBE RIVER District" ~	"DUGBE",
      Country	== "LIBERIA" &
        District	== "GBLONEE District" ~	"GBLONEE",
      Country	== "LIBERIA" & District	== "JEADE District" ~	"JEADE",
      Country	== "LIBERIA" & District	== "JEDEPO District" ~	"JEDEPO",
      Country	== "LIBERIA" &
        District	== "KPANYAN District" ~	"KPANYAN",
      Country	== "LIBERIA" &
        District	== "PYNES TOWN District" ~	"PYNES",
      Country	== "LIBERIA" &
        District	== "TARJUWON District" ~	"TARJUWON",
      Country	== "LIBERIA" & District	== "TARSUE District" ~	"TARSUE",
      Country	== "LIBERIA" &	District	== "TAPPITA District" ~	"TAPPITA",
      Country	== "LIBERIA" &	District	== "B'HAI District" ~	"B'HAI",
      Country	== "LIBERIA" &
        District	== "SACLEPEA MAH District" ~	"SACLEPEA-MAHN",
      Country	== "LIBERIA" &
        District	== "YARWEIN MEHNSOHNNEH District" ~	"YARWEIN MEHNSOHNNEH",
      Country	== "LIBERIA" &	District	== "ZOE-GEH District" ~	"ZOE-GEH",
      Country	== "LIBERIA" &
        District	== "SANIQUELLIE - MAH" ~	"SANNIQUELLEH MAHN",
      Country	== "LIBERIA" &	District	== "DOEDIAN" ~	"DOEDAIN",
      Country	== "LIBERIA" &	District	== "BUU-YAO" ~	"BUAH",
      Country	== "LIBERIA" &
        District	== "COMMONWEALTH-C District" ~	"COMMONWEALTH-C",
      Country	== "LIBERIA" &
        District	== "MAMBAH-KABA District" ~	"MAMBAH-KABA",
      Country	== "LIBERIA" &
        District	== "CENTRAL MONROVIA District" ~	"CENTRAL MONROVIA",
      Country	== "LIBERIA" &
        District	== "SOMALIA DRIVE District" ~	"SOMALIA DRIVE",
      Country	== "LIBERIA" &	District	== "JOE" ~	"JOE RIVER",
      Country	== "LIBERIA" &	District	== "JOWEIN District" ~	"JOWEIN",
      Country	== "LIBERIA" &	District	== "PYNES" ~	"PYNES TOWN",
      Country	== "LIBERIA" &
        District	== "COMMONWEALTH District" ~	"COMMONWEALTH",
      Country	== "LIBERIA" &	District	== "DUGBE" ~	"DUGBE RIVER",
      Country	== "LIBERIA" &	District	== "BAROBO FARJAH" ~	"BAROBO FAJAH",
      Country	== "MALI" &	District	 == "KALABAN-CORO" ~	"KALABANCORO",
      Country	== "MALI" &	District	 == "TAOUDENIT" ~	"TAOUDENI",
      Country	== "MALI" &
        District	 == "District COMMUNE I" ~	"COMMUNE I",
      Country	== "MALI" &
        District	 == "District COMMUNE II" ~	"COMMUNE II",
      Country	== "MALI" &
        District	 == "District COMMUNE III" ~	"COMMUNE III",
      Country	== "MALI" &
        District	 == "District COMMUNE IV" ~	"COMMUNE IV",
      Country	== "MALI" &
        District	 == "District COMMUNE V" ~	"COMMUNE V",
      Country	== "MALI" &
        District	 == "District COMMUNE VI" ~	"COMMUNE VI",
      Country	== "MALI" &
        District	 == "District KOULIKORO" ~	"KOULIKORO",
      Country	== "MALI" &	District	 == "District SIKASSO" ~	"SIKASSO",
      Country	== "MALI" &	District	 == "District KITA" ~	"KITA",
      Country	== "MALI" &	District	 == "District BANAMBA" ~	"BANAMBA",
      Country	== "MALI" &	District	 == "District DIOILA" ~	"DIOILA",
      Country	== "MALI" &	District	 == "District FANA" ~	"FANA",
      Country	== "MALI" &
        District	 == "District KALABANCORO" ~	"KALABANCORO",
      Country	== "MALI" &	District	 == "District KANGABA" ~	"KANGABA",
      Country	== "MALI" &	District	 == "District KATI" ~	"KATI",
      Country	== "MALI" &	District	 == "District KOLOKANI" ~	"KOLOKANI",
      Country	== "MALI" &	District	 == "District NARA" ~	"NARA",
      Country	== "MALI" &
        District	 == "District OUELESSEBOUGOU" ~	"OUELESSEBOUGOU",
      Country	== "MALI" &	District	 == "District BOUGOUNI" ~	"BOUGOUNI",
      Country	== "MALI" &	District	 == "District KADIOLO" ~	"KADIOLO",
      Country	== "MALI" &	District	 == "District KIGNAN" ~	"KIGNAN",
      Country	== "MALI" &
        District	 == "District KOLONDIEBA" ~	"KOLONDIEBA",
      Country	== "MALI" &	District	 == "District KOUTIALA" ~	"KOUTIALA",
      Country	== "MALI" &	District	 == "District NIENA" ~	"NIENA",
      Country	== "MALI" &	District	 == "District SELINGUE" ~	"SELINGUE",
      Country	== "MALI" &
        District	 == "District YANFOLILA" ~	"YANFOLILA",
      Country	== "MALI" &	District	 == "District YOROSSO" ~	"YOROSSO",
      Country	== "MALI" &	District	== "ALMOUSTRAT" ~	"ALMOUSTARAT",
      Country	== "MALI" &	District	== "COMMUNE 1" ~	"COMMUNE I",
      Country	== "MALI" &	District	== "COMMUNE 2" ~	"COMMUNE II",
      Country	== "MALI" &	District	== "COMMUNE 3" ~	"COMMUNE III",
      Country	== "MALI" &	District	== "COMMUNE 4" ~	"COMMUNE IV",
      Country	== "MALI" &	District	== "COMMUNE 5" ~	"COMMUNE V",
      Country	== "MALI" &	District	== "COMMUNE 6" ~	"COMMUNE VI",
      Country	== "MALI" &	District	== "DJENNE" ~	"DJENNÉ",
      Country	== "MALI" &	District	== "SAGABARY" ~	"SAGABARI",
      Country	== "MALI" &
        District	== "OUSSOUBIDIAGNIA" ~	"OUSSOUBIDIAGNA",
      Country	== "MALI" &
        District	== "District GOURMA-RHAROUS" ~	"GOURMA-RHAROUS",
      Country	== "MALI" &	District	== "District TIDERMENE" ~	"TIDERMENE",
      Country	== "MALI" &	District	== "District BOUREM" ~	"BOUREM",
      Country	== "MALI" &	District	== "District GAO" ~	"GAO",
      Country	== "MALI" &	District	== "District ABEIBARA" ~	"ABEIBARA",
      Country	== "MALI" &	District	== "District MENAKA" ~	"MENAKA",
      Country	== "MALI" &	District	== "District NIAFUNKE" ~	"NIAFUNKE",
      Country	== "MALI" &	District	== "District INEKAR" ~	"INEKAR",
      Country	== "MALI" &	District	== "District DJENNE" ~	"DJENNÉ",
      Country	== "MALI" &
        District	== "District BANDIAGARA" ~	"BANDIAGARA",
      Country	== "MALI" &	District	== "District DIRE" ~	"DIRE",
      Country	== "MALI" &	District	== "District TESSALIT" ~	"TESSALIT",
      Country	== "MALI" &
        District	== "District ANDERAMBOUKANE" ~	"ANDERAMBOUKANE",
      Country	== "MALI" &	District	== "District KORO" ~	"KORO",
      Country	== "MALI" &	District	== "District TENENKOU" ~	"TENENKOU",
      Country	== "MALI" &	District	== "District YOUWAROU" ~	"YOUWAROU",
      Country	== "MALI" &	District	== "District AL-OURCHE" ~	"AL-OURCHE",
      Country	== "MALI" &	District	== "District KIDAL" ~	"KIDAL",
      Country	== "MALI" &	District	== "District TINESSAKO" ~	"TINESSAKO",
      Country	== "MALI" &	District	== "District ARAWANE" ~	"ARAWANE",
      Country	== "MALI" &	District	== "District GOUNDAM" ~	"GOUNDAM",
      Country	== "MALI" &	District	== "District BOUJBEHA" ~	"BOUJBEHA",
      Country	== "MALI" &
        District	== "District ALMOUSTARAT" ~	"ALMOUSTARAT",
      Country	== "MALI" &	District	== "District ANSONGO" ~	"ANSONGO",
      Country	== "MALI" &	District	== "District BANKASS" ~	"BANKASS",
      Country	== "MALI" &	District	== "District DOUENTZA" ~	"DOUENTZA",
      Country	== "MALI" &	District	== "District MOPTI" ~	"MOPTI",
      Country	== "MALI" &	District	== "District ACHOURATT" ~	"ACHOURATT",
      Country	== "MALI" &	District	== "District FOUM-ALBA" ~	"FOUM-ALBA",
      Country	== "MALI" &	District	== "District TAOUDENIT" ~	"TAOUDENI",
      Country	== "MALI" &
        District	== "District TOMBOUCTOU" ~	"TOMBOUCTOU",
      Country	== "MALI" &
        District	== "District OUSSOUBIDIAGNA" ~	"OUSSOUBIDIAGNA",
      Country	== "MALI" &	District	== "District BLA" ~	"BLA",
      Country	== "MALI" &	District	== "District MACINA" ~	"MACINA",
      Country	== "MALI" &	District	== "District MARKALA" ~	"MARKALA",
      Country	== "MALI" &	District	== "District NIONO" ~	"NIONO",
      Country	== "MALI" &	District	== "District DIEMA" ~	"DIEMA",
      Country	== "MALI" &	District	== "District SAGABARI" ~	"SAGABARI",
      Country	== "MALI" &	District	== "District SAN" ~	"SAN",
      Country	== "MALI" &	District	== "District KENIEBA" ~	"KENIEBA",
      Country	== "MALI" &	District	== "District KAYES" ~	"KAYES",
      Country	== "MALI" &	District	== "District SEFETO" ~	"SEFETO",
      Country	== "MALI" &	District	== "District TOMINIAN" ~	"TOMINIAN",
      Country	== "MALI" &	District	== "District BAFOULABE" ~	"BAFOULABE",
      Country	== "MALI" &	District	== "District NIORO" ~	"NIORO",
      Country	== "MALI" &	District	== "District YELIMANE" ~	"YELIMANE",
      Country	== "MALI" &	District	== "District BAROUELI" ~	"BAROUELI",
      Country	== "MALI" &	District	== "District SEGOU" ~	"SEGOU",
      Country	== "MAURITANIA" &
        District	 == "MAGHTALAHJAR" ~	"MAGTA LAHJAR",
      Country	== "MAURITANIA" &	District	 == "SELIBABI" ~	"SELIBABY",
      Country	== "MAURITANIA" &
        District	 == "AIOUN AL ATROUSS" ~	"AIOUN",
      Country	== "MAURITANIA" &	District	 == "KOUBENNI" ~	"KOBENI",
      Country	== "MAURITANIA" &	District	 == "TICHITT" ~	"TICHIT",
      Country	== "MAURITANIA" &	District	 == "F_DERICK" ~	"F'DERICK",
      Country	== "MAURITANIA" &	District	 == "CHINGUITTI" ~	"CHINGUITTY",
      Country	== "MAURITANIA" &	District	 == "BARKEOL" ~	"BARKÉOLE",
      Country	== "MAURITANIA" &	District	 == "KANKOUSSA" ~	"KANKOSSA",
      Country	== "MAURITANIA" &	District	 == "M_BAGNE" ~	"M'BAGNE",
      Country	== "MAURITANIA" &
        District	 == "MAGHTA LEHJAR" ~	"MAGTA LAHJAR",
      Country	== "MAURITANIA" &	District	 == "LEKSEIBE" ~	"LEXEIBA",
      Country	== "MAURITANIA" &	District	 == "M_BOUT" ~	"M'BOUT",
      Country	== "MAURITANIA" &	District	 == "SEILIBABY" ~	"SELIBABY",
      Country	== "MAURITANIA" &	District	 == "WOMPOU" ~	"WOMPO",
      Country	== "MAURITANIA" &
        District	 == "ADEL BAGHROU" ~	"ADEL BEGROU",
      Country	== "MAURITANIA" &	District	 == "D_HAR" ~	"D'HAR",
      Country	== "MAURITANIA" &	District	 == "KOBENNI" ~	"KOBENI",
      Country	== "MAURITANIA" &	District	 == "TAMCHEKETT" ~	"TAMCHAKET",
      Country	== "MAURITANIA" &	District	 == "AKJOUJET" ~	"AKJOUJT",
      Country	== "MAURITANIA" &
        District	 == "TEVRAGH ZEINE" ~	"TEVRAGH ZEINA",
      Country	== "MAURITANIA" &	District	 == "RIYADH" ~	"RIYAD",
      Country	== "MAURITANIA" &	District	 == "MOUDJRIA" ~	"MOUDJÉRIA",
      Country	== "MAURITANIA" &
        District	 == "BIR OUMGREINE" ~	"BIR MOGHREN",
      Country	== "MAURITANIA" &	District	 == "F_DERIK" ~	"F'DERICK",
      Country	== "MAURITANIA" &
        District	 == "BOUTILIMITT" ~	"BOUTILIMIT",
      Country	== "MAURITANIA" &	District	 == "OUAD-NAGA" ~	"OUAD NAGA",
      Country	== "MAURITANIA" &	District	 == "R_KIZ" ~	"R'KIZ",
      Country	== "MAURITANIA" &	District	== "GUEROU" ~	"GUÉRROU",
      Country	== "MAURITANIA" &	District	== "BASSEKNOU" ~	"BASSIKNOU",
      Country	== "MAURITANIA" &	District	== "BARKEIWEL" ~	"BARKÉOLE",
      Country	== "MAURITANIA" &
        District	== "BIRMOUGREIN" ~	"BIR MOGHREN",
      Country	== "MAURITANIA" &	District	== "TIJIKJA" ~	"TIDJIKJA",
      Country	== "MAURITANIA" &	District	== "BARKEOLE" ~	"BARKÉOLE",
      Country	== "MAURITANIA" &	District	== "GUERROU" ~	"GUÉRROU",
      Country	== "MAURITANIA" &	District	== "BABABE" ~	"BABABÉ",
      Country	== "MAURITANIA" &	District	== "BOGHE" ~	"BOGHÉ",
      Country	== "MAURITANIA" &	District	== "NEMA" ~	"NÉMA",
      Country	== "MAURITANIA" &	District	== "TIMBEDRA" ~	"TIMBÉDRA",
      Country	== "MAURITANIA" &	District	== "MOUDJERIA" ~	"MOUDJÉRIA",
      Country	== "MAURITANIA" &	District	== "ZOUERAT" ~	"ZOUÉRAT",
      Country	== "MAURITANIA" &	District	== "ZOUERATE" ~	"ZOUÉRAT",
      Country	== "MADAGASCAR" &
        District	== "ANTANANARIVO RENIVOHITRA" ~	"ANTANANARIVO-RENIVOHITRA",
      Country	== "MADAGASCAR" &
        District	== "ANKAZOABO ATSIMO" ~	"ANKAZOABO-ATSIMO",
      Country	== "MADAGASCAR" &
        District	== "TOLIARA I" ~	"TOLIARA I ET II",
      Country	== "MADAGASCAR" &
        District	== "MIDONGY DU SUD" ~	"MIDONGY-ATSIMO",
      Country	== "MADAGASCAR" &
        District	== "IKONGO (FORT_CARNOT)" ~	"IKONGO",
      Country	== "MADAGASCAR" &
        District	== "ANTANANARIVO ATSIMONDRANO" ~	"ANTANANARIVO-ATSIMONDRANO",
      Country	== "MADAGASCAR" &
        District	== "FENOARIVO ATSINANANA" ~	"FENOARIVO-ATSINANANA",
      Country	== "MADAGASCAR" &
        District	== "SOANIERANA IVONGO" ~	"SOANIERANA-IVONGO",
      Country	== "MADAGASCAR" &
        District	== "AMBOVOMBE ANDROY" ~	"AMBOVOMBE-ANDROY",
      Country	== "MADAGASCAR" &	District	== "BELOHA ANDROY" ~	"BELOHA",
      Country	== "MADAGASCAR" &
        District	== "BELO SUR TSIRIBIHINA" ~	"BELO-TSIRIBIHINA",
      Country	== "MADAGASCAR" &
        District	== "ANTANANARIVO AVARADRANO" ~	"ANTANANARIVO-AVARADRANO",
      Country	== "MADAGASCAR" &
        District	== "MANANARA AVARATRA" ~	"MANANARA-AVARATRA",
      Country	== "MADAGASCAR" &
        District	== "AMPANIHY OUEST" ~	"AMPANIHY",
      Country	== "MADAGASCAR" &
        District	== "BETIOKY ATSIMO" ~	"BETIOKY-ATSIMO",
      Country	== "MADAGASCAR" &
        District	== "ANTANAMBAO MANAMPONTSY" ~	"ANTANAMBAO-MANAMPOTSY",
      Country	== "MADAGASCAR" &
        Province	== "BONGOLAVA" &
        District	== "FENOARIVOBE" ~	"FENOARIVO-AFOVOANY",
      Country	== "MADAGASCAR" &
        Province	== "ANOSY" &
        District	== "AMBOASARY SUD" ~	"AMBOASARY-ATSIMO",
      Country	== "MADAGASCAR" &
        District	== "TOLIARA II" ~	"TOLIARA I ET II",
      Country	== "MADAGASCAR" &
        District	== "AMBATOBOENY" ~	"AMBATO-BOINA",
      Country	== "MADAGASCAR" &
        District	== "ANOSIBE AN ALA" ~	"ANOSIBE AN-ALA",
      Country	== "MADAGASCAR" &
        District	== "ANOSIBE AN'ALA" ~	"ANOSIBE AN-ALA",
      Country	== "MADAGASCAR" &
        District	== "NOSY BORAHA (SAINTE MARIE)" ~	"NOSY-BORAHA (SAINTE MARIE)",
      Country	== "MADAGASCAR" &	District	== "NOSY BE" ~	"NOSY-BE",
      Country	== "MADAGASCAR" &
        District	== "MANAKARA ATSIMO" ~	"MANAKARA-ATSIMO",
      Country	== "MADAGASCAR" &
        District	== "VOHIMARINA (VOHÃ©MAR)" ~	"VOHIMARINA (VOHEMAR)",
      Country	== "MADAGASCAR" &
        District	== "BEFANDRIANA AVARATRA" ~	"BEFANDRIANA-AVARATRA",
      Country	== "MADAGASCAR" &
        District	== "BORIZINY (PORT BERGE)" ~	"BORIZINY (PORT-BERGER)",
      Country	== "MADAGASCAR" &
        District	== "NOSY VARIKA" ~	"NOSY-VARIKA",
      Country	== "MALAWI" &	District	== "MZIMBA NORTH" ~	"M'ZIMBA",
      Country	== "MALAWI" &	District	== "MZIMBA SOUTH" ~	"M'ZIMBA",
      Country	== "MOZAMBIQUE" &	District	 == "PEMBA" ~	"PEMBA-METUGE",
      Country	== "MOZAMBIQUE" &
        District	 == "CHIMOIO" ~	"CIDADE DE CHIMOIO",
      Country	== "MOZAMBIQUE" &
        District	 == "NACALA PORTO" ~	"NACALA PORTO",
      Country	== "MOZAMBIQUE" &	District	 == "GORONGOZA" ~	"GORONGOSA",
      Country	== "MOZAMBIQUE" &	District	 == "TETE" ~	"CIDADE DE TETE",
      Country	== "NIGERIA" &	District	== "BURUKU" ~	"BUKURU",
      Country	== "NIGERIA" &	District	== "ONUIMO" ~	"UNUIMO",
      Country	== "NIGERIA" &	District	== "MUNYA" ~	"MUYA",
      Country	== "NIGERIA" &	District	== "AYEDADE" ~	"AIYEDADE",
      Country	== "NIGERIA" &	District	== "AYEDIRE" ~	"AIYEDIRE",
      Country	== "NIGERIA" &	District	== "GIREI" ~	"GIRIE",
      Country	== "NIGERIA" &	District	== "TOUNGO" ~	"TEUNGO",
      Country	== "NIGERIA" &	District	== "KIRI KASAMMA" ~	"KIRI KASAMA",
      Country	== "NIGERIA" &	District	== "LAMURDE" ~	"LARMURDE",
      Country	== "NIGERIA" &	District	== "BIRNIWA" ~	"BIRNIN KUDU",
      Country	== "NIGERIA" &	District	== "MALAM MADORI" ~	"MALAM MADURI",
      Country	== "NIGERIA" &
        District	== "SULE TANKARKAR" ~	"SULE TANKAKAR",
      Country	== "NIGERIA" &	District	== "KUBAU" ~	"KUBAN",
      Country	== "NIGERIA" &	District	== "UNGOGO" ~	"UNGONGO",
      Country	== "NIGERIA" &	District	== "WAMAKKO" ~	"WAMAKO",
      Country	== "NIGERIA" &	District	== "BADE" ~	"BARDE",
      Country	== "NIGERIA" &	District	== "BURSARI" ~	"BORSARI",
      Country	== "NIGERIA" &	District	== "TARMUWA" ~	"TARMUA",
      Country	== "NIGER" &	District	== "KANTCHE" ~	"KANTCHÉ",
      Country	== "NIGER" &	District	== "TCHIROZERINE" ~	"TCHIROZÉRINE",
      Country	== "NIGER" &	District	== "IFEROUANE" ~	"IFÉROUANE",
      Country	== "NIGER" &
        District	== "DAMAGARAM TAKAYYA" ~	"DAMAGARAM TAKAYA",
      Country	== "NIGER" &	District	== "NGOURTI" ~	"N'GOURTI",
      Country	== "NIGER" &	District	== "DOGONDOUTCHI" ~	"DOGON DOUTCHI",
      Country	== "NIGER" &	District	== "GUIDAN ROUMDJI" ~	"G. ROUMDJI",
      Country	== "NIGER" &	District	== "TAHOUA DEPT" ~	"TAHOUA DEP",
      Country	== "NIGER" &	District	== "TILLABERY" ~	"TILLABERI",
      Country	== "NIGER" &	District	== "TAKIETA" ~	"TAKEITA",
      Country	== "NIGER" &	District	== "TARKA (BELBEJI)" ~	"BELBEDJI",
      Country	== "NIGER" &	District	== "ZINDER VILLE" ~	"ZINDER",
      Country	== "CENTRAL AFRICAN REPUBLIC" &
        District	== "NANGHA-BOGUILA" ~	"NANGA-BOGUILA",
      Country	== "CENTRAL AFRICAN REPUBLIC" &
        District	== "NANA-GRIBIZI" ~	"NANA-GREBIZI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KWILU NGONGO" ~	"KWILU-NGONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KIMBAO" ~	"KIMBAU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "WAMBA LWADI" ~	"WAMBA LUADI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "NIANIA" ~	"NIA-NIA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BENATSHIADI" ~	"BENA-TSHIADI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "SHABUNDA" ~	"SHABUNDA CENTRE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MUFUNGA SAMPWE" ~	"MUFUNGA-SAMPWE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BENALEKA" ~	"BENA-LEKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "LUBONDAIE" ~	"LUBONDAYI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KASAVUBU" ~	"KASA-VUBU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MONT NGAFULA I" ~	"MONT-NGAFULA I",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MONT NGAFULA II" ~	"MONT-NGAFULA II",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KASONGO LUNDA" ~	"KASONGOLUNDA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MASIMANIMBA" ~	"MASI-MANIMBA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "YASA BONGA" ~	"YASA-BONGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BANDJOW MOKE" ~	"BANJOW MOKE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "SARAMABILA" ~	"SALAMABILA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MOBAYI-MBONGO" ~	"MOBAYI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BENA DIBELE" ~	"BENA-DIBELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "DJALO NDJEKA" ~	"DJALO-NDJEKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KATAKO KOMBE" ~	"KATAKO-KOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "TSHUDI LOTO" ~	"TSHUDI-LOTO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "WEMBO NYAMA" ~	"WEMBO-NYAMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BAGIRA" ~	"BAGIRA-KASHA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KIMBI-LULENGE" ~	"KIMBI LULENGE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MITI-MURHESA" ~	"MITI-MURRHESA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BANGA BOLA" ~	"BANGABOLA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "WANIE RUKULA" ~	"WANIE-RUKULA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KALAMBAYI" ~	"KALAMBAYI KABANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KALONDA EST" ~	"KALONDA-EST",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "LUDIMBI LUKULA" ~	"LUDIMBI-LUKULA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "RUASHI" ~	"RWASHI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BAMBU" ~	"BAMBU-MINES",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BINZA METEO" ~	"BINZA-METEO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BINZA OZONE" ~	"BINZA-OZONE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "NGIRI NGIRI" ~	"NGIRI-NGIRI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BOKO KIVULU" ~	"BOKO-KIVULU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "GOMBE MATADI" ~	"GOMBE-MATADI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MBANZA NGUNGU" ~	"MBANZA-NGUNGU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "NSONA MPANGU" ~	"NSONA-PANGU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "SEKE BANZA" ~	"SEKEBANZA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "SONA BATA" ~	"SONA-BATA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KIKWIT NORD" ~	"KIKWIT-NORD",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KIKWIT SUD" ~	"KIKWIT-SUD",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BANDJOW-MOKE" ~	"BANJOW MOKE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "NTANDEMBELO" ~	"NTAND EMBELO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "PENDJWA" ~	"PENDJUA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MOBAYI MBONGO" ~	"MOBAYI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "VANGA KETE" ~	"VANGA-KETE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BAGIRA KASHA" ~	"BAGIRA-KASHA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BOGOSENUBEA" ~	"BOGOSE NUBEA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MWENEDITU" ~	"MWENE DITU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BENA LEKA" ~	"BENA-LEKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BENA TSHADI" ~	"BENA-TSHIADI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MASSA" ~	"MASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KAMWESHA" ~	"KAMUESHA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "NDJOKO MPUNDA" ~	"NDJOKO PUNDA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "HAUT PLATEAU" ~	"HAUTS PLATEAUX UVIRA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MITI MURHESA" ~	"MITI-MURRHESA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MALEMBA NKULU" ~	"MALEMBA-NKULU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "GETY" ~	"GETHY",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "NIA NIA" ~	"NIA-NIA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KALONDA OUEST" ~	"KALONDA-OUEST",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KALAMU 1" ~	"KALAMU I",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KALAMU 2" ~	"KALAMU II",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KASA VUBU" ~	"KASA-VUBU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MALUKU 1" ~	"MALUKU I",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MALUKU 2" ~	"MALUKU II",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MASINA 1" ~	"MASINA I",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MASINA 2" ~	"MASINA II",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MONT NGAFULA 1" ~	"MONT-NGAFULA I",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MONT NGAFULA 2" ~	"MONT-NGAFULA II",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KISANJI" ~	"KISANDJI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "MASI MANIMBA" ~	"MASI-MANIMBA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BANZOW MOKE" ~	"BANJOW MOKE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BOSO MONDANDA" ~	"BOSOMODANDA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "KABONDO DIANDA" ~	"KABONDO-DIANDA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        District	== "BOSO MANZI" ~	"BOSOMANZI",
      Country	== "SENEGAL" &	District	== "BIRKELANE" ~	"BIRKILANE",
      Country	== "SENEGAL" &	District	== "MALEM HODAR" ~	"MALEM HODDAR",
      Country	== "SENEGAL" &	District	== "DAROU-MOUSTY" ~	"DAROU MOUSTY",
      Country	== "SENEGAL" &	District	== "KOKI" ~	"COKI",
      Country	== "SENEGAL" &	District	== "SAINT-LOUIS" ~	"SAINT LOUIS",
      Country	== "SENEGAL" &
        District	== "DIANKHE MAKHAN" ~	"DIANKE MAKHA",
      Country	== "SENEGAL" &
        District	== "MAKACOLIBANTANG" ~	"MAKA COLIBANTANG",
      Country	== "SENEGAL" &
        District	== "THIONCK-ESSYL" ~	"THIONCK ESSYL",
      Country	== "SIERRA LEONE" &
        District	== "WESTERN RUR" ~	"WESTERN RURAL",
      Country	== "SIERRA LEONE" &
        District	== "WESTERN URB" ~	"WESTERN URBAN",
      Country	== "SOUTH SUDAN" &	District	== "KAJO-KEJI" ~	"KAJO KEJI",
      Country	== "SOUTH SUDAN" &	District	== "LAPON" ~	"LAFON/LOPA",
      Country	== "SOUTH SUDAN" &	District	== "BOR SOUTH" ~	"SOUTH BOR",
      Country	== "SOUTH SUDAN" &
        District	== "AWEIL CENTRE" ~	"AWEIL CENTRAL",
      Country	== "SOUTH SUDAN" &	District	== "RUBKONA" ~	"RUBKOANA",
      Country	== "SOUTH SUDAN" &	District	== "LUAKPINY/NASIR" ~	"NASIR",
      Country	== "SOUTH SUDAN" &	District	== "RAGA" ~	"RAJA",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "MICHEWENI DC" ~	"MICHEWENI",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "WETE DC" ~	"WETE",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "KASKAZINI A DC" ~	"KASKAZINI A",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "KASKAZINI B DC" ~	"KASKAZINI B",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "CHAKE CHAKE DC" ~	"CHAKE CHAKE",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "MKOANI DC" ~	"MKOANI",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "KATI DC" ~	"KATI",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "KUSINI DC" ~	"KUSINI",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "MJINI DC" ~	"MJINI",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "KIBITI DC" ~	"KIBITI TC",
      Country	== "UNITED REPUBLIC OF TANZANIA" &
        District	== "KIGOMA UJIJI MC" ~	"KIGOMA MC",
      Country	== "TOGO" &	District	== "BAS MONO" ~	"BAS-MONO",
      Country	== "TOGO" &	District	== "AGOE NYIVE" ~	"AGOE",
      Country	== "TOGO" &	District	== "EST MONO" ~	"EST-MONO",
      Country	== "TOGO" &	District	== "MOYEN MONO" ~	"MOYEN-MONO",
      Country	== "UGANDA" &	District	== "ADJUMANI District" ~	"ADJUMANI",
      Country	== "UGANDA" &	District	== "ARUA CITY" ~	"ARUA",
      Country	== "UGANDA" &	District	== "ARUA District" ~	"ARUA",
      Country	== "UGANDA" &	District	== "KOBOKO District" ~	"KOBOKO",
      Country	== "UGANDA" &
        District	== "MADI-OKOLLO District" ~	"MADI-OKOLLO",
      Country	== "UGANDA" &	District	== "MARACHA District" ~	"MARACHA",
      Country	== "UGANDA" &	District	== "MOYO District" ~	"MOYO",
      Country	== "UGANDA" &	District	== "TEREGO" ~	"TEREGO",
      Country	== "UGANDA" &	District	== "ZOMBO District" ~	"ZOMBO",
      Country	== "UGANDA" &	District	== "BUIKWE District" ~	"BUIKWE",
      Country	== "UGANDA" &
        District	== "BUTAMBALA District" ~	"BUTAMBALA",
      Country	== "UGANDA" &	District	== "BUVUMA District" ~	"BUVUMA",
      Country	== "UGANDA" &	District	== "GOMBA District" ~	"GOMBA",
      Country	== "UGANDA" &	District	== "KAYUNGA District" ~	"KAYUNGA",
      Country	== "UGANDA" &	District	== "AMURU District" ~	"AMURU",
      Country	== "UGANDA" &	District	== "GULU CITY" ~	"GULU",
      Country	== "UGANDA" &	District	== "KITGUM District" ~	"KITGUM",
      Country	== "UGANDA" &	District	== "LAMWO District" ~	"LAMWO",
      Country	== "UGANDA" &	District	== "NWOYA District" ~	"NWOYA",
      Country	== "UGANDA" &	District	== "BULIISA District" ~	"BULIISA",
      Country	== "UGANDA" &	District	== "HOIMA CITY" ~	"HOIMA",
      Country	== "UGANDA" &	District	== "HOIMA District" ~	"HOIMA",
      Country	== "UGANDA" &	District	== "KAKUMIRO District" ~	"KAKUMIRO",
      Country	== "UGANDA" &	District	== "KIKUUBE District" ~	"KIKUUBE",
      Country	== "UGANDA" &	District	== "MASINDI District" ~	"MASINDI",
      Country	== "UGANDA" &	District	== "BUGWERI District" ~	"BUGWERI",
      Country	== "UGANDA" &	District	== "BUYENDE District" ~	"BUYENDE",
      Country	== "UGANDA" &	District	== "IGANGA District" ~	"IGANGA",
      Country	== "UGANDA" &	District	== "JINJA CITY" ~	"JINJA",
      Country	== "UGANDA" &	District	== "JINJA District" ~	"JINJA",
      Country	== "UGANDA" &	District	== "KAMULI District" ~	"KAMULI",
      Country	== "UGANDA" &	District	== "LUUKA District" ~	"LUUKA",
      Country	== "UGANDA" &
        District	== "NAMAYINGO District" ~	"NAMAYINGO",
      Country	== "UGANDA" &
        District	== "NAMUTUMBA District" ~	"NAMUTUMBA",
      Country	== "UGANDA" &	District	== "KABALE District" ~	"KABALE",
      Country	== "UGANDA" &	District	== "KANUNGU District" ~	"KANUNGU",
      Country	== "UGANDA" &	District	== "KISORO District" ~	"KISORO",
      Country	== "UGANDA" &	District	== "RUBANDA District" ~	"RUBANDA",
      Country	== "UGANDA" &
        District	== "RUKUNGIRI District" ~	"RUKUNGIRI",
      Country	== "UGANDA" &
        District	== "BUNDIBUGYO District" ~	"BUNDIBUGYO",
      Country	== "UGANDA" &
        District	== "BUNYANGABU District" ~	"BUNYANGABU",
      Country	== "UGANDA" &
        District	== "FORT PORTAL CITY" ~	"FORT PORTAL",
      Country	== "UGANDA" &	District	== "KABAROLE District" ~	"KABAROLE",
      Country	== "UGANDA" &	District	== "KASESE District" ~	"KASESE",
      Country	== "UGANDA" &
        District	== "KITAGWENDA District" ~	"KITAGWENDA",
      Country	== "UGANDA" &	District	== "KYEGEGWA District" ~	"KYEGEGWA",
      Country	== "UGANDA" &	District	== "KYENJOJO District" ~	"KYENJOJO",
      Country	== "UGANDA" &	District	== "NTOROKO District" ~	"NTOROKO",
      Country	== "UGANDA" &	District	== "ALEBTONG District" ~	"ALEBTONG",
      Country	== "UGANDA" &	District	== "APAC District" ~	"APAC",
      Country	== "UGANDA" &	District	== "KOLE District" ~	"KOLE",
      Country	== "UGANDA" &	District	== "KWANIA District" ~	"KWANIA",
      Country	== "UGANDA" &	District	== "LIRA CITY" ~	"LIRA",
      Country	== "UGANDA" &	District	== "LIRA District" ~	"LIRA",
      Country	== "UGANDA" &	District	== "OTUKE District" ~	"OTUKE",
      Country	== "UGANDA" &
        District	== "BUKOMANSIMBI District" ~	"BUKOMANSIMBI",
      Country	== "UGANDA" &
        District	== "KALANGALA District" ~	"KALANGALA",
      Country	== "UGANDA" &	District	== "KYOTERA District" ~	"KYOTERA",
      Country	== "UGANDA" &
        District	== "LYANTONDE District" ~	"LYANTONDE",
      Country	== "UGANDA" &	District	== "MASAKA CITY" ~	"MASAKA",
      Country	== "UGANDA" &	District	== "MASAKA District" ~	"MASAKA",
      Country	== "UGANDA" &	District	== "SSEMBABAULE" ~	"SSEMBABAULE",
      Country	== "UGANDA" &	District	== "BUDUDA District" ~	"BUDUDA",
      Country	== "UGANDA" &
        District	== "BULAMBULI District" ~	"BULAMBULI",
      Country	== "UGANDA" &	District	== "BUTEBO District" ~	"BUTEBO",
      Country	== "UGANDA" &
        District	== "KAPCHORWA District" ~	"KAPCHORWA",
      Country	== "UGANDA" &	District	== "KIBUKU District" ~	"KIBUKU",
      Country	== "UGANDA" &	District	== "MBALE CITY" ~	"MBALE",
      Country	== "UGANDA" &	District	== "MBALE District" ~	"MBALE",
      Country	== "UGANDA" &	District	== "TORORO District" ~	"TORORO",
      Country	== "UGANDA" &	District	== "BUHWEJU District" ~	"BUHWEJU",
      Country	== "UGANDA" &	District	== "IBANDA District" ~	"IBANDA",
      Country	== "UGANDA" &	District	== "ISINGIRO District" ~	"ISINGIRO",
      Country	== "UGANDA" &	District	== "KAZO District" ~	"KAZO",
      Country	== "UGANDA" &	District	== "KIRUHURA District" ~	"KIRUHURA",
      Country	== "UGANDA" &	District	== "MBARARA CITY" ~	"MBARARA",
      Country	== "UGANDA" &	District	== "MBARARA District" ~	"MBARARA",
      Country	== "UGANDA" &	District	== "MITOOMA District" ~	"MITOOMA",
      Country	== "UGANDA" &	District	== "NTUNGAMO District" ~	"NTUNGAMO",
      Country	== "UGANDA" &	District	== "RUBIRIZI District" ~	"RUBIRIZI",
      Country	== "UGANDA" &	District	== "RWAMPARA District" ~	"RWAMPARA",
      Country	== "UGANDA" &	District	== "SHEEMA District" ~	"SHEEMA",
      Country	== "UGANDA" &	District	== "ABIM District" ~	"ABIM",
      Country	== "UGANDA" &	District	== "AMUDAT District" ~	"AMUDAT",
      Country	== "UGANDA" &	District	== "KARENGA District" ~	"KARENGA",
      Country	== "UGANDA" &	District	== "KOTIDO District" ~	"KOTIDO",
      Country	== "UGANDA" &	District	== "MOROTO District" ~	"MOROTO",
      Country	== "UGANDA" &
        District	== "NABILATUK District" ~	"NABILATUK",
      Country	== "UGANDA" &
        District	== "NAKAPIRIPIRIT District" ~	"NAKAPIRIPIRIT",
      Country	== "UGANDA" &	District	== "NAPAK District" ~	"NAPAK",
      Country	== "UGANDA" &	District	== "KIBOGA District" ~	"KIBOGA",
      Country	== "UGANDA" &
        District	== "KYANKWANZI District" ~	"KYANKWANZI",
      Country	== "UGANDA" &	District	== "LUWERO District" ~	"LUWERO",
      Country	== "UGANDA" &	District	== "NAKASEKE District" ~	"NAKASEKE",
      Country	== "UGANDA" &
        District	== "NAKASONGOLA District" ~	"NAKASONGOLA",
      Country	== "UGANDA" &	District	== "AMURIA District" ~	"AMURIA",
      Country	== "UGANDA" &	District	== "BUKEDEA District" ~	"BUKEDEA",
      Country	== "UGANDA" &
        District	== "KABERAMAIDO District" ~	"KABERAMAIDO",
      Country	== "UGANDA" &	District	== "KALAKI District" ~	"KALAKI",
      Country	== "UGANDA" &	District	== "KATAKWI District" ~	"KATAKWI",
      Country	== "UGANDA" &	District	== "KUMI District" ~	"KUMI",
      Country	== "UGANDA" &	District	== "NGORA District" ~	"NGORA",
      Country	== "UGANDA" &	District	== "SERERE District" ~	"SERERE",
      Country	== "UGANDA" &	District	== "SOROTI CITY" ~	"SOROTI",
      Country	== "UGANDA" &	District	== "SOROTI District" ~	"SOROTI",
      Country	== "UGANDA" &	District	== "CENTRAL DIVISION" ~	"CENTRAL",
      Country	== "UGANDA" &	District	== "ENTEBBE DIVISION" ~	"ENTEBBE",
      Country	== "UGANDA" &	District	== "KAWEMPE DIVISION" ~	"KAWEMPE",
      Country	== "UGANDA" &	District	== "MAKINDYE DIVISION" ~	"MAKINDYE",
      Country	== "UGANDA" &	District	== "MUKONO District" ~	"MUKONO",
      Country	== "UGANDA" &	District	== "NAKAWA DIVISION" ~	"NAKAWA",
      Country	== "UGANDA" &	District	== "RUBAGA DIVISION" ~	"RUBAGA",
      Country	== "UGANDA" &	District	== "WAKISO District" ~	"WAKISO",
      Country	== "UGANDA" &	District	== "KASSANDA District" ~	"KASSANDA",
      Country	== "UGANDA" &	District	== "BUTALEJA District" ~	"BUTALEJA",
      Country	== "UGANDA" &	District	== "KAMPALA District" ~	"KAMPALA",
      Country	== "UGANDA" &
        District	== "KIRYANDONGO District" ~	"KIRYANDONGO",
      Country	== "UGANDA" &	District	== "BUKWO District" ~	"BUKWO",
      Country	== "UGANDA" &	District	== "BUDAKA District" ~	"BUDAKA",
      Country	== "UGANDA" &	District	== "KALUNGU District" ~	"KALUNGU",
      Country	== "UGANDA" &	District	== "NEBBI District" ~	"NEBBI",
      Country	== "UGANDA" &	District	== "YUMBE District" ~	"YUMBE",
      Country	== "UGANDA" &	District	== "MPIGI District" ~	"MPIGI",
      Country	== "UGANDA" &	District	== "OMORO District" ~	"OMORO",
      Country	== "UGANDA" &	District	== "PADER District" ~	"PADER",
      Country	== "UGANDA" &	District	== "KAGADI District" ~	"KAGADI",
      Country	== "UGANDA" &	District	== "KIBAALE District" ~	"KIBAALE",
      Country	== "UGANDA" &	District	== "KALIRO District" ~	"KALIRO",
      Country	== "UGANDA" &	District	== "MAYUGE District" ~	"MAYUGE",
      Country	== "UGANDA" &	District	== "RUKIGA District" ~	"RUKIGA",
      Country	== "UGANDA" &	District	== "KAMWENGE District" ~	"KAMWENGE",
      Country	== "UGANDA" &	District	== "AMOLATAR District" ~	"AMOLATAR",
      Country	== "UGANDA" &	District	== "DOKOLO District" ~	"DOKOLO",
      Country	== "UGANDA" &	District	== "OYAM District" ~	"OYAM",
      Country	== "UGANDA" &	District	== "LWENGO District" ~	"LWENGO",
      Country	== "UGANDA" &	District	== "RAKAI District" ~	"RAKAI",
      Country	== "UGANDA" &
        District	== "SEMBABULE District" ~	"SEMBABULE",
      Country	== "UGANDA" &	District	== "BUSIA District" ~	"BUSIA",
      Country	== "UGANDA" &	District	== "KWEEN District" ~	"KWEEN",
      Country	== "UGANDA" &	District	== "MANAFWA District" ~	"MANAFWA",
      Country	== "UGANDA" &
        District	== "NAMISINDWA District" ~	"NAMISINDWA",
      Country	== "UGANDA" &	District	== "PALLISA District" ~	"PALLISA",
      Country	== "UGANDA" &	District	== "SIRONKO District" ~	"SIRONKO",
      Country	== "UGANDA" &	District	== "BUSHENYI District" ~	"BUSHENYI",
      Country	== "UGANDA" &
        District	== "KAPELEBYONG District" ~	"KAPELEBYONG",
      Country	== "UGANDA" &	District	== "MADI-OKOLLO" ~	"MADI OKOLLO",
      Country	== "UGANDA" &	District	== "PAKWACH District" ~	"PAKWACH",
      Country	== "UGANDA" &	District	== "GULU District" ~	"GULU",
      Country	== "UGANDA" &	District	== "MITYANA District" ~	"MITYANA",
      Country	== "UGANDA" &	District	== "MUBENDE District" ~	"MUBENDE",
      Country	== "UGANDA" &	District	== "OBONGI District" ~	"OBONGI",
      Country	== "UGANDA" &	District	== "BUGIRI District" ~	"BUGIRI",
      Country	== "UGANDA" &	District	== "KAABONG District" ~	"KAABONG",
      Country	== "UGANDA" &	District	== "SSEMBABAULE" ~	"SEMBABULE",
      Country	== "UGANDA" &	District	== "KYANKWANZI" ~	"KYAKWANZI",
      Country	== "UGANDA" &	District	== "AGAGO District" ~	"AGAGO",
      Country	== "UGANDA" &	District	== "KASSANDA" ~	"KASANDA",
      Country	== "ZAMBIA" &	District	== "LAVUSHI" ~	"LAVUSHI MANDA",
      Country	== "NIGERIA" &	District	== "YEWA SOUTH" ~	"EGBADO SOUTH",
      Country	== "NIGERIA" &	District	== "YEWA NORTH" ~	"EGBADO NORTH",
      Country	== "ZIMBABWE" &	District	== "MUTARE CITY" ~	"MUTARE",
      Country	== "ZIMBABWE" &	District	== "MT DARWIN" ~	"MOUNT DARWIN",
      Country	== "ZIMBABWE" &	District	== "MHONDORO" ~	"MHONDORO NGEZI",
      Country	== "ZIMBABWE" &	District	== "MUREWA" ~	"MUREHWA",
      TRUE ~ District
    ),
    Province = case_when(
      Country	== "BENIN" &
        Province	== "ATLANTIQUE" &
        District	== "ABOMEY-CALAVI 1" ~	"ATLANTIQUE",
      Country	== "BENIN" &
        Province	== "ATLANTIQUE" &
        District	== "ABOMEY-CALAVI 2" ~	"ATLANTIQUE",
      Country	== "BENIN" &
        Province	== "ATLANTIQUE" &
        District	== "ABOMEY-CALAVI 3" ~	"ATLANTIQUE",
      Country	== "BENIN" &
        Province	== "ATLANTIQUE" &	District	== "SO-AVA" ~	"ATLANTIQUE",
      Country	== "BENIN" &
        Province	== "ATLANTIQUE" &	District	== "TOFFO" ~	"ATLANTIQUE",
      Country	== "BENIN" &
        Province	== "BORGOU" &	District	== "BEMBEREKE" ~	"BORGOU",
      Country	== "BENIN" &
        Province	== "BORGOU" &	District	== "NIKKI" ~	"BORGOU",
      Country	== "BENIN" &
        Province	== "BORGOU" &	District	== "PARAKOU" ~	"BORGOU",
      Country	== "BENIN" &
        Province	== "BORGOU" &	District	== "PERERE" ~	"BORGOU",
      Country	== "BENIN" &
        Province	== "BORGOU" &	District	== "TCHAOUROU" ~	"BORGOU",
      Country	== "BENIN" &
        Province	== "DONGA" &	District	== "DJOUGOU" ~	"DONGA",
      Country	== "BENIN" &
        Province	== "LITTORAL" &	District	== "COTONOU 1" ~	"LITTORAL",
      Country	== "BENIN" &
        Province	== "LITTORAL" &	District	== "COTONOU 2" ~	"LITTORAL",
      Country	== "BENIN" &
        Province	== "LITTORAL" &	District	== "COTONOU 3" ~	"LITTORAL",
      Country	== "BENIN" &
        Province	== "LITTORAL" &	District	== "COTONOU 4" ~	"LITTORAL",
      Country	== "BENIN" &
        Province	== "LITTORAL" &	District	== "COTONOU 5" ~	"LITTORAL",
      Country	== "BENIN" &
        Province	== "LITTORAL" &	District	== "COTONOU 6" ~	"LITTORAL",
      Country	== "BENIN" &
        Province	== "OUEME" &	District	== "AGUEGUES" ~	"OUEME",
      Country	== "BENIN" &
        Province	== "OUEME" &	District	== "PORTO-NOVO 1" ~	"OUEME",
      Country	== "BENIN" &
        Province	== "OUEME" &	District	== "PORTO-NOVO 2" ~	"OUEME",
      Country	== "BENIN" &
        Province	== "OUEME" &	District	== "PORTO-NOVO 3" ~	"OUEME",
      Country	== "BENIN" &
        Province	== "OUEME" &	District	== "SEME-KPODJI" ~	"OUEME",
      Country	== "COTE D IVOIRE" &
        Province	== "CAVALLY" &	District	== "GUIGLO" ~	"CAVALLY",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN1" &	District	== "ABOBO OUEST" ~	"ABIDJAN 1",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN1" &	District	== "ANYAMA" ~	"ABIDJAN 1",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN1" &	District	== "YOPOUGON EST" ~	"ABIDJAN 1",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN1" &
        District	== "YOPOUGON OUEST-SONGON" ~	"ABIDJAN 1",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN2" &
        District	== "COCODY-BINGERVILLE" ~	"ABIDJAN 2",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN2" &	District	== "KOUMASSI" ~	"ABIDJAN 2",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN2" &	District	== "PORT BOUET-VRIDI" ~	"ABIDJAN 2",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN2" &
        District	== "TREICHVILLE-MARCORY" ~	"ABIDJAN 2",
      Country	== "COTE D IVOIRE" &
        Province	== "AGNEBY_TIASSA" &
        District	== "AGBOVILLE" ~	"AGNEBY-TIASSA",
      Country	== "COTE D IVOIRE" &
        Province	== "AGNEBY_TIASSA" &
        District	== "TIASSALE" ~	"AGNEBY-TIASSA",
      Country	== "COTE D IVOIRE" &
        Province	== "BAFING" &	District	== "KORO" ~	"BAFING",
      Country	== "COTE D IVOIRE" &
        Province	== "BAFING" &	District	== "TOUBA" ~	"BAFING",
      Country	== "COTE D IVOIRE" &
        Province	== "BAGOUE" &	District	== "BOUNDIALI" ~	"BAGOUE",
      Country	== "COTE D IVOIRE" &
        Province	== "BAGOUE" &	District	== "KOUTO" ~	"BAGOUE",
      Country	== "COTE D IVOIRE" &
        Province	== "BAGOUE" &	District	== "TENGRELA" ~	"BAGOUE",
      Country	== "COTE D IVOIRE" &
        Province	== "BELIER" &	District	== "DIDIEVI" ~	"BELIER",
      Country	== "COTE D IVOIRE" &
        Province	== "BELIER" &	District	== "TIEBISSOU" ~	"BELIER",
      Country	== "COTE D IVOIRE" &
        Province	== "BELIER" &	District	== "TOUMODI" ~	"BELIER",
      Country	== "COTE D IVOIRE" &
        Province	== "BELIER" &	District	== "YAMOUSSOUKRO" ~	"BELIER",
      Country	== "COTE D IVOIRE" &
        Province	== "BERE" &	District	== "DIANRA" ~	"BERE",
      Country	== "COTE D IVOIRE" &
        Province	== "BERE" &	District	== "KOUNAHIRI" ~	"BERE",
      Country	== "COTE D IVOIRE" &
        Province	== "BERE" &	District	== "MANKONO" ~	"BERE",
      Country	== "COTE D IVOIRE" &
        Province	== "BOUKANI" &	District	== "BOUNA" ~	"BOUNKANI",
      Country	== "COTE D IVOIRE" &
        Province	== "BOUKANI" &	District	== "DOROPO" ~	"BOUNKANI",
      Country	== "COTE D IVOIRE" &
        Province	== "BOUKANI" &	District	== "NASSIAN" ~	"BOUNKANI",
      Country	== "COTE D IVOIRE" &
        Province	== "BOUKANI" &	District	== "TEHINI" ~	"BOUNKANI",
      Country	== "COTE D IVOIRE" &
        Province	== "CAVALLY" &	District	== "BLOLEQUIN" ~	"CAVALLY",
      Country	== "COTE D IVOIRE" &
        Province	== "CAVALLY" &	District	== "DUEKOUE" ~	"MOYEN CAVALLY",
      Country	== "COTE D IVOIRE" &
        Province	== "CAVALLY" &	District	== "TAI" ~	"CAVALLY",
      Country	== "COTE D IVOIRE" &
        Province	== "CAVALLY" &	District	== "TOULEPLEU" ~	"CAVALLY",
      Country	== "COTE D IVOIRE" &
        Province	== "FOLON" &	District	== "KANIASSO" ~	"FOLON",
      Country	== "COTE D IVOIRE" &
        Province	== "FOLON" &	District	== "MINIGNAN" ~	"FOLON",
      Country	== "COTE D IVOIRE" &
        Province	== "GBEKE" &	District	== "BEOUMI" ~	"GBEKE",
      Country	== "COTE D IVOIRE" &
        Province	== "GBEKE" &	District	== "BOTRO" ~	"GBEKE",
      Country	== "COTE D IVOIRE" &
        Province	== "GBEKE" &	District	== "BOUAKE NORD-EST" ~	"GBEKE",
      Country	== "COTE D IVOIRE" &
        Province	== "GBEKE" &	District	== "BOUAKE NORD-OUEST" ~	"GBEKE",
      Country	== "COTE D IVOIRE" &
        Province	== "GBEKE" &	District	== "BOUAKE SUD" ~	"GBEKE",
      Country	== "COTE D IVOIRE" &
        Province	== "GBEKE" &	District	== "SAKASSOU" ~	"GBEKE",
      Country	== "COTE D IVOIRE" &
        Province	== "GBOKLE" &	District	== "SASSANDRA" ~	"GBOKLE",
      Country	== "COTE D IVOIRE" &
        Province	== "GOH" &	District	== "OUME" ~	"GOH",
      Country	== "COTE D IVOIRE" &
        Province	== "GONTOUGO" &	District	== "BONDOUKOU" ~	"GONTOUGO",
      Country	== "COTE D IVOIRE" &
        Province	== "GONTOUGO" &	District	== "KOUN-FAO" ~	"GONTOUGO",
      Country	== "COTE D IVOIRE" &
        Province	== "GONTOUGO" &	District	== "SANDEGUE" ~	"GONTOUGO",
      Country	== "COTE D IVOIRE" &
        Province	== "GONTOUGO" &	District	== "TANDA" ~	"GONTOUGO",
      Country	== "COTE D IVOIRE" &
        Province	== "GONTOUGO" &	District	== "TRANSUA" ~	"GONTOUGO",
      Country	== "COTE D IVOIRE" &
        Province	== "GRANDS_PONTS" &	District	== "DABOU" ~	"GRANDS PONTS",
      Country	== "COTE D IVOIRE" &
        Province	== "GRANDS_PONTS" &
        District	== "GRAND-LAHOU" ~	"GRANDS PONTS",
      Country	== "COTE D IVOIRE" &
        Province	== "GRANDS_PONTS" &
        District	== "JACQUEVILLE" ~	"GRANDS PONTS",
      Country	== "COTE D IVOIRE" &
        Province	== "GUEMON" &	District	== "BANGOLO" ~	"GUEMON",
      Country	== "COTE D IVOIRE" &
        Province	== "GUEMON" &	District	== "KOUIBLY" ~	"GUEMON",
      Country	== "COTE D IVOIRE" &
        Province	== "HAMBOL" &	District	== "DABAKALA" ~	"HAMBOL",
      Country	== "COTE D IVOIRE" &
        Province	== "HAMBOL" &	District	== "KATIOLA" ~	"HAMBOL",
      Country	== "COTE D IVOIRE" &
        Province	== "HAMBOL" &	District	== "NIAKARAMADOUGOU" ~	"HAMBOL",
      Country	== "COTE D IVOIRE" &
        Province	== "HAUT_SASSANDRA" &	District	== "DALOA" ~	"HAUT SASSANDRA",
      Country	== "COTE D IVOIRE" &
        Province	== "HAUT_SASSANDRA" &	District	== "ISSIA" ~	"HAUT SASSANDRA",
      Country	== "COTE D IVOIRE" &
        Province	== "HAUT_SASSANDRA" &
        District	== "VAVOUA" ~	"HAUT SASSANDRA",
      Country	== "COTE D IVOIRE" &
        Province	== "HAUT_SASSANDRA" &
        District	== "ZOUKOUGBEU" ~	"HAUT-SASSANDRA",
      Country	== "COTE D IVOIRE" &
        Province	== "IFFOU" &	District	== "DAOUKRO" ~	"IFOU",
      Country	== "COTE D IVOIRE" &
        Province	== "IFFOU" &	District	== "MBAHIAKRO" ~	"IFOU",
      Country	== "COTE D IVOIRE" &
        Province	== "IFFOU" &	District	== "PRIKRO" ~	"IFOU",
      Country	== "COTE D IVOIRE" &
        Province	== "INDENIE_DJUABLIN" &
        District	== "AGNIBILEKROU" ~	"INDENIE-DJUABLIN",
      Country	== "COTE D IVOIRE" &
        Province	== "INDENIE_DJUABLIN" &
        District	== "BETTIE" ~	"INDENIE-DJUABLIN",
      Country	== "COTE D IVOIRE" &
        Province	== "KABADOUGOU" &	District	== "MADINANI" ~	"KABADOUGOU",
      Country	== "COTE D IVOIRE" &
        Province	== "KABADOUGOU" &	District	== "ODIENNE" ~	"KABADOUGOU",
      Country	== "COTE D IVOIRE" &
        Province	== "LAME" &	District	== "YAKASSE-ATTOBROU" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "LOH_DJIBOUA" &	District	== "DIVO" ~	"LOH-DJIBOUA",
      Country	== "COTE D IVOIRE" &
        Province	== "LOH_DJIBOUA" &	District	== "GUITRY" ~	"LOH-DJIBOUA",
      Country	== "COTE D IVOIRE" &
        Province	== "LOH_DJIBOUA" &	District	== "LAKOTA" ~	"LOH-DJIBOUA",
      Country	== "COTE D IVOIRE" &
        Province	== "MARAHOUE" &	District	== "BOUAFLE" ~	"MARAHOUT",
      Country	== "COTE D IVOIRE" &
        Province	== "MARAHOUE" &	District	== "SINFRA" ~	"MARAHOUT",
      Country	== "COTE D IVOIRE" &
        Province	== "MARAHOUE" &	District	== "ZUENOULA" ~	"MARAHOUT",
      Country	== "COTE D IVOIRE" &
        Province	== "MORONOU" &	District	== "ARRAH" ~	"MORONOU",
      Country	== "COTE D IVOIRE" &
        Province	== "MORONOU" &	District	== "BONGOUANOU" ~	"MORONOU",
      Country	== "COTE D IVOIRE" &
        Province	== "MORONOU" &	District	== "MBATTO" ~	"MORONOU",
      Country	== "COTE D IVOIRE" &
        Province	== "NAWA" &	District	== "BUYO" ~	"NAWA",
      Country	== "COTE D IVOIRE" &
        Province	== "NAWA" &	District	== "GUEYO" ~	"NAWA",
      Country	== "COTE D IVOIRE" &
        Province	== "NAWA" &	District	== "MEAGUI" ~	"NAWA",
      Country	== "COTE D IVOIRE" &
        Province	== "NAWA" &	District	== "SOUBRE" ~	"NAWA",
      Country	== "COTE D IVOIRE" &
        Province	== "NZI" &	District	== "BOCANDA" ~	"NZI",
      Country	== "COTE D IVOIRE" &
        Province	== "NZI" &	District	== "DIMBOKRO" ~	"NZI",
      Country	== "COTE D IVOIRE" &
        Province	== "NZI" &	District	== "KOUASSI-KOUASSIKRO" ~	"NZI",
      Country	== "COTE D IVOIRE" &
        Province	== "PORO" &	District	== "DIKODOUGOU" ~	"PORO",
      Country	== "COTE D IVOIRE" &
        Province	== "PORO" &	District	== "KORHOGO 1" ~	"PORO",
      Country	== "COTE D IVOIRE" &
        Province	== "PORO" &	District	== "KORHOGO 2" ~	"PORO",
      Country	== "COTE D IVOIRE" &
        Province	== "PORO" &	District	== "MBENGUE" ~	"PORO",
      Country	== "COTE D IVOIRE" &
        Province	== "PORO" &	District	== "SINEMATIALI" ~	"PORO",
      Country	== "COTE D IVOIRE" &
        Province	== "SAN_PEDRO1" &	District	== "SAN PEDRO" ~	"SAN-PEDRO",
      Country	== "COTE D IVOIRE" &
        Province	== "SAN_PEDRO1" &	District	== "TABOU" ~	"SAN-PEDRO",
      Country	== "COTE D IVOIRE" &
        Province	== "SUD_COMOE" &	District	== "ABOISSO" ~	"SUD-COMOE",
      Country	== "COTE D IVOIRE" &
        Province	== "SUD_COMOE" &	District	== "GRAND-BASSAM" ~	"SUD COMOE",
      Country	== "COTE D IVOIRE" &
        Province	== "SUD_COMOE" &	District	== "TIAPOUM" ~	"SUD-COMOE",
      Country	== "COTE D IVOIRE" &
        Province	== "TCHOLOGO" &	District	== "FERKESSEDOUGOU" ~	"TCHOLOGO",
      Country	== "COTE D IVOIRE" &
        Province	== "TCHOLOGO" &	District	== "KONG" ~	"TCHOLOGO",
      Country	== "COTE D IVOIRE" &
        Province	== "TCHOLOGO" &	District	== "OUANGOLODOUGOU" ~	"TCHOLOGO",
      Country	== "COTE D IVOIRE" &
        Province	== "TONKPI" &	District	== "BIANKOUMA" ~	"TONKPI",
      Country	== "COTE D IVOIRE" &
        Province	== "TONKPI" &	District	== "DANANE" ~	"TONKPI",
      Country	== "COTE D IVOIRE" &
        Province	== "TONKPI" &	District	== "MAN" ~	"TONKPI",
      Country	== "COTE D IVOIRE" &
        Province	== "TONKPI" &	District	== "ZOUAN-HOUNIEN" ~	"TONKPI",
      Country	== "COTE D IVOIRE" &
        Province	== "WORODOUGOU" &	District	== "KANI" ~	"WORODOUGOU",
      Country	== "COTE D IVOIRE" &
        Province	== "WORODOUGOU" &	District	== "SEGUELA" ~	"WORODOUGOU",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN1" &	District	== "ABOBO EST" ~	"ABIDJAN 1",
      Country	== "COTE D IVOIRE" &
        Province	== "GBOKLE" &	District	== "FRESCO" ~	"GBOKLE",
      Country	== "COTE D IVOIRE" &
        Province	== "GOH" &	District	== "GAGNOA 1" ~	"GOH",
      Country	== "COTE D IVOIRE" &
        Province	== "SUD_COMOE" &	District	== "ADIAKE" ~	"SUD-COMOE",
      Country	== "COTE D IVOIRE" &
        Province	== "ABIDJAN2" &
        District	== "ADJAME-PLATEAU-ATTECOUBE" ~	"ABIDJAN 2",
      Country	== "COTE D IVOIRE" &
        Province	== "AGNEBY_TIASSA" &	District	== "SIKENSI" ~	"AGNEBY-TIASSA",
      Country	== "COTE D IVOIRE" &
        Province	== "BAFING" &	District	== "OUANINOU" ~	"BAFING",
      Country	== "COTE D IVOIRE" &
        Province	== "GOH" &	District	== "GAGNOA 2" ~	"GOH",
      Country	== "COTE D IVOIRE" &
        Province	== "INDENIE_DJUABLIN" &
        District	== "ABENGOUROU" ~	"INDENIE-DJUABLIN",
      Country	== "COTE D IVOIRE" &
        Province	== "LAME" &	District	== "AKOUPE" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "LAME" &	District	== "ALEPE" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "LAME" &	District	== "ADZOPE" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "ME" &	District	== "ADZOPE" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "ME" &	District	== "AKOUPE" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "ME" &	District	== "YAKASSE-ATTOBROU" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "SAN PEDRO" &	District	== "TABOU" ~	"SAN-PEDRO",
      Country	== "COTE D IVOIRE" &
        Province	== "ME" &	District	== "ALEPE" ~	"ME",
      Country	== "COTE D IVOIRE" &
        Province	== "SAN PEDRO" &	District	== "SAN PEDRO" ~	"SAN-PEDRO",
      Country	== "GUINEA" &
        Province	== "FARANAH" &	District	== "DABOLA" ~	"FARANAH",
      Country	== "GUINEA" &
        Province	== "FARANAH" &	District	== "DINGUIRAYE" ~	"FARANAH",
      Country	== "GUINEA" &
        Province	== "FARANAH" &	District	== "FARANAH" ~	"FARANAH",
      Country	== "GUINEA" &
        Province	== "FARANAH" &	District	== "KISSIDOUGOU" ~	"FARANAH",
      Country	== "GUINEA" &
        Province	== "KANKAN" &	District	== "KANKAN" ~	"KANKAN",
      Country	== "GUINEA" &
        Province	== "KANKAN" &	District	== "KÉROUANE" ~	"KANKAN",
      Country	== "GUINEA" &
        Province	== "KANKAN" &	District	== "KOUROUSSA" ~	"KANKAN",
      Country	== "GUINEA" &
        Province	== "KANKAN" &	District	== "MANDIANA" ~	"KANKAN",
      Country	== "GUINEA" &
        Province	== "KANKAN" &	District	== "SIGUIRI" ~	"KANKAN",
      Country	== "GUINEA" &
        Province	== "LABE" &	District	== "KOUBIA" ~	"LABE",
      Country	== "GUINEA" &	Province	== "LABE" &
        District	== "MALI" ~	"LABE",
      Country	== "GUINEA" &
        Province	== "LABE" &	District	== "TOUGUÉ" ~	"LABE",
      Country	== "GUINEA" &
        Province	== "NZEREKORE" &	District	== "BEYLA" ~	"NZEREKORE",
      Country	== "GUINEA" &
        Province	== "NZEREKORE" &	District	== "GUECKÉDOU" ~	"NZEREKORE",
      Country	== "GUINEA" &
        Province	== "NZEREKORE" &	District	== "LOLA" ~	"NZEREKORE",
      Country	== "GUINEA" &
        Province	== "NZEREKORE" &	District	== "MACENTA" ~	"NZEREKORE",
      Country	== "GUINEA" &
        Province	== "NZEREKORE" &	District	== "N'ZÉRÉKORÉ" ~	"NZEREKORE",
      Country	== "GUINEA" &
        Province	== "NZEREKORE" &	District	== "YOMOU" ~	"NZEREKORE",
      Country	== "GUINEA" &	Province	== "LABE" &
        District	== "LABÉ" ~	"LABE",
      Country	== "GUINEA" &
        Province	== "BOKE" &	District	== "BOFFA" ~	"BOKE",
      Country	== "GUINEA" &	Province	== "BOKE" &
        District	== "BOKÉ" ~	"BOKE",
      Country	== "GUINEA" &	Province	== "BOKE" &
        District	== "FRIA" ~	"BOKE",
      Country	== "GUINEA" &
        Province	== "BOKE" &	District	== "GAOUAL" ~	"BOKE",
      Country	== "GUINEA" &
        Province	== "BOKE" &	District	== "KOUNDARA" ~	"BOKE",
      Country	== "GUINEA" &
        Province	== "CONAKRY" &	District	== "DIXINN" ~	"CONAKRY",
      Country	== "GUINEA" &
        Province	== "CONAKRY" &	District	== "KALOUM" ~	"CONAKRY",
      Country	== "GUINEA" &
        Province	== "CONAKRY" &	District	== "MATAM" ~	"CONAKRY",
      Country	== "GUINEA" &
        Province	== "CONAKRY" &	District	== "MATOTO" ~	"CONAKRY",
      Country	== "GUINEA" &
        Province	== "CONAKRY" &	District	== "RATOMA" ~	"CONAKRY",
      Country	== "GUINEA" &
        Province	== "KINDIA" &	District	== "COYAH" ~	"KINDIA",
      Country	== "GUINEA" &
        Province	== "KINDIA" &	District	== "DUBRÉKA" ~	"KINDIA",
      Country	== "GUINEA" &
        Province	== "KINDIA" &	District	== "FORÉCARIAH" ~	"KINDIA",
      Country	== "GUINEA" &
        Province	== "KINDIA" &	District	== "KINDIA" ~	"KINDIA",
      Country	== "GUINEA" &
        Province	== "KINDIA" &	District	== "TÉLIMÉLÉ" ~	"KINDIA",
      Country	== "GUINEA" &
        Province	== "LABE" &	District	== "LÉLOUMA" ~	"LABE",
      Country	== "GUINEA" &
        Province	== "MAMOU" &	District	== "DALABA" ~	"MAMOU",
      Country	== "GUINEA" &
        Province	== "MAMOU" &	District	== "MAMOU" ~	"MAMOU",
      Country	== "GUINEA" &
        Province	== "MAMOU" &	District	== "PITA" ~	"MAMOU",
      Country	== "BURKIN FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" ~	"DEDOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-SUD" ~	"MANGA",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      Country	== "BURKIN FASO" &	Province	== "HAUTS-BASSINS" ~	"BOBO",
      Country	== "BURKIN FASO" &	Province	== "NORD" ~	"OUAHIGOUYA",
      Country	== "BURKIN FASO" &	Province	== "CENTRE" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-NORD" ~	"KAYA",
      Country	== "BURKIN FASO" &	Province	== "CENTRE" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "PLATEAU_CENTRAL" ~	"ZINIARE",
      Country	== "BURKIN FASO" &	Province	== "PLATEAU_CENTRAL" ~	"ZINIARE",
      Country	== "BURKIN FASO" &	Province	== "NORD" ~	"OUAHIGOUYA",
      Country	== "BURKIN FASO" &	Province	== "PLATEAU_CENTRAL" ~	"ZINIARE",
      Country	== "BURKIN FASO" &	Province	== "SAHEL" ~	"DORI",
      Country	== "BURKIN FASO" &	Province	== "SUD-OUEST" ~	"GAOUA",
      Country	== "BURKIN FASO" &	Province	== "SUD-OUEST" ~	"GAOUA",
      Country	== "BURKIN FASO" &	Province	== "SAHEL" ~	"DORI",
      Country	== "BURKIN FASO" &	Province	== "SAHEL" ~	"DORI",
      Country	== "BURKIN FASO" &	Province	== "NORD" ~	"OUAHIGOUYA",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-EST" ~	"TENKODOGO",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-EST" ~	"TENKODOGO",
      Country	== "BURKIN FASO" &	Province	== "CASCADES" ~	"BANFORA",
      Country	== "BURKIN FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" ~	"DEDOUGOU",
      Country	== "BURKIN FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" ~	"DEDOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-EST" ~	"TENKODOGO",
      Country	== "BURKIN FASO" &	Province	== "HAUTS-BASSINS" ~	"BOBO",
      Country	== "BURKIN FASO" &	Province	== "HAUTS-BASSINS" ~	"BOBO",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-SUD" ~	"MANGA",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-SUD" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "EST" ~	"OUAGADOUGOU",
      Country	== "BURKIN FASO" &	Province	== "EST" ~	"FADA",
      Country	== "BURKIN FASO" &	Province	== "CENTRE-NORD" ~	"KAYA",
      Country	== "BURKIN FASO" &	Province	== "HAUTS-BASSINS" ~	"BOBO",
      Country	== "BURKIN FASO" &	Province	== "NORD" ~	"OUAHIGOUYA",
      Country	== "BURKIN FASO" &	Province	== "CASCADES" ~	"BANFORA",
      Country	== "BURKIN FASO" &	Province	== "EST" ~	"FADA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD-UBANGI" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMANI" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-CENTRAL" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS-UELE" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD-KIVU" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-KATANGA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-UELE" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-CENTRAL" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAI-NDOMBE" ~	"MAINDOMBE",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ATSINANANA" &
        District	== "MIDONGY-ATSIMO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ATSINANANA" &
        District	== "VONDROZO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "AMBOHIMAHASOA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &	District	== "BEROROHA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &	District	== "MAROLAMBO" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "MENABE" &	District	== "BELO-TSIRIBIHINA" ~	"MENABE",
      Country	== "MADAGASCAR" &
        Province	== "MENABE" &	District	== "MIANDRIVAZO" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "MENABE" &	District	== "MORONDAVA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &	District	== "BENENITRA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &
        District	== "ANTSIRABE I" ~	"VAKINANKARATRA",
      Country	== "MADAGASCAR" &
        Province	== "VATOVAVY FITOVINANY" &
        District	== "NOSY-VARIKA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "AMORON'I MANIA" &
        District	== "FANDRIANA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &
        District	== "ANTANANARIVO-ATSIMONDRANO" ~	"ANALAMANGA",
      Country	== "MADAGASCAR" &
        Province	== "ANOSY" &	District	== "BETROKA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &	District	== "SAKARAHA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "IHOROMBE" &	District	== "IAKORA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "IHOROMBE" &	District	== "IVOHIBE" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "VATOVAVY FITOVINANY" &
        District	== "MANANJARY" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "AMORON'I MANIA" &
        District	== "AMBATOFINANDRAHANA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &
        District	== "AMBOHIDRATRIMO" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &	District	== "ANDRAMASINA" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &	District	== "ANJOZOROBE" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &	District	== "ANKAZOBE" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ANALANJIROFO" &	District	== "VAVATENINA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ANDROY" &	District	== "AMBOVOMBE-ANDROY" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ANOSY" &	District	== "AMBOASARY-ATSIMO" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ANOSY" &	District	== "TAOLAGNARO" ~	"ANOSY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &	District	== "MOROMBE" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ATSINANANA" &
        District	== "FARAFANGANA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ATSINANANA" &
        District	== "VANGAINDRANO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &
        District	== "ANTANAMBAO-MANAMPOTSY" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &	District	== "MAHANORO" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &
        District	== "VOHIBINANY (BRICKAVILLE)" ~	"ATSINANANA",
      Country	== "MADAGASCAR" &
        Province	== "BOENY" &	District	== "AMBATO-BOINA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "BOENY" &	District	== "MAHAJANGA II" ~	"BOENI",
      Country	== "MADAGASCAR" &
        Province	== "BOENY" &	District	== "MITSINJO" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "AMBALAVAO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "FIANARANTSOA I" ~	"HAUTE-MATSIATRA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "IKALAMAVONY" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "MELAKY" &	District	== "MORAFENOBE" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &
        District	== "AMBATOLAMPY" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &
        District	== "ANTANIFOTSY" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &
        District	== "ANTSIRABE II" ~	"VAKINANKARATRA",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &	District	== "BETAFO" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &
        District	== "MANDOTO" ~	"VAKINANKARATRA",
      Country	== "MADAGASCAR" &
        Province	== "VATOVAVY FITOVINANY" &
        District	== "VOHIPENO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &
        District	== "ANTANANARIVO-AVARADRANO" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &
        District	== "ANTANANARIVO-RENIVOHITRA" ~	"ANALAMANGA",
      Country	== "MADAGASCAR" &
        Province	== "ANALAMANGA" &
        District	== "MANJAKANDRIANA" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ANALANJIROFO" &
        District	== "SOANIERANA-IVONGO" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &	District	== "VATOMANDRY" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "BOENY" &	District	== "MAROVOAY" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "BOENY" &	District	== "SOALALA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "VOHIBATO" ~	"HAUTE-MATSIATRA",
      Country	== "MADAGASCAR" &
        Province	== "IHOROMBE" &	District	== "IHOSY" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "MELAKY" &	District	== "ANTSALOVA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "MENABE" &	District	== "MAHABO" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "AMORON'I MANIA" &
        District	== "MANANDRIANA" ~	"AMORON'I MANIA",
      Country	== "MADAGASCAR" &
        Province	== "ANALANJIROFO" &
        District	== "FENOARIVO-ATSINANANA" ~	"ANALANJIROFO",
      Country	== "MADAGASCAR" &
        Province	== "ANALANJIROFO" &	District	== "MAROANTSETRA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ANALANJIROFO" &
        District	== "NOSY-BORAHA (SAINTE MARIE)" ~	"ANALANJIROFO",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &
        District	== "ANKAZOABO-ATSIMO" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &
        District	== "BETIOKY-ATSIMO" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &
        District	== "TOLIARA I ET II" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &	District	== "TOAMASINA I" ~	"ATSINANANA",
      Country	== "MADAGASCAR" &
        Province	== "ATSINANANA" &	District	== "TOAMASINA II" ~	"ATSINANANA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "ISANDRA" ~	"HAUTE-MATSIATRA",
      Country	== "MADAGASCAR" &
        Province	== "HAUTE MATSIATRA" &
        District	== "LALANGINA" ~	"HAUTE-MATSIATRA",
      Country	== "MADAGASCAR" &
        Province	== "MELAKY" &	District	== "AMBATOMAINTY" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "MELAKY" &	District	== "BESALAMPY" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "MENABE" &	District	== "MANJA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "VAKINANKARATRA" &
        District	== "FARATSIHO" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "VATOVAVY FITOVINANY" &
        District	== "IFANADIANA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "VATOVAVY FITOVINANY" &
        District	== "IKONGO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "VATOVAVY FITOVINANY" &
        District	== "MANAKARA-ATSIMO" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "AMORON'I MANIA" &
        District	== "AMBOSITRA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ANDROY" &	District	== "BEKILY" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ANDROY" &	District	== "TSIHOMBE" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ATSINANANA" &
        District	== "BEFOTAKA" ~	"FIANARANTSOA",
      Country	== "MADAGASCAR" &
        Province	== "ANDROY" &	District	== "BELOHA" ~	"TOLIARY",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &	District	== "AMPANIHY" ~	"SUD-OUEST",
      Country	== "MADAGASCAR" &
        Province	== "BOENY" &	District	== "MAHAJANGA I" ~	"BOENI",
      Country	== "MADAGASCAR" &
        Province	== "MELAKY" &	District	== "MAINTIRANO" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "ANALANJIROFO" &
        District	== "MANANARA-AVARATRA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ITASY" &	District	== "MIARINARIVO" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ALAOTRA MANGORO" &
        District	== "AMPARAFARAVOLA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "BONGOLAVA" &
        District	== "TSIROANOMANDIDY" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "SAVA" &	District	== "VOHIMARINA (VOHÉMAR)" ~	"SAVA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &	District	== "BEALANANA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "BONGOLAVA" &
        District	== "FENOARIVO-AFOVOANY" ~	"BONGOLAVA",
      Country	== "MADAGASCAR" &
        Province	== "DIANA" &	District	== "ANTSIRANANA II" ~	"DIANA",
      Country	== "MADAGASCAR" &
        Province	== "DIANA" &	District	== "NOSY-BE" ~	"DIANA",
      Country	== "MADAGASCAR" &
        Province	== "SAVA" &	District	== "ANDAPA" ~	"ANTSIRANANA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &	District	== "ANALALAVA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &
        District	== "BEFANDRIANA-AVARATRA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &
        District	== "BORIZINY (PORT-BERGER)" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &	District	== "MAMPIKONY" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &	District	== "MANDRITSARA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "ALAOTRA MANGORO" &
        District	== "ANOSIBE AN-ALA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "BETSIBOKA" &	District	== "KANDREHO" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "BETSIBOKA" &	District	== "TSARATANANA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "DIANA" &	District	== "AMBANJA" ~	"ANTSIRANANA",
      Country	== "MADAGASCAR" &
        Province	== "DIANA" &	District	== "AMBILOBE" ~	"ANTSIRANANA",
      Country	== "MADAGASCAR" &
        Province	== "DIANA" &	District	== "ANTSIRANANA I" ~	"ANTSIRANANA",
      Country	== "MADAGASCAR" &
        Province	== "SAVA" &	District	== "ANTALAHA" ~	"ANTSIRANANA",
      Country	== "MADAGASCAR" &
        Province	== "SOFIA" &	District	== "ANTSOHIHY" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "ALAOTRA MANGORO" &
        District	== "AMBATONDRAZAKA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ALAOTRA MANGORO" &
        District	== "ANDILAMENA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ALAOTRA MANGORO" &	District	== "MORAMANGA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "SAVA" &	District	== "SAMBAVA" ~	"ANTSIRANANA",
      Country	== "MADAGASCAR" &
        Province	== "BETSIBOKA" &	District	== "MAEVATANANA" ~	"MAHAJANGA",
      Country	== "MADAGASCAR" &
        Province	== "ITASY" &	District	== "ARIVONIMAMO" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "ITASY" &	District	== "SOAVINANDRIANA" ~	"ANTANANARIVO",
      Country	== "MADAGASCAR" &
        Province	== "BONGOLAVA" &
        District	== "FENOARIVO-AFOVOANY" ~	"BONGOLAVA",
      Country	== "MADAGASCAR" &
        Province	== "ALAOTRA MANGORO" &
        District	== "ANOSIBE AN-ALA" ~	"TOAMASINA",
      Country	== "MADAGASCAR" &
        Province	== "ATSIMO ANDREFANA" &
        District	== "TOLIARA I ET II" ~	"TOLIARY",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO CENTRAL" &
        District	== "NSONA-PANGU" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO CENTRAL" &	District	== "MATADI" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO CENTRAL" &	District	== "MUANDA" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO CENTRAL" &	District	== "LUOZI" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "BOMA BUNGU" ~	"BAS-CONGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "BITTOU" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "OUARGAYE" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-NORD" &	District	== "BOUSSOUMA" ~	"KAYA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE" &	District	== "BASKUY" ~	"OUAGADOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-NORD" &	District	== "TOUGOURI" ~	"KAYA",
      Country	== "BURKINA FASO" &
        Province	== "PLATEAU_CENTRAL" &	District	== "ZORGHO" ~	"ZINIARE",
      Country	== "BURKINA FASO" &
        Province	== "EST" &	District	== "MANNI" ~	"FADA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE" &	District	== "BOULMIOUGOU" ~	"OUAGADOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "SABOU" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-NORD" &	District	== "KAYA" ~	"KAYA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-NORD" &	District	== "BARSALOGHO" ~	"KAYA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "LEO" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "NANORO" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "REO" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "SAPOUY" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "TENADO" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "EST" &	District	== "BOGANDE" ~	"FADA",
      Country	== "BURKINA FASO" &
        Province	== "EST" &	District	== "DIAPAGA" ~	"FADA",
      Country	== "BURKINA FASO" &
        Province	== "EST" &	District	== "FADA" ~	"FADA",
      Country	== "BURKINA FASO" &
        Province	== "EST" &	District	== "GAYERI" ~	"FADA",
      Country	== "BURKINA FASO" &
        Province	== "EST" &	District	== "PAMA" ~	"FADA",
      Country	== "BURKINA FASO" &
        Province	== "PLATEAU_CENTRAL" &	District	== "BOUSSE" ~	"ZINIARE",
      Country	== "BURKINA FASO" &
        Province	== "PLATEAU_CENTRAL" &	District	== "ZINIARE" ~	"ZINIARE",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "GARANGO" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "KOUPELA" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "POUYTENGA" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "TENKODOGO" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-EST" &	District	== "ZABRE" ~	"TENKODOGO",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-NORD" &	District	== "BOULSA" ~	"KAYA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-NORD" &	District	== "KONGOUSSI" ~	"KAYA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-OUEST" &	District	== "KOUDOUGOU" ~	"KOUDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-SUD" &	District	== "KOMBISSIRI" ~	"MANGA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-SUD" &	District	== "MANGA" ~	"MANGA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-SUD" &	District	== "PÔ" ~	"MANGA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE-SUD" &	District	== "SAPONE" ~	"MANGA",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE" &	District	== "BOGODOGO" ~	"OUAGADOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE" &	District	== "NONGR MASSOM" ~	"OUAGADOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CENTRE" &	District	== "SIG NOGHIN" ~	"OUAGADOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "NORD" &	District	== "GOURCY" ~	"OUAHIGOUYA",
      Country	== "BURKINA FASO" &
        Province	== "NORD" &	District	== "THIOU" ~	"OUAHIGOUYA",
      Country	== "BURKINA FASO" &
        Province	== "NORD" &	District	== "YAKO" ~	"OUAHIGOUYA",
      Country	== "BURKINA FASO" &
        Province	== "SAHEL" &	District	== "DORI" ~	"DORI",
      Country	== "BURKINA FASO" &
        Province	== "NORD" &	District	== "OUAHIGOUYA" ~	"OUAHIGOUYA",
      Country	== "BURKINA FASO" &
        Province	== "NORD" &	District	== "SEGUENEGA" ~	"OUAHIGOUYA",
      Country	== "BURKINA FASO" &
        Province	== "NORD" &	District	== "TITAO" ~	"OUAHIGOUYA",
      Country	== "BURKINA FASO" &
        Province	== "SAHEL" &	District	== "DJIBO" ~	"DORI",
      Country	== "BURKINA FASO" &
        Province	== "SAHEL" &	District	== "GOROM" ~	"DORI",
      Country	== "BURKINA FASO" &
        Province	== "SAHEL" &	District	== "SEBBA" ~	"DORI",
      Country	== "BURKINA FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" &	District	== "DEDOUGOU" ~	"DEDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" &	District	== "SOLENZO" ~	"DEDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" &	District	== "TOMA" ~	"DEDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "CASCADES" &	District	== "SINDOU" ~	"BANFORA",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "HOUNDE" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "KARANGASSO-VIGUE" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "SUD-OUEST" &	District	== "DANO" ~	"GAOUA",
      Country	== "BURKINA FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" &	District	== "BOROMO" ~	"DEDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "DAFRA" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "SUD-OUEST" &	District	== "GAOUA" ~	"GAOUA",
      Country	== "BURKINA FASO" &
        Province	== "CASCADES" &	District	== "MANGODARA" ~	"BANFORA",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "DANDE" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "CASCADES" &	District	== "BANFORA" ~	"BANFORA",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "N'DOROLA" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" &	District	== "NOUNA" ~	"DEDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "BOUCLE_DU_MOUHOUN" &	District	== "TOUGAN" ~	"DEDOUGOU",
      Country	== "BURKINA FASO" &
        Province	== "SUD-OUEST" &	District	== "BATIE" ~	"GAOUA",
      Country	== "BURKINA FASO" &
        Province	== "SUD-OUEST" &	District	== "DIEBOUGOU" ~	"GAOUA",
      Country	== "BURKINA FASO" &
        Province	== "SUD-OUEST" &	District	== "KAMPTI" ~	"GAOUA",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "DO" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "LENA" ~	"BOBO",
      Country	== "BURKINA FASO" &
        Province	== "HAUTS-BASSINS" &	District	== "ORODARA" ~	"BOBO",
      Country	== "MAURITANIA" &
        Province	== "HODH EL GHARBI" &
        District	== "TAMCHAKET" ~	"HODH EL GHARBI",
      Country	== "MAURITANIA" &
        Province	== "ASSABA" &	District	== "GUÉRROU" ~	"ASSABA",
      Country	== "MAURITANIA" &
        Province	== "TIRIS ZEMMOUR" &
        District	== "F'DERICK" ~	"TIRIS ZEMMOUR",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "ROSSO" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "HODH EL GHARBI" &
        District	== "TINTANE" ~	"HODH EL GHARBI",
      Country	== "MAURITANIA" &
        Province	== "HODH EL CHARGUI" &
        District	== "BASSIKNOU" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "MEDERDRA" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "BRAKNA" &	District	== "ALEG" ~	"BRAKNA",
      Country	== "MAURITANIA" &
        Province	== "HODH EL CHARGUI" &
        District	== "AMOURJ" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "HODH EL CHARGUI" &
        District	== "DJIGUENI" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT NORD" &	District	== "TEYARETT" ~	"NOUAKCHOTT",
      Country	== "MAURITANIA" &
        Province	== "ASSABA" &	District	== "BARKÉOLE" ~	"ASSABA",
      Country	== "MAURITANIA" &
        Province	== "ASSABA" &	District	== "KIFFA" ~	"ASSABA",
      Country	== "MAURITANIA" &
        Province	== "BRAKNA" &	District	== "BABABÉ" ~	"BRAKNA",
      Country	== "MAURITANIA" &
        Province	== "BRAKNA" &	District	== "BOGHÉ" ~	"BRAKNA",
      Country	== "MAURITANIA" &
        Province	== "DAKHLET NOUADHIBOU" &
        District	== "NOUADHIBOU" ~	"DAKHLET NOUADHIBOU",
      Country	== "MAURITANIA" &
        Province	== "GORGOL" &	District	== "KAEDI" ~	"GORGOL",
      Country	== "MAURITANIA" &
        Province	== "GORGOL" &	District	== "MAGHAMA" ~	"GORGOL",
      Country	== "MAURITANIA" &
        Province	== "GUIDIMAKHA" &	District	== "GHABOU" ~	"GUIODIMAKHA",
      Country	== "MAURITANIA" &
        Province	== "GUIDIMAKHA" &	District	== "OULD YENGE" ~	"GUIODIMAKHA",
      Country	== "MAURITANIA" &
        Province	== "GUIDIMAKHA" &	District	== "SELIBABY" ~	"GUIODIMAKHA",
      Country	== "MAURITANIA" &
        Province	== "HODH EL GHARBI" &	District	== "AIOUN" ~	"HODH EL GHARBI",
      Country	== "MAURITANIA" &
        Province	== "HODH EL CHARGUI" &	District	== "NÉMA" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "INCHIRI" &	District	== "AKJOUJT" ~	"INCHIRI",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT OUEST" &
        District	== "TEVRAGH ZEINA" ~	"NOUAKCHOTT",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT SUD" &
        District	== "ARAFAT" ~	"NOUAKCHOTT SUD",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT SUD" &
        District	== "EL MINA" ~	"NOUAKCHOTT SUD",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT SUD" &	District	== "RIYAD" ~	"NOUAKCHOTT SUD",
      Country	== "MAURITANIA" &
        Province	== "TIRIS ZEMMOUR" &
        District	== "BIR MOGHREN" ~	"TIRIS ZEMMOUR",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "OUAD NAGA" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "ADRAR" &	District	== "ATAR" ~	"ADRAR",
      Country	== "MAURITANIA" &
        Province	== "BRAKNA" &	District	== "MAGTA LAHJAR" ~	"BRAKNA",
      Country	== "MAURITANIA" &
        Province	== "DAKHLET NOUADHIBOU" &
        District	== "CHAMI" ~	"DAKHLET NOUADHIBOU",
      Country	== "MAURITANIA" &
        Province	== "HODH EL GHARBI" &
        District	== "KOBENI" ~	"HODH EL GHARBI",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT OUEST" &
        District	== "KSAR" ~	"NOUAKCHOTT OUEST",
      Country	== "MAURITANIA" &
        Province	== "TAGANT" &	District	== "TIDJIKJA" ~	"TAGANT",
      Country	== "MAURITANIA" &
        Province	== "ASSABA" &	District	== "BOUMDEID" ~	"ASSABA",
      Country	== "MAURITANIA" &
        Province	== "ADRAR" &	District	== "AOUJEFT" ~	"ADRAR",
      Country	== "MAURITANIA" &
        Province	== "ADRAR" &	District	== "CHINGUITTY" ~	"ADRAR",
      Country	== "MAURITANIA" &
        Province	== "ADRAR" &	District	== "OUADANE" ~	"ADRAR",
      Country	== "MAURITANIA" &
        Province	== "ASSABA" &	District	== "KANKOSSA" ~	"ASSABA",
      Country	== "MAURITANIA" &
        Province	== "GORGOL" &	District	== "LEXEIBA" ~	"GORGOL",
      Country	== "MAURITANIA" &
        Province	== "GORGOL" &	District	== "M'BOUT" ~	"GORGOL",
      Country	== "MAURITANIA" &
        Province	== "GORGOL" &	District	== "MONGUEL" ~	"GORGOL",
      Country	== "MAURITANIA" &
        Province	== "GUIDIMAGHA" &	District	== "WOMPO" ~	"GUIODIMAKHA",
      Country	== "MAURITANIA" &
        Province	== "HODH ECHARGHI" &
        District	== "ADEL BEGROU" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "INCHIRI" &	District == "BENECHABE" ~	"INCHIRI",
      Country	== "MAURITANIA" &
        Province	== "TAGANT" &	District	== "TICHIT" ~	"TAGANT",
      Country	== "MAURITANIA" &
        Province	== "BRAKNA" &	District	== "M'BAGNE" ~	"BRAKNA",
      Country	== "MAURITANIA" &
        Province	== "BRAKNA" &	District	== "MALE" ~	"BRAKNA",
      Country	== "MAURITANIA" &
        Province	== "TAGANT" &	District	== "MOUDJÉRIA" ~	"TAGANT",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "R'KIZ" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "HODH ECHARGHI" &
        District	== "TIMBÉDRA" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT NORD" &
        District	== "DAR NAIM" ~	"NOUAKCHOTT NORD",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT NORD" &
        District	== "TOUJOUNINE" ~	"NOUAKCHOTT",
      Country	== "MAURITANIA" &
        Province	== "NOUAKCHOTT OUEST" &	District	== "SEBKHA" ~	"NOUAKCHOTT",
      Country	== "MAURITANIA" &
        Province	== "TIRIS EZMMOUR" &	District	== "ZOUÉRAT" ~	"TIRIS ZEMMOUR",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "BOUTILIMIT" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "KEUR MACENE" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "HODH EL GHARBI" &	District	== "TOUIL" ~	"HODH EL GHARBI",
      Country	== "MAURITANIA" &
        Province	== "TRARZA" &	District	== "TEIKANE" ~	"TRARZA",
      Country	== "MAURITANIA" &
        Province	== "HODH ECHARGHI" &	District	== "D'HAR" ~	"HODH ECHARGUI",
      Country	== "MAURITANIA" &
        Province	== "HODH ECHARGHI" &	District	== "OUALATA" ~	"HODH ECHARGUI",
      Country	== "MALI" &
        Province	== "GAO" &	District	== "ALMOUSTARAT" ~	"GAO",
      Country	== "MALI" &	Province	== "GAO" &
        District	== "ANSONGO" ~	"GAO",
      Country	== "MALI" &	Province	== "GAO" &	District	== "BOUREM" ~	"GAO",
      Country	== "MALI" &
        Province	== "MENAKA" &	District	== "INEKAR" ~	"MENAKA",
      Country	== "MALI" &	Province	== "GAO" &	District	== "GAO" ~	"GAO",
      Country	== "MALI" &
        Province	== "MENAKA" &	District	== "ANDERAMBOUKANE" ~	"MENAKA",
      Country	== "MALI" &
        Province	== "MENAKA" &	District	== "MENAKA" ~	"GAO",
      Country	== "MALI" &
        Province	== "MENAKA" &	District	== "TIDERMENE" ~	"MENAKA",
      Country	== "MALI" &	Province	== "SEGOU" &
        District	== "SAN" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "OUELESSEBOUGOU" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "DJENNÉ" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "SEGOU" &	District	== "NIONO" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "SEGOU" &	District	== "MARKALA" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "KOLONDIEBA" ~	"SIKASSO",
      Country	== "MALI" &	Province	== "KAYES" &
        District	== "KITA" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "SAGABARI" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "KOULIKORO" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "DOUENTZA" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "BOUGOUNI" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "KENIEBA" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "DIOILA" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "FANA" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "KALABANCORO" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "KANGABA" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "KATI" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "KATI" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "BANDIAGARA" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "BANKASS" ~	"MOPTI",
      Country	== "MALI" &	Province	== "MOPTI" &
        District	== "KORO" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "MOPTI" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "TENENKOU" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "SEGOU" &	District	== "BAROUELI" ~	"SÉGOU",
      Country	== "MALI" &	Province	== "SEGOU" &
        District	== "BLA" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "SEGOU" &	District	== "MACINA" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "SEGOU" &	District	== "SEGOU" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "SEGOU" &	District	== "TOMINIAN" ~	"SÉGOU",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "KADIOLO" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "KIGNAN" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "NIENA" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "SELINGUE" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "YANFOLILA" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "YOROSSO" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "TOMBOUCTOU" &
        District	== "GOURMA-RHAROUS" ~	"TOMBOUCTOU",
      Country	== "MALI" &
        Province	== "BAMAKO" &	District	== "COMMUNE I" ~	"BAMAKO",
      Country	== "MALI" &
        Province	== "BAMAKO" &	District	== "COMMUNE II" ~	"BAMAKO",
      Country	== "MALI" &
        Province	== "BAMAKO" &	District	== "COMMUNE III" ~	"BAMAKO",
      Country	== "MALI" &
        Province	== "BAMAKO" &	District	== "COMMUNE IV" ~	"BAMAKO",
      Country	== "MALI" &
        Province	== "BAMAKO" &	District	== "COMMUNE V" ~	"BAMAKO",
      Country	== "MALI" &
        Province	== "BAMAKO" &	District	== "COMMUNE VI" ~	"BAMAKO",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "KOUTIALA" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "MOPTI" &	District	== "YOUWAROU" ~	"MOPTI",
      Country	== "MALI" &
        Province	== "SIKASSO" &	District	== "SIKASSO" ~	"SIKASSO",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "BANAMBA" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "YELIMANE" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "NIORO" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "NARA" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "TOMBOUCTOU" &	District	== "DIRE" ~	"TOMBOUCTOU",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "BAFOULABE" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "DIEMA" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "OUSSOUBIDIAGNA" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "SEFETO" ~	"KAYES",
      Country	== "MALI" &
        Province	== "KOULIKORO" &	District	== "KOLOKANI" ~	"KOULIKORO",
      Country	== "MALI" &
        Province	== "TOMBOUCTOU" &	District	== "NIAFUNKE" ~	"TOMBOUCTOU",
      Country	== "MALI" &
        Province	== "KAYES" &	District	== "KAYES" ~	"KAYES",
      Country	== "MALI" &
        Province	== "TAOUDENI" &	District	== "ACHOURATT" ~	"TAOUDENIT",
      Country	== "MALI" &
        Province	== "TAOUDENI" &	District	== "AL-OURCHE" ~	"TAOUDENIT",
      Country	== "MALI" &
        Province	== "TAOUDENI" &	District	== "BOUJBEHA" ~	"TAOUDENIT",
      Country	== "MALI" &
        Province	== "TOMBOUCTOU" &	District	== "GOUNDAM" ~	"TOMBOUCTOU",
      Country	== "MALI" &
        Province	== "TOMBOUCTOU" &	District	== "TOMBOUCTOU" ~	"TOMBOUCTOU",
      Country	== "MALI" &
        Province	== "KIDAL" &	District	== "KIDAL" ~	"KIDAL",
      Country	== "MALI" &
        Province	== "TAOUDENIT" &	District	== "ARAWANE" ~	"TAOUDENIT",
      Country	== "MALI" &
        Province	== "TAOUDENIT" &	District	== "TAOUDENI" ~	"TAOUDENIT",
      Country	== "MALI" &
        Province	== "KIDAL" &	District	== "ABEIBARA" ~	"KIDAL",
      Country	== "MALI" &
        Province	== "TAOUDENIT" &	District	== "FOUM-ALBA" ~	"TAOUDENIT",
      Country	== "MALI" &
        Province	== "KIDAL" &	District	== "TESSALIT" ~	"KIDAL",
      Country	== "MALI" &
        Province	== "KIDAL" &	District	== "TINESSAKO" ~	"KIDAL",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "DOSSO" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "FALMEY" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "DIOUNDOU" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "GAYA" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "LOGA" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "TILLABERI" &	District	== "KOLLO" ~	"TILLABTRI",
      Country	== "NIGER" &
        Province	== "TILLABERI" &	District	== "TILLABERI" ~	"TILLABTRI",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "DOGON DOUTCHI" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "TIBIRI" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "NIAMEY" &	District	== "NIAMEY 1" ~	"NIAMEY",
      Country	== "NIGER" &
        Province	== "NIAMEY" &	District	== "NIAMEY 2" ~	"NIAMEY",
      Country	== "NIGER" &
        Province	== "NIAMEY" &	District	== "NIAMEY 4" ~	"NIAMEY",
      Country	== "NIGER" &
        Province	== "NIAMEY" &	District	== "NIAMEY 5" ~	"NIAMEY",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "BAGAROUA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "BIRNI N'KONNI" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "BOUZA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "ILLÉLA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "KEITA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "MADAOUA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "MALBAZA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "TAHOUA DEP" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TILLABERI" &	District	== "BALLEYARA" ~	"TILLABERI",
      Country	== "NIGER" &
        Province	== "DOSSO" &	District	== "BOBOYE" ~	"DOSSO",
      Country	== "NIGER" &
        Province	== "NIAMEY" &	District	== "NIAMEY 3" ~	"NIAMEY",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "TAHOUA COM" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "TASSARA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "TAKEITA" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "DAKORO" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "GAZAOUA" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "G. ROUMDJI" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "MADAROUNFA" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "MAYAHI" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "TESSAOUA" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "ABALAK" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "TCHINTABARADEN" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "KANTCHÉ" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "AGUIÉ" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "MARADI VILLE" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "AGUIE" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "MARADI" &	District	== "BERMO" ~	"MARADI",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "ILLELA" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "ZINDER" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "DAMAGARAM TAKAYA" ~ "ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "DUNGASS" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "GOURE" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "MAGARIA" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "MIRRIAH" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "TANOUT" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "BELBEDJI" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "ZINDER" &	District	== "TESKER" ~	"ZINDER",
      Country	== "NIGER" &
        Province	== "AGADEZ" &	District	== "ADERBISSANAT" ~	"AGADEZ",
      Country	== "NIGER" &
        Province	== "AGADEZ" &	District	== "AGADEZ" ~	"AGADEZ",
      Country	== "NIGER" &
        Province	== "AGADEZ" &	District	== "ARLIT" ~	"AGADEZ",
      Country	== "NIGER" &
        Province	== "AGADEZ" &	District	== "IFÉROUANE" ~	"AGADEZ",
      Country	== "NIGER" &
        Province	== "AGADEZ" &	District	== "INGALL" ~	"AGADEZ",
      Country	== "NIGER" &
        Province	== "AGADEZ" &	District	== "TCHIROZÉRINE" ~	"AGADEZ",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "DIFFA" ~	"DIFFA",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "GOUDOUMARIA" ~	"DIFFA",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "MAINE SOROA" ~	"DIFFA",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "N'GUIGMI" ~	"DIFFA",
      Country	== "NIGER" &
        Province	== "TILLABERI" &	District	== "SAY" ~	"TILLABTRI",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "N'GOURTI" ~	"DIFFA",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "NGUIGMI" ~	"DIFFA",
      Country	== "NIGER" &
        Province	== "TAHOUA" &	District	== "BIRNI NKONNI" ~	"TAHOUA",
      Country	== "NIGER" &
        Province	== "DIFFA" &	District	== "BOSSO" ~	"DIFFA",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "GOUNDI" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "LOGONE_OCCIDENTAL" &
        District	== "BENOYE" ~	"LOGONE OCCIDENTAL",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &	District	== "DANAMADJI" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "LOGONE_OCCIDENTAL" &
        District	== "BEINAMAR" ~	"LOGONE OCCIDENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "BEBEDJIA" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "DONIA" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "BEDJONDO" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "GAGAL" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "KANEM" &	District	== "MONDO" ~	"KANEM",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "BODO" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "KOUMRA" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &	District	== "YOUE" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &	District	== "KOUMOGO" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "BERE" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "BARH_EL_GAZAL" &	District	== "CHADRA" ~	"BARH EL GAZEL",
      Country	== "CHAD" &
        Province	== "BARH_EL_GAZAL" &
        District	== "MICHEMIRE" ~	"BAHR ELGHAZAL",
      Country	== "CHAD" &
        Province	== "BARH_EL_GAZAL" &	District	== "MOUSSORO" ~	"KANEM",
      Country	== "CHAD" &
        Province	== "BARH_EL_GAZAL" &	District	== "SALAL" ~	"BAHR ELGHAZAL",
      Country	== "CHAD" &
        Province	== "BATHA" &	District	== "ASSINET" ~	"BATHA",
      Country	== "CHAD" &	Province	== "BATHA" &
        District	== "ATI" ~	"BATHA",
      Country	== "CHAD" &
        Province	== "BATHA" &	District	== "DJEDDA" ~	"BATHA",
      Country	== "CHAD" &
        Province	== "BATHA" &	District	== "KOUNDJOUROU" ~	"BATHA",
      Country	== "CHAD" &
        Province	== "BATHA" &	District	== "OUM HADJER" ~	"BATHA",
      Country	== "CHAD" &
        Province	== "DAR_SILA" &	District	== "GOZ BEIDA" ~	"SILA",
      Country	== "CHAD" &
        Province	== "DAR_SILA" &	District	== "KOUKOU ANGARANA" ~	"SILA",
      Country	== "CHAD" &
        Province	== "DAR_SILA" &	District	== "TISSI" ~	"SILA",
      Country	== "CHAD" &
        Province	== "HADJER_LAMIS" &	District	== "KARAL" ~	"HADJER LAMIS",
      Country	== "CHAD" &
        Province	== "HADJER_LAMIS" &	District	== "MANI" ~	"HADJER LAMIS",
      Country	== "CHAD" &	Province	== "KANEM" &
        District	== "MAO" ~	"KANEM",
      Country	== "CHAD" &
        Province	== "KANEM" &	District	== "NOKOU" ~	"KANEM",
      Country	== "CHAD" &
        Province	== "KANEM" &	District	== "N'TIONA" ~	"KANEM",
      Country	== "CHAD" &
        Province	== "KANEM" &	District	== "RIG RIG" ~	"KANEM",
      Country	== "CHAD" &
        Province	== "LAC" &	District	== "BAGASSOLA" ~	"LAC",
      Country	== "CHAD" &	Province	== "LAC" &
        District	== "ISSEIROM" ~	"LAC",
      Country	== "CHAD" &
        Province	== "LAC" &	District	== "KOULOUDIA" ~	"LAC",
      Country	== "CHAD" &	Province	== "LAC" &	District	== "NGOURI" ~	"LAC",
      Country	== "CHAD" &
        Province	== "LOGONE_OCCIDENTAL" &
        District	== "LAOKASSY" ~	"LOGONE OCCIDENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_OCCIDENTAL" &
        District	== "MOUNDOU" ~	"LOGONE OCCIDENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "BESSAO" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "DOBA" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "GORE" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "KARA" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "LARMANAYE" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "BEDAYA" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "MOISSALA" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &
        District	== "FIANGA" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &
        District	== "GOUNOU GAYA" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &
        District	== "MOULKOU" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "BINDER" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "GUEGOU" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "GUELAO" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "LAGON" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "LAME" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "LERE" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "TORROCK" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &
        District	== "BIOBE SINGAKO" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &	District	== "KORBOL" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &	District	== "KYABE" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &	District	== "MARO" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "MOYEN_CHARI" &	District	== "SARH" ~	"MOYEN CHARI",
      Country	== "CHAD" &
        Province	== "OUADDAI" &	District	== "ABECHE" ~	"OUADDAI",
      Country	== "CHAD" &
        Province	== "OUADDAI" &	District	== "ABOUGOUDAM" ~	"OUADDAI",
      Country	== "CHAD" &
        Province	== "OUADDAI" &	District	== "ADRE" ~	"OUADDAI",
      Country	== "CHAD" &
        Province	== "OUADDAI" &	District	== "AMDAM" ~	"OUADDAI",
      Country	== "CHAD" &
        Province	== "SALAMAT" &	District	== "AM TIMAN" ~	"SALAMAT",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "BAKCTCHORO" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "DAFRA" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "DONOMANGA" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "KELO" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "KOLON" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "LAI" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "WADI_FIRA" &	District	== "ARADA" ~	"WADI FIRA",
      Country	== "CHAD" &
        Province	== "WADI_FIRA" &	District	== "BILTINE" ~	"WADI FIRA",
      Country	== "CHAD" &
        Province	== "WADI_FIRA" &	District	== "IRIBA" ~	"WADI FIRA",
      Country	== "CHAD" &
        Province	== "WADI_FIRA" &	District	== "MATADJANA" ~	"WADI FIRA",
      Country	== "CHAD" &
        Province	== "DAR_SILA" &	District	== "ABDI" ~	"QUADDAI",
      Country	== "CHAD" &
        Province	== "HADJER_LAMIS" &	District	== "BOKORO" ~	"HADJAR LAMIS",
      Country	== "CHAD" &	Province	== "LAC" &	District	== "BOL" ~	"LAC",
      Country	== "CHAD" &	Province	== "LAC" &	District	== "LIWA" ~	"LAC",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "BEKOUROU" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &
        District	== "PONT CAROL" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "WADI_FIRA" &	District	== "AMZOER" ~	"WADI FIRA",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &
        District	== "BONGOR" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_OUEST" &
        District	== "PALA" ~	"MAYO KEBBI OUEST",
      Country	== "CHAD" &
        Province	== "SALAMAT" &	District	== "HARAZE MANGUEIGNE" ~	"SALAMAT",
      Country	== "CHAD" &
        Province	== "TANDJILE" &	District	== "GUIDARI" ~	"TANDJILE",
      Country	== "CHAD" &
        Province	== "GUERA" &	District	== "MANGALME" ~	"GUERA",
      Country	== "CHAD" &
        Province	== "BATHA" &	District	== "ALIFA" ~	"BATHA",
      Country	== "CHAD" &
        Province	== "NDJAMENA" &	District	== "N'DJAMENA SUD" ~	"N'DJAMENA",
      Country	== "CHAD" &
        Province	== "GUERA" &	District	== "MELFI" ~	"GUERA",
      Country	== "CHAD" &
        Province	== "NDJAMENA" &	District	== "N'DJAMENA NORD" ~	"N'DJAMENA",
      Country	== "CHAD" &
        Province	== "CHARI_BAGUIRMI" &
        District	== "MANDELIA" ~	"CHARI BAGUIRMI",
      Country	== "CHAD" &	Province	== "GUERA" &
        District	== "BARO" ~	"GUERA",
      Country	== "CHAD" &
        Province	== "GUERA" &	District	== "MONGO" ~	"GUERA",
      Country	== "CHAD" &
        Province	== "LOGONE_ORIENTAL" &
        District	== "BEBOTO" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "NDJAMENA" &	District	== "N'DJAMENA CENTRE" ~	"N'DJAMENA",
      Country	== "CHAD" &
        Province	== "SALAMAT" &	District	== "ABOUDEIA" ~	"SALAMAT",
      Country	== "CHAD" &
        Province	== "BORKOU" &	District	== "FAYA" ~	"BORKOU-TIBESTI",
      Country	== "CHAD" &
        Province	== "CHARI_BAGUIRMI" &
        District	== "BOUSSO" ~	"CHARI BAGUIRMI",
      Country	== "CHAD" &
        Province	== "CHARI_BAGUIRMI" &
        District	== "DOURBALI" ~	"CHARI BAGUIRMI",
      Country	== "CHAD" &
        Province	== "CHARI_BAGUIRMI" &
        District	== "MASSENYA" ~	"CHARI BAGUIRMI",
      Country	== "CHAD" &
        Province	== "ENNEDI_EST" &	District	== "AMDJARASS" ~	"ENNEDI EST",
      Country	== "CHAD" &
        Province	== "ENNEDI_OUEST" &	District	== "FADA" ~	"ENNEDI QUEST",
      Country	== "CHAD" &
        Province	== "HADJER_LAMIS" &	District	== "MASSAGUET" ~	"HADJAR LAMIS",
      Country	== "CHAD" &
        Province	== "HADJER_LAMIS" &	District	== "MASSAKORY" ~	"HADJAR LAMIS",
      Country	== "CHAD" &
        Province	== "NDJAMENA" &
        District	== "N'DJAMENA EST" ~	"CHARI BAGUITMI",
      Country	== "CHAD" &
        Province	== "WADI_FIRA" &	District	== "GUEREDA" ~	"WADI FIRA",
      Country	== "CHAD" &	Province	== "BATHA" &
        District	== "YAO" ~	"BATHA",
      Country	== "CHAD" &
        Province	== "CHARI_BAGUIRMI" &
        District	== "BA ILLI" ~	"CHARI BAGUIRMI",
      Country	== "CHAD" &
        Province	== "CHARI_BAGUIRMI" &	District	== "KOUNO" ~	"CHARI BAGUIRMI",
      Country	== "CHAD" &
        Province	== "HADJER_LAMIS" &	District	== "GAMA" ~	"HADJER LAMIS",
      Country	== "CHAD" &
        Province	== "MANDOUL" &	District	== "BOUNA" ~	"MANDOUL",
      Country	== "CHAD" &
        Province	== "GUERA" &	District	== "BITKINE" ~	"GUERA",
      Country	== "CHAD" &
        Province	== "MAYO_KEBBI_EST" &
        District	== "GUELENDENG" ~	"MAYO KEBBI EST",
      Country	== "CHAD" &
        Province	== "LOGONE ORIENTAL" &
        District	== "BAIBOKOUM" ~	"LOGONE ORIENTAL",
      Country	== "CHAD" &
        Province	== "OUADDAI" &	District	== "AM DAM" ~	"SILA",
      Country	== "CHAD" &
        Province	== "TIBESTI" &	District	== "BARDAI" ~	"TIBESTI",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &
        District	== "BUIKWE" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BUTALEJA" ~	"BUKEDI",
      Country	== "UGANDA" &	Province	== "ARUA" &
        District	== "MOYO" ~	"MOYO",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "HOIMA" ~	"HOIMA",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "KAMULI" ~	"KAMULI",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "NAMUTUMBA" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "KALANGALA" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "TORORO" ~	"TORORO",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "BUKEDEA" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "ARUA" ~	"MARACHA",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &	District	== "KAMPALA" ~	"KAMPALA",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "AGAGO" ~	"ACHOLI",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "BUGWERI" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "KIRUHURA" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "LUWERO" ~	"LUWERO",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &
        District	== "BUVUMA" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "KIRYANDONGO" ~	"BUNYORO",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "JINJA" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "KABALE" &	District	== "RUBANDA" ~	"KIGEZI",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "KABAROLE" ~	"KAMWNGE",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "KASESE" ~	"KASESE",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "LYANTONDE" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BUKWO" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "MBALE" ~	"MBALE",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "KABERAMAIDO" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "MITOOMA" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "SOROTI" ~	"SOROTI",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BUDAKA" ~	"BUKEDI",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "MBARARA" ~	"KIRUHURA",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "KYENJOJO" ~	"TORO",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "KUMI" ~	"KUMI",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "BUYENDE" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "KABALE" &	District	== "KABALE" ~	"KABALE",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "NTOROKO" ~	"TORO",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "KALUNGU" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "MASAKA" ~	"KALANGALA",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "MARACHA" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "NEBBI" ~	"NEBBI",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "YUMBE" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "ZOMBO" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &	District	== "MPIGI" ~	"MPIGI",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &	District	== "MUKONO" ~	"MUKONO",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &
        District	== "WAKISO" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "AMURU" ~	"ACHOLI",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "OMORO" ~	"ACHOLI",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "PADER" ~	"ACHOLI",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "BULIISA" ~	"BUNYORO",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "KAGADI" ~	"BUNYORO",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "KAKUMIRO" ~	"BUNYORO",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "KIBAALE" ~	"KIBAALE",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "KIKUUBE" ~	"BUNYORO",
      Country	== "UGANDA" &
        Province	== "HOIMA" &	District	== "MASINDI" ~	"MASINDI",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "IGANGA" ~	"MAYUNGE",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "KALIRO" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "LUUKA" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "MAYUGE" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "NAMAYINGO" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "KABALE" &	District	== "KANUNGU" ~	"KIGEZI",
      Country	== "UGANDA" &
        Province	== "KABALE" &	District	== "KISORO" ~	"KIGEZI",
      Country	== "UGANDA" &
        Province	== "KABALE" &	District	== "RUKIGA" ~	"KIGEZI",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "BUNDIBUGYO" ~	"HOIMA",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "BUNYANGABU" ~	"TORO",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "KAMWENGE" ~	"TORO",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "ALEBTONG" ~	"LANGO",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "AMOLATAR" ~	"LANGO",
      Country	== "UGANDA" &	Province	== "LIRA" &
        District	== "APAC" ~	"APAC",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "DOKOLO" ~	"LANGO",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "KOLE" ~	"LANGO",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "KWANIA" ~	"LANGO",
      Country	== "UGANDA" &	Province	== "LIRA" &
        District	== "LIRA" ~	"LIRA",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "OTUKE" ~	"LANGO",
      Country	== "UGANDA" &
        Province	== "LIRA" &	District	== "OYAM" ~	"LANGO",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "BUKOMANSIMBI" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "KYOTERA" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "LWENGO" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "RAKAI" ~	"RAKAI",
      Country	== "UGANDA" &
        Province	== "MASAKA" &	District	== "SEMBABULE" ~	"SEMBABULE",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BUDUDA" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BUSIA" ~	"BUKEDI",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BUTEBO" ~	"BUKEDI",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "KAPCHORWA" ~	"KAPCHORWA",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "KIBUKU" ~	"BUKEDI",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "KWEEN" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "MANAFWA" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "NAMISINDWA" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "PALLISA" ~	"PALLISA",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "SIRONKO" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "BUSHENYI" ~	"BUSHENYI",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "ISINGIRO" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "KAZO" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "NTUNGAMO" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "RUBIRIZI" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "RWAMPARA" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "SHEEMA" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "ABIM" ~	"KARAMOJA",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "AMUDAT" ~	"KARAMOJA",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "KOTIDO" ~	"KAABONG",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "NABILATUK" ~	"KARAMOJA",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "NAPAK" ~	"KARAMOJA",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "KASANDA" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "KIBOGA" ~	"KIBOGA",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "KYAKWANZI" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "NAKASEKE" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "NAKASONGOLA" ~	"NAKASONGOLA",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "AMURIA" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "KALAKI" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "KAPELEBYONG" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "KATAKWI" ~	"AMURIA",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "NGORA" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "MADI OKOLLO" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "PAKWACH" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &
        District	== "BUTAMBALA" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &	District	== "GOMBA" ~	"SOUTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "GREATER KAMPALA" &
        District	== "KAYUNGA" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "GULU" ~	"KILAK",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "KITGUM" ~	"KITGUM",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "LAMWO" ~	"ACHOLI",
      Country	== "UGANDA" &
        Province	== "GULU" &	District	== "NWOYA" ~	"ACHOLI",
      Country	== "UGANDA" &
        Province	== "KABALE" &	District	== "RUKUNGIRI" ~	"RUKUNGIRI",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "KITAGWENDA" ~	"TORO",
      Country	== "UGANDA" &
        Province	== "KABAROLE" &	District	== "KYEGEGWA" ~	"TORO",
      Country	== "UGANDA" &
        Province	== "MBALE" &	District	== "BULAMBULI" ~	"ELGON",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "KARENGA" ~	"KARAMOJA",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "NAKAPIRIPIRIT" ~	"KARAMOJA",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "MITYANA" ~	"NORTH BUGANDA",
      Country	== "UGANDA" &
        Province	== "SOROTI" &	District	== "SERERE" ~	"TESO",
      Country	== "UGANDA" &
        Province	== "MUBENDE" &	District	== "MUBENDE" ~	"MUBENDE",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "ADJUMANI" ~	"ADJUMANI",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "OBONGI" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "JINJA" &	District	== "BUGIRI" ~	"BUSOGA",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "IBANDA" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "MOROTO" ~	"MOROTO",
      Country	== "UGANDA" &
        Province	== "ARUA" &	District	== "KOBOKO" ~	"WEST NILE",
      Country	== "UGANDA" &
        Province	== "MBARARA" &	District	== "BUHWEJU" ~	"ANKOLE",
      Country	== "UGANDA" &
        Province	== "MOROTO" &	District	== "KAABONG" ~	"KARAMOJA",
      Country	== "KENYA" &
        Province	== "MANDERA" &	District	== "MANDERA WEST" ~	"MANDERA",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "BALAMBALA" ~	"GARISSA",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "FAFI" ~	"GARISSA",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "GARISSA" ~	"NORTH EASTERN",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "LAGDERA" ~	"GARISSA",
      Country	== "KENYA" &
        Province	== "ISIOLO" &	District	== "ISIOLO" ~	"KENEASTERN",
      Country	== "KENYA" &
        Province	== "ISIOLO" &	District	== "MERTI" ~	"ISIOLO",
      Country	== "KENYA" &
        Province	== "KAJIADO" &	District	== "KAJIADO EAST" ~	"KAJIADO",
      Country	== "KENYA" &
        Province	== "KAJIADO" &	District	== "KAJIADO NORTH" ~	"KAJIADO",
      Country	== "KENYA" &
        Province	== "KAJIADO" &	District	== "KAJIADO WEST" ~	"KAJIADO",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "JUJA" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "KABETE" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "KIAMBAA" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "KIAMBU TOWN" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "KIKUYU" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "THIKA TOWN" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KILIFI" &	District	== "KALOLENI" ~	"KILIFI",
      Country	== "KENYA" &
        Province	== "KILIFI" &	District	== "KILIFI NORTH" ~	"KILIFI",
      Country	== "KENYA" &
        Province	== "KILIFI" &	District	== "MAGARINI" ~	"KILIFI",
      Country	== "KENYA" &
        Province	== "KILIFI" &	District	== "MALINDI" ~	"COAST",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "KITUI EAST" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "KITUI SOUTH" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "MWINGI CENTRAL" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "MWINGI NORTH" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "LAMU" &	District	== "LAMU EAST" ~	"LAMU",
      Country	== "KENYA" &
        Province	== "LAMU" &	District	== "LAMU WEST" ~	"LAMU",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "ATHI RIVER" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "MACHAKOS" ~	"KENEASTERN",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "MASINGA" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "MATUNGULU" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "MANDERA" &	District	== "MANDERA EAST" ~	"MANDERA",
      Country	== "KENYA" &
        Province	== "MANDERA" &	District	== "MANDERA NORTH" ~	"MANDERA",
      Country	== "KENYA" &
        Province	== "MANDERA" &	District	== "MANDERA SOUTH" ~	"MANDERA",
      Country	== "KENYA" &
        Province	== "MOMBASA" &	District	== "KISAUNI" ~	"MOMBASA",
      Country	== "KENYA" &
        Province	== "MOMBASA" &	District	== "LIKONI" ~	"MOMBASA",
      Country	== "KENYA" &
        Province	== "MOMBASA" &	District	== "NYALI" ~	"MOMBASA",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "DAGORETTI SOUTH" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "EMBAKASI EAST" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "EMBAKASI NORTH" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "EMBAKASI WEST" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "KASARANI" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "KIBRA" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "MATHARE" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "RUARAKA" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "STAREHE" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "WESTLANDS" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "TANA RIVER" &	District	== "BURA" ~	"TANA RIVER",
      Country	== "KENYA" &
        Province	== "TANA RIVER" &	District	== "GALOLE" ~	"TANA RIVER",
      Country	== "KENYA" &
        Province	== "WAJIR" &	District	== "WAJIR EAST" ~	"WAJIR",
      Country	== "KENYA" &
        Province	== "WAJIR" &	District	== "WAJIR NORTH" ~	"WAJIR",
      Country	== "KENYA" &
        Province	== "WAJIR" &	District	== "WAJIR SOUTH" ~	"WAJIR",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "MWALA" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "TANA RIVER" &	District	== "GARSEN" ~	"TANA RIVER",
      Country	== "KENYA" &
        Province	== "MOMBASA" &	District	== "MVITA" ~	"MOMBASA",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "MAKADARA" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "IJARA" ~	"GARISSA",
      Country	== "KENYA" &
        Province	== "KAJIADO" &	District	== "KAJIADO CENTRAL" ~	"KAJIADO",
      Country	== "KENYA" &
        Province	== "KILIFI" &	District	== "RABAI" ~	"KILIFI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "KITUI WEST" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "HULUGHO" ~	"GARISSA",
      Country	== "KENYA" &
        Province	== "ISIOLO" &	District	== "GARBATULLA" ~	"ISIOLO",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "LANGATA" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "ROYSAMBU" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "GARISSA" &	District	== "DADAAB" ~	"GARISSA",
      Country	== "KENYA" &
        Province	== "KAJIADO" &	District	== "LOITOKITOK" ~	"KAJIADO",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "EMBAKASI CENTRAL" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "EMBAKASI SOUTH" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "KAMUKUNJI" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "GATUNDU NORTH" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "GATUNDU SOUTH" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "GITHUNGURI" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "LARI" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "LIMURU" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "KIAMBU" &	District	== "RUIRU" ~	"KIAMBU",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "DAGORETTI NORTH" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "KITUI CENTRAL" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "KITUI RURAL" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "KITUI" &	District	== "MWINGI WEST" ~	"KITUI",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "KANGUNDO" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "KATHIANI" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "YATTA" ~	"MACHAKOS",
      Country	== "KENYA" &
        Province	== "MANDERA" &	District	== "LAFEY" ~	"MANDERA",
      Country	== "KENYA" &
        Province	== "MANDERA" &	District	== "BANISSA" ~	"MANDERA",
      Country	== "KENYA" &
        Province	== "NAIROBI" &	District	== "DAGORETTI" ~	"NAIROBI",
      Country	== "KENYA" &
        Province	== "WAJIR" &	District	== "ELDAS" ~	"WAJIR",
      Country	== "KENYA" &
        Province	== "WAJIR" &	District	== "TARBAJ" ~	"WAJIR",
      Country	== "KENYA" &
        Province	== "WAJIR" &	District	== "WAJIR WEST" ~	"WAJIR",
      Country	== "KENYA" &
        Province	== "MACHAKOS" &	District	== "KALAMA" ~	"MACHAKOS",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "KATETE" ~	"ZAMEASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "LUNDAZI" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "KASENENGWA" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "LUMEZI" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "MAMBWE" ~	"ZAMEASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "CHADIZA" ~	"ZAMEASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "CHASEFU" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "CHIPANGALI" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "CHIPATA" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "PETAUKE" ~	"ZAMEASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "VUBWI" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "CHONGWE" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "NAKONDE" ~	"MUCHINGA",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "SHIWANG'ANDU" ~	"MUCHINGA",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "NYIMBA" ~	"ZAMEASTERN",
      Country	== "ZAMBIA" &
        Province	== "EASTERN" &	District	== "SINDA" ~	"EASTERN",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "KAFUE" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "LUANGWA" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "LUSAKA" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "RUFUNSA" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "CHAMA" ~	"ZAMEASTERN",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "CHINSALI" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "ISOKA" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "KANCHIBIYA" ~	"MUCHINGA",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "MAFINGA" ~	"MUCHINGA",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "MPIKA" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "CHILANGA" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "LUSAKA" &	District	== "CHIRUNDU" ~	"LUSAKA",
      Country	== "ZAMBIA" &
        Province	== "MUCHINGA" &	District	== "LAVUSHI MANDA" ~	"MUCHINGA",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "SESHEKE" ~	"ZAMWESTERN",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "KITWE" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "NCHELENGE" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "CHITAMBO" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "KABWE" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "SERENJE" ~	"ZAMCENTRAL",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "MILENGE" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "LUPOSOSHI" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "MBALA" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "NSAMA" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "GWEMBE" ~	"ZAMSOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "MWANDI" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "CHIBOMBO" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "LUANO" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "MASAITI" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "MPONGWE" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &	District	== "CHAVUMA" ~	"NORTH-WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &	District	== "KABOMPO" ~	"NORTH-WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &
        District	== "MANYINGA" ~	"NORTH WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "CHILUBI" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "KAPUTA" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "LUWINGU" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "MPOROKOSO" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "MAZABUKA" ~	"ZAMSOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "KALABO" ~	"ZAMWESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "NKEYEMA" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "SIKONGO" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "CHISAMBA" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "KAPIRI-MPOSHI" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "CHINGOLA" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "LUANSHYA" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "MUFULIRA" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &	District	== "KASEMPA" ~	"NORTH-WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &
        District	== "MUSHINDANO" ~	"NORTH WESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "NALOLO" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "KALULUSHI" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "CHIENGE" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "SAMFYA" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &
        District	== "MUFUMBWE" ~	"NORTH-WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &
        District	== "MWINILUNGA" ~	"NORTH-WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &	District	== "SOLWEZI" ~	"NORTH WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "KASAMA" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "LUNTE" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "MUNGWI" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "CHOMA" ~	"ZAMSOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "MKUSHI" ~	"ZAMCENTRAL",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "NGABWE" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "SHIBUYUNJI" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "CHIPILI" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "LUNGA" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "MANSA" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "MWANSABOMBWE" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "SENGA" ~	"NORTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "KALOMO" ~	"ZAMSOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "NAMWALA" ~	"ZAMSOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "SIAVONGA" ~	"SOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "LUAMPA" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "LUKULU" ~	"ZAMWESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "MONGU" ~	"ZAMWESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "SENANGA" ~	"ZAMWESTERN",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "KAWAMBWA" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "KAZUNGULA" ~	"ZAMSOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "MONZE" ~	"SOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "KAOMA" ~	"ZAMWESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "MULOBEZI" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "ITEZHI-TEZHI" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &
        District	== "CHILILABOMBWE" ~	"NORTH WESTERN",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "LUFWANYAMA" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "COPPERBELT" &	District	== "NDOLA" ~	"COPPERBELT",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "CHEMBE" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "CHIFUNABULI" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "LUAPULA" &	District	== "MWENSE" ~	"LUAPULA",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &
        District	== "IKELENGE" ~	"NORTH WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &
        District	== "KALUMBILA" ~	"NORTH WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTH WESTERN" &	District	== "ZAMBEZI" ~	"NORTH-WESTERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "CHIKANKATA" ~	"SOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "LIVINGSTONE" ~	"SOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "PEMBA" ~	"SOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "SINAZONGWE" ~	"SOUTHERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "LIMULUNGA" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "MITETE" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "NORTHERN" &	District	== "MPULUNGU" ~	"ZAMNORTHERN",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "SIOMA" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "CENTRAL" &	District	== "MUMBWA" ~	"CENTRAL",
      Country	== "ZAMBIA" &
        Province	== "WESTERN" &	District	== "SHANG'OMBO" ~	"WESTERN",
      Country	== "ZAMBIA" &
        Province	== "SOUTHERN" &	District	== "ZIMBA" ~	"SOUTHERN",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "CHEGUTU" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "SANYATI" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "CENTENARY" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "UMGUZA" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "BEITBRIDGE" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "BULAWAYO" &	District	== "BULAWAYO" ~	"BULAWAYO",
      Country	== "ZIMBABWE" &
        Province	== "HARARE" &	District	== "CHITUNGWIZA" ~	"HARARE",
      Country	== "ZIMBABWE" &
        Province	== "HARARE" &	District	== "HARARE" ~	"HARARE",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "BUHERA" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "CHIPINGE" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "MUTARE" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "MUTASA" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "NYANGA" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "RUSHINGA" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "SHAMVA" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "HWEDZA" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "MARONDERA" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "MUDZI" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "MUTOKO" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "SEKE" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "UMP" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "KARIBA" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "ZVIMBA" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "BIKITA" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "CHIVI" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "GUTU" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "MASVINGO" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "MWENEZI" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "BUBI" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "HWANGE" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "LUPANE" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "TSHOLOTSHO" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "BULILIMA" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "MANGWE" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "MATOBO" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "CHIRUMHANZU" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "GOKWE NORTH" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "GOKWE SOUTH" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "GWERU" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "KWEKWE" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "SHURUGWI" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "ZVISHAVANE" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "MAKONI" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "BINDURA" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "GURUVE" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "GOROMONZI" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "HURUNGWE" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "MAKONDE" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND WEST" &
        District	== "MHONDORO NGEZI" ~	"MASHONALAND WEST",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "ZAKA" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MIDLANDS" &	District	== "MBERENGWA" ~	"MIDLANDS",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "MUREHWA" ~	"MASHONALAND EAST",
      Country	== "ZIMBABWE" &
        Province	== "MANICALAND" &	District	== "CHIMANIMANI" ~	"MANICALAND",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "MAZOWE" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "MBIRE" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND CENTRAL" &
        District	== "MOUNT DARWIN" ~	"MASHONALAND CENTRAL",
      Country	== "ZIMBABWE" &
        Province	== "MASVINGO" &	District	== "CHIREDZI" ~	"MASVINGO",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "BINGA" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND NORTH" &
        District	== "NKAYI" ~	"MATABELELAND NORTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "GWANDA" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "UMZINGWANE" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "MATABELELAND SOUTH" &
        District	== "INSIZA" ~	"MATABELELAND SOUTH",
      Country	== "ZIMBABWE" &
        Province	== "MASHONALAND EAST" &
        District	== "CHIKOMBA" ~	"MASHONALAND EAST",
      Country	== "ANGOLA" &	Province	== "BIE" &
        District	== "NHAREA" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CACULA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &	District	== "AMBACA" ~	"CUANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &	District	== "CAZENGO" ~	"CUANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "AMBOIM" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "SUMBE" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "BENGO" &	District	== "NAMBUANGONGO" ~	"BENGO",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "BALOMBO" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "CABINDA" &	District	== "BUCO ZAU" ~	"CABINDA",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "MENONGUE" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "CATCHIUNGO" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "LONDUIMBALE" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "QUISSAMA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUNDA SUL" &	District	== "SAURIMO" ~	"LUNDA SUL",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CALANDULA" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "MOXICO (LUENA)" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "CUBAL" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "BIE" &	District	== "CHINGUAR" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "CAUNGULA" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "QUELA" ~	"MALANJE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "QUIMBELE" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "CAIMBAMBO" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &	District	== "CUCHI" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "DIRICO" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "BELAS" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "ZAIRE" &	District	== "NOQUI" ~	"ZAIRE",
      Country	== "ANGOLA" &
        Province	== "ZAIRE" &	District	== "SOYO" ~	"ZAIRE",
      Country	== "ANGOLA" &
        Province	== "BENGO" &	District	== "AMBRIZ" ~	"BENGO",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "BAIA FARTA" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "BENGUELA" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "BOCOIO" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "CATUMBELA" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "CHONGOROI" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "LOBITO" ~	"BENGUELA",
      Country	== "ANGOLA" &
        Province	== "CABINDA" &	District	== "BELIZE" ~	"CABINDA",
      Country	== "ANGOLA" &
        Province	== "CABINDA" &	District	== "CABINDA" ~	"CABINDA",
      Country	== "ANGOLA" &
        Province	== "CABINDA" &	District	== "CACONGO" ~	"CABINDA",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "CUANGAR" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "MAVINGA" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "RIVUNGO" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &
        District	== "GOLUNGO ALTO" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUNENE" &	District	== "CUROCA" ~	"CUNENE",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "LONGONJO" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "GAMBOS" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "LUBANGO" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "QUIPUNGO" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "ICOLO E BENGO" ~	"BENGO",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "LUQUEMBO" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "MARIMBA" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "LUCHAZES" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "BEMBE" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "ZAIRE" &	District	== "MBANZA CONGO" ~	"ZAIRE",
      Country	== "ANGOLA" &
        Province	== "ZAIRE" &	District	== "TOMBOCO" ~	"ZAIRE",
      Country	== "ANGOLA" &
        Province	== "BENGO" &	District	== "BULA ATUMBA" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "BENGO" &	District	== "DANDE" ~	"BENGO",
      Country	== "ANGOLA" &
        Province	== "BENGO" &	District	== "DEMBOS (QUIBAXE)" ~	"BENGO",
      Country	== "ANGOLA" &
        Province	== "BENGO" &	District	== "PANGO ALUQUEM" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "BENGUELA" &	District	== "GANDA" ~	"BENGUELA",
      Country	== "ANGOLA" &	Province	== "BIE" &
        District	== "ANDULO" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "BIE" &	District	== "CAMACUPA" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "BIE" &	District	== "CATABOLA" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "BIE" &	District	== "CHITEMBO" ~	"BIE",
      Country	== "ANGOLA" &	Province	== "BIE" &
        District	== "CUEMBA" ~	"BIE",
      Country	== "ANGOLA" &	Province	== "BIE" &
        District	== "CUITO" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "BIE" &	District	== "CUNHINGA" ~	"BIE",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &	District	== "CALAI" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "CUITO CUANAVALE" ~	"KUANDO KUBANGO",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &
        District	== "BOLONGONGO" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &	District	== "CAMBAMBE" ~	"CUANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &	District	== "GONGUEMBO" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &	District	== "LUCALA" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &
        District	== "QUICULUNGO" ~	"CUANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &
        District	== "SAMBA CAJU" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "CASSONGUE" ~	"CUANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "CELA" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "CONDA" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "EBO" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "LIBOLO" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "MUSSENDE" ~	"CUANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "PORTO AMBOIM" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "QUIBALA" ~	"CUANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "QUILENDA" ~	"CUANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUANZA SUL" &	District	== "SELES" ~	"KWANZA SUL",
      Country	== "ANGOLA" &
        Province	== "CUNENE" &	District	== "CAHAMA" ~	"CUNENE",
      Country	== "ANGOLA" &
        Province	== "CUNENE" &	District	== "CUANHAMA" ~	"CUNENE",
      Country	== "ANGOLA" &
        Province	== "CUNENE" &	District	== "CUVELAI" ~	"CUNENE",
      Country	== "ANGOLA" &
        Province	== "CUNENE" &	District	== "NAMACUNDE" ~	"CUNENE",
      Country	== "ANGOLA" &
        Province	== "CUNENE" &	District	== "OMBADJA" ~	"CUNENE",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "BAILUNDO" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "CAALA" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "ECUNHA" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "HUAMBO" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "MUNGO" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "TCHIKALA TCHOLOHANGA" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "TCHINJENJE" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUAMBO" &	District	== "UKUMA" ~	"HUAMBO",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CACONDA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CALUQUEMBE" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CHIBIA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CHICOMBA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CHIPINDO" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "CUVANGO" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "HUMPATA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "JAMBA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "MATALA" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "HUILA" &	District	== "QUILENGUES" ~	"HUILA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "CACUACO" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "CAZENGA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "INGOMBOTA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "KILAMBA KIAXI" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "LUANDA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "MAIANGA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "RANGEL" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "SAMBA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "SAMBIZANGA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "TALATONA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "VIANA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "CAMBULO" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &
        District	== "CAPENDA CAMULEMBA" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "CHITATO" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "CUANGO" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "CUILO" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "LOVUA" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "LUBALO" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA NORTE" &	District	== "LUCAPA" ~	"LUNDA NORTE",
      Country	== "ANGOLA" &
        Province	== "LUNDA SUL" &	District	== "CACOLO" ~	"LUNDA SUL",
      Country	== "ANGOLA" &
        Province	== "LUNDA SUL" &	District	== "DALA" ~	"LUNDA SUL",
      Country	== "ANGOLA" &
        Province	== "LUNDA SUL" &	District	== "MUCONDA" ~	"LUNDA SUL",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CACULAMA (MUCARI)" ~	"MALANJE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CACUSO" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CAHOMBO" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CAMBUNDI CATEMBO" ~	"MALANJE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CANGANDALA" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "CUNDA DIA BAZE" ~	"MALANJE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "KIUABA NZOJI" ~	"MALANJE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "MALANJE" ~	"MALANJE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "MASSANGO" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MALANJE" &	District	== "QUIRIMA" ~	"MALANGE",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "ALTO ZAMBEZE" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "BUENGAS" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "CAMANONGUE" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "LEUA" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "LUACANO" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "LUAU" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "MOXICO" &	District	== "LUMEJE (CAMEIA)" ~	"MOXICO",
      Country	== "ANGOLA" &
        Province	== "NAMIBE" &	District	== "BIBALA" ~	"NAMIBE",
      Country	== "ANGOLA" &
        Province	== "NAMIBE" &	District	== "CAMUCUIO" ~	"NAMIBE",
      Country	== "ANGOLA" &
        Province	== "NAMIBE" &	District	== "NAMIBE" ~	"NAMIBE",
      Country	== "ANGOLA" &
        Province	== "NAMIBE" &	District	== "TOMBUA" ~	"NAMIBE",
      Country	== "ANGOLA" &
        Province	== "NAMIBE" &	District	== "VIREI" ~	"NAMIBE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "AMBUILA" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "BUNGO" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "CANGOLA" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "DAMBA" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "MAQUELA DO ZOMBO" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "MILUNGA" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "MUCABA" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "NEGAGE" ~	"UIGE",
      Country	== "ANGOLA" &	Province	== "UIGE" &
        District	== "PURI" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "QUITEXE" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "SANZA POMBO" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "UIGE" &	District	== "SONGO" ~	"UIGE",
      Country	== "ANGOLA" &	Province	== "UIGE" &
        District	== "UIGE" ~	"UIGE",
      Country	== "ANGOLA" &
        Province	== "CUANZA NORTE" &	District	== "BANGA" ~	"KWANZA NORTE",
      Country	== "ANGOLA" &
        Province	== "ZAIRE" &	District	== "CUIMBA" ~	"ZAIRE",
      Country	== "ANGOLA" &
        Province	== "CUANDO CUBANGO" &
        District	== "NANCOVA" ~	"CUANDO CUBANGO",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "NEVES BENDINHA" ~	"LUANDA",
      Country	== "ANGOLA" &
        Province	== "ZAIRE" &	District	== "NZETO" ~	"ZAIRE",
      Country	== "ANGOLA" &
        Province	== "LUANDA" &	District	== "NGOLA QUILUANGE" ~	"LUANDA",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "KARONGI" ~	"OUE",
      Country	== "RWANDA" &
        Province	== "EAST" &	District	== "BUGESERA" ~	"EST",
      Country	== "RWANDA" &
        Province	== "EAST" &	District	== "GATSIBO" ~	"EST",
      Country	== "RWANDA" &
        Province	== "EAST" &	District	== "KIREHE" ~	"EST",
      Country	== "RWANDA" &	Province	== "EAST" &
        District	== "NGOMA" ~	"EST",
      Country	== "RWANDA" &
        Province	== "NORTH" &	District	== "RULINDO" ~	"NOR",
      Country	== "RWANDA" &	Province	== "SOUTH" &
        District	== "HUYE" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "NYAMAGABE" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "NYARUGURU" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "RUHANGO" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "NYAMASHEKE" ~	"OUE",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "RUSIZI" ~	"OUE",
      Country	== "RWANDA" &
        Province	== "KIGALI_CITY" &	District	== "GASABO" ~	"MVK",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "KAMONYI" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "MUHANGA" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "EAST" &	District	== "NYAGATARE" ~	"EST",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "NGORORERO" ~	"OUE",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "NYANZA" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "NORTH" &	District	== "GAKENKE" ~	"NOR",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "RUBAVU" ~	"OUE",
      Country	== "RWANDA" &
        Province	== "EAST" &	District	== "KAYONZA" ~	"EST",
      Country	== "RWANDA" &
        Province	== "EAST" &	District	== "RWAMAGANA" ~	"EST",
      Country	== "RWANDA" &
        Province	== "KIGALI_CITY" &	District	== "KICUKIRO" ~	"MVK",
      Country	== "RWANDA" &
        Province	== "KIGALI_CITY" &	District	== "NYARUGENGE" ~	"MVK",
      Country	== "RWANDA" &
        Province	== "NORTH" &	District	== "BURERA" ~	"NOR",
      Country	== "RWANDA" &
        Province	== "NORTH" &	District	== "GICUMBI" ~	"NOR",
      Country	== "RWANDA" &
        Province	== "NORTH" &	District	== "MUSANZE" ~	"NOR",
      Country	== "RWANDA" &
        Province	== "SOUTH" &	District	== "GISAGARA" ~	"SUD",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "NYABIHU" ~	"OUE",
      Country	== "RWANDA" &
        Province	== "WEST" &	District	== "RUTSIRO" ~	"OUE",
      Country	== "BOTSWANA" &
        Province	== "CHARLESHILL" &	District	== "GANTSI" ~	"GANTSI",
      Country	== "BOTSWANA" &
        Province	== "PALAPYE" &
        District	== "SEROWE/PALAPYE" ~	"SEROWE/PALAPYE",
      Country	== "BOTSWANA" &
        Province	== "SOUTH EAST" &	District	== "SOUTH EAST" ~	"SOUTH EAST",
      Country	== "BOTSWANA" &
        Province	== "KGALAGADI NORTH" &
        District	== "KGALAGADI" ~	"KGALAGADI SOUTH",
      Country	== "BOTSWANA" &
        Province	== "KGATLENG" &	District	== "KGATLENG" ~	"KGATLENG",
      Country	== "BOTSWANA" &
        Province	== "KWENENG WEST" &
        District	== "KWENENG WEST" ~	"KWENENG WEST",
      Country	== "BOTSWANA" &
        Province	== "LOBATSE" &	District	== "LOBATSE" ~	"LOBATSE",
      Country	== "BOTSWANA" &
        Province	== "MOSHUPA" &	District	== "KANYE/MOSHUPA" ~	"KANYE/MOSHUPA",
      Country	== "BOTSWANA" &
        Province	== "TUTUME" &	District	== "TUTUME" ~	"TUTUME",
      Country	== "BOTSWANA" &
        Province	== "BOBIRWA" &	District	== "BOBIRWA" ~	"BOBIRWA",
      Country	== "BOTSWANA" &
        Province	== "GABORONE" &	District	== "GABORONE" ~	"GABORONE",
      Country	== "BOTSWANA" &
        Province	== "GOODHOPE" &	District	== "GOODHOPE" ~	"GOODHOPE",
      Country	== "BOTSWANA" &
        Province	== "MABUTSANE" &	District	== "MABUTSANE" ~	"MABUTSANE",
      Country	== "BOTSWANA" &
        Province	== "SELIBE PHIKWE" &
        District	== "SELIBE PHIKWE" ~	"SELIBE PHIKWE",
      Country	== "BOTSWANA" &
        Province	== "BOTETI" &	District	== "BOTETI" ~	"BOTETI",
      Country	== "BOTSWANA" &
        Province	== "CHOBE" &	District	== "CHOBE" ~	"CHOBE",
      Country	== "BOTSWANA" &
        Province	== "GREATER FRANCISTOWN" &
        District	== "FRANCISTOWN" ~	"FRANCISTOWN",
      Country	== "BOTSWANA" &
        Province	== "MAHALAPYE" &	District	== "MAHALAPYE" ~	"MAHALAPYE",
      Country	== "BOTSWANA" &
        Province	== "NORTH EAST" &	District	== "NORTH EAST" ~	"NORTH EAST",
      Country	== "BOTSWANA" &
        Province	== "JWANENG" &	District	== "JWANENG" ~	"JWANENG",
      Country	== "BOTSWANA" &
        Province	== "KWENENG EAST" &
        District	== "KWENENG EAST" ~	"KWENENG EAST",
      Country	== "BOTSWANA" &
        Province	== "NGAMI" &	District	== "NGAMI" ~	"NGAMI",
      Country	== "BOTSWANA" &
        Province	== "OKAVANGO" &	District	== "OKAVANGO" ~	"OKAVANGO",
      Country	== "LIBERIA" &
        Province	== "GRAND KRU" &	District	== "DORBOR" ~	"GRAND KRU",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "JORQUELLEH" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "LOFA" &	District	== "ZORZOR" ~	"LOFA",
      Country	== "LIBERIA" &
        Province	== "NIMBA" &	District	== "TAPPITA" ~	"NIMBA",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "KOKOYAH" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "GBARPOLU" &	District	== "GBARMA" ~	"GBARPOLU",
      Country	== "LIBERIA" &
        Province	== "GRAND CAPE MOUNT" &
        District	== "GARWULA" ~	"GRAND CAPE MOUNT",
      Country	== "LIBERIA" &
        Province	== "GRAND GEDEH" &	District	== "GBAO" ~	"GRAND GEDEH",
      Country	== "LIBERIA" &
        Province	== "GRAND GEDEH" &	District	== "KONOBO" ~	"GRAND GEDEH",
      Country	== "LIBERIA" &
        Province	== "LOFA" &	District	== "FOYA" ~	"LOFA",
      Country	== "LIBERIA" &
        Province	== "LOFA" &	District	== "SALAYEA" ~	"LOFA",
      Country	== "LIBERIA" &
        Province	== "LOFA" &	District	== "VOINJAMA" ~	"LOFA",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &
        District	== "CENTRAL MONROVIA" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "RIVERCESS" &	District	== "DOEDAIN" ~	"RIVERCESS",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "DUGBE RIVER" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "BOMI" &	District	== "DOWEIN" ~	"BOMI",
      Country	== "LIBERIA" &
        Province	== "BOMI" &	District	== "KLAY" ~	"BOMI",
      Country	== "LIBERIA" &
        Province	== "BOMI" &	District	== "SENJEH" ~	"BOMI",
      Country	== "LIBERIA" &
        Province	== "BOMI" &	District	== "SUEHN MECCA" ~	"BOMI",
      Country	== "LIBERIA" &
        Province	== "GBARPOLU" &	District	== "BOKOMU" ~	"GBARPOLU",
      Country	== "LIBERIA" &
        Province	== "GBARPOLU" &	District	== "BOPOLU" ~	"GBARPOLU",
      Country	== "LIBERIA" &
        Province	== "GBARPOLU" &	District	== "KONGBA" ~	"GBARPOLU",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &
        District	== "District # 3A & 3B" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &
        District	== "District # 4" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND CAPE MOUNT" &
        District	== "COMMONWEALTH-C" ~	"GRAND CAPE MOUNT",
      Country	== "LIBERIA" &
        Province	== "GRAND CAPE MOUNT" &
        District	== "GOLAKONNEH" ~	"GRAND CAPE MOUNT",
      Country	== "LIBERIA" &
        Province	== "GRAND CAPE MOUNT" &
        District	== "PORKPA" ~	"GRAND CAPE MOUNT",
      Country	== "LIBERIA" &
        Province	== "GRAND CAPE MOUNT" &
        District	== "TEWOR" ~	"GRAND CAPE MOUNT",
      Country	== "LIBERIA" &
        Province	== "GRAND GEDEH" &	District	== "B'HAI" ~	"GRAND GEDEH",
      Country	== "LIBERIA" &
        Province	== "GRAND GEDEH" &	District	== "CAVALLA" ~	"GRAND GEDEH",
      Country	== "LIBERIA" &
        Province	== "GRAND GEDEH" &	District	== "PUTU" ~	"GRAND GEDEH",
      Country	== "LIBERIA" &
        Province	== "GRAND GEDEH" &	District	== "TCHIEN" ~	"GRAND GEDEH",
      Country	== "LIBERIA" &
        Province	== "GRAND KRU" &	District	== "BARCLAYVILLE" ~	"GRAND KRU",
      Country	== "LIBERIA" &
        Province	== "GRAND KRU" &	District	== "JRAOH" ~	"GRAND KRU",
      Country	== "LIBERIA" &
        Province	== "GRAND KRU" &	District	== "TREHN" ~	"GRAND KRU",
      Country	== "LIBERIA" &
        Province	== "MARYLAND" &	District	== "HARPER" ~	"MARYLAND",
      Country	== "LIBERIA" &
        Province	== "MARYLAND" &	District	== "KARLUWAY 2" ~	"MARYLAND",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &	District	== "BUSHROD" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "NIMBA" &	District	== "SANNIQUELLEH MAHN" ~	"NIMBA",
      Country	== "LIBERIA" &
        Province	== "RIVER-GEE" &	District	== "SARBO" ~	"RIVER GEE",
      Country	== "LIBERIA" &
        Province	== "RIVER-GEE" &	District	== "TIENPO" ~	"RIVER GEE",
      Country	== "LIBERIA" &
        Province	== "RIVER-GEE" &	District	== "WEBBO" ~	"RIVER GEE",
      Country	== "LIBERIA" &
        Province	== "RIVERCESS" &	District	== "CENTRAL C" ~	"RIVERCESS",
      Country	== "LIBERIA" &
        Province	== "RIVERCESS" &	District	== "JOE RIVER" ~	"RIVERCESS",
      Country	== "LIBERIA" &
        Province	== "RIVERCESS" &	District	== "JOWEIN" ~	"RIVERCESS",
      Country	== "LIBERIA" &
        Province	== "RIVERCESS" &	District	== "TIMBO" ~	"RIVERCESS",
      Country	== "LIBERIA" &
        Province	== "RIVERCESS" &	District	== "YARNIE" ~	"RIVERCESS",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "BUTAW" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "GBLONEE" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "GREENVILLE" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "JEADE" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "JEDEPO" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "KPANYAN" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "PYNES TOWN" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "TARJUWON" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "SINOE" &	District	== "TARSUE" ~	"SINOE",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "PANTA" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "SALALA" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "SANOYEAH" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &	District	== "BUCHANAN" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &	District	== "CAMP WOOD" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &
        District	== "District # 1" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &
        District	== "District # 2" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &
        District	== "District # 3C" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "GRAND BASSA" &	District	== "OWENSGROVE" ~	"GRAND BASSA",
      Country	== "LIBERIA" &
        Province	== "LOFA" &	District	== "VAHUN" ~	"LOFA",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &	District	== "TODEE" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "NIMBA" &	District	== "SACLEPEA-MAHN" ~	"NIMBA",
      Country	== "LIBERIA" &
        Province	== "NIMBA" &	District	== "YARWEIN MEHNSOHNNEH" ~	"NIMBA",
      Country	== "LIBERIA" &
        Province	== "NIMBA" &	District	== "ZOE-GEH" ~	"NIMBA",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "SUAKOKO" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "MARGIBI" &	District	== "FIRESTONE" ~	"MARGIBI",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &	District	== "CAREYSBURG" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &
        District	== "COMMONWEALTH" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &
        District	== "SOMALIA DRIVE" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "MONTSERRADO" &
        District	== "ST. PAUL RIVER" ~	"MONTSERRADO",
      Country	== "LIBERIA" &
        Province	== "MARGIBI" &	District	== "KAKATA" ~	"MARGIBI",
      Country	== "LIBERIA" &
        Province	== "RIVER GEE" &	District	== "CHEDEPO" ~	"RIVER GEE",
      Country	== "LIBERIA" &
        Province	== "RIVER GEE" &	District	== "POTUPO" ~	"RIVER GEE",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "KPAAI" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "GBARPOLU" &	District	== "BELLEH" ~	"GBARPOLU",
      Country	== "LIBERIA" &
        Province	== "GRAND KRU" &	District	== "BUAH" ~	"GRAND KRU",
      Country	== "LIBERIA" &
        Province	== "LOFA" &	District	== "KOLAHUN" ~	"LOFA",
      Country	== "LIBERIA" &
        Province	== "MARGIBI" &	District	== "MAMBAH-KABA" ~	"MARGIBI",
      Country	== "LIBERIA" &
        Province	== "MARYLAND" &	District	== "BAROBO FAJAH" ~	"MARYLAND",
      Country	== "LIBERIA" &
        Province	== "MARYLAND" &	District	== "BARROBO WHOJAH" ~	"MARYLAND",
      Country	== "LIBERIA" &
        Province	== "MARYLAND" &	District	== "KARLUWAY 1" ~	"MARYLAND",
      Country	== "LIBERIA" &
        Province	== "BONG" &	District	== "ZOTA" ~	"BONG",
      Country	== "LIBERIA" &
        Province	== "RIVER GEE" &	District	== "GBEAPO" ~	"RIVER GEE",
      Country	== "LIBERIA" &
        Province	== "NIMBA" &	District	== "GBEHLAY-GEH" ~	"NIMBA",
      Country	== "LIBERIA" &
        Province	== "MARGIBI" &	District	== "GIBI" ~	"MARGIBI",
      
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KALENDA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KALONDA-EST" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KAMIJI" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KANDA KANDA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "LUDIMBI-LUKULA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "MULUMBA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "TSHOFA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "WIKONG" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KAMANA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KALAMBAYI KABANGA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "LUPUTA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "MAKOTA" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "MWENE DITU" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "NGANDAJIKA" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "LUBAO" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LOMAMI" &	District	== "KABINDA" ~	"LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "BUTA" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "PANDA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "TITULE" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KAMBOVE" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KAMPEMBA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KIKULA" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KOWE" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "LIKASI" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "RWASHI" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "VANGU" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "KANZALA" ~	"KASAI-OCCIDENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "KALONDA-OUEST" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "TSHIKAPA" ~	"KASAI-OCCIDENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "KATOKA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "LUIZA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "LUKONGA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "NDESHA" ~	"KASAI-OCCIDENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "TSHIKAJI" ~	"KASAI-OCCIDENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "DILALA" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "FUNGURUME" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "KANZENZE" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "LUALABA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "MANIKA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "NTAND EMBELO" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "GBADOLITE" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "BAGIRA-KASHA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "FIZI" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "IBANDA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KABARE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KADUTU" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KALONGE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KATANA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KAZIBA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MUBUMBANO" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "NUNDU" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "NYANGEZI" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "NYANTENDE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "UVIRA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "WALUNGU" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BOGOSE NUBEA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "KABONDO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "MANGOBO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "TSHOPO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "AKETI" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "GANGA" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "BASANKUSU" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "BIKORO" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "BOLENGE" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "BOLOMBA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "BOMONGO" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "DJOMBO" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "IBOKO" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "INGENDE" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "LILANGA BOBANGI" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "MBANDAKA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "NTONDO" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "WANGATA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KAMALONDO" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KATUBA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KISANGA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "LUBUMBASHI" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "MUMBUNDA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "TSHAMILEMBA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "BAKA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KAMINA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KINDA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KITENGE" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &
        District	== "KABONDO-DIANDA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &
        District	== "MALEMBA-NKULU" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "SONGA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "DUNGU" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "ISIRO" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "PAWA" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "RUNGU" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "WATSA" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "ADJA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "ARIWARA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "ARU" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "BAMBU-MINES" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "BUNIA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "KOMANDA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "LITA" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "LOGO" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "LOLWA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "MAHAGI" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "MANGALA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "NIZI" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "NYANKUNDE" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "RWAMPARA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "BOBOZO" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "KANANGA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "BIPEMBA" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "BONZOLA" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "DIBINDI" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &	District	== "DIULU" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "KANSELE" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "LUBILANJI" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "LUKELENGE" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "MPOKOLO" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &	District	== "MUYA" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &	District	== "NZABA" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "BANDALUNGWA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "BARUMBU" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "BINZA-METEO" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "BINZA-OZONE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "BIYELA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "BUMBU" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "GOMBE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KASA-VUBU" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KIKIMI" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KIMBANSEKE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KINGABWA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KINGASANI" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KINSHASA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KINTAMBO" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KISENSO" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KOKOLO" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KALAMU I" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "KALAMU II" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "LEMBA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "LIMETE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "LINGWALA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MAKALA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MATETE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MALUKU I" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MASINA I" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MASINA II" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MONT-NGAFULA I" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MONT-NGAFULA II" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "NDJILI" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "NGABA" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "NGIRI-NGIRI" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "NSELE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "POLICE" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "SELEMBAO" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "BOKO-KIVULU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "BOMA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS-CONGO" &	District	== "BOMA BUNGU" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "GOMBE-MATADI" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "INGA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KANGU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KIBUNZI" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KIMPESE" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KIMVULA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "KINKONZI" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KISANTU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KITONA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KIZU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "MATADI" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "MBANZA-NGUNGU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "MUANDA" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "NSONA-PANGU" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "NZANZA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "SEKEBANZA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "TSHELA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "BANDUNDU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "BULUNGU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "IDIOFA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "KIKWIT-NORD" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "KIKWIT-SUD" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "MOSANGO" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "BOKORO" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "BANJOW MOKE" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "INONGO" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "KIRI" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "NIOKI" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "YUMBI" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "ALUNGULI" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KAILO" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KALIMA" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KAMPENE" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KASONGO" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KIBOMBO" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KINDU" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "LUBUTU" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "BINGA" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "LISALA" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "YAMONGILI" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "BENI" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "BUTEMBO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "GOMA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KALUNGUTA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KARISIMBI" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KATWA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MABALAKO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MUSIENENE" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "NYIRAGONGO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "OICHA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "VUHOVI" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "BOSOBOLO" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "BUSINGA" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "KARAWA" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "LOKO" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "MOBAYI" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "KOLE" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "LODJA" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "OTOTO" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "TSHUMBE" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "VANGA-KETE" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "GEMENA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "ANKORO" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "KABALO" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "KALEMIE" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "KANSIMBA" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "KIAMBI" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "KONGOLO" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "MANONO" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "MBULULA" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "MOBA" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "NYEMBA" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TANGANYIKA" &	District	== "NYUNZU" ~	"TANGANYIKA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "BANALIA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "BASALI" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "BASOKO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "BENGAMISA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "MAKISO-KISANGANI" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "OPALA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "BEFALE" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "BOENDE" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "DJOLU" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "IKELA" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "WEMA" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "YALIFAFU" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "BOKO" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "BAMBO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KIROTSHE" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "LOLANGA MAMPOKO" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "YANGALA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "FEREKENI" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "ITOMBWE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "YAHISULI" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "LIKATI" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "LOTUMBE" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KENYA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "MULONGO" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "FARADJE" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "WAMBA" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "RIMBA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "BENA-LEKA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "BENA-TSHIADI" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "BUNKONDE" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "KALOMBA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "LUBONDAYI" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "LUBUNGA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "MUTOTO" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "TSHIBALA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "BIBANGA" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "CILUNDU" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "KABEYA KAMWANGA" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &	District	== "MIABI" ~	"KASAI-ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "MUKUMBI" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "TSHILENGE" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "TSHISHIMBI" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "KUIMBA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "LUOZI" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "MANGEMBO" ~	"BAS-CONGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "MASA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "NGIDINGA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "NSELO" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "ILEBO" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "KAMUESHA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "LUEBO" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "NDJOKO PUNDA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "NYANGA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "FESHI" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KIMBAU" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "POPOKABAKA" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "TEMBO" ~	"BANDUNDU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "BAGATA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "SIA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "BOSOBE" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "KWAMOUTH" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "MIMIA" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "OSHWE" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "PENDJUA" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "LUSANGI" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "PANGI" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "BIENA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "BIRAMBIZO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KAYNA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KYONDO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "LUBERO" ~	"NORD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MASEREKA" ~	"NORD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "PINGA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "WALIKALE" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &
        District	== "HAUTS PLATEAUX UVIRA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "IDJWI" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KALOLE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KAMITUGA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KANIOLA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KIMBI LULENGE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KITUTU" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "LEMERA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MINEMBWE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MITI-MURRHESA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MULUNGU" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MWANA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "SHABUNDA CENTRE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "BONDO" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "MONGA" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "VIADANA" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "LUKOLELA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "MONIEKA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &
        District	== "MUFUNGA-SAMPWE" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "BUTUMBA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KANIAMA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KAYAMBA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KINKONDJA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "LWAMBA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "MUKANGA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "ABA" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "BOMA MANGBETU" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &
        District	== "KILELA BALANDA" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "MITWABA" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "AUNGBA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "BIRINGI" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "BOGA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "GETHY" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "JIBA" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "KAMBALA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "KILO" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "MANDIMA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "MONGBWALU" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "NIA-NIA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "NYARAMBE" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "RETHY" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "BILOMBA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "DEMBA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "DIBAYA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "KATENDE" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "MASUIKA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "MIKALAYI" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "MUETSHI" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "NDEKESHA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &
        District	== "TSHIKULA" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "CITENGE" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI-ORIENTAL" &
        District	== "KASANSA" ~	"KASAI ORIENTAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "KIMPANGU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "KWILU-NGONGO" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "LUKULA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &
        District	== "SONA-BATA" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KONGO-CENTRAL" &	District	== "VAKU" ~	"KONGO CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "BANGA LUBAKA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "BULAPE" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "DEKESE" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "KAKENGE" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "KAMONIA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "KITANGWA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "MIKOPE" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "MUSHENGE" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "MUTENA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI" &	District	== "MWEKA" ~	"KASAI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KINSHASA" &	District	== "MALUKU II" ~	"KINSHASA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KAHEMBA" ~	"BANDUNDU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KAJIJI" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KASONGOLUNDA" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KENGE" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KISANDJI" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "KITENDA" ~	"BANDUNDU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "PANZI" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "WAMBA LUADI" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "DJUMA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "GUNGU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "IPAMU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "KIKONGO" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "KIMPUTU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "KINGANDU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "LUSANGA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "MASI-MANIMBA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "MOANZA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "MOKALA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "MUKEDI" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "MUNGINDU" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "PAY KONGILA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "VANGA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "YASA-BONGA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "BOLOBO" ~	"BANDUNDU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MAINDOMBE" &	District	== "MUSHIE" ~	"MAINDOMBE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KABAMBARE" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "KUNDA" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "PUNIA" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "SALAMABILA" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "TUNDA" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "BOSOMODANDA" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "LOLO" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "ALIMBONGO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KAMANGO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KIBUA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MUTWANGA" ~	"NORD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MWESO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "RUTSHURU" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "RWANGUBA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "BUNYAKIRI" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "LULINGU" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "RUZIZI" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "BAFWAGBOGBO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "BAFWASENDE" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "OPIENGE" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "YAKUSU" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "BOKUNGU" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "MOMPONO" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "MONDOMBE" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "ANGO" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "BILI" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "BAS UELE" &	District	== "POKO" ~	"BAS UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "IREBU" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "EQUATEUR" &	District	== "MAKANZA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KAFUBU" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KASENGA" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KILWA" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "LUKAFU" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "PWETO" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "SAKANIA" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "BUKAMA" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT-LOMAMI" &	District	== "KABONGO" ~	"HAUT LOMAMI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "DORUMA" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "GOMBARI" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KASHOBWE" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "ADI" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "ANGUMU" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "FATAKI" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "LAYBO" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "MAMBASA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KASAI CENTRAL" &	District	== "LUAMBO" ~	"KASAI CENTRAL",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWANGO" &	District	== "MWELA LEMBWA" ~	"KWANGO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "KWILU" &	District	== "KOSHIBANDA" ~	"KWILU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "DILOLO" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "BUNKEYA" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "KAFAKUMBA" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "KALAMBA" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "KAPANGA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "KASAJI" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "LUBUDI" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "MUTSHATSHA" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "OBOKOTE" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MANIEMA" &	District	== "SAMBA" ~	"MANIEMA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "BONGANDANGA" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "BOSOMANZI" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "BOSONDJO" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "BUMBA" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "PIMU" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "YAMALUKA" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "MONGALA" &	District	== "YAMBUKU" ~	"MONGALA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "ITEBERO" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MANGUREDJIPA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "MASISI" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "ABUZI" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "WAPINDA" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "WASOLO" ~	"NORD UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD UBANGI" &	District	== "YAKOMA" ~	"EQUATEUR",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "KALEHE" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MINOVA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-KIVU" &	District	== "MWENGA" ~	"SUD-KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "BENA-DIBELE" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "DIKUNGU" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "KATAKO-KOMBE" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "LOMELA" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "LUSAMBO" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "MINGA" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "PANIA MUTOMBO" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "TSHUDI-LOTO" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "WEMBO-NYAMA" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "ISANGI" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "LOWA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "UBUNDU" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "WANIE-RUKULA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "YABAONDO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "YAHUMA" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "BUSANGA" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KAPOLOWE" ~	"KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT KATANGA" &	District	== "KIPUSHI" ~	"HAUT KATANGA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "MAKORO" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "HAUT UELE" &	District	== "NIANGARA" ~	"HAUT UELE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "TCHOMIA" ~	"ORIENTALE",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "LUALABA" &	District	== "SANDOA" ~	"LUALABA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "BINZA" ~	"NORD KIVU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BANGABOLA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BOKONZI" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BOMINENGE" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BOTO" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BUDJALA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BULU" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "BWAMANDA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "KUNGU" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "LIBENGE" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "MAWUYA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "MBAYA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "NDAGE" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "TANDALA" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SUD-UBANGI" &	District	== "ZONGO" ~	"SUD-UBANGI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "DJALO-NDJEKA" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "SANKURU" &	District	== "OMENDJADI" ~	"SANKURU",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "YALEKO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHOPO" &	District	== "YALIMBONGO" ~	"TSHOPO",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "LINGOMO" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "TSHUAPA" &	District	== "MONKOTO" ~	"TSHUAPA",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "DAMAS" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "ITURI" &	District	== "LINGA" ~	"ITURI",
      Country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        Province	== "NORD KIVU" &	District	== "KIBIRIZI" ~	"NORD KIVU",
      TRUE ~ Province))

write_xlsx(Admin, "C:/Users/TOURE/Documents/REPOSITORIES/admin/AFRO_admin _data.xlsx")
