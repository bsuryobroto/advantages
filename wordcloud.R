# ===========================================================
#
# Q: What are the advantages to live in a volcanic risk area?
#
# ___________________________________________________________

    #
    # ---- load data ----
    #

# raw data..
advantages <- read.csv( "data/advantages.csv" )
    # see..
    str(advantages)
## 'data.frame':   753 obs. of  3 variables:
##  $ Risky_Area          : chr  "Risky" "Risky" "Risky" "Risky" ...
##  $ gender              : chr  "Male" "Male" "Female" "Female" ...
##  $ advantage_risky_area: chr  "tanah subur" "tanah subur, mata air besar" "Jauh dari polusi  Gampang Mandapat bahan makanan" "Bisa makan, dapat pangasilan,dekat sama keluarga" ...

# item responses as words to cloud..
(words <- read.csv("data/advant_dict.csv",header=FALSE)[,1])
##  [1] "sda"         "pasir"       "tambang"     "batu"        "subur"      
##  [6] "tanah"       "lahan"       "air"         "ekonomi"     "kerja"      
## [11] "pencaharian" "makan"       "rezeki"      "wisata"      "tanaman"    
## [16] "pangan"      "tani"        "kebun"       "lahir"       "parent"     
## [21] "keluarga"    "choice"      "safe"        "comfort"     "tentram"    
## [26] "asri"        "tenang"      "rukun"       "gotong"      "udara"      
## [31] "sejuk"       "segar"       "dingin"      "polusi"      "cuaca"      
## [36] "adem"       

    #
    # ---- item response table ----
    #
    #   the `Y` matrix of `WYZ` in MRCV analysis
    #   see `genlogli.R`
    #
        # `1` denote an item chosen (positive response)
        # `0` denote an item not chosen (negative response)
# directly append this `Y` matrix to original dataframe..
    # main engine..`grepl()`
    # ..pattern matching the item response to `advantage_risky_area`..
    for (item in words) {
advantages[,item] <- ifelse( grepl( item , 
                                   advantages$advantage_risky_area,
                                   ignore.case = TRUE, fixed = FALSE), 
                                   1, 0)    # pos/neg responses
                          }
    str(advantages)
## 'data.frame':   753 obs. of  39 variables:
##  $ Risky_Area          : chr  "Risky" "Risky" "Risky" "Risky" ...
##  $ gender              : chr  "Male" "Male" "Female" "Female" ...
##  $ advantage_risky_area: chr  "tanah subur" "tanah subur, mata air besar" "Jauh dari polusi  Gampang Mandapat bahan makanan" "Bisa makan, dapat pangasilan,dekat sama keluarga" ...
##  $ sda        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ pasir      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ tambang    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ batu       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ subur      : num  1 1 0 0 0 1 1 1 0 0 ...
##  $ tanah      : num  1 1 0 0 0 0 1 0 0 0 ...
##  $ lahan      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ air        : num  0 1 0 0 0 0 0 0 0 0 ...
##  $ ekonomi    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ kerja      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ pencaharian: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ makan      : num  0 0 1 1 1 0 0 0 0 1 ...
##  $ rezeki     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ wisata     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ tanaman    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ pangan     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ tani       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ kebun      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ lahir      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ parent     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ keluarga   : num  0 0 0 1 1 0 0 0 0 0 ...
##  $ choice     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ safe       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ comfort    : num  0 0 0 0 0 0 0 0 0 1 ...
##  $ tentram    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ asri       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ tenang     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ rukun      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gotong     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ udara      : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ sejuk      : num  0 0 0 0 0 1 1 0 1 0 ...
##  $ segar      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ dingin     : num  0 0 0 0 0 0 0 1 0 0 ...
##  $ polusi     : num  0 0 1 0 0 0 0 0 0 0 ...
##  $ cuaca      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ adem       : num  0 0 0 0 0 0 0 0 0 0 ...

    #
    # word frequencies..
    #

    MALEinRISKY <- advantages[advantages$Risky_Area=="Risky"&advantages$gender=="Male",4:39]
    MALEinSAFE <- advantages[advantages$Risky_Area=="Safe"&advantages$gender=="Male",4:39]
    FEMALEinRISKY <- advantages[advantages$Risky_Area=="Risky"&advantages$gender=="Female",4:39]
    FEMALEinSAFE <- advantages[advantages$Risky_Area=="Safe"&advantages$gender=="Female",4:39]
    #
advant.words <- data.frame(
    apply(MALEinRISKY,2,sum) |> 
        as.matrix() |> 
        as.data.frame() ,
    apply(MALEinSAFE,2,sum) |> 
        as.matrix() |> 
        as.data.frame() ,
    apply(FEMALEinRISKY,2,sum) |> 
        as.matrix() |> 
        as.data.frame() ,
    apply(FEMALEinSAFE,2,sum) |> 
        as.matrix() |> 
        as.data.frame()
    )
    names(advant.words) <- c( "MALEinRISKY" , "MALEinSAFE" ,
                              "FEMALEinRISKY" , "FEMALEinSAFE" )
    advant.words    # see the contingency table..
##             MALEinRISKY MALEinSAFE FEMALEinRISKY FEMALEinSAFE
## sda                   3          3             1            5
## pasir                 8         15             7           27
## tambang               4          4             3            9
## batu                  3          1             1            5
## subur                90        138            73          136
## tanah                68        122            56          122
## lahan                 4          6             2            5
## air                  16         45            19           90
## ekonomi              35         36            28           40
## kerja                 5         16             4            9
## pencaharian           4          3             5            3
## makan                 0          2             4            0
## rezeki                1          2             0            3
## wisata                0          2             0            0
## tanaman               5          5             9            2
## pangan                1          6             4            2
## tani                 19         19             6           17
## kebun                 1          3             1            7
## lahir                 9          3            12            6
## parent                3          0             1            0
## keluarga              0          1             3            0
## choice                0          2             1            6
## safe                  4          3             3           10
## comfort               6         14            11           11
## tentram               3          3             2            2
## asri                  3          2             4            2
## tenang                3          3             1            1
## rukun                 0          0             1            2
## gotong                1          0             0            1
## udara                37         72            28          106
## sejuk                41         84            46          109
## segar                 4         16             5           17
## dingin                6         17             6           21
## polusi                3         15             7            9
## cuaca                 0         10             2           14
## adem                  0          3             0            2
#
    # check..!
    # marginal sum is wrong..as expected..
    apply(advant.words,2,sum)
##   MALEinRISKY    MALEinSAFE FEMALEinRISKY  FEMALEinSAFE 
##           390           676           356           801 

    #
    # word clouds..
    #

    # normalize the counts..convert to percentages..
    advant.words <- apply(advant.words, 2, log1p) # log(1 + x) to handle zero values
        NORMALIZE <- function(x) { return((x - min(x)) / (max(x) - min(x))) }
advant.words <- apply(advant.words, 2, NORMALIZE)
    # check..!
    summary(advant.words)
##   MALEinRISKY       MALEinSAFE     FEMALEinRISKY     FEMALEinSAFE   
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.1537   1st Qu.:0.2226   1st Qu.:0.1610   1st Qu.:0.2233  
##  Median :0.3321   Median :0.3036   Median :0.3739   Median :0.3955  
##  Mean   :0.3574   Mean   :0.4131   Mean   :0.3850   Mean   :0.4298  
##  3rd Qu.:0.4453   3rd Qu.:0.5742   3rd Qu.:0.4961   3rd Qu.:0.5875  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    # add column for words..
advant.words <- data.frame( WORDS = row.names(advant.words) , 
                            advant.words)
    str(advant.words)
## 'data.frame':   36 obs. of  5 variables:
##  $ WORDS        : chr  "sda" "pasir" "tambang" "batu" ...
##  $ MALEinRISKY  : num  0.307 0.487 0.357 0.307 1 ...
##  $ MALEinSAFE   : num  0.281 0.562 0.326 0.14 1 ...
##  $ FEMALEinRISKY: num  0.161 0.483 0.322 0.161 1 ...
##  $ FEMALEinSAFE : num  0.364 0.677 0.468 0.364 1 ...
     #
    library(wordcloud2)
        set.seed(1)     # for reproducibility..but..
    # if you want to save the cloud..first make the html widget as an R object
MALEinRISKY <- wordcloud2::wordcloud2( data=advant.words[,1:2] , minRotation = -pi/2, maxRotation = -pi/2)
MALEinSAFE <- wordcloud2::wordcloud2( data=advant.words[,c(1,3)] , minRotation = -pi/2, maxRotation = -pi/2)
FEMALEinRISKY <- wordcloud2::wordcloud2( data=advant.words[,c(1,4)] , minRotation = -pi/2, maxRotation = -pi/2)
FEMALEinSAFE <- wordcloud2::wordcloud2( data=advant.words[,c(1,5)] , minRotation = -pi/2, maxRotation = -pi/2)
        # save the html widgets as html files
        ## install.packages("htmlwidgets")
        library(htmlwidgets)
    htmlwidgets::saveWidget(MALEinRISKY,"Males_in_RISKY.html",selfcontained = TRUE)
    htmlwidgets::saveWidget(MALEinSAFE,"Males_in_SAFE.html",selfcontained = TRUE)
    htmlwidgets::saveWidget(FEMALEinRISKY,"Females_in_RISKY.html",selfcontained = TRUE)
    htmlwidgets::saveWidget(FEMALEinSAFE,"Females_in_SAFE.html",selfcontained = TRUE)
