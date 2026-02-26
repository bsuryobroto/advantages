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

# item responses as words..
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
    #   see `genloglin.R`
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
    # ---- word frequencies ----
    #
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
    row.names(advant.words) <- NULL
    str(advant.words)
## 'data.frame':   36 obs. of  5 variables:
##  $ WORDS        : chr  "sda" "pasir" "tambang" "batu" ...
##  $ MALEinRISKY  : num  0.307 0.487 0.357 0.307 1 ...
##  $ MALEinSAFE   : num  0.281 0.562 0.326 0.14 1 ...
##  $ FEMALEinRISKY: num  0.161 0.483 0.322 0.161 1 ...
##  $ FEMALEinSAFE : num  0.364 0.677 0.468 0.364 1 ...


    #
    # ---- bump chart ----
    #

    # ggbump::geom_bump() requires long format data..
    advant.words <- reshape( 
        advant.words, 
            direction = "long",     # long format..
            varying = list(2:5),
            v.names = "FREQUENCY",
            timevar = "GROUP",
            times = c("MALEinRISKY", "MALEinSAFE", "FEMALEinRISKY", "FEMALEinSAFE"),
            idvar = "WORDS"
        )
        head(advant.words)
##                       WORDS       GROUP FREQUENCY
## sda.MALEinRISKY         sda MALEinRISKY 0.3073238
## pasir.MALEinRISKY     pasir MALEinRISKY 0.4870967
## tambang.MALEinRISKY tambang MALEinRISKY 0.3567919
## batu.MALEinRISKY       batu MALEinRISKY 0.3073238
## subur.MALEinRISKY     subur MALEinRISKY 1.0000000
## tanah.MALEinRISKY     tanah MALEinRISKY 0.9386474
        row.names(advant.words) <- NULL
    # create numeric version of GROUP for positioning labels
    advant.words$GROUP_NUM <- 
        factor( advant.words$GROUP,
        levels = c( "MALEinRISKY", 
                    "FEMALEinRISKY", 
                    "MALEinSAFE", 
                    "FEMALEinSAFE") ) |>
        as.numeric()
    str(advant.words)
## 'data.frame':   144 obs. of  4 variables:
##  $ WORDS    : chr  "sda" "pasir" "tambang" "batu" ...
##  $ GROUP    : chr  "MALEinRISKY" "MALEinRISKY" "MALEinRISKY" "MALEinRISKY" ...
##  $ FREQUENCY: num  0.307 0.487 0.357 0.307 1 ...
##  $ GROUP_NUM: num  1 1 1 1 1 1 1 1 1 1 ...
##  - attr(*, "reshapeLong")=List of 4
##   ..$ varying:List of 1
##   .. ..$ : chr [1:4] "MALEinRISKY" "MALEinSAFE" "FEMALEinRISKY" "FEMALEinSAFE"
##   ..$ v.names: chr "FREQUENCY"
##   ..$ idvar  : chr "WORDS"
##   ..$ timevar: chr "GROUP"
#
    # load required libraries..
    library(ggplot2)        # the plotting grid
    library(ggbump)         # the bump engine
    library(ggrepel)        # for geom_text_repel
    library(cowplot)        # for theme_minimal_grid
    library(wesanderson)    # for color palette

    #
    # define conceptual frameworks..
    #
# NATURAL RESOURCE : Direct economic and material benefits from extracting volcanic sand and stone
# LAND : Exceptional soil fertility that forms the foundation of the local agrarian economy
# ECONOMIC : Broader livelihood opportunities, including agriculture and volcano-driven tourism
# BIRTH/FAMILY : Lineage/kin ties, birthright, and deep social networks that create an intergenerational connection to the land
# COMFORT : A profound sense of safety, tranquility, and emotional well-being derived from the homeland
# CLIMATE : A preferred quality of life attributed to the region’s cool, fresh air and superior air quality
    #
    # define word groups..
    NAT_RES = c("sda","pasir","tambang","batu")
    LAND = c("subur","tanah","lahan","air")
    ECONOMIC = c("ekonomi","kerja","pencaharian","makan","rezeki","wisata","tanaman","pangan","tani","kebun")
    BIRTH = c("lahir","parent","keluarga","choice","rukun","gotong")
    COMFORT = c("safe","comfort","tentram","asri","tenang") 
    CLIMATE = c("udara","sejuk","segar","dingin","polusi","cuaca","adem")

    #
    # ---- plotting function ----
    #
    plot_advantage_bumpchart <- function(data,
                                         words_to_highlight,
                                         framework_name,
                                         framework_description,
                                         output_filename) {
      
      p <- ggplot2::ggplot(
        data = data,
        aes( x = GROUP_NUM,
             y = FREQUENCY,
             group = WORDS,
             color = WORDS )
      ) +
        cowplot::theme_minimal_grid() +
        # bump lines and points..!
        # -- plot all lines in light gray first..
        ggbump::geom_bump(
          linewidth = 0.6,
          smooth = 10,
          color = "gray90",
          show.legend = F ) +
        # -- overplot target words..
        ggbump::geom_bump(
          data = subset( data , WORDS %in% words_to_highlight ),
          aes(color = WORDS),
          linewidth = 0.8,
          smooth = 10,
          show.legend = F ) +
        # -- show points..
        ggplot2::geom_point(
          data = subset( data, WORDS %in% words_to_highlight ),
          size = 2, show.legend = F) +
        # labelling the rightmost column..
        ggplot2::coord_cartesian(clip = "off") +
        # prepare x-axis..
        ggplot2::scale_x_discrete(
          limits = c("MALEinRISKY", "FEMALEinRISKY", "MALEinSAFE", "FEMALEinSAFE"),
          labels = c("MALE in RISKY", "FEMALE in RISKY", "MALE in SAFE", "FEMALE in SAFE"),
          expand = expansion(add = c(0, 0.3))
        ) +
        ggrepel::geom_text_repel(
          data = subset(data, GROUP == "FEMALEinSAFE" & WORDS %in% words_to_highlight),
          aes(label = WORDS , color = WORDS),
          x = 4.01,
          hjust = -0.5,
          size = 4,
          show.legend = F
        ) +
        # titles and theme..
        ggplot2::labs(
          title = "Conceptual Framework of Advantage\nto Live in a Volcanic Risk Area",
          subtitle = framework_description,
          caption = paste("Mentions of Advantageous Items of", framework_name, "Framework"),
          x = "",
          y = "Normalized Frequency"
        ) +
        ggplot2::theme(
          plot.title = element_text(hjust = 0, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0, size = 14, face = "italic"),
          plot.caption = element_text(hjust = 1, size = 10)
        )
      
      # save the plot..
      svglite::svglite(output_filename, width = 8, height = 8)
      print(p)
      dev.off()
      # return the plot object invisibly
      invisible(p)
    }

    #
    # ---- generate plots for all frameworks ----
    #
    # CLIMATE framework
    plot_advantage_bumpchart(
      data = advant.words,
      words_to_highlight = CLIMATE,
      framework_name = "CLIMATE",
      framework_description = "CLIMATE : A preferred quality of life attributed to the region's cool,\nfresh air and superior air quality",
      output_filename = "out/advantages_climate_words.svg"
    )
    
    # COMFORT framework
    plot_advantage_bumpchart(
      data = advant.words,
      words_to_highlight = COMFORT,
      framework_name = "COMFORT",
      framework_description = "COMFORT : A profound sense of safety, tranquility, and emotional well-being\nderived from the homeland",
      output_filename = "out/advantages_comfort_words.svg"
    )
    
    # BIRTH framework
    plot_advantage_bumpchart(
      data = advant.words,
      words_to_highlight = BIRTH,
      framework_name = "BIRTH/FAMILY",
      framework_description = "BIRTH/FAMILY : Lineage/kin ties, birthright, and deep social networks\nthat create an intergenerational connection to the land",
      output_filename = "out/advantages_birth_words.svg"
    )
    
    # ECONOMIC framework
    plot_advantage_bumpchart(
      data = advant.words,
      words_to_highlight = ECONOMIC,
      framework_name = "ECONOMIC",
      framework_description = "ECONOMIC : Broader livelihood opportunities, including agriculture\nand volcano-driven tourism",
      output_filename = "out/advantages_economic_words.svg"
    )
    
    # LAND framework
    plot_advantage_bumpchart(
      data = advant.words,
      words_to_highlight = LAND,
      framework_name = "LAND",
      framework_description = "LAND : Exceptional soil fertility that forms the foundation\nof the local agrarian economy",
      output_filename = "out/advantages_land_words.svg"
    )
    
    # NATURAL RESOURCE framework
    plot_advantage_bumpchart(
      data = advant.words,
      words_to_highlight = NAT_RES,
      framework_name = "NATURAL RESOURCE",
      framework_description = "NATURAL RESOURCE : Direct economic and material benefits\nfrom extracting volcanic sand and stone",
      output_filename = "out/advantages_natres_words.svg"
    )
