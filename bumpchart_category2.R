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


NAT_RES = "sda|pasir|tambang|batu"
LAND = "subur|tanah|lahan|air"
ECONOMIC = "ekonomi|kerja|pencaharian|makan|rezeki|wisata|tanaman|pangan|tani|kebun"
BIRTH = "lahir|parent|keluarga|choice|rukun|gotong"
COMFORT = "safe|comfort|tentram|asri|tenang"
CLIMATE = "udara|sejuk|segar|dingin|polusi|cuaca|adem" 


#
# this is the new `Y` matrix..
    elements <- c(NAT_RES, LAND, ECONOMIC, BIRTH, COMFORT, CLIMATE)
    for (element in elements) {
advantages[,element] <- ifelse( grepl( element , 
                                    advantages$advantage_risky_area,
                                    ignore.case = TRUE, fixed = FALSE), 
                                    1, 0)
                          }
str(advantages)
## 'data.frame':   753 obs. of  9 variables:
##  $ Risky_Area                                                             : chr  "Risky" "Risky" "Risky" "Risky" ...
##  $ gender                                                                 : chr  "Male" "Male" "Female" "Female" ...
##  $ advantage_risky_area                                                   : chr  "tanah subur" "tanah subur, mata air besar" "Jauh dari ## polusi  Gampang Mandapat bahan makanan" "Bisa makan, dapat pangasilan,dekat sama keluarga" ...
##  $ sda|pasir|tambang|batu                                                 : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ subur|tanah|lahan|air                                                  : num  1 1 0 0 0 1 1 1 0 0 ...
##  $ ekonomi|kerja|pencaharian|makan|rezeki|wisata|tanaman|pangan|tani|kebun: num  0 0 1 1 1 0 0 0 0 1 ...
##  $ lahir|parent|keluarga|choice|rukun|gotong                              : num  0 0 0 1 1 0 0 0 0 0 ...
##  $ safe|comfort|tentram|asri|tenang                                       : num  0 0 0 0 0 0 0 0 0 1 ...
##  $ udara|sejuk|segar|dingin|polusi|cuaca|adem                             : num  0 0 1 0 0 1 1 1 1 0 ...
    names(advantages)[4:9] <- 
        c( "NAT_RES" , "LAND" , "ECONOMIC" , "BIRTH" , "COMFORT" , "CLIMATE" )

    #
    # ---- category ----
    #
#
    MALEinRISKY <- advantages[advantages$Risky_Area=="Risky"&advantages$gender=="Male",4:9]
    MALEinSAFE <- advantages[advantages$Risky_Area=="Safe"&advantages$gender=="Male",4:9]
    FEMALEinRISKY <- advantages[advantages$Risky_Area=="Risky"&advantages$gender=="Female",4:9]
    FEMALEinSAFE <- advantages[advantages$Risky_Area=="Safe"&advantages$gender=="Female",4:9]
    #
category <- data.frame(
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
    names(category) <- c( "MALEinRISKY" , "MALEinSAFE" ,
                              "FEMALEinRISKY" , "FEMALEinSAFE" )
    category    # see the contingency table..
##          MALEinRISKY MALEinSAFE FEMALEinRISKY FEMALEinSAFE
## NAT_RES           12         17             9           34
## LAND              93        154            79          163
## ECONOMIC          63         84            56           69
## BIRTH             12          5            16           11
## COMFORT           17         19            16           24
## CLIMATE           51        121            58          145

    # normalize the counts..convert to percentages..
    category <- apply(category, 2, log1p) # log(1 + x) to handle zero values
        NORMALIZE <- function(x) { return((x - min(x)) / (max(x) - min(x))) }
    category <- apply(category, 2, NORMALIZE)


    # add column for words..
category <- data.frame( CATEGORY = row.names(category) , 
                            category)
    row.names(category) <- NULL
    str(category)
## 'data.frame':   6 obs. of  5 variables:
##  $ CATEGORY     : chr  "NAT_RES" "LAND" "ECONOMIC" "BIRTH" ...
##  $ MALEinRISKY  : num  0 1 0.806 0 0.164 ...
##  $ MALEinSAFE   : num  0.338 1 0.815 0 0.37 ...
##  $ FEMALEinRISKY: num  0 1 0.837 0.255 0.255 ...
##  $ FEMALEinSAFE : num  0.409 1 0.674 0 0.281 ...

    # ggbump::geom_bump() requires long format data..
    category <- reshape( 
        category, 
            direction = "long",     # long format..
            varying = list(2:5),
            v.names = "FREQUENCY",
            timevar = "GROUP",
            times = c("MALEinRISKY", "MALEinSAFE", "FEMALEinRISKY", "FEMALEinSAFE"),
            idvar = "CATEGORY"
        )
        head(category)
##                       CATEGORY       GROUP FREQUENCY
## sda.MALEinRISKY         sda MALEinRISKY 0.3073238
## pasir.MALEinRISKY     pasir MALEinRISKY 0.4870967
## tambang.MALEinRISKY tambang MALEinRISKY 0.3567919
## batu.MALEinRISKY       batu MALEinRISKY 0.3073238
## subur.MALEinRISKY     subur MALEinRISKY 1.0000000
## tanah.MALEinRISKY     tanah MALEinRISKY 0.9386474
        row.names(category) <- NULL
    # create numeric version of GROUP for positioning labels
    category$GROUP_NUM <- 
        factor( category$GROUP,
        levels = c( "MALEinRISKY", 
                    "FEMALEinRISKY", 
                    "MALEinSAFE", 
                    "FEMALEinSAFE") ) |>
        as.numeric()
    str(category)

    # load required libraries..
    library(ggplot2)        # the plotting grid
    library(ggbump)         # the bump engine
    library(ggrepel)        # for geom_text_repel
    library(cowplot)        # for theme_minimal_grid
    library(wesanderson)    # for color palette




ggplot2::ggplot( 
    data = category, 
    aes( x = GROUP_NUM, 
        y =FREQUENCY, 
        group = CATEGORY ) 
    ) +
    cowplot::theme_minimal_grid() +  # the simplest white paper-like theme..
    #
    # bump lines and points..!
    # -- plot all lines in light gray first..
    ggbump::geom_bump( 
        data = category,
        aes(color = CATEGORY),
        linewidth = 0.8, 
        smooth = 10, 
        show.legend = F ) +
    # -- show points..
    ggplot2::geom_point(
        data = category,
        aes(color = CATEGORY),
            size = 2, show.legend = F) +
    # labelling the rightmost column..
            # allow text to extend beyond the plot area
        ggplot2::coord_cartesian(clip = "off") +
            # prepare x-axis..
        ggplot2::scale_x_discrete(
            # REORDER the x-axis to contrast gender in an area..!
            limits = c("MALEinRISKY", "FEMALEinRISKY", "MALEinSAFE", "FEMALEinSAFE"),
            labels = c("MALE in RISKY", "FEMALE in RISKY", "MALE in SAFE", "FEMALE in SAFE"),
            # extend x-axis to accommodate the new label column
            expand = expansion(add = c(0, 0.4))
            ) +
        ggplot2::geom_text(
        data = subset(category, GROUP == "FEMALEinSAFE"),
            aes(label = CATEGORY , color = CATEGORY), 
            x = 4.05,
            hjust = 0,
            size = 4,
            show.legend = F
        ) +
            # titles and theme..
        ggplot2::labs(
            title = "Conceptual Framework of Advantage\nto Live in a Volcanic Risk Area",
            subtitle = "Categories of experienced feelings, perspectives and opinions in living in a volcanic risk area",
            caption = "Categories consisting of Word Items Conferring Advantages",
            x = "",
            y = "Normalized Frequency"
            ) +
        ggplot2::theme(
            plot.title = element_text(hjust = 0, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0, size = 14, face = "italic"),
            plot.caption = element_text(hjust = 1, size = 10)
            ) -> p
    # save the plot..
svglite::svglite("out/advantages_framework.svg", width = 8, height = 8)
    print(p)
    dev.off()
