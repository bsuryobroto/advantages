# ..advantages of living in volcanic areas..

> Title
- Perceptual Advantages of People Living in Semeru Volcanic Areas, Indonesia: An MRCV (Multiple Response Categorical Variable) Analysis

> Authors:
- Yanti Ariyanti, Sarah Nila, Winati Nurhayu, Dimitri Dubois, Kanthi Arum Widayati, Tri Atmowidi, Clément Mettling, Michel Raymond, Marc Willinger, Bambang Suryobroto

> Publisher:
- [HAYATI Journal of Biosciences (HAYATI J Biosci; p-ISSN: 1978-3019; e-ISSN: 2086-4094)](https://doi.org/10.4308/hjb.33.3.739-749)

> This is the repository for data..statistical `R` script..and results..
[![DOI](https://zenodo.org/badge/1122168799.svg)](https://doi.org/10.5281/zenodo.18044638)


## aims..

This study aims to 
- identify the perceptual advantages experienced by communities living on Mount Semeru, 
- measure their association with residencies in high-risk zones, 
- and analyze gender’s moderating role, testing the hypothesis that these advantages offset objective risk in hazardous-area residency.

## perceptual advantages..
> perceptual advantages are tangible and intangible benefits experienced by people in their local environment..

## MRCV (Multiple Response Categorical Variable) (Coombs 1964)..
> We asked an open-ended question: “What are the advantages to live in a volcanic risk area?” 

### multiple answers and recurrence..and their inferred item responses..
> [AAA] wrote “tanah subur, sumber mata air melimpah, dan sejuk”
- we infer `subur`, `air`, and `sejuk` **item response**s of advantageousness..
> [BBB] wrote “cuacanya dingin, subur”
- we infer this as `sejuk` and `subur`..

These item responses are compiled into an English dictionary in `advant_dict.csv`..

> to get frequencies of item responses..use `base::grepl()` to match each item to the original written answers..

## visualization...

- item response frequencies are visualized in wordclouds grouped by residency and gender..you can find the plots in our publication..
- however..each of the four wordclouds is not fully interpretable due to randomness in the placement of the items..
- we therefore now provide bumpcharts of item response frequencies..