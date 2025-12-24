# ..advantages of living in volcanic areas..

[![DOI](https://zenodo.org/badge/1122168799.svg)](https://doi.org/10.5281/zenodo.18044638)


> Title
- Perceptual Advantages of People Living in Semeru Volcanic Areas, Indonesia: An MRCV (Multiple Response Categorical Variable) Analysis

> Authors:
- Yanti Ariyanti, Sarah Nila, Winati Nurhayu, Dimitri Dubois, Kanthi Arum Widayati, Tri Atmowidi, Clément Mettling, Michel Raymond, Marc Willinger, Bambang Suryobroto

> This is the repository for data..statistical `R` script..and results..

## aims..

This study aims to 
- identify the perceptual advantages held by communities on Mount Semeru, 
- measure their association with residencies in high-risk zones, 
- and analyze gender’s moderating role, 

testing the hypothesis that these advantages offset objective risk in hazardous-area residency.

## perceptual advantages..
> perceptual advantages are tangible and intangible benefits experienced by people in their local environment..

## MRCV (Multiple Response Categorical Variable) (Coombs 1964)..
> We asked an open-ended question: “What are the advantages to live in a volcanic risk area?” 

### multiple answers and recurrence..
> [AAA] wrote “tanah subur, sumber mata air melimpah, dan sejuk”
- we infer `subur`, `air`, and `sejuk` **item response**s of advantageousness..
> [BBB] wrote “cuacanya dingin, subur”
- we infer this as `sejuk` and `subur`..

These item responses are compiled into an English dictionary in `advant_dict.csv`..

> to get frequencies of item responses..use `base::grepl()` to match each item to the original written answers..
