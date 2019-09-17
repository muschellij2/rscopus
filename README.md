
<!-- README.md is generated from README.Rmd. Please edit that file -->

R Package to interface with Elsevier and Scopus APIs

<!-- ![Sticker](sticker.png) -->

<img src="sticker.png" width="100">

[![Travis-CI Build
Status](https://travis-ci.org/muschellij2/rscopus.svg?branch=master)](https://travis-ci.org/muschellij2/rscopus)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/rscopus?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/rscopus)

# rscopus

The goal of rscopus is to provide an R Scopus Database ‘API’ Interface.

## Installation

You can install `rscopus` from github with:

``` r
# install.packages("devtools")
devtools::install_github("muschellij2/rscopus")
```

## Steps to get API key

In order to use this package, you need an API key from
<https://dev.elsevier.com/sc_apis.html>. You should login from your
institution and go to Create API Key. You need to provide a website URL
and a label, but the website can be your personal website, and agree to
the terms of service.

1.  Go to <https://dev.elsevier.com/user/login>. Login or create a free
    account.
2.  Click “Create API Key”. Put in a label, such as `rscopus key`. Add a
    website. <http://example.com> is fine if you do not have a site.
3.  **Read** and agree to the TOS if you do indeed agree.
4.  Add `Elsevier_API = "API KEY GOES HERE"` to `~/.Renviron` file, or
    add `export Elsevier_API=API KEY GOES HERE` to your
    `~/.bash_profile`.

Alternatively, you you can either set the API key using
`rscopus::set_api_key` or by `options("elsevier_api_key" = api_key)`.
You can access the API key using `rscopus::get_api_key`.

You should be able to test out the API key using the [interactive Scopus
APIs](https://dev.elsevier.com/scopus.html).

### A note about API keys and IP addresses

The API Key is bound to a set of IP addresses, usually bound to your
institution. Therefore, if you are using this for a Shiny application,
you must host the Shiny application from your institution servers in
some way. Also, you cannot access the Scopus API with this key if you
are offsite and must VPN into the server or use a computing cluster with
an institution IP.

See <https://dev.elsevier.com/tecdoc_api_authentication.html>

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rscopus)
library(dplyr)
res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE)
names(res)
#>  [1] "auth_order"            "affilname_1"          
#>  [3] "n_auth"                "affilname_2"          
#>  [5] "n_affils"              "citations"            
#>  [7] "journal"               "description"          
#>  [9] "title"                 "pii"                  
#> [11] "doi"                   "eid"                  
#> [13] "cover_date"            "cover_display_date"   
#> [15] "prism_url"             "dc_identifier"        
#> [17] "dc_creator"            "prism_issn"           
#> [19] "prism_eIssn"           "prism_pageRange"      
#> [21] "dc_description"        "prism_aggregationType"
#> [23] "subtype"               "authkeywords"         
#> [25] "source_id"             "first_name"           
#> [27] "last_name"             "au_id"
head(res[, c("title", "journal", "description")])
#>                                                                                                                                                   title
#> 1                                                                                            Neuroconductor: An R platform for medical imaging analysis
#> 2                                         MIMoSA: An Approach to automatically segment T2 hyperintense and T1 hypointense lesions in multiple sclerosis
#> 3                                  Objective Evaluation of Multiple Sclerosis Lesion Segmentation using a Data Management and Processing Infrastructure
#> 4                                                  MIMoSA: An Automated Method for Intermodal Segmentation Analysis of Multiple Sclerosis Brain Lesions
#> 5 Radiomic subtyping improves disease stratification beyond key molecular, clinical, and standard imaging characteristics in patients with glioblastoma
#> 6              Feasibility of Coping Effectiveness Training for Caregivers of Children with Autism Spectrum Disorder: a Genetic Counseling Intervention
#>                                                                                                                                journal
#> 1                                                                                                                        Biostatistics
#> 2 Lecture Notes in Computer Science (including subseries Lecture Notes in Artificial Intelligence and Lecture Notes in Bioinformatics)
#> 3                                                                                                                   Scientific Reports
#> 4                                                                                                              Journal of Neuroimaging
#> 5                                                                                                                       Neuro-Oncology
#> 6                                                                                                        Journal of Genetic Counseling
#>        description
#> 1          Article
#> 2 Conference Paper
#> 3          Article
#> 4          Article
#> 5          Article
#> 6          Article
unique(res$au_id)
#> [1] "40462056100"
unique(as.character(res$affilname_1))
#> [1] "Johns Hopkins Bloomberg School of Public Health"
#> [2] "Johns Hopkins University"                       
#> [3] "Departments of Biostatistics"                   
#> [4] "Kennedy Krieger Institute"                      
#> [5] "Johns Hopkins Medical Institutions"             
#> [6] "The Johns Hopkins School of Medicine"

all_dat = author_data(last_name = "Muschelli", 
                 first_name = "John", verbose = FALSE, general = TRUE)
res2 = all_dat$df
res2 = res2 %>% 
  rename(journal = `prism:publicationName`,
         title = `dc:title`,
         description = `dc:description`)
head(res[, c("title", "journal", "description")])
#>                                                                                                                                                   title
#> 1                                                                                            Neuroconductor: An R platform for medical imaging analysis
#> 2                                         MIMoSA: An Approach to automatically segment T2 hyperintense and T1 hypointense lesions in multiple sclerosis
#> 3                                  Objective Evaluation of Multiple Sclerosis Lesion Segmentation using a Data Management and Processing Infrastructure
#> 4                                                  MIMoSA: An Automated Method for Intermodal Segmentation Analysis of Multiple Sclerosis Brain Lesions
#> 5 Radiomic subtyping improves disease stratification beyond key molecular, clinical, and standard imaging characteristics in patients with glioblastoma
#> 6              Feasibility of Coping Effectiveness Training for Caregivers of Children with Autism Spectrum Disorder: a Genetic Counseling Intervention
#>                                                                                                                                journal
#> 1                                                                                                                        Biostatistics
#> 2 Lecture Notes in Computer Science (including subseries Lecture Notes in Artificial Intelligence and Lecture Notes in Bioinformatics)
#> 3                                                                                                                   Scientific Reports
#> 4                                                                                                              Journal of Neuroimaging
#> 5                                                                                                                       Neuro-Oncology
#> 6                                                                                                        Journal of Genetic Counseling
#>        description
#> 1          Article
#> 2 Conference Paper
#> 3          Article
#> 4          Article
#> 5          Article
#> 6          Article
```

## Using an Institution Token

As per <https://dev.elsevier.com/tecdoc_api_authentication.html>: “Using
a proprietary token (an”Institutional Token“) created for you by our
integration support team”, so you need to contact Scopus to get one. If
you have one and it’s located in an object called `token`, you should be
able to use it as:

``` r
# token is from Scopus dev
hdr = inst_token_header(token)
res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE, headers = hdr)
```

but I have not tried it extensively.
