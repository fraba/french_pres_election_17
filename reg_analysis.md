Regression analysis of results of the first round of the 2017 French presidential election (commune level)
================
Francesco Bailo
May 12, 2017

Results by region
=================

``` r
load('~/public_git/french_pres_election_17/results_fr_20170423_wt_demog.RData')

require(dplyr)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
region_grouped <- 
  results_fr_20170423_wt_demog %>%
  group_by(code_region) %>%
  summarize(`LE PEN %` = sum(`LE PEN`) / sum(Exprimés),
            immigrants = sum(immigrants),
            chomeurs = sum(chomeurs),
            actifs = sum(actifs),
            retraites_img2a = sum(retraites_img2a),
            surf_ha = sum(surf_ha),
            Inscrits = sum(Inscrits))

mod_reg1 <- 
  lm("I(`LE PEN %`) ~ 
    I(immigrants/Inscrits) + 
    I(chomeurs/(chomeurs+actifs)) + 
    I(retraites_img2a/Inscrits) + 
    I(Inscrits/surf_ha)", 
     data = region_grouped)
```

<table style="text-align:center">
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
First round - Regional level
</td>
</tr>
<tr>
<td>
</td>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Le Pen votes over Valid votes
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Immigrants over Registered voters
</td>
<td>
-0.056
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.119)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Unemployed over Workforce
</td>
<td>
-0.612
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.549)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Retired over Registered voters
</td>
<td>
-0.856
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.785)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Registered voters over region surface
</td>
<td>
-0.030<sup>**</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.012)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
0.611<sup>*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.336)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
17
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.368
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.157
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error
</td>
<td>
0.055 (df = 12)
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic
</td>
<td>
1.744 (df = 4; 12)
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>

Results by department
=====================

``` r
require(dplyr)

department_grouped <- 
  results_fr_20170423_wt_demog %>%
  group_by(code_department) %>%
  summarize(`LE PEN %` = sum(`LE PEN`) / sum(Exprimés),
            immigrants = sum(immigrants),
            chomeurs = sum(chomeurs),
            actifs = sum(actifs),
            retraites_img2a = sum(retraites_img2a),
            surf_ha = sum(surf_ha),
            Inscrits = sum(Inscrits))

mod_dep1 <- 
  lm("I(`LE PEN %`) ~ 
    I(immigrants/Inscrits) + 
    I(chomeurs/(chomeurs+actifs)) + 
    I(retraites_img2a/Inscrits) + 
    I(Inscrits/surf_ha)", 
     data = department_grouped)
```

<table style="text-align:center">
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
First round - Regional level
</td>
</tr>
<tr>
<td>
</td>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Le Pen votes over Valid votes
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Immigrants over Registered voters
</td>
<td>
-0.056
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.119)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Unemployed over Workforce
</td>
<td>
-0.612
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.549)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Retired over Registered voters
</td>
<td>
-0.856
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.785)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Registered voters over region surface
</td>
<td>
-0.030<sup>**</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.012)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
0.611<sup>*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.336)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
17
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.368
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.157
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error
</td>
<td>
0.055 (df = 12)
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic
</td>
<td>
1.744 (df = 4; 12)
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>

Results by commune
==================

``` r
load('~/public_git/french_pres_election_17/results_fr_20170423_wt_demog.RData')

mod1 <- 
  lm("I(`LE PEN %`) ~ 
    I(immigrants/Inscrits) + 
    # I(immigrants_50km/inscrits_50km) + 
    I(chomeurs/(chomeurs+actifs)) + 
    # I(chomeurs_50km/(chomeurs_50km+actifs_50km)) +  
    I(retraites_img2a/Inscrits) + 
    # I(retraites_img2a_50km/inscrits_50km) + 
    I(Inscrits/(surf_ha /0.01))", 
     data = results_fr_20170423_wt_demog)

mod2 <- 
  lm("I(`LE PEN %`) ~ 
    I(immigrants/Inscrits) + 
    # I(immigrants_50km/inscrits_50km) + 
    I(chomeurs/(chomeurs+actifs)) + 
    # I(chomeurs_50km/(chomeurs_50km+actifs_50km)) +  
    I(retraites_img2a/Inscrits) + 
    # I(retraites_img2a_50km/inscrits_50km) + 
    I(Inscrits/(surf_ha /0.01)) + 
    I(revenue_median/1000)", 
     data = results_fr_20170423_wt_demog)
```

<table style="text-align:center">
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="2">
First round
</td>
</tr>
<tr>
<td>
</td>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="2">
Le Pen votes over Valid votes
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
</tr>
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Immigrants over Registered voters
</td>
<td>
-0.162<sup>***</sup>
</td>
<td>
-0.168<sup>***</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.006)
</td>
<td>
(0.006)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Unemployed over Workforce
</td>
<td>
0.355<sup>***</sup>
</td>
<td>
0.443<sup>***</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.010)
</td>
<td>
(0.012)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Retired over Registered voters
</td>
<td>
-0.171<sup>***</sup>
</td>
<td>
-0.247<sup>***</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.005)
</td>
<td>
(0.005)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Registered voters over commune surface (km2)
</td>
<td>
-0.025<sup>***</sup>
</td>
<td>
-0.023<sup>***</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.002)
</td>
<td>
(0.002)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Median revenue per person (thousands €)
</td>
<td>
</td>
<td>
-0.008<sup>***</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.0002)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
0.289<sup>***</sup>
</td>
<td>
0.403<sup>***</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.002)
</td>
<td>
(0.004)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
35,434
</td>
<td>
30,836
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.074
</td>
<td>
0.148
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.074
</td>
<td>
0.148
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error
</td>
<td>
0.088 (df = 35429)
</td>
<td>
0.079 (df = 30830)
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic
</td>
<td>
703.865<sup>***</sup> (df = 4; 35429)
</td>
<td>
1,073.569<sup>***</sup> (df = 5; 30830)
</td>
</tr>
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="2" style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>
The number of immigrants, unemployed and retired are calculated at the commune level based on the 2013 census.
