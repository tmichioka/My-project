Lab6
================
Toko Michioka
2024-10-10

\#changes to only this file, not the original file will be reflected in
the github website

``` r
library(haven)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(performance)
library(sjPlot)
library(bruceR)
```

    ## 
    ## bruceR (v2024.6)
    ## Broadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## ✔ data.table ✔ emmeans
    ## ✔ dplyr      ✔ lmerTest
    ## ✔ tidyr      ✔ effectsize
    ## ✔ stringr    ✔ performance
    ## ✔ ggplot2    ✔ interactions
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## For full functionality, please install all dependencies:
    ## install.packages("bruceR", dep=TRUE)
    ## 
    ## Online documentation:
    ## https://psychbruce.github.io/bruceR
    ## 
    ## To use this package in publications, please cite:
    ## Bao, H.-W.-S. (2024). bruceR: Broadly useful convenient and efficient R functions (Version 2024.6) [Computer software]. https://CRAN.R-project.org/package=bruceR

    ## 
    ## These packages are dependencies of `bruceR` but not installed:
    ## - pacman, openxlsx, ggtext, lmtest, vars, phia, MuMIn, GGally
    ## 
    ## ***** Install all dependencies *****
    ## install.packages("bruceR", dep=TRUE)

``` r
data = read.csv("/Users/toko/Desktop/UofT/2024/Fall/PSY329/My project/Self-disclosure and Health Study 1_cleaned.csv")

# Re-coding: Re-code gender identity, sex at birth, race/ethnicity, partner’s gender identity and combine them into one column
combined_data <- data %>%
  mutate(Gender = case_when(
    G_Male == 1 ~ "Male",
    G_Female == 1 ~ "Female",
    G_FTM == 1 ~ "FTM",
    G_MTF == 1 ~ "MTF",
    G_Genderqueer == 1 ~ "Genderqueer",
    G_other == 1 ~ "Other",
    G_decline == 1 ~ "Decline")) %>%
  
  mutate(Partner_Gender = case_when(
    Partner_G_Male == 1 ~ "Male",
    Partner_G_Female == 1 ~ "Female",
    Partner_G_FTM == 1 ~ "FTM",
    Partner_G_MTF == 1 ~ "MTF",
    Partner_G_Genderqueer == 1 ~ "Genderqueer",
    Partner_G_Other == 1 ~ "Other",
    Partner_G_decline == 1 ~ "Decline")) %>%
  
  mutate(Sex = recode(Sex, "1" = "Male", "2" = "Female")) %>%
  
  mutate(Race = case_when(
    R_White == 1 ~ "White",
    R_Black == 1 ~ "Black",
    R_Hispanic == 1 ~ "Hispanic",
    R_NativeAmerican == 1 ~ "NativeAmerican",
    R_Asian == 1 ~ "Asian",
    R_Pacific == 1 ~ "Pacific",
    R_Arab == 1 ~ "Arab",
    R_Other == 1 ~ "Other"))

new_data <- combined_data %>%
  select(LOVE1, LOVE2, LOVE3, LOVE4, LOVE5, LOVE6, LOVE7, LOVE8, Satisfaction_global1, Satisfaction_global2, Satisfaction_global3, Satisfaction_global4, Satisfaction_global5, Time_together, SelfDisclosure1, SelfDisclosure2, SelfDisclosure3, SelfDisclosure4, SelfDisclosure5, SelfDisclosure6, SelfDisclosure7, SelfDisclosure8, SelfDisclosure9, SelfDisclosure10, SelfDisclosure11, SelfDisclosure12, SelfDisclosure13, SelfDisclosure14, SelfDisclosure15, SelfDisclosure16, SelfDisclosure17, SelfDisclosure18, SelfDisclosure19, SelfDisclosure20, SelfDisclosure21, SelfDisclosure22, SelfDisclosure23, SelfDisclosure24, SelfDisclosure25, SelfDisclosure26, SelfDisclosure27, SelfDisclosure28, SelfDisclosure29, SelfDisclosure30, SelfDisclosure31, SelfDisclosure32, SelfDisclosure33, SelfDisclosure34, SelfDisclosure35, SelfDisclosure36, SelfDisclosure37, SelfDisclosure38, SelfDisclosure39, SelfDisclosure40, Gender, Sex, Partner_Gender, Race, Time_together)

#make composites (Romantic Love Scale (RLS), Investment Model Scale (IMS), Emotional Self-Disclosure Scale (ESS))
composite_data <- new_data %>%
  mutate(RLS = rowMeans(cbind(LOVE1, LOVE2, LOVE3, LOVE4, LOVE5, LOVE6, LOVE7, LOVE8)),
         IMS = rowMeans(cbind(Satisfaction_global1, Satisfaction_global2, Satisfaction_global3, Satisfaction_global4, Satisfaction_global5)),
         ESS = rowMeans(cbind(SelfDisclosure1, SelfDisclosure2, SelfDisclosure3, SelfDisclosure4, SelfDisclosure5, SelfDisclosure6, SelfDisclosure7, SelfDisclosure8, SelfDisclosure9, SelfDisclosure10, SelfDisclosure11, SelfDisclosure12, SelfDisclosure13, SelfDisclosure14, SelfDisclosure15, SelfDisclosure16, SelfDisclosure17, SelfDisclosure18, SelfDisclosure19, SelfDisclosure20, SelfDisclosure21, SelfDisclosure22, SelfDisclosure23, SelfDisclosure24, SelfDisclosure25, SelfDisclosure26, SelfDisclosure27, SelfDisclosure28, SelfDisclosure29, SelfDisclosure30, SelfDisclosure31, SelfDisclosure32, SelfDisclosure33, SelfDisclosure34, SelfDisclosure35, SelfDisclosure36, SelfDisclosure37, SelfDisclosure38, SelfDisclosure39, SelfDisclosure40)))
  
#removing n/a
composite_clean_data <- na.omit(composite_data)
```

``` r
mo<- lm(data = composite_clean_data, ESS ~ RLS + IMS )

check_model(mo)
```

![](My-project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
model_summary(mo)
```

    ## 
    ## Model Summary
    ## 
    ## ────────────────────────
    ##              (1) ESS    
    ## ────────────────────────
    ## (Intercept)    1.847 ***
    ##               (0.164)   
    ## RLS            0.253 ***
    ##               (0.032)   
    ## IMS            0.043    
    ##               (0.027)   
    ## ────────────────────────
    ## R^2            0.304    
    ## Adj. R^2       0.300    
    ## Num. obs.    355        
    ## ────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##  Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##   RLS 1.91 [1.66, 2.25]         1.38      0.52     [0.44, 0.60]
    ##   IMS 1.91 [1.66, 2.25]         1.38      0.52     [0.44, 0.60]

``` r
plot_model(mo,  type ="est",  show.values = TRUE, vline.color = "#1B191999", line.size = 1.5, dot.size = 2.5, colors = "blue") + theme_bruce()
```

![](My-project_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
mo2<-lm(data = composite_clean_data, ESS ~ RLS + IMS + Race + Gender)

model_summary(mo2)
```

    ## 
    ## Model Summary
    ## 
    ## ──────────────────────────────
    ##                    (1) ESS    
    ## ──────────────────────────────
    ## (Intercept)          0.503    
    ##                     (0.889)   
    ## RLS                  0.256 ***
    ##                     (0.032)   
    ## IMS                  0.039    
    ##                     (0.026)   
    ## RaceBlack           -0.057    
    ##                     (0.176)   
    ## RaceHispanic         0.203    
    ##                     (0.231)   
    ## RaceOther            0.480    
    ##                     (0.516)   
    ## RaceWhite            0.052    
    ##                     (0.130)   
    ## GenderFemale         1.408    
    ##                     (0.870)   
    ## GenderFTM            1.473    
    ##                     (1.121)   
    ## GenderGenderqueer    2.010 *  
    ##                     (0.964)   
    ## GenderMale           1.053    
    ##                     (0.874)   
    ## ──────────────────────────────
    ## R^2                  0.353    
    ## Adj. R^2             0.335    
    ## Num. obs.          355        
    ## ──────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##    Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##     RLS 1.94 [1.69, 2.27]         1.39      0.52     [0.44, 0.59]
    ##     IMS 1.94 [1.70, 2.28]         1.39      0.51     [0.44, 0.59]
    ##    Race 1.55 [1.37, 1.80]         1.24      0.65     [0.56, 0.73]
    ##  Gender 1.55 [1.37, 1.80]         1.24      0.65     [0.56, 0.73]

``` r
tab_model(mo2)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
ESS
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-1.25 – 2.25
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.572
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
RLS
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.19 – 0.32
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IMS
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.01 – 0.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.134
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Race \[Black\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.40 – 0.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.747
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Race \[Hispanic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.25 – 0.66
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.381
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Race \[Other\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.53 – 1.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.353
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Race \[White\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.20 – 0.31
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.689
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender \[Female\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.41
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.30 – 3.12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.107
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender \[FTM\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.47
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.73 – 3.68
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.190
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender \[Genderqueer\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.11 – 3.91
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.038</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender \[Male\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.67 – 2.77
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.230
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
355
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.353 / 0.335
</td>
</tr>
</table>

``` r
plot_model(mo2,  type ="est",  show.values = TRUE, vline.color = "#1B191999", line.size = 1.5, dot.size = 2.5, colors = "blue") + theme_bruce()
```

![](My-project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
PROCESS(composite_clean_data, y = "ESS", x = "RLS", mods = c("Gender"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : ESS
    ## -  Predictor (X) : RLS
    ## -  Mediators (M) : -
    ## - Moderators (W) : Gender
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    ESS ~ RLS*Gender
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ───────────────────────────────────────────────
    ##                        (1) ESS      (2) ESS    
    ## ───────────────────────────────────────────────
    ## (Intercept)              3.820 ***    2.825 ***
    ##                         (0.039)      (0.710)   
    ## RLS                      0.289 ***    0.278 ***
    ##                         (0.024)      (0.043)   
    ## GenderFemale                          1.105    
    ##                                      (0.712)   
    ## GenderFTM                             1.135    
    ##                                      (1.007)   
    ## GenderGenderqueer                     2.120 *  
    ##                                      (0.873)   
    ## GenderMale                            0.754    
    ##                                      (0.714)   
    ## RLS:GenderFemale                      0.019    
    ##                                      (0.051)   
    ## RLS:GenderGenderqueer                -0.887    
    ##                                      (0.705)   
    ## ───────────────────────────────────────────────
    ## R^2                      0.299        0.348    
    ## Adj. R^2                 0.297        0.335    
    ## Num. obs.              355          355        
    ## ───────────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 355
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "ESS" (Y)
    ## ────────────────────────────────────
    ##                  F df1 df2     p    
    ## ────────────────────────────────────
    ## RLS * Gender  0.88   2 347  .414    
    ## ────────────────────────────────────
    ## 
    ## Simple Slopes: "RLS" (X) ==> "ESS" (Y)
    ## ────────────────────────────────────────────────────────────
    ##  "Gender"    Effect    S.E.      t     p            [95% CI]
    ## ────────────────────────────────────────────────────────────
    ##  Decline      0.278 (0.043)  6.484 <.001 *** [ 0.194, 0.363]
    ##  Female       0.297 (0.027) 10.903 <.001 *** [ 0.243, 0.351]
    ##  FTM          0.278 (0.043)  6.484 <.001 *** [ 0.194, 0.363]
    ##  Genderqueer -0.608 (0.703) -0.865  .388     [-1.992, 0.775]
    ##  Male         0.278 (0.043)  6.484 <.001 *** [ 0.194, 0.363]
    ## ────────────────────────────────────────────────────────────

``` r
PROCESS(composite_clean_data, y = "ESS", x = "RLS", mods = c("Partner_Gender"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : ESS
    ## -  Predictor (X) : RLS
    ## -  Mediators (M) : -
    ## - Moderators (W) : Partner_Gender
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    ESS ~ RLS*Partner_Gender
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ───────────────────────────────────────────────────
    ##                            (1) ESS      (2) ESS    
    ## ───────────────────────────────────────────────────
    ## (Intercept)                  3.820 ***    3.567 ***
    ##                             (0.039)      (0.064)   
    ## RLS                          0.289 ***    0.314 ***
    ##                             (0.024)      (0.042)   
    ## Partner_GenderGenderqueer                -0.010    
    ##                                          (0.713)   
    ## Partner_GenderMale                        0.387 ***
    ##                                          (0.079)   
    ## RLS:Partner_GenderMale                   -0.029    
    ##                                          (0.050)   
    ## ───────────────────────────────────────────────────
    ## R^2                          0.299        0.345    
    ## Adj. R^2                     0.297        0.337    
    ## Num. obs.                  355          355        
    ## ───────────────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 355
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "ESS" (Y)
    ## ────────────────────────────────────────────
    ##                          F df1 df2     p    
    ## ────────────────────────────────────────────
    ## RLS * Partner_Gender  0.33   1 350  .566    
    ## ────────────────────────────────────────────
    ## 
    ## Simple Slopes: "RLS" (X) ==> "ESS" (Y)
    ## ────────────────────────────────────────────────────────────────
    ##  "Partner_Gender" Effect    S.E.      t     p           [95% CI]
    ## ────────────────────────────────────────────────────────────────
    ##  Female            0.314 (0.042)  7.447 <.001 *** [0.231, 0.397]
    ##  Genderqueer       0.285 (0.027) 10.428 <.001 *** [0.231, 0.339]
    ##  Male              0.285 (0.027) 10.428 <.001 *** [0.231, 0.339]
    ## ────────────────────────────────────────────────────────────────

``` r
PROCESS(composite_clean_data, y = "ESS", x = "RLS", mods = c("Race"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : ESS
    ## -  Predictor (X) : RLS
    ## -  Mediators (M) : -
    ## - Moderators (W) : Race
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    ESS ~ RLS*Race
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ──────────────────────────────────────────
    ##                   (1) ESS      (2) ESS    
    ## ──────────────────────────────────────────
    ## (Intercept)         3.820 ***    3.716 ***
    ##                    (0.039)      (0.130)   
    ## RLS                 0.289 ***    0.244 ** 
    ##                    (0.024)      (0.085)   
    ## RaceBlack                        0.061    
    ##                                 (0.186)   
    ## RaceHispanic                     0.225    
    ##                                 (0.240)   
    ## RaceOther                        2.455    
    ##                                 (1.642)   
    ## RaceWhite                        0.119    
    ##                                 (0.138)   
    ## RLS:RaceBlack                    0.166    
    ##                                 (0.113)   
    ## RLS:RaceHispanic                -0.051    
    ##                                 (0.139)   
    ## RLS:RaceOther                    1.911    
    ##                                 (1.283)   
    ## RLS:RaceWhite                    0.035    
    ##                                 (0.090)   
    ## ──────────────────────────────────────────
    ## R^2                 0.299        0.314    
    ## Adj. R^2            0.297        0.296    
    ## Num. obs.         355          355        
    ## ──────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 355
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "ESS" (Y)
    ## ──────────────────────────────────
    ##                F df1 df2     p    
    ## ──────────────────────────────────
    ## RLS * Race  1.48   4 345  .208    
    ## ──────────────────────────────────
    ## 
    ## Simple Slopes: "RLS" (X) ==> "ESS" (Y)
    ## ─────────────────────────────────────────────────────────
    ##  "Race"   Effect    S.E.      t     p            [95% CI]
    ## ─────────────────────────────────────────────────────────
    ##  Asian     0.244 (0.085)  2.860  .004 **  [ 0.076, 0.412]
    ##  Black     0.410 (0.075)  5.489 <.001 *** [ 0.263, 0.557]
    ##  Hispanic  0.193 (0.110)  1.757  .080 .   [-0.023, 0.408]
    ##  Other     2.155 (1.280)  1.683  .093 .   [-0.363, 4.673]
    ##  White     0.279 (0.027) 10.318 <.001 *** [ 0.226, 0.333]
    ## ─────────────────────────────────────────────────────────

``` r
PROCESS(composite_clean_data, y = "ESS", x = "RLS", mods = c("Sex"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : ESS
    ## -  Predictor (X) : RLS
    ## -  Mediators (M) : -
    ## - Moderators (W) : Sex
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    ESS ~ RLS*Sex
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ─────────────────────────────────────
    ##              (1) ESS      (2) ESS    
    ## ─────────────────────────────────────
    ## (Intercept)    3.820 ***    3.936 ***
    ##               (0.039)      (0.046)   
    ## RLS            0.289 ***    0.297 ***
    ##               (0.024)      (0.027)   
    ## SexMale                    -0.357 ***
    ##                            (0.081)   
    ## RLS:SexMale                -0.014    
    ##                            (0.051)   
    ## ─────────────────────────────────────
    ## R^2            0.299        0.337    
    ## Adj. R^2       0.297        0.331    
    ## Num. obs.    355          355        
    ## ─────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 355
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "ESS" (Y)
    ## ─────────────────────────────────
    ##               F df1 df2     p    
    ## ─────────────────────────────────
    ## RLS * Sex  0.07   1 351  .785    
    ## ─────────────────────────────────
    ## 
    ## Simple Slopes: "RLS" (X) ==> "ESS" (Y)
    ## ──────────────────────────────────────────────────────
    ##  "Sex"  Effect    S.E.      t     p           [95% CI]
    ## ──────────────────────────────────────────────────────
    ##  Female  0.297 (0.027) 10.893 <.001 *** [0.244, 0.351]
    ##  Male    0.283 (0.043)  6.627 <.001 *** [0.199, 0.367]
    ## ──────────────────────────────────────────────────────

``` r
ggplot(composite_clean_data, aes(x = RLS, y = ESS)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~ Sex) + 
  theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](My-project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(composite_clean_data, aes(x = RLS, y = ESS)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~ Partner_Gender) + 
  theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](My-project_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
ggplot(composite_clean_data, aes(x = RLS, y = ESS)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~ Gender) + 
  theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](My-project_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ggplot(composite_clean_data, aes(x = RLS, y = ESS)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~ Race) + 
  theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](My-project_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->
