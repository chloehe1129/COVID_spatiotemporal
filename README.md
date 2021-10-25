Delay in the effect of restricting community mobility on the spread of COVID-19 during the first wave in the United States COVID_spatiotemporal

Shan He, Jooyoung Lee, Benjamin Langworthy, Junyi Xin, Peter James, Yang Yang, Molin Wang


## INTRODUCTION

It remains unclear how changes in human mobility shaped the transmission dynamic of COVID-19 during its first wave in the United States. By coupling a Bayesian hierarchical spatiotemporal model with reported case data and Google mobility data at the county level, we found that changes in movement were associated with notable changes in reported COVID-19 incidence rate about 5 to 7 weeks later. Among all movement types, residential stay was the most influential driver of COVID-19 incidence rate, with a 10% increase 7 weeks ago reducing the disease incidence rate by 13% (95% credible interval: 6-20%). A 10% increase in movement from home to workplaces, retail and recreation stores, public transits, grocery, and pharmacies 7 weeks ago was associated with an increase of 5-8% in the COVID-10 incidence rate. In contrast, parks-related movement showed minimal impact. Policymakers should anticipate such a delay when planning intervention strategies restricting human movement.

## STATISTICAL METHODS

In this paper, we use Bayesian hierarchical spatiotemporal models to assess the potential temporal lag in the effect of particular human movement indices and reported incidence of COVID-19. We are also able to investigate whether the lag varies spatially. These models explicitly consider over-dispersion in reported incidence and potential spatial autocorrelations among counties. The models are adjusted for potential risk factors of COVID-19, including temperature, age profiles, percentage of African Americans, percentage of Latinos, percentage below the poverty line, percentage of obesity, percentage with bachelor’s degree or above and population density.  

To quantify the impact of human movement on COVID-19 incidence, we combined a Bayesian spatiotemporal generalized additive mixed model (GAMM) with a distributed lag model (DLM).The GAMM allows the effects of continuous risk factors to have flexible shapes, and the DLM permits lagged effects of human movement at each time point on future incidences. In addition, to account for spatial dependency between adjacent counties and the non-linear time trends, spatial random effects and temporal non-linear effects are included.

## ABOUT THIS REPOSITORY
This repository contains all the codes and data needed to reproduce the study results. Data Processing contains R-codes to clean the data extracted from various sources including U.S Census reports and other github repositories. Code folder has the main R-code file to execute the whole analysis. Data folder contains state-level independent variables, stay-at-home policy onset date, as well as various intermediate outputs from main.R 
