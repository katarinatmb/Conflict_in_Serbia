# Protest Dynamics in Serbia Following the Train Station Roof Collapse
[View the project website](#view-the-project-website). https://katarinatmb.github.io/Conflict_in_Serbia/

## Table of Contents
- [Project Overview](#project-overview)
- [Data Source](#data-source)
- [Style Choices](#style-choices)
- [Limitations and Assumptions](#limitations-and-assumptions).
- [References](#references). 

## Project Overview
Protest Dynamics in Serbia Following the Train Station Roof Collapse investigates the surge and evolution of civic engagement in Serbia after the tragic November 11, 2024 collapse of the Novi Sad train station roof, which claimed 15 lives. Initially driven by demands for accountability, protests spread rapidly—led by student groups and civil society—to call for broader political reform. This analysis focuses on identifying changes in protest activity by group type, spatial distribution, and timing, aiming to provide a data-based understanding of how this crisis reshaped public mobilization across Serbia.

## Data Source
The project is built upon weekly-updated event-level data from the Armed Conflict Location & Event Data Project (ACLED), covering January 2018 through early April 2025. ACLED provides disaggregated records of political violence and protest events—detailing dates, locations, actors, and event types. Two primary datasets were used: serb_exploded, which lists each protest event by location, date, and organizing group, and pol_viol_cmy, a country-level monthly summary of political violence in Serbia. Spatial analyses are supported by geodata shape files from the {geodata} package.

## Style Choices 
Given multiple categorical variables, visual choices were deliberate: muted color hues for clarity, green shades for peaceful protest types, red gradations for violent events, and off-white backgrounds with black borders for clean presentation. For the spatial map of Serbia, I set the national flag as a backdrop and highlighted the top ten cities by post-collapse protest frequency.

## Limitations and Assumptions 
This analysis reflects the situation only up to April 2025, and the political unrest in Serbia remains ongoing, meaning that protest patterns may shift beyond the current timeframe. Moreover, ACLED data likely underestimates actual events, particularly in rural or media-suppressed areas. It relies on automated scraping, manual validation, and media reporting, all of which can miss smaller or politically sensitive protests. Media bias, editorial choices, communication access, and political censorship all contribute to inconsistent event capture across regions . Lastly, geolocation precision varies, especially when events are assigned to town centers due to vague reports. Collectively, these factors suggest that the dataset likely presents a conservative snapshot of Serbia’s protest activities—one that is both incomplete and still evolving.

## References
1. ACLED Data Export Tool – Official political violence & protest dataset used for analysis: acleddata.com/data/#download

2. Protests Leave Serbia’s Politics Finely Balanced – GMFUS summary of evolving civic unrest in Serbia: gmfus.org/news/protests-leave-serbias-politics-finely-balanced

3. Tensions Mount Ahead of a Pro‑government Rally in Serbia to Counter Massive Student Protests – ABC News reporting on April 2025 events: https://abcnews.go.com/International/wireStory/tensions-mount-ahead-pro-government-rally-serbia-counter-120741785

4. Serbia Train Station Roof Collapse, Arrests – AP News on arrests linked to the Novi Sad disaster: apnews.com/article/serbia-train-station-roof-collapse-arrests-75ce32518db2b255229655bce39cb4f8

5. Serbia Students Strasbourg EU Protest – AP News covering the student cycling protest to Strasbourg: apnews.com/article/serbia-students-strasbourg-eu-protest-398094de8251ffb3579231e805fc28a7

6. Serbia Protest: Students, Roof Collapse, Prosecutor, Vučić – AP News on student demonstrations and state response: apnews.com/article/serbia-protest-students-roof-collapse-prosecutor-vucic-b2aa7a208e53006dab4e086588b7355d
