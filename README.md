# COVID-19_Restaurants-R_Final_Project
Final Team Project conducted with Tuty Chau as a part of the Data Science: R - DAT-5302 course by Lawrence De Geest 

## Files & Task
The project consistet of a broad task but included the self-selection of data (data collection), Variable Selection, Data Analysis & Modelling. 

We choose to analyze investigate the impact of Food Safety Violations in Chicago on COVID-19 cases in communities utilizing publicly available information of the City of Chicago. 
The data portal of the city provides access to every food inspection conducted by the city’s Department of Public Health. Each inspection that is conducted by one of the approximately 30 inspectors is recorded including various identifiers of the business, their risk class, general state (is it out of business etc.), violations found and instructions given to the business based on findings. 

In addition to access to the food inspection data, the city provides dedicated statistics for COVID-19 cases, tests, and deaths by ZIP code. This is of significance as it can be assumed that the data collection, more precisely the location identifiers are correct and align between the two datasets that provide the foundation of the analysis. 

The respective links to each of the datasets and data used are:

* [COVID-19 Data](https://data.cityofchicago.org/Health-Human-Services/COVID-19-Cases-Tests-and-Deaths-by-ZIP-Code/yhhz-zm2v) (Assignment COVID-19 Data: [Apprentice Chef Course Case](https://github.com/maxlembke/COVID-19_Restaurants-R_Final_Project/blob/main/COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv))

* [Food Inspection Data](https://data.cityofchicago.org/Health-Human-Services/Food-Inspections/4ijn-s7e5/data) (Too large to provide here - please download at link) 

We conducted our analysis in late December 2020. Please be aware that both datasets are updated frequently. The date of retrieval of each dataset are 12/07/2020 and 12/04/2020 for COVID-19 and Food Inspection data, respectively.

The submission files are: 

* [Analysis R File](https://github.com/maxlembke/COVID-19_Restaurants-R_Final_Project/blob/main/Team1_Final_Project_R.R)
* [Rmd Markdown Submission](https://github.com/maxlembke/COVID-19_Restaurants-R_Final_Project/blob/main/Team1_Final_Project_v2.1.rmd)
