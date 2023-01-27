import pandas as pd
import numpy as np

acs_1 = pd.read_csv("../input/acs_foreign_born/2019_1_year.csv", skipinitialspace=True)
acs_5 = pd.read_csv("../input/acs_foreign_born/2019_5_year.csv", skipinitialspace=True)

acs = pd.merge(acs_1, acs_5, on = "Label (Grouping)")
acs = acs.drop(columns = ["United States!!Margin of Error_x", "United States!!Margin of Error_y"])
acs = acs.rename(columns = {"Label (Grouping)": "country", "United States!!Estimate_x": "1year", "United States!!Estimate_y": "5year"})

acs['country'] = acs['country'].str.split().str.join('')
print(acs.head())
acs.to_csv("../temporary/acs_foreignborn_2019_cleaned.csv")