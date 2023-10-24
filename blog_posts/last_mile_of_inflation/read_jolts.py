#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 10 08:55:24 2023

@author: mkonczal
"""
import pandas as pd
import requests
import io

# Set user agent for requests
user_agent = {"User-Agent": "rortybomb@gmail.com"}

# URLs for the data
urls = {
    'jolts_data': "https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems",
    'series': "https://download.bls.gov/pub/time.series/jt/jt.series",
    'Jindustry': "https://download.bls.gov/pub/time.series/jt/jt.industry",
    'Jsizecode': "https://download.bls.gov/pub/time.series/jt/jt.sizeclass",
    'data_element': "https://download.bls.gov/pub/time.series/jt/jt.dataelement",
    'Jstate': "https://download.bls.gov/pub/time.series/jt/jt.state"
}

# Function to fetch and load data
def fetch_data(url, user_agent):
    response = requests.get(url, headers=user_agent)
    if response.status_code == 200:
        return pd.read_csv(io.StringIO(response.content.decode('utf-8')), sep="\t")
    else:
        print(f"Failed to fetch data from {url}: {response.status_code}")
        return pd.DataFrame()

# Fetch and load all data
data = {}
for key, url in urls.items():
    print(f"Fetching data for {key} from {url}")  # Debugging print statement
    try:
        data[key] = fetch_data(url, user_agent)
    except Exception as e:
        print(f"An error occurred fetching data for {key}: {str(e)}")


# Data cleanup and transformations

# jolts_data
data['jolts_data'].columns = data['jolts_data'].columns.str.lower().str.replace(' ', '')
data['jolts_data']['value'] = pd.to_numeric(data['jolts_data']['value'], errors='coerce')
data['jolts_data']['series_id'] = data['jolts_data']['series_id'].str.strip()
#data['jolts_data']['date'] = pd.to_datetime(data['jolts_data']['year'].astype(str) + 
#                                             data['jolts_data']['period'].str[1:], 
#                                             format="%Y%m")

# Other data
for key in ['series', 'Jindustry', 'Jsizecode', 'data_element', 'Jstate']:
    data[key].columns = data[key].columns.str.lower().str.replace(' ', '')
    if 'series_id' in data[key].columns:
        data[key]['series_id'] = data[key]['series_id'].str.strip()

# Merge all data together
jolts = (
    data['jolts_data']
    .merge(data['series'], on='series_id', how='inner')
    .merge(data['data_element'], on='dataelement_code', how='inner', suffixes=('.series', '.data_element'))
    .merge(data['Jindustry'], on='industry_code', how='inner', suffixes=('.x', '.industry_code'))
    .merge(data['Jsizecode'], on='sizeclass_code', how='inner')
    .merge(data['Jstate'], on='state_code', how='inner')
)

# Clean up
del data
