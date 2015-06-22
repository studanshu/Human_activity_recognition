import csv
import pandas as pd

sitting_data = pd.read_table('data/sit_data.csv', sep=',', header=0 )[:,['time','value1','value2','value3']]
sleeping_data = pd.read_table('data/sleep_data.csv', sep=',', header=0 )
walking_data = pd.read_table('data/walk_data.csv', sep=',', header=0 )

