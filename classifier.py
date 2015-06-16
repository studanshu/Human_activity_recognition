import csv
import pandas as pd

sitting_data = pd.read_table('acc_sensor_sitting.csv', sep=',', header=0 )
sleeping_data = pd.read_table('acc_sensor_sitting.csv', sep=',', header=0 )
walking_data = pd.read_table('acc_sensor_sitting.csv', sep=',', header=0 )