import rapidjson
import pandas as pd
import re
import os
import sys
import psycopg2

def copy_from_stringio(conn, df, table):
	# save dataframe to an in memory buffer
	buffer = StringIO()
	df.to_csv(buffer, index = False, header=False)
	buffer.seek(0)
	cur = conn.cursor()
	try:
		cur.copy_from(buffer, table, sep=",")
		conn.commit()
	except (Exception, psycopg2.DatabaseError) as error:
		print("Error: %s" % error)
		conn.rollback()
		cur.close()
		return 1

def update_postgres(data_dir):
	PSQL_CREDENTIALS = rapidjson.load(open('/Users/joelhoward/Desktop/uToronto/repos/NIMBUS_96W_Column_Randomizer/PSQL_CREDENTIALS.json'))
	conn = psycopg2.connect("dbname=" + PSQL_CREDENTIALS['dbname'] + " user=" + PSQL_CREDENTIALS['user'] +  " password=" + PSQL_CREDENTIALS['password'])
	cur = conn.cursor()
	table = os.path.basename(data_dir)
	cur.execute("SELECT Experiment_ID FROM " + table + ";")
	ids = list(set([x for x in cur.fetchall()]))
	files = glob.glob(data_dir + '/*')
	exp_id = re.search(pattern = r"runID_(.*)_plateID", string = files[0]).group(1)
	if exp_id in ids:
		sys.exit("Experiment ID " + exp_id + " is already in the database.")
	
	#read in files using pandas
	#add them to the database

	for file in unfiltered:
		df = pd.read_csv(file)
	copy_from_stringio(conn, df, 'unfiltered')
	
	conn.commit()
	cur.close()
	conn.close()
	