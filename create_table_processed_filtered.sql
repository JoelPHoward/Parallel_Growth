CREATE TABLE filtered(
	Experiment_ID varchar(100),
	Date date,
	Time double precision,
	Temp double precision,
	SPL varchar(6),
	SPLC varchar(7),
	Strain varchar(50),
	Substrate varchar(50),
	Media varchar(50),
	Replicate integer,
	Plate varchar(10),
	Well varchar(3),
	Mean_OD_Blanked double precision,
	SD_OD_Blanked double precision
);