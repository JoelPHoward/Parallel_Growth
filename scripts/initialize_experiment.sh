#!/bin/bash

SCRIPT_DIR="$(dirname "$( cd -- "$( dirname -- "${BASH_SOURCE[0]:-$0}"; )" &> /dev/null && pwd 2> /dev/null;)";)"

if [ -d "$1" ]
then
	echo "The directory $1 already exists. Eiher delete this directory or choose a unique path and rerun initialization."
	exit 0
fi

echo "From Parallel Growth repo in $SCRIPT_DIR ..."
echo "Creating new folder for experiment in $1"
mkdir $1
cd $1

echo $SCRIPT_DIR > repo_path

cp $SCRIPT_DIR/design_file.json ./

echo "#!/bin/bash" > randomize_plates.sh
echo "Rscript $SCRIPT_DIR/scripts/randomize_plates.R" >> randomize_plates.sh
chmod +x randomize_plates.sh

echo "#!/bin/bash" > process_export.sh
echo "Rscript $SCRIPT_DIR/scripts/process_export.R" >> process_export.sh
chmod +x process_export.sh

echo "#!/bin/bash" > update_postgres.sh
echo "source $SCRIPT_DIR/renv/python/virtualenvs/renv-python-3.8/bin/activate" >> update_postgres.sh
echo "python $SCRIPT_DIR/scripts/update_postgres.py $1 $2 > postgres_update.log" >> update_postgres.sh
chmod +x update_postgres.sh

mkdir sample_plates
mkdir data
mkdir data/raw_data
mkdir data/processed_data
mkdir data/processed_data/filtered
mkdir data/processed_data/unfiltered
mkdir img
mkdir img/microscope
mkdir img/growth_curves
mkdir img/setup