This repository contains the code used in the experiments for the following paper:

R. Morel, A. Cropper and L. Ong. Typed meta-interpretive learning of logic programs. JELIA 2019. To appear.

# data description

For each experiment, the experimental data are in the folders marked by the date they were run on. The folders suffixed '-results' contain the outputs of the system.
The csv files prefixed by 'data-typed' contain the data collection for the typed experiments,
likewise for the 'data-untyped' files for the untyped runs of the experiments.
The '.table' files contain means and standard deviations. These files were generated from the csv files by the 'raw-data-to-plot-data.py' script.

# Rerunning the experiments

Each subfolder in the experiments directory contains a 'run.bash' script which run the respective experiment. The ASP experiments are in the sub-folders named 'asp'.

Contact rolf.morel@cs.ox.ac.uk with any queries.
