#!/bin/bash

# PBS Settings
#PBS -S /bin/bash
#PBS -l select=1:ncpus=50:mem=128gb
#PBS -l walltime=240:00:00
#PBS -N spatialCMC-covid_data-pois_reg_run
#PBS -o log/spatialCMC-covid_data-pois_reg_run.out
#PBS -e log/spatialCMC-covid_data-pois_reg_run.err

# Set current working directory
cd ${PBS_O_WORKDIR}

# Create log directory
mkdir -p log

# Create alias for apptainer execution
export APPTAINER=/opt/mox/apptainer/bin/apptainer
export SPATIALCMC=/u/gianella/containers/spatialcmc_latest.sif
in-apptainer () {
	$APPTAINER exec --pwd /workdir --bind `pwd`:/workdir $SPATIALCMC $@
}
export -f in-apptainer

# Execution in containerized environment
in-apptainer cook exec -j 16 covid_data:pois_reg_run &> log/covid_data-pois_reg_run.log
