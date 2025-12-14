#!/bin/bash

# PBS Settings
#PBS -S /bin/bash
#PBS -l select=1:ncpus=24:mem=64gb
#PBS -l walltime=240:00:00
#PBS -N spatialCMC-covid_data:run
#PBS -o log/spatialCMC-covid_data:run.out
#PBS -e log/spatialCMC-covid_data:run.err

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
in-apptainer cook exec -j 24 covid_data:run &> log/covid_data:run.log
