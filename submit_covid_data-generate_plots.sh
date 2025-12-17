#!/bin/bash

# PBS Settings
#PBS -S /bin/bash
#PBS -l select=1:ncpus=24:mem=128gb
#PBS -l walltime=24:00:00
#PBS -N spatialCMC-covid_data-generate_plots
#PBS -o log/spatialCMC-covid_data-generate_plots.out
#PBS -e log/spatialCMC-covid_data-generate_plots.err

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
in-apptainer cook exec generate_plot &> log/covid_data-generate_plots.log
