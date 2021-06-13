#!/bin/bash

#SBATCH -A woeste
#SBATCH --nodes=2

#SBATCH --ntasks-per-node=12
#SBATCH --mem=192000
#SBATCH --time=12-12:00:00
#SBATCH --job-name=map.g.emc

cd ~
module load r
module load gdal

cd $RCAC_SCRATCH

R --no-save < DeriveMACA.EMC.Dec7.Future.R
