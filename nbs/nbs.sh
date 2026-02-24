#!/usr/bin/bash

#SBATCH --partition=cglab
#SBATCH --time=00:55:00
#SBATCH -n 4
#SBATCH --mem=50G
#SBATCH --nodes=1
#SBATCH --job-name=NBS
#SBATCH --account=cgratton-ic
# Outputs ----------------------------------
#SBATCH --output=/projects/illinois/las/psych/cgratton/networks-pm//%x_%j.out
# ------------------------------------------
# Load MATLAB module

module load matlab

THRES = $1

main_path="/projects/illinois/las/psych/cgratton/networks-pm"

SCRIPT_DIR="$\{main_path\}/arousal/"
LOG_DIR="$\{main_path\}/arousal/logs"
mkdir -p $\{LOG_DIR\}

# Print debug message
echo "Running NBS for threshold: ${THRES}"

# Run MATLAB script with subject ID and log output
matlab -nodisplay -nosplash -r "addpath(genpath('${SCRIPT_DIR}')); my_NBS_test('${THRES}'); exit;" > ${LOG_DIR}/NBS_${THRES}.log 2>&1

# Print completion message
echo "NBS completed for threshold ${THRES}. Log saved to ${LOG_DIR}/NBS_${THRES}.log"