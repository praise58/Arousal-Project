#!/usr/bin/bash

#SBATCH --partition=cglab
#SBATCH --time=00:55:00
#SBATCH -n 4
#SBATCH --mem=50G
#SBATCH --nodes=1
#SBATCH --job-name=NBS
#SBATCH --account=cgratton-ic
#SBATCH --output=/projects/illinois/las/psych/cgratton/networks-pm//%x_%j.out

module load matlab

THRES=$1
TEST=$2

main_path="/projects/illinois/las/psych/cgratton/networks-pm/arousal"

SCRIPT_DIR="${main_path}/arousal/nbs/nbs_tests"
LOG_DIR="${main_path}/arousal/nbs/nbs_tests/logs"
mkdir -p ${LOG_DIR}

# Print debug message
echo "Running NBS for threshold: ${THRES}"

# Run MATLAB script with subject ID and log output
matlab -nodisplay -nosplash -r "addpath(genpath('${SCRIPT_DIR}')); my_NBS_test('${THRES}','${TEST}'); exit;" > ${LOG_DIR}/NBS_${THRES}_${TEST}.log 2>&1

# Print completion message
echo "NBS completed for threshold ${THRES}. Log saved to ${LOG_DIR}/NBS_${THRES}.log"#!/usr/bin/bash