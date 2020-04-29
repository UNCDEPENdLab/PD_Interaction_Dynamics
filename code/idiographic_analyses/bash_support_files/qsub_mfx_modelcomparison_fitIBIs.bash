#PBS -A mnh5174_a_g_sc_default
#PBS -l walltime=80:00:00
#PBS -l nodes=1:ppn=18
#PBS -j oe
#PBS -M ams939@psu.edu
#PBS -m abe

export G=/gpfs/group/mnh5174/default

module use $G/sw/modules

env
cd $PBS_O_WORKDIR

module load matlab
export matlab_cpus=18

echo "Currently in: $PWD"

matlab -nodisplay -r mfx_modelcomparison_fitIBIs
