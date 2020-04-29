#PBS -A mnh5174_a_g_sc_default
#PBS -l walltime=96:00:00
#PBS -l nodes=1:ppn=37
#PBS -j oe
#PBS -M ams939@psu.edu
#PBS -m abe

export G=/gpfs/group/mnh5174/default

module use $G/sw/modules

env
cd $PBS_O_WORKDIR

module load matlab
export matlab_cpus=37

echo "Currently in: $PWD"

matlab -nodisplay -r m2_mfx_modelcomparison_fitIBIs
