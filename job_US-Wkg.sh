#!/bin/bash
#SBATCH --cpus-per-task 3
#SBATCH --mem=10000
#SBATCH --job-name=US-Wkg
#SBATCH --output=/srv/ccrc/data56/z5293113/NEE_project/job_US-Wkg.out	# this is the file your output and errors go to
#SBATCH --time=100:00:00
#SBATCH --workdir=/srv/ccrc/data56/z5293113/NEE_project/# your work directory
##############SBATCH --qos=debug       	                # take this line out when you are done testing

# load a module, for example
module load R/3.3.3

# run your application, precede the application command with srun
srun date
srun R --vanilla < ./workflow/NEE_workflow_US-Wkg.r > /srv/ccrc/data56/z5293113/NEE_project/NEE_US-Wkg.out
srun date
