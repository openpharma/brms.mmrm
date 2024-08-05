#$ -N load-brms-mmrm-sbc
#$ -j y
#$ -o main-log.txt
#$ -cwd
#$ -V
R CMD BATCH run.R
