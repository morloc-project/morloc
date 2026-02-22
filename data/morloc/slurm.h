#ifndef __MORLOC_SLURM_H__
#define __MORLOC_SLURM_H__

#define MAX_SLURM_COMMAND_LENGTH 1024

// For each field, -1 indicates undefined
typedef struct resources_s {
  int memory; // in Gb
  int time; // walltime in seconds
  int cpus;
  int gpus;
} resources_t;

#endif // __MORLOC_SLURM_H__
