import os
import sys

from cook import create_task, Task
from cook.contexts import create_group

# Add parent directory to path to import shared functions
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from utils import create_output_path

# Unique name labelling for current subfolder
def name(root_name: str) -> str:
     return f"s1:{root_name}"

# Get directory of the current file
workdir = os.path.dirname(os.path.abspath(__file__))

# Define quantities for simulation study
num_datasets = 1
ids = [f"{i:03d}" for i in range(1, num_datasets+1)]

# Define generate_shapefiles task
generate_shapefiles_action = ["Rscript", os.path.join(workdir,"src/generate_shapefiles.R")] + \
    ["--num-datasets", num_datasets] + \
    ["--dest-dir", "input"]
generate_shapefiles_targets = [os.path.join(workdir,f"input/shapefile{i:03d}.dat") for i in range(1, num_datasets+1)]
create_task(name("generate_shapefiles"), 
            action = generate_shapefiles_action, targets = generate_shapefiles_targets)

# Define create_run_sampler_task function
def create_run_sampler_task(id: str, algo_type: str, hier_prior: str, mix_prior: str) -> Task:
    filename = "mcmc_chain" if algo_type == "MCMC" else "cmc_chain"
    output_path = create_output_path(hier_prior, mix_prior)
    output_file = f"output/{output_path}/{filename}{id}.dat"
    run_sampler_action = ["Rscript", os.path.join(workdir,"src/run_sampler.R")] + \
        ["--input-file", f"input/shapefile{id}.dat"] + \
        ["--hier-prior", hier_prior] + \
        ["--mix-prior", mix_prior] + \
        ["--algo-params", algo_params] + \
        ["--output-file", output_file]
    run_sampler_task_dependencies = [name("generate_shapefiles")]
    run_sampler_targets = [os.path.join(workdir, output_file)]
    if algo_type == "MCMC":
        run_sampler_action += ["--use-mcmc"]
    return create_task(f"_{name(f"run-{output_path}-{filename}{id}")}", action=run_sampler_action,
                       task_dependencies=run_sampler_task_dependencies, targets=run_sampler_targets)

# Define run_sampler task group
algo_params = 'algo_id: "Neal2" rng_seed: 10092022 iterations: 40 burnin: 10 init_num_clusters: 5'
algo_types = ["MCMC", "CMC"]
hier_priors = ["fixed_values { shape: 3.25 rate: 0.25 }"]
mix_priors = ["fixed_value { totalmass: 1.0 lambda: 0.35 }"]
with create_group(name("run")):
    for algo_type in algo_types:
        for hier_prior in hier_priors:
            for mix_prior in mix_priors:
                for id in ids:
                    create_run_sampler_task(id, algo_type, hier_prior, mix_prior)


# Define create_generate_plots_task function
def create_generate_plots_task(id: str, algo_type: str, hier_prior: str, mix_prior: str) -> Task:
    filename = "mcmc_chain" if algo_type == "MCMC" else "cmc_chain"
    sim_path = create_output_path(hier_prior, mix_prior)
    sim_file = f"output/{sim_path}/{filename}{id}.dat"
    generate_plots_action = ["Rscript", os.path.join(workdir,"src/generate_plots.R")] + \
        ["--data-file", f"input/shapefile{id}.dat"] + \
        ["--sim-file", sim_file] + \
        ["--output-dir", f"plots/{sim_path}/{algo_type.lower()}"]
    generate_plots_task_dependencies = [f"_{name(f"run-{sim_path}-{filename}{id}")}"]
    return create_task(f"_{name(f"generate-plots-{sim_path}-{filename}{id}")}", action=generate_plots_action,
                       task_dependencies=generate_plots_task_dependencies)

# Define generate_plots task group
with create_group(name("generate_plots")):
    for algo_type in algo_types:
        for hier_prior in hier_priors:
            for mix_prior in mix_priors:
                for id in ids:
                    create_generate_plots_task(id, algo_type, hier_prior, mix_prior)
