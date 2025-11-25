import os
import re

from cook import create_task, Task
from cook.contexts import create_group
from typing import Optional

# Get directory of the current file
workdir = os.path.dirname(os.path.abspath(__file__))

def _clean_prior_string(prior_str: str, pattern: str) -> str:
    """
    Helper function to extract and clean the core part of a prior string.

    This function removes a given pattern (like 'beta_prior { ... }'),
    replaces whitespace sequences with a single underscore, and removes
    any resulting ':_' sequences.

    Args:
        prior_str (str): The input string to clean.
        pattern (str): The regex pattern for the wrapper to remove.

    Returns:
        str: The cleaned string.
    """
    # Remove the wrapper (e.g., "beta_prior { ... }")
    cleaned_str = re.sub(pattern, "", prior_str, flags=re.DOTALL)
    # Replace one or more whitespace characters with a single underscore
    cleaned_str = re.sub(r'\s+', '_', cleaned_str).strip('_')
    # Remove the pattern ':_' that can result from the previous step
    cleaned_str = cleaned_str.replace(":_", "")
    return cleaned_str

def create_output_path(hierarchy_prior: Optional[str] = None, mixing_prior: Optional[str] = None) -> str:
    """
    Generate an output path given priors in ASCII protocol buffer format.

    Args:
        hierarchy_prior (str, optional): Prior on the hierarchy. Defaults to None.
        mixing_prior (str, optional): Prior on the mixing. Defaults to None.

    Returns:
        str: A formatted output path string.
        
    Raises:
        ValueError: If any prior string has an unrecognized format.
    """
    out_path_parts = []

    # Prior on hierarchy prior
    if hierarchy_prior is not None:
        if "fixed_values" in hierarchy_prior:
            pattern = r"fixed_values\s*\{|\}"
            cleaned_str = _clean_prior_string(hierarchy_prior, pattern)
            out_path_parts.append(f"hier-{cleaned_str}")
        else:
            raise ValueError("Wrong format for 'hierarchy_prior' input.")
    
    # Prior on mixing prior
    if mixing_prior is not None:
        if "fixed_value" in mixing_prior:
            pattern = r"fixed_value\s*\{|\}"
            cleaned_str = _clean_prior_string(mixing_prior, pattern)
            out_path_parts.append(f"mix-{cleaned_str}")
        else:
            raise ValueError("Wrong format for 'mixing_prior' input.")

    # Join all parts into a single path string
    return os.path.join(*out_path_parts)

# Define quantities for simulation study
num_datasets = 1
ids = [f"{i:03d}" for i in range(1, num_datasets+1)]

# Define generate_shapefiles task
generate_shapefiles_action = ["Rscript", os.path.join(workdir,"src/generate_shapefiles.R")] + \
    ["--num-datasets", num_datasets] + \
    ["--dest-dir", "input"]
generate_shapefiles_targets = [os.path.join(workdir,f"input/shapefile{i:03d}.dat") for i in range(1, num_datasets+1)]
create_task("generate_shapefiles", 
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
    if algo_type == "MCMC":
        run_sampler_action += ["--use-mcmc"]
    return create_task(f"_run-{output_path}-{filename}{id}", action=run_sampler_action)

# Define run_sampler task group
algo_params = 'algo_id: "Neal2" rng_seed: 10092022 iterations: 40 burnin: 10 init_num_clusters: 5'
algo_types = ["MCMC", "CMC"]
hier_priors = ["fixed_values { shape: 3.25 rate: 0.25 }"]
mix_priors = ["fixed_value { totalmass: 1.0 lambda: 0.35 }"]
with create_group("run"):
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
    return create_task(f"_generate-plots-{sim_path}-{filename}{id}", action=generate_plots_action)

# Define generate_plots task group
with create_group("generate_plots"):
    for algo_type in algo_types:
        for hier_prior in hier_priors:
            for mix_prior in mix_priors:
                for id in ids:
                    create_generate_plots_task(id, algo_type, hier_prior, mix_prior)

