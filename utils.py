import os
import re
import zipfile

import requests
from typing import Optional

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


def download_and_extract(url: str, dest_folder: str):
    """
    Downloads a zip file from a URL and extracts it to a specified folder.

    Args:
        url (str): URL of the zip file to download
        dest_folder (str): Name of the folder to extract files to
    """
    # Create destination folder if it doesn't exist
    os.makedirs(dest_folder, exist_ok=True)
    
    # Download the file
    print(f"Downloading from {url}")
    response = requests.get(url)
    zip_path = os.path.join(dest_folder, "temp.zip")
    
    # Save the zip file
    with open(zip_path, 'wb') as f:
        f.write(response.content)
    
    # Extract the contents
    print(f"Extracting files to {dest_folder}")
    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extractall(dest_folder)
    
    # Remove the temporary zip file
    os.remove(zip_path)
    print(f"Successfully downloaded and extracted files to {dest_folder}")
