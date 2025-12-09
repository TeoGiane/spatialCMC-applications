import os
import sys

from cook import create_task, Task
from cook.contexts import FunctionContext

# Include the current directory for extra modules
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Modify task name according to subfolder
def append_name(task: Task, name: str) -> Task:
    root_name = task.name.split(':', 1)[1] if ":" in task.name else task.name
    task.name = f"_{name}:{root_name}" if task.name.startswith("_") else f"{name}:{root_name}"
    return task

# Create requirements task
create_task("requirements", action="pip-compile -v", targets=["requirements.txt"],
            dependencies=["requirements.in"])

# Create pip-sync task
create_task("pip-sync", action="pip-sync", dependencies=["requirements.txt"])

# Include tasks from 'poisson_model_lattice' subfolder
with FunctionContext(append_name, "poisson_model_lattice"):
    from poisson_model_lattice.recipe import *

# include tasks from 'covid_data' subfolder
with FunctionContext(append_name, "covid_data"):
    from covid_data.recipe import *