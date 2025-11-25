import sys
import string
from cook import create_task, Task
from cook.contexts import FunctionContext

# Modify task name according to subfolder
def append_name(task: Task, name: str) -> Task:
    task.name = f"_{name}:{task.name}" if task.name.startswith("_") else f"{name}:{task.name}"
    return task

# Include the current directory for extra modules
sys.path.append(".")

# Create requirements task
create_task("requirements", action="pip-compile -v", targets=["requirements.txt"],
            dependencies=["requirements.in"])

# Create pip-sync task
create_task("pip-sync", action="pip-sync", dependencies=["requirements.txt"])

# Include tasks from 'poisson_model_lattice' subfolder
with FunctionContext(append_name, "poisson_model_lattice"):
    from poisson_model_lattice.recipe import *