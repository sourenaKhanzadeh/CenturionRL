import os

# Define the folder structure
folders = [
    "data/contracts",
    "data/processed/ast_json",
    "data/processed/bytecode",
    "data/processed/yul_ir",
    "data/processed/graphs",
    "data/reports",
    "data/metrics",
    
    "scripts/preprocessing",
    "scripts/optimization",
    "scripts/evaluation",
    "scripts/deployment",
    
    "models/graph_models",
    "models/rl_models",
    "models/traditional_ml",
    "models/saved_models",
    "models/evaluation",
    
    "tests/unit_tests",
    "tests/integration_tests",
    "tests/contracts",
    "tests/security",
    
    "notebooks",
    
    "config",
    
    "docs",
    
    "results/optimized_contracts",
    "results/benchmarks",
    "results/validation_reports",
    "results/visualizations",
    "results/logs",
    
    "experiments/hyperparameter_tuning",
    "experiments/feature_selection",
    "experiments/deployment_strategies",
    "experiments/scaling_tests",
    "experiments/custom_optimizations"
]

# Define the placeholder files to create within the folders
files = {
    "requirements.txt": "numpy\nnetworkx\nsolc\npytorch\ntorch-geometric\n",
    "package.json": '{\n  "name": "smart-contract-optimizer",\n  "version": "1.0.0"\n}\n',
    "setup.py": 'from setuptools import setup, find_packages\n\nsetup(name="smart_contract_optimizer", packages=find_packages())\n',
    "run_pipeline.sh": "#!/bin/bash\npython scripts/preprocessing/extract_ast.py\npython scripts/optimization/optimize_ast.py\n",
    "LICENSE": "MIT License\n\nCopyright (c) 2024",
    "README.md": "# Smart Contract Optimizer\n\nThis project optimizes smart contracts using graph-based AI techniques.",
    "docs/INSTALL.md": "## Installation Guide\n\n1. Install dependencies:\n   ```bash\n   pip install -r requirements.txt\n   ```",
    "docs/USAGE.md": "## Usage Guide\n\nRun the pipeline with:\n```bash\nbash run_pipeline.sh\n```",
    "docs/ARCHITECTURE.md": "## Project Architecture\n\n- **Data Processing**\n- **Optimization Pipeline**\n- **Deployment**",
    "config/settings.yaml": "optimization:\n  max_iterations: 100\n  learning_rate: 0.01",
    "config/model_params.yaml": "model:\n  type: GNN\n  layers: 3\n  hidden_units: 64",
    "config/optimization_config.yaml": "storage_optimization: true\nloop_unrolling: true",
    "config/paths.yaml": "data_path: data/processed/\noutput_path: results/",
}

# Create directories
for folder in folders:
    os.makedirs(folder, exist_ok=True)

# Create files with placeholder content
for file_path, content in files.items():
    with open(file_path, 'w') as f:
        f.write(content)

print("Project structure has been successfully created.")
