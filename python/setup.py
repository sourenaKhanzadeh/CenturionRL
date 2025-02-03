from setuptools import setup, find_packages
import os

# Handle missing README.md
readme_path = "README.md"
long_description = open(readme_path, "r").read() if os.path.exists(readme_path) else "Solidity RL Optimization Package"

setup(
    name="solidity_rl",
    version="0.1.0",
    author="Sourena Khanzadeh",
    author_email="sourena.khanzadeh@gmail.com",
    description="A reinforcement learning-based Solidity optimization package",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/sourenakhanzadeh/solidity_rl",
    packages=find_packages(),  # Automatically finds all packages inside solidity_rl
    install_requires=[
        "numpy",
        "pandas",
        "torch",  # Add dependencies your project needs
        "gym",
        "matplotlib",
        "web3"
    ],
    python_requires=">=3.8",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
)
