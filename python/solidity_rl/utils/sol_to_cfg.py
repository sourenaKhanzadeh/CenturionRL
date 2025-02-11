import subprocess
import tempfile
import os
import shutil
import pathlib
import glob
import time

__INFO__ ="""
Author: Sourena Khanzadeh
Date: 2025-01-25

This script is used to convert a SOL file to a CFG format.
"""

DATA_DIR = pathlib.Path(__file__).parent.parent / "data"
CONTRACTS_DIR = DATA_DIR / "raw_contracts"
CFG_OUTPUT_DIR = DATA_DIR / "processed" / "cfg"
CFG_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)



def main():
    """
    Main function to convert a SOL file to a CFG format.
    """

    for contract in CONTRACTS_DIR.iterdir():
        if contract.is_file() and contract.suffix == ".sol":
            print(f"Processing {contract.name}")
            contract_name = contract.name.split(".")[0]
            contract_path = contract.resolve()
            if os.path.exists(CFG_OUTPUT_DIR / contract_name):
                print(f"Skipping {contract_name} because it already exists in {CFG_OUTPUT_DIR}")
                continue
            try:
                subprocess.run(["slither", str(contract_path), "--print", "cfg"], check=True)

                # Small delay to ensure files are written
                time.sleep(2)

                # Search for .dot files using os.walk
                dot_files = []
                for root, _, files in os.walk(CONTRACTS_DIR):
                    for file in files:
                        if file.endswith(".dot"):
                            dot_files.append(pathlib.Path(root) / file)

                # Check if any .dot files were found
                if not dot_files:
                    print(f"No .dot files found in {DATA_DIR}. Double-check Slither output path.")
                else:
                    os.makedirs(CFG_OUTPUT_DIR / contract_name, exist_ok=True)
                    for dot_file in dot_files:
                        shutil.move(dot_file, CFG_OUTPUT_DIR / contract_name / dot_file.name)
                        print(f"Moved {dot_file.name} to {CFG_OUTPUT_DIR / contract_name}")

            except subprocess.CalledProcessError as e:
                print("Error running Slither:", e)

            print(f"CFG dot files saved to {CFG_OUTPUT_DIR.resolve()}")



def convert_sol_to_cfg(sol_pth: str):
    """
    Convert a SOL file to a CFG format.
    """
    subprocess.run(["slither", str(sol_pth), "--print", "cfg"], check=True)
    time.sleep(1)

    # move dot files to data\\processed\\cfg\\<contract_name>
    contract_name = sol_pth.split("/")[-1].split(".")[0]
    cfg_dir = CFG_OUTPUT_DIR / contract_name
    for file in os.listdir(CFG_OUTPUT_DIR):
        if file.endswith(".dot"):
            shutil.move(CFG_OUTPUT_DIR / file, cfg_dir / file)

    return cfg_dir

if __name__ == "__main__":
    main()
