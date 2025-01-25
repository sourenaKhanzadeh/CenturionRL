import networkx as nx
import matplotlib.pyplot as plt
import pathlib
from io import StringIO


__INFO__ = """
Author: Sourena Khanzadeh
Date: 2025-01-26

This script is used to visualize the CFG of a contract.
"""

class CFG:
    """
    Class to represent a Control Flow Graph (CFG).
    """

    def __init__(self, dot_file_path: pathlib.Path):
        self.dot_file_path = dot_file_path
        self.graph = self.load_graph()

    def load_graph(self):
        """
        Load the DOT file into a NetworkX graph.
        """
        try:
            with open(self.dot_file_path, "r") as file:
                dot_content = file.read()
            return nx.drawing.nx_pydot.read_dot(StringIO(dot_content))
        except Exception as e:
            print(f"Error loading {self.dot_file_path}: {e}")
            return None

def main():
    """
    Main function to iterate through DOT files and visualize CFGs.
    """
    cfg_dir = pathlib.Path("data/cfg") / input("Enter the name of the contract: ")

    if not cfg_dir.exists() or not cfg_dir.is_dir():
        print(f"Directory {cfg_dir} does not exist.")
        return

    for cfg_file in cfg_dir.iterdir():
        if cfg_file.suffix == ".dot":
            print(f"Processing: {cfg_file}")
            cfg = CFG(cfg_file)
            if cfg.graph:
                plt.figure(figsize=(10, 6))
                pos = nx.spring_layout(cfg.graph)  # Use spring layout for spacing
                nx.draw(cfg.graph, pos, with_labels=True, node_size=4000, node_color="lightblue", edge_color="gray", font_size=10)
                
                # Extract node labels from attributes and display them
                labels = {node: cfg.graph.nodes[node]['label'] for node in cfg.graph.nodes}
                nx.draw_networkx_labels(cfg.graph, pos, labels, font_size=8, font_color="black")
                
                plt.title(f"Control Flow Graph: {cfg_file.stem}")
                plt.show()
            else:
                print(f"Skipping {cfg_file.name} due to loading errors.")

if __name__ == "__main__":
    main()
