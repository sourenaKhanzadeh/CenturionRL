import yaml
import os

def load_config(config_path: str):
    """
    Loads a YAML configuration file.

    Args:
        config_path (str): Path to the YAML configuration file.

    Returns:
        dict: Parsed configuration data.
    """
    if not os.path.exists(config_path):
        raise FileNotFoundError(f"Config file not found: {config_path}")

    with open(config_path, "r") as file:
        try:
            config = yaml.safe_load(file)
            return config
        except yaml.YAMLError as e:
            raise ValueError(f"Error parsing YAML file: {e}")

# Example Usage
if __name__ == "__main__":
    import pathlib
    config_path = pathlib.Path(__file__).parent.parent / "config" / "default.yaml"
    try:
        config = load_config(config_path)
        print("Loaded Config:", config)
    except Exception as e:
        print(f"Failed to load config: {e}")
