import pickle
import json
import os

def save_pickle(obj, file_path: str):
    """
    Serializes and saves an object to a pickle file.

    Args:
        obj: The object to be saved.
        file_path (str): Path where the object will be saved.
    """
    with open(file_path, "wb") as file:
        pickle.dump(obj, file)

def load_pickle(file_path: str):
    """
    Loads and deserializes an object from a pickle file.

    Args:
        file_path (str): Path to the pickle file.

    Returns:
        The deserialized object.
    """
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Pickle file not found: {file_path}")

    with open(file_path, "rb") as file:
        return pickle.load(file)

def save_json(data: dict, file_path: str):
    """
    Saves a dictionary to a JSON file.

    Args:
        data (dict): Dictionary to save.
        file_path (str): Path to the JSON file.
    """
    with open(file_path, "w", encoding="utf-8") as file:
        json.dump(data, file, indent=4)

def load_json(file_path: str):
    """
    Loads a dictionary from a JSON file.

    Args:
        file_path (str): Path to the JSON file.

    Returns:
        dict: Parsed JSON data.
    """
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"JSON file not found: {file_path}")

    with open(file_path, "r", encoding="utf-8") as file:
        return json.load(file)

# Example Usage
if __name__ == "__main__":
    test_data = {"name": "SoloRL", "version": 1.0}
    test_pickle_file = "test_data.pkl"
    test_json_file = "test_data.json"

    # Save and Load Pickle
    save_pickle(test_data, test_pickle_file)
    loaded_pickle = load_pickle(test_pickle_file)
    print("Loaded Pickle Data:", loaded_pickle)

    # Save and Load JSON
    save_json(test_data, test_json_file)
    loaded_json = load_json(test_json_file)
    print("Loaded JSON Data:", loaded_json)
