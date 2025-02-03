import os
import json
import shutil

def ensure_directory_exists(directory: str):
    """
    Ensures that a directory exists. Creates it if it does not exist.

    Args:
        directory (str): Path to the directory.
    """
    if not os.path.exists(directory):
        os.makedirs(directory)

def read_json(file_path: str):
    """
    Reads and returns the contents of a JSON file.

    Args:
        file_path (str): Path to the JSON file.

    Returns:
        dict: Parsed JSON data.
    """
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"File not found: {file_path}")

    with open(file_path, "r", encoding="utf-8") as file:
        return json.load(file)

def write_json(file_path: str, data: dict):
    """
    Writes a dictionary to a JSON file.

    Args:
        file_path (str): Path to the JSON file.
        data (dict): Data to write to the file.
    """
    with open(file_path, "w", encoding="utf-8") as file:
        json.dump(data, file, indent=4)

def copy_file(src: str, dest: str):
    """
    Copies a file from source to destination.

    Args:
        src (str): Source file path.
        dest (str): Destination file path.
    """
    if not os.path.exists(src):
        raise FileNotFoundError(f"Source file not found: {src}")

    shutil.copy(src, dest)

def delete_file(file_path: str):
    """
    Deletes a file if it exists.

    Args:
        file_path (str): Path to the file to be deleted.
    """
    if os.path.exists(file_path):
        os.remove(file_path)

# Example Usage
if __name__ == "__main__":
    test_dir = "test_data"
    test_file = os.path.join(test_dir, "example.json")
    
    # Ensure test directory exists
    ensure_directory_exists(test_dir)
    
    # Write sample JSON
    sample_data = {"name": "SoloRL", "version": 1.0}
    write_json(test_file, sample_data)
    
    # Read JSON
    data = read_json(test_file)
    print("Read Data:", data)
    
    # Copy file
    copy_file(test_file, os.path.join(test_dir, "copy.json"))
    
    # Delete file
    delete_file(test_file)
