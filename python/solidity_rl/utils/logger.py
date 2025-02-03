import logging
import os
from datetime import datetime

def setup_logger(name: str, log_dir: str = "logs", level=logging.DEBUG):
    """
    Creates and configures a logger.
    
    Args:
        name (str): Name of the logger.
        log_dir (str): Directory where log files will be stored.
        level (int): Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL).
    
    Returns:
        logging.Logger: Configured logger instance.
    """
    
    # Ensure log directory exists
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    # Define log file name with timestamp
    log_file = os.path.join(log_dir, f"{name}_{datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}.log")

    # Create logger
    logger = logging.getLogger(name)
    logger.setLevel(level)
    
    # Formatter
    formatter = logging.Formatter("[%(asctime)s] [%(levelname)s] %(name)s: %(message)s", "%Y-%m-%d %H:%M:%S")
    
    # Console Handler (prints logs to terminal)
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)
    console_handler.setFormatter(formatter)

    # File Handler (saves logs to a file)
    file_handler = logging.FileHandler(log_file, mode="a", encoding="utf-8")
    file_handler.setLevel(logging.DEBUG)
    file_handler.setFormatter(formatter)

    # Avoid duplicate handlers
    if not logger.hasHandlers():
        logger.addHandler(console_handler)
        logger.addHandler(file_handler)

    return logger

# Example Usage
if __name__ == "__main__":
    logger = setup_logger("solidity_rl")
    logger.info("Logging system initialized.")
    logger.debug("This is a debug message.")
    logger.warning("Warning: This is a test warning.")
    logger.error("Error: Something went wrong.")
    logger.critical("Critical: System failure!")
