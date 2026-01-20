import sys
import os

# Add the app_py directory to Python path
app_py_path = r'C:\CylinderCenterlineApp\app_py'
if app_py_path not in sys.path:
    sys.path.insert(0, app_py_path)

# Change to the app_py directory
os.chdir(app_py_path)

# Now import and run main.py
if __name__ == "__main__":
    # Import the main module
    import main
    
    # Get command line argument (the temp file path)
    if len(sys.argv) > 1:
        file_path = sys.argv[1]
        
        # Load file and convert to bytes (same logic as in main.py)
        import pandas as pd
        import numpy as np
        
        df = pd.read_csv(file_path, sep=None, engine="python")
        arr = df.to_numpy(dtype=np.float64)
        raw_bytes = arr.reshape(-1).tobytes()
        
        # Call the main function with bytes
        result_json = main.main(raw_bytes, enable_plots=False, cylinder_name="bridge_test")
        
        # Output the result for Delphi bridge to capture
        print("\n" + "="*60)
        print("JSON RESULT FOR DELPHI:")
        print("="*60)
        print(result_json)
        print("="*60)
    else:
        print("Error: No file path provided")
        sys.exit(1)