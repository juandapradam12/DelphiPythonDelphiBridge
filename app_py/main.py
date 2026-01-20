#!/usr/bin/env python3
"""
Simple Point Cloud Shape Reader
Reads point cloud data and returns its shape (dimensions)
"""

import json
import sys
import time
import pandas as pd
from datetime import datetime
from src.profiler.import_profiler import profiler


def get_shape_from_file(file_path: str) -> str:
    """
    Read point cloud file and return its shape.
    
    Args:
        file_path: Path to point cloud file
        
    Returns:
        JSON string with shape information
    """
    try:
        processing_start = time.time()
        
        # Profile library imports
        import_summary = profiler.profile_imports()
        
        # Read the file into a DataFrame
        read_start = time.time()
        df = pd.read_csv(file_path, comment='#', skip_blank_lines=True)
        read_time = time.time() - read_start
        
        # Get the shape
        rows, cols = df.shape
        
        # Calculate total processing time
        total_time = time.time() - processing_start
        
        # Return simple JSON with shape info and profiling data
        result = {
            "success": True,
            "timestamp": datetime.now().isoformat(),
            "file": file_path,
            "shape": {
                "rows": rows,
                "columns": cols
            },
            "performance": {
                "file_read_time_seconds": round(read_time, 4),
                "total_processing_time_seconds": round(total_time, 4),
                "import_profiling": profiler.get_summary()
            }
        }
        
        return json.dumps(result, indent=2)
        
    except Exception as e:
        return json.dumps({
            "success": False,
            "error": str(e),
            "timestamp": datetime.now().isoformat()
        }, indent=2)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python main.py <point_cloud_file>")
        sys.exit(1)
    
    result = get_shape_from_file(sys.argv[1])
    print(result)
