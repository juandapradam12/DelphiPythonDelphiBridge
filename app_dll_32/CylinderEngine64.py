"""
CylinderEngine64.exe - 64-bit Python Engine for Clean Architecture

Role: 64-bit compute engine that receives data from 32-bit bridge
Responsibilities:
- Listen for requests via named pipe
- Read point cloud data from shared memory  
- Run full main.py with scikit-learn
- Return JSON results

This is where ALL the ML computation happens.
The 32-bit bridge never touches Python.
"""

import sys
import os
import json
import numpy as np
import threading
import time
from datetime import datetime
import mmap
import struct

# Add app_py to path so we can import main
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(__file__)), 'app_py'))

try:
    from main import main
    MAIN_AVAILABLE = True
    print(f"✓ Successfully imported main.py with full scikit-learn support")
except ImportError as e:
    MAIN_AVAILABLE = False
    print(f"✗ Failed to import main.py: {e}")
    print("Engine will provide basic analysis only")

class CylinderEngine64:
    def __init__(self):
        self.running = True
        self.shared_memory_name = "CylinderBridge32_Data"
        self.pipe_name = r"\\.\pipe\CylinderBridge32_Pipe"
        
    def log(self, message):
        timestamp = datetime.now().strftime("%H:%M:%S.%f")[:-3]
        print(f"[{timestamp}] Engine64: {message}")
        
    def process_point_cloud_data(self, raw_bytes, cylinder_name="unknown"):
        """Process point cloud data using full main.py"""
        try:
            if not MAIN_AVAILABLE:
                return self.fallback_analysis(raw_bytes, cylinder_name)
            
            self.log(f"Processing {len(raw_bytes)} bytes with full scikit-learn pipeline")
            
            # Call the real main.py with full scikit-learn support
            result = main(raw_bytes, enable_plots=False, cylinder_name=cylinder_name)
            
            self.log("✓ Full scikit-learn processing completed successfully")
            return result
            
        except Exception as e:
            self.log(f"✗ Error in main.py processing: {e}")
            return self.fallback_analysis(raw_bytes, cylinder_name)
    
    def fallback_analysis(self, raw_bytes, cylinder_name):
        """Basic analysis if main.py fails"""
        try:
            # Convert bytes back to point cloud
            points_flat = np.frombuffer(raw_bytes, dtype=np.float64)
            n_points = len(points_flat) // 3
            points = points_flat.reshape(n_points, 3)
            
            # Basic statistics
            min_coords = np.min(points, axis=0)
            max_coords = np.max(points, axis=0)
            center = (min_coords + max_coords) / 2
            lengths = max_coords - min_coords
            
            # Simple radius estimation
            distances = np.sqrt((points[:, 0] - center[0])**2 + (points[:, 1] - center[1])**2)
            estimated_radius = float(np.mean(distances))
            estimated_length = float(lengths[2])
            
            result = {
                "success": True,
                "session_id": f"engine64_{int(time.time())}",
                "timestamp": datetime.now().isoformat(),
                "engine": "CylinderEngine64_fallback",
                "fallback_fit": {
                    "radius": estimated_radius,
                    "length": estimated_length,
                    "center_point": {
                        "x": float(center[0]),
                        "y": float(center[1]),
                        "z": float(center[2])
                    },
                    "method": "basic_bounding_box"
                },
                "input_source": {
                    "type": "shared_memory_64bit",
                    "cylinder_name": cylinder_name,
                    "point_count": n_points
                }
            }
            
            return json.dumps(result, indent=2)
            
        except Exception as e:
            return json.dumps({
                "success": False,
                "error": f"Engine64 fallback analysis failed: {str(e)}",
                "timestamp": datetime.now().isoformat()
            })
    
    def handle_bridge_request(self, request_data):
        """Handle a request from the 32-bit bridge"""
        try:
            # Parse request
            data_size = request_data.get('data_size', 0)
            cylinder_name = request_data.get('cylinder_name', 'unknown')
            
            self.log(f"Received request: {data_size} bytes, cylinder='{cylinder_name}'")
            
            # Read data from shared memory
            raw_bytes = self.read_shared_memory(data_size)
            if raw_bytes is None:
                return json.dumps({
                    "success": False, 
                    "error": "Failed to read shared memory",
                    "timestamp": datetime.now().isoformat()
                })
            
            # Process with full Python pipeline
            result = self.process_point_cloud_data(raw_bytes, cylinder_name)
            
            return result
            
        except Exception as e:
            self.log(f"✗ Error handling bridge request: {e}")
            return json.dumps({
                "success": False,
                "error": f"Engine64 request handling failed: {str(e)}",
                "timestamp": datetime.now().isoformat()
            })
    
    def read_shared_memory(self, data_size):
        """Read point cloud data from shared memory"""
        try:
            # TODO: Implement shared memory reading
            # For now, return dummy data for testing
            self.log(f"TODO: Reading {data_size} bytes from shared memory")
            
            # Create dummy point cloud data for testing
            n_points = data_size // (3 * 8)  # 8 bytes per double, 3 doubles per point
            dummy_points = np.random.random((n_points, 3)).astype(np.float64)
            dummy_bytes = dummy_points.reshape(-1).tobytes()
            
            self.log(f"✓ Created dummy data: {len(dummy_bytes)} bytes ({n_points} points)")
            return dummy_bytes
            
        except Exception as e:
            self.log(f"✗ Error reading shared memory: {e}")
            return None
    
    def listen_for_requests(self):
        """Listen for requests from 32-bit bridge"""
        self.log("Starting 64-bit Python engine...")
        self.log(f"Listening for bridge requests...")
        self.log(f"Main.py available: {MAIN_AVAILABLE}")
        
        # TODO: Implement named pipe listening
        # For now, run a test
        self.run_test()
    
    def run_test(self):
        """Test the engine with dummy data"""
        self.log("=== Running Engine64 Test ===")
        
        # Simulate a bridge request
        test_request = {
            'data_size': 2328,  # Same size as your test
            'cylinder_name': 'test_cylinder_64'
        }
        
        result = self.handle_bridge_request(test_request)
        
        self.log("Test result:")
        print(result)
        
        self.log("=== Engine64 Test Complete ===")

def main_engine():
    engine = CylinderEngine64()
    try:
        engine.listen_for_requests()
    except KeyboardInterrupt:
        engine.log("Shutting down engine...")
    except Exception as e:
        engine.log(f"Engine error: {e}")

if __name__ == "__main__":
    main_engine()