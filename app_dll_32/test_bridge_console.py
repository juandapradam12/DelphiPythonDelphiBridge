#!/usr/bin/env python3
"""
Console test of SimpleBridge32.dll 
Tests the complete 32-bit to 64-bit bridge functionality
"""
import ctypes
import sys
import os
import struct

def test_bridge():
    """Test the SimpleBridge32.dll with sample point cloud data"""
    
    # Load the DLL
    try:
        bridge_dll = ctypes.CDLL('./SimpleBridge32.dll')
        print("✓ Successfully loaded SimpleBridge32.dll")
    except Exception as e:
        print(f"✗ Failed to load SimpleBridge32.dll: {e}")
        return False
    
    # Define function signatures
    bridge_dll.CB_ProcessPointCloud.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.POINTER(ctypes.c_char_p)]
    bridge_dll.CB_ProcessPointCloud.restype = ctypes.c_int
    
    bridge_dll.CB_FreeMemory.argtypes = [ctypes.c_char_p]
    bridge_dll.CB_FreeMemory.restype = None
    
    # Load test point cloud file
    test_file = "C:/CylinderCenterlineApp/data/input/Point Cloud - 76.2 mm OD - 25 points total.txt"
    
    if not os.path.exists(test_file):
        print(f"✗ Test file not found: {test_file}")
        return False
    
    print(f"Loading test file: {test_file}")
    
    # Parse the point cloud file (same logic as Delphi)
    points = []
    with open(test_file, 'r') as f:
        for line in f:
            line = line.strip()
            # Skip comments and headers
            if not line or line.startswith('#') or line.startswith('//') or \
               'x,y,z' in line.lower() or line.startswith('point'):
                continue
            
            # Split by comma or space
            parts = line.replace(',', ' ').split()
            if len(parts) >= 3:
                try:
                    x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
                    points.extend([x, y, z])
                except ValueError:
                    continue
    
    if not points:
        print("✗ No valid points found in test file")
        return False
    
    print(f"✓ Loaded {len(points)//3} points ({len(points)} coordinates)")
    
    # Convert to IEEE 754 doubles (8 bytes each)
    data = struct.pack(f'{len(points)}d', *points)
    data_size = len(data)
    
    print(f"✓ Converted to {data_size} bytes of IEEE 754 double data")
    
    # Create buffer and copy data
    buffer = ctypes.create_string_buffer(data)
    result_ptr = ctypes.c_char_p()
    
    print("Calling CB_ProcessPointCloud...")
    
    # Call the bridge function
    result_code = bridge_dll.CB_ProcessPointCloud(
        ctypes.cast(buffer, ctypes.c_void_p),
        data_size,
        ctypes.byref(result_ptr)
    )
    
    if result_code == 0:
        print("✓ Bridge call successful!")
        if result_ptr.value:
            json_result = result_ptr.value.decode('utf-8')
            print("JSON Result:")
            print("=" * 60)
            print(json_result)
            print("=" * 60)
            
            # Free the memory
            bridge_dll.CB_FreeMemory(result_ptr)
            print("✓ Memory freed")
            return True
        else:
            print("✗ No result returned")
            return False
    else:
        print(f"✗ Bridge call failed with error code: {result_code}")
        if result_ptr.value:
            error_msg = result_ptr.value.decode('utf-8')
            print(f"Error details: {error_msg}")
            bridge_dll.CB_FreeMemory(result_ptr)
        return False

if __name__ == "__main__":
    print("Testing SimpleBridge32.dll - 32bit to 64bit bridge")
    print("=" * 60)
    
    success = test_bridge()
    
    print("=" * 60)
    if success:
        print("✓ Bridge test PASSED")
        sys.exit(0)
    else:
        print("✗ Bridge test FAILED")
        sys.exit(1)