"""
Library Import Profiler
Tracks import time for heavy libraries to monitor performance
"""

import time
from typing import Dict, List


class ImportProfiler:
    """Profiles import times for libraries"""
    
    def __init__(self):
        self.import_times: Dict[str, float] = {}
        self.start_time = time.time()
    
    def profile_imports(self) -> Dict[str, float]:
        """Import all required libraries and profile their load times"""
        
        libraries = [
            'numpy',
            'pandas',
            'sklearn',
            'matplotlib',
            'seaborn',
            'dotenv',
            'psutil',
            'win32api'
        ]
        
        for lib in libraries:
            start = time.time()
            try:
                if lib == 'sklearn':
                    import sklearn
                elif lib == 'dotenv':
                    import dotenv
                elif lib == 'win32api':
                    import win32api
                else:
                    __import__(lib)
                
                elapsed = time.time() - start
                self.import_times[lib] = round(elapsed, 4)
            except ImportError as e:
                self.import_times[lib] = f"Not installed: {str(e)}"
        
        # Calculate total import time
        self.import_times['total_import_time'] = round(
            sum(v for v in self.import_times.values() if isinstance(v, float)), 
            4
        )
        
        return self.import_times
    
    def get_summary(self) -> Dict[str, any]:
        """Get summary of import performance"""
        return {
            "library_import_times_seconds": self.import_times,
            "total_libraries": len([k for k in self.import_times.keys() if k != 'total_import_time']),
            "successfully_loaded": len([v for v in self.import_times.values() if isinstance(v, float)])
        }


# Global profiler instance
profiler = ImportProfiler()
