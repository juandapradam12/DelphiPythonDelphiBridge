# Persistent Worker Implementation Guide

## Problem Statement
The current bridge architecture spawns a new Python process per call, causing heavy libraries (numpy, pandas, sklearn, matplotlib, seaborn) to be reimported each time, adding ~4s latency per request.

## Solution Overview
Implement a persistent Python worker that:
1. Loads all heavy libraries **once** at startup
2. Listens for requests on a local TCP socket
3. Processes file paths and returns JSON results
4. Allows subsequent calls to reuse already-loaded libraries (~milliseconds latency)

## Architecture

```
Delphi App
    ↓
SimpleBridge32.dll (run_python_to_file.bat)
    ↓
worker_client.py (auto-starts worker if needed)
    ↓
worker_server.py (long-running, imports loaded once)
    ↓
main.py (get_shape_from_file function)
```

## Implementation Steps

### Step 1: Create the Persistent Worker Server
**File:** `app_py/worker_server.py`

Purpose: Long-running process that loads heavy libraries once and listens for requests.

Key components:
- Load numpy, pandas, sklearn, matplotlib, seaborn at startup (only once)
- Listen on TCP localhost:47832
- Accept JSON requests with format: `{"file": "<path>"}`
- Call `main.get_shape_from_file(file_path)` 
- Send JSON response back to client
- Handle multiple connections sequentially (simple accept-process-close pattern)

Protocol:
- Client sends: `{"file": "C:\\path\\to\\file.txt"}\n`
- Server responds: `{json result}\n`

Error handling:
- If request JSON is invalid, return `{"success": false, "error": "..."}`
- If file processing fails, return error JSON
- Log all activity with timestamps for debugging

### Step 2: Create the Worker Client Launcher
**File:** `app_py/worker_client.py`

Purpose: Thin client that connects to the worker, auto-starts it if needed.

Key components:
- Accept command-line argument: file path
- Try to connect to worker at 127.0.0.1:47832
- If connection refused, auto-start `worker_server.py` with `--serve` flag as detached process
- Retry connecting (up to 10 attempts, 0.5s delay between retries)
- Send JSON request: `{"file": "<path>"}`
- Receive and print response
- Wrap response with delimiters for Delphi bridge extraction:
  ```
  ============================================================
  {json result}
  ============================================================
  ```

Error handling:
- If all retries fail, print error JSON with delimiters
- Log startup/connection attempts

### Step 3: Update Batch File
**File:** `app_dll_32/run_python_to_file.bat`

Change from:
```batch
"C:\CylinderCenterlineApp\app_py\venv\Scripts\python.exe" -O "C:\CylinderCenterlineApp\app_py\main.py" "%~1" > "%~2" 2>&1
```

To:
```batch
cd /d "%~dp0\..\app_py"
set PYTHONPATH=%CD%
set PYTHONIOENCODING=utf-8
"%CD%\venv\Scripts\python.exe" -O "%CD%\worker_client.py" "%~1" > "%~2" 2>&1
```

This ensures relative paths work and calls worker_client instead of main directly.

### Step 4: Update Delphi Bridge (SimpleBridge32.dpr)
No changes required if run_python_to_file.bat is already being called. The DLL will transparently use the worker client.

Optional: Increase timeout to 30s (if not already done) to allow first-start worker initialization:
```delphi
WaitResult := WaitForSingleObject(ProcessInfo.hProcess, 30000);
```

## Testing

### Test 1: Manual Worker Start
```powershell
cd C:\YourProject\app_py
.\venv\Scripts\python.exe worker_server.py --serve
```
Leave running in a terminal. In another terminal:
```powershell
cd C:\YourProject\app_py
.\venv\Scripts\python.exe worker_client.py "C:\path\to\file.txt"
```
Expected: Instant response (imports already loaded).

### Test 2: Auto-Start Test
Kill the running worker server. Then:
```powershell
cd C:\YourProject\app_py
.\venv\Scripts\python.exe worker_client.py "C:\path\to\file.txt"
```
Expected: Client auto-starts server (may take 4-5s first time), then processes file.

### Test 3: Repeated Calls (Real Latency)
Run multiple times:
```powershell
.\venv\Scripts\python.exe worker_client.py "C:\path\to\file.txt"
.\venv\Scripts\python.exe worker_client.py "C:\path\to\file2.txt"
.\venv\Scripts\python.exe worker_client.py "C:\path\to\file3.txt"
```
Expected: First ~4-5s, subsequent calls ~0.3-0.5s total (mostly IPC overhead).

### Test 4: Delphi Bridge Integration
Run the Delphi test app (TestBridge32App or similar):
1. First call: ~4-5s total (worker startup + processing)
2. Second call: ~0.4-0.5s total (worker reused)
3. Verify JSON output is parsed correctly by the bridge

Expected JSON from worker_client:
```json
{
  "success": true,
  "timestamp": "2026-01-20T...",
  "file": "...",
  "shape": {"rows": X, "columns": 3},
  "performance": {
    "file_read_time_seconds": 0.xxx,
    "total_processing_time_seconds": 0.xxx,
    "import_profiling": {
      "library_import_times_seconds": {...}
    }
  }
}
```

## Performance Expectations

| Scenario | Latency |
|----------|---------|
| First call (worker cold start) | ~4-5 seconds |
| Subsequent calls (worker warm) | ~0.4-0.5 seconds |
| Python processing only | ~0.003 seconds |
| DLL + IPC overhead | ~0.4 seconds |

## Implementation Checklist

- [ ] Create `app_py/worker_server.py` with:
  - TCP socket listener on 127.0.0.1:47832
  - Library imports at startup (numpy, pandas, sklearn, matplotlib, seaborn, dotenv, psutil, win32api)
  - JSON request/response handling
  - Call to `main.get_shape_from_file()`
  
- [ ] Create `app_py/worker_client.py` with:
  - Auto-start server if not running
  - Retry logic (10 attempts, 0.5s delay)
  - Socket timeout of 10s
  - JSON request formatting
  - Delimiter wrapping for Delphi bridge
  
- [ ] Update `app_dll_32/run_python_to_file.bat` to:
  - Use relative paths
  - Call worker_client.py instead of main.py
  
- [ ] Update `app_dll_32/SimpleBridge32.dpr` to:
  - Use relative paths for batch file
  - Increase process timeout to 30s (if not done)
  
- [ ] Test all scenarios:
  - Manual worker start + client
  - Auto-start on cold worker
  - Repeated calls (warm latency)
  - Delphi bridge integration
  - Multiple file processing

## Notes for Copilot/Developer

- The worker uses TCP localhost; no firewall issues expected
- Worker process name will be `python.exe` running `worker_server.py`
- To kill the worker manually: `taskkill /F /IM python.exe` (kills all Python processes; more surgical option: add a shutdown command via socket)
- Import times shown in JSON are post-warm; they'll be 0.0 for reused connections
- If worker crashes, client will auto-restart it on next call
- No persistent state needed; worker can be safely terminated and restarted

## Future Optimizations

1. **Direct DLL-to-Worker Socket:** Instead of batch file, have Delphi DLL connect directly to worker socket (eliminates batch + process overhead, saves ~0.2s)
2. **Worker Lifecycle Management:** Add explicit shutdown command or idle timeout (keep-alive until N minutes of inactivity)
3. **Multiple Worker Threads:** If multiple concurrent requests expected, make worker multi-threaded
4. **Memory Pre-warming:** Pre-allocate buffers or cache common operations
