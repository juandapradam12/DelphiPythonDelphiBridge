#!/usr/bin/env python3
"""
Client helper to talk to the persistent worker_server.
If the server is not running, auto-start it and retry.
Outputs the JSON result with delimiters for compatibility with the bridge.
"""

import json
import os
import socket
import subprocess
import sys
import time
from typing import Optional

HOST = "127.0.0.1"
PORT = 47832
ENCODING = "utf-8"
# More generous retries to allow initial worker start/imports
RETRY_ATTEMPTS = 10
RETRY_DELAY = 0.5  # seconds


def start_server() -> None:
    script_dir = os.path.dirname(os.path.abspath(__file__))
    server_path = os.path.join(script_dir, "worker_server.py")
    # Start detached so it keeps running
    creationflags = 0
    if os.name == "nt":
        creationflags = subprocess.CREATE_NEW_PROCESS_GROUP  # type: ignore[attr-defined]
    subprocess.Popen([sys.executable, server_path, "--serve"],
                     cwd=script_dir,
                     stdout=subprocess.DEVNULL,
                     stderr=subprocess.DEVNULL,
                     creationflags=creationflags)


def send_request(file_path: str) -> str:
    req = {"file": file_path}
    last_err: Optional[Exception] = None
    for attempt in range(RETRY_ATTEMPTS):
        try:
            with socket.create_connection((HOST, PORT), timeout=10) as sock:
                sock.sendall((json.dumps(req) + "\n").encode(ENCODING))
                resp = sock.recv(65536)
                return resp.decode(ENCODING, errors="replace").strip()
        except (ConnectionRefusedError, socket.timeout) as e:
            last_err = e
            if attempt == 0:
                start_server()
            time.sleep(RETRY_DELAY)
        except Exception as e:
            last_err = e
            break
    raise RuntimeError(f"Failed to contact worker: {last_err}")


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: python worker_client.py <point_cloud_file>")
        sys.exit(1)
    file_path = sys.argv[1]
    result = send_request(file_path)
    # Print with delimiters for the Delphi bridge extraction logic
    print("============================================================")
    print(result)
    print("============================================================")


if __name__ == "__main__":
    main()
