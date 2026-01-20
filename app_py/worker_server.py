#!/usr/bin/env python3
"""
Persistent Python worker to avoid re-importing heavy libraries on each bridge call.
Listens on a local TCP port and processes requests for file-based shape analysis.
"""

import json
import socket
import sys
import traceback
from datetime import datetime
from typing import Tuple

HOST = "127.0.0.1"
PORT = 47832  # arbitrary local port
BUFFER_SIZE = 65536
ENCODING = "utf-8"


def safe_log(msg: str) -> None:
    try:
        print(f"[worker] {datetime.now().isoformat()} - {msg}")
    except Exception:
        pass


def process_file(file_path: str) -> str:
    """Use existing get_shape_from_file to process the file and return JSON string."""
    from main import get_shape_from_file  # local import; heavy libs already loaded at worker start
    return get_shape_from_file(file_path)


def handle_connection(conn: socket.socket, addr: Tuple[str, int]) -> None:
    try:
        data = b""
        while True:
            chunk = conn.recv(BUFFER_SIZE)
            if not chunk:
                break
            data += chunk
            if b"\n" in chunk:
                break
        if not data:
            return
        line = data.decode(ENCODING, errors="replace").strip()
        try:
            req = json.loads(line)
        except json.JSONDecodeError:
            resp = {"success": False, "error": "Invalid JSON request"}
        else:
            if "file" in req:
                try:
                    result_json = process_file(req["file"])
                    # result_json is already a JSON string; send as text
                    conn.sendall((result_json + "\n").encode(ENCODING))
                    return
                except Exception as e:
                    resp = {"success": False, "error": f"Processing error: {e}"}
            else:
                resp = {"success": False, "error": "Missing 'file' field"}
        conn.sendall((json.dumps(resp) + "\n").encode(ENCODING))
    finally:
        conn.close()


def serve() -> None:
    safe_log("starting worker server (imports loaded once)")
    # Load heavy libraries once at startup
    import numpy  # noqa: F401
    import pandas  # noqa: F401
    import sklearn  # noqa: F401
    import matplotlib  # noqa: F401
    import seaborn  # noqa: F401
    import dotenv  # noqa: F401
    import psutil  # noqa: F401
    try:
        import win32api  # noqa: F401
    except Exception:
        pass  # optional on non-Windows

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind((HOST, PORT))
        s.listen(5)
        safe_log(f"listening on {HOST}:{PORT}")
        try:
            while True:
                conn, addr = s.accept()
                handle_connection(conn, addr)
        except KeyboardInterrupt:
            safe_log("shutdown requested (KeyboardInterrupt)")
        except Exception:
            safe_log("unhandled server error:\n" + traceback.format_exc())


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--serve":
        serve()
    else:
        print("Usage: python worker_server.py --serve")
