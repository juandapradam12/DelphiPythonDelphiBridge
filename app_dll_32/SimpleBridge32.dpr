// SimpleBridge32.dll - 32-bit Process Bridge to 64-bit Python
library SimpleBridge32;

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.AnsiStrings,
  Winapi.Windows;

function CB_ProcessPointCloud(PointData: Pointer; DataSize: Integer; out ResultJSON: PAnsiChar): Integer; stdcall;
var
  TempDir, TempFile, OutputFile, Command: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
  ResultStr: AnsiString;
  BufSize: Integer;
  FullOutput, JsonResult: string;
  WaitResult: DWORD;
  JsonStartPos, JsonEndPos: Integer;
  JsonSection: string;
begin
  Result := -1;
  ResultJSON := nil;
  
  try
    if (PointData = nil) or (DataSize <= 0) then
    begin
      JsonResult := '{"success": false, "error": "Invalid input: null data or zero size"}';
    end
    else
    begin
      TempDir := GetEnvironmentVariable('TEMP');
      if TempDir = '' then TempDir := 'C:\Windows\Temp';
      TempFile := TPath.Combine(TempDir, Format('cylinder_bridge_%d_%d.txt', [GetCurrentProcessId, GetTickCount]));
      OutputFile := TPath.Combine(TempDir, Format('cylinder_output_%d_%d.txt', [GetCurrentProcessId, GetTickCount]));
      
      try
        // Just copy the original file path instead of processing binary data
        TempFile := string(PAnsiChar(PointData));  // Assume PointData contains the file path
        
        // Find the batch file relative to the DLL location
        var DllDir := TPath.GetDirectoryName(GetModuleName(HInstance));
        var BatchFile := TPath.Combine(DllDir, 'run_python_to_file.bat');
        var WorkDir := TPath.GetDirectoryName(BatchFile);
        
        Command := Format('"%s" "%s" "%s"', [BatchFile, TempFile, OutputFile]);
        
        if not FileExists(BatchFile) then
        begin
          JsonResult := '{"success": false, "error": "run_python_to_file.bat not found at: ' + BatchFile + '"}';
        end
        else
        begin
          ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
          StartupInfo.cb := SizeOf(StartupInfo);
          StartupInfo.wShowWindow := SW_HIDE;
          StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
          
          if CreateProcess(nil, PChar(Command), nil, nil, False, 
                          CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS, nil, 
                          PChar(WorkDir), 
                          StartupInfo, ProcessInfo) then
          begin
            try
              // Bump timeout to handle initial library imports in main.py (can take ~10s)
              // Allow up to 30s so first-run imports don't trigger false timeouts
              WaitResult := WaitForSingleObject(ProcessInfo.hProcess, 30000);
              if WaitResult = WAIT_OBJECT_0 then
              begin
                GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
                
                if FileExists(OutputFile) then
                begin
                  try
                    // Read output file with UTF-8 encoding
                    FullOutput := TFile.ReadAllText(OutputFile, TEncoding.UTF8);
                    
                    // Simple and reliable JSON extraction using Pos (not LastIndexOf)
                    JsonStartPos := Pos('============================================================', FullOutput);
                    if JsonStartPos > 0 then
                    begin
                      // Get everything after the delimiter
                      JsonSection := Copy(FullOutput, JsonStartPos, Length(FullOutput));
                      JsonStartPos := Pos('{', JsonSection);
                      
                      if JsonStartPos > 0 then
                      begin
                        // Extract from the first { onwards
                        JsonResult := Copy(JsonSection, JsonStartPos, Length(JsonSection));
                        
                        // Find the last closing brace using simple loop
                        JsonEndPos := Length(JsonResult);
                        while (JsonEndPos > 0) and (JsonResult[JsonEndPos] <> '}') do
                          Dec(JsonEndPos);
                        
                        if JsonEndPos > 0 then
                          JsonResult := Copy(JsonResult, 1, JsonEndPos)
                        else
                          JsonResult := '{"success": false, "error": "JSON end brace not found"}';
                      end
                      else
                      begin
                        JsonResult := '{"success": false, "error": "JSON start brace not found"}';
                      end;
                    end
                    else
                    begin
                      JsonResult := Format('{"success": false, "error": "No JSON delimiter found", "exit_code": %d}', [ExitCode]);
                    end;
                    
                    DeleteFile(PChar(OutputFile));
                  except
                    on E: Exception do
                      JsonResult := Format('{"success": false, "error": "Failed to read output file: %s", "exit_code": %d}', [E.Message, ExitCode]);
                  end;
                end
                else
                begin
                  JsonResult := Format('{"success": false, "error": "No output file created", "exit_code": %d, "command": "%s"}', [ExitCode, Command]);
                end;
              end
              else
              begin
                TerminateProcess(ProcessInfo.hProcess, 1);
                JsonResult := Format('{"success": false, "error": "Python process timeout (10 seconds)", "command": "%s"}', [Command]);
              end;
            finally
              CloseHandle(ProcessInfo.hProcess);
              CloseHandle(ProcessInfo.hThread);
            end;
          end
          else
          begin
            JsonResult := Format('{"success": false, "error": "Failed to start process", "command": "%s"}', [Command]);
          end;
        end;
      finally
        if FileExists(OutputFile) then DeleteFile(PChar(OutputFile));
      end;
    end;
    
    ResultStr := AnsiString(JsonResult);
    BufSize := Length(ResultStr) + 1;
    GetMem(ResultJSON, BufSize);
    System.AnsiStrings.StrLCopy(ResultJSON, PAnsiChar(ResultStr), BufSize - 1);
    
    Result := 0;
    
  except
    on E: Exception do
    begin
      JsonResult := Format('{"success": false, "error": "Bridge32 exception: %s"}', [E.Message]);
      ResultStr := AnsiString(JsonResult);
      BufSize := Length(ResultStr) + 1;
      GetMem(ResultJSON, BufSize);
      System.AnsiStrings.StrLCopy(ResultJSON, PAnsiChar(ResultStr), BufSize - 1);
      Result := -2;
    end;
  end;
end;

procedure CB_FreeMemory(P: PAnsiChar); stdcall;
begin
  if P <> nil then
    FreeMem(P);
end;

function CB_GetVersion(): PAnsiChar; stdcall;
const
  VERSION_INFO = 'SimpleBridge32 v2.0 - File-based Process Bridge';
var
  ResultStr: AnsiString;
  BufSize: Integer;
begin
  ResultStr := AnsiString(VERSION_INFO);
  BufSize := Length(ResultStr) + 1;
  GetMem(Result, BufSize);
  System.AnsiStrings.StrLCopy(Result, PAnsiChar(ResultStr), BufSize - 1);
end;

exports
  CB_ProcessPointCloud,
  CB_FreeMemory,
  CB_GetVersion;

begin
end.