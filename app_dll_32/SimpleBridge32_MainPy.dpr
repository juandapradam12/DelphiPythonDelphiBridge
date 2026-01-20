// Simple Bridge32 - Compatible with existing main.py
library SimpleBridge32;

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows;

{$R *.res}

// Clean API - exactly what client needs
function CB_ProcessPointCloud(PointData: Pointer; DataSize: Integer; out ResultJSON: PAnsiChar): Integer; stdcall;
var
  TempDir: string;
  TextFile: string;
  ResultFile: string;
  Command: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
  TestResult: string;
  ResultStr: AnsiString;
  BufSize: Integer;
begin
  try
    // Validate inputs
    if (PointData = nil) or (DataSize <= 0) then
    begin
      Result := -1; // Invalid input
      Exit;
    end;

    // Convert binary data to text format compatible with main.py
    TempDir := GetEnvironmentVariable('TEMP');
    if TempDir = '' then TempDir := 'C:\Windows\Temp';
    
    TextFile := Format('%s\cylinder_bridge_%d.txt', [TempDir, GetTickCount]);
    
    // Convert binary point cloud data to text format (X Y Z per line)
    var FileHandle := CreateFile(PChar(TextFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      TestResult := '{"success": false, "error": "Failed to create temp file"}';
    end
    else
    begin
      try
        // Assume binary data is array of doubles (X,Y,Z triplets)
        var PointCount := DataSize div (3 * SizeOf(Double));
        var Points := PDouble(PointData);
        var TextData := '';
        
        // Convert to text format: "X Y Z" per line
        for var i := 0 to PointCount - 1 do
        begin
          var X := Points[i * 3 + 0];
          var Y := Points[i * 3 + 1];  
          var Z := Points[i * 3 + 2];
          TextData := TextData + Format('%.6f %.6f %.6f'#13#10, [X, Y, Z]);
        end;
        
        var TextBytes := TEncoding.UTF8.GetBytes(TextData);
        var BytesWritten: DWORD;
        WriteFile(FileHandle, TextBytes[0], Length(TextBytes), BytesWritten, nil);
        CloseHandle(FileHandle);
        
        // Call main.py with text file (using existing main.py API)
        Command := Format('python "C:\CylinderCenterlineApp\app_py\main.py" "%s"', [TextFile]);
        
        // Setup process with stdout capture
        ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
        StartupInfo.cb := SizeOf(StartupInfo);
        StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        StartupInfo.wShowWindow := SW_HIDE;
        
        // Create pipe for stdout
        var ReadPipe, WritePipe: THandle;
        var SecurityAttr: TSecurityAttributes;
        SecurityAttr.nLength := SizeOf(SecurityAttr);
        SecurityAttr.bInheritHandle := True;
        SecurityAttr.lpSecurityDescriptor := nil;
        
        if not CreatePipe(ReadPipe, WritePipe, @SecurityAttr, 0) then
        begin
          TestResult := '{"success": false, "error": "Failed to create stdout pipe"}';
        end
        else
        begin
          StartupInfo.hStdOutput := WritePipe;
          StartupInfo.hStdError := WritePipe;
          
          // Execute 64-bit Python
          if CreateProcess(nil, PChar(Command), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
          begin
            try
              CloseHandle(WritePipe); // Close our copy
              
              // Wait for completion (30 seconds max)
              if WaitForSingleObject(ProcessInfo.hProcess, 30000) = WAIT_OBJECT_0 then
              begin
                GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
                
                // Read stdout
                var OutputData: TBytes;
                SetLength(OutputData, 0);
                var Buffer: array[0..4095] of Byte;
                var BytesRead: DWORD;
                
                while ReadFile(ReadPipe, Buffer[0], SizeOf(Buffer), BytesRead, nil) and (BytesRead > 0) do
                begin
                  var OldLen := Length(OutputData);
                  SetLength(OutputData, OldLen + BytesRead);
                  Move(Buffer[0], OutputData[OldLen], BytesRead);
                end;
                
                if Length(OutputData) > 0 then
                begin
                  var FullOutput := TEncoding.UTF8.GetString(OutputData);
                  
                  // Extract JSON from output (look for the line after "📋 JSON RESULT FOR DELPHI:")
                  var Lines := FullOutput.Split([#13#10, #10]);
                  var JsonFound := False;
                  for var Line in Lines do
                  begin
                    if JsonFound and (Trim(Line) <> '') then
                    begin
                      TestResult := Trim(Line);
                      Break;
                    end;
                    if Line.Contains('JSON RESULT FOR DELPHI') then
                      JsonFound := True;
                  end;
                  
                  if not JsonFound then
                    TestResult := Format('{"success": false, "error": "No JSON result found in output, exit code: %d"}', [ExitCode]);
                end
                else
                begin
                  TestResult := Format('{"success": false, "error": "main.py produced no output, exit code: %d"}', [ExitCode]);
                end;
              end
              else
              begin
                TerminateProcess(ProcessInfo.hProcess, 1);
                TestResult := '{"success": false, "error": "main.py timeout"}';
              end;
            finally
              CloseHandle(ProcessInfo.hProcess);
              CloseHandle(ProcessInfo.hThread);
              CloseHandle(ReadPipe);
            end;
          end
          else
          begin
            CloseHandle(ReadPipe);
            CloseHandle(WritePipe);
            TestResult := Format('{"success": false, "error": "Failed to start main.py: %d"}', [GetLastError]);
          end;
        end;
        
        // Cleanup temp files
        if FileExists(TextFile) then DeleteFile(PChar(TextFile));
        if FileExists(ResultFile) then DeleteFile(PChar(ResultFile));
        
      except
        on E: Exception do
        begin
          if FileHandle <> INVALID_HANDLE_VALUE then CloseHandle(FileHandle);
          TestResult := Format('{"success": false, "error": "File operation failed: %s"}', [E.Message]);
        end;
      end;
    end;
    
    // Allocate and return result string
    ResultStr := AnsiString(TestResult);
    BufSize := Length(ResultStr) + 1;
    GetMem(ResultJSON, BufSize);
    StrLCopy(ResultJSON, PAnsiChar(ResultStr), BufSize - 1);
    
    Result := 0; // Success
  except
    on E: Exception do
    begin
      // Return error
      TestResult := Format('{"success": false, "error": "Bridge32 error: %s"}', [E.Message]);
      ResultStr := AnsiString(TestResult);
      BufSize := Length(ResultStr) + 1;
      GetMem(ResultJSON, BufSize);
      StrLCopy(ResultJSON, PAnsiChar(ResultStr), BufSize - 1);
      Result := -2; // Error
    end;
  end;
end;

// Clean memory management
procedure CB_FreeMemory(P: PAnsiChar); stdcall;
begin
  if P <> nil then
    FreeMem(P);
end;

exports
  CB_ProcessPointCloud,
  CB_FreeMemory;

begin
end.