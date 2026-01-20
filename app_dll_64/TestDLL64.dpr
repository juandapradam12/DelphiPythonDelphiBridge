program TestDLL64;

{$APPTYPE CONSOLE}

{
  Simple DLL test client - just like MainForm but via DLL
  
  Usage: TestDLL64.exe "C:\path\to\file.txt"
  
  Same workflow as MainForm:
  1. Load Python (one-time delay)
  2. Process file
  3. Show results
}

uses
  SysUtils, Winapi.Windows;

// DLL function declarations
function CC_Initialize(): Integer; stdcall; external 'CylinderCore64.dll';
function CC_ProcessFile(FilePath: PAnsiChar): PAnsiChar; stdcall; external 'CylinderCore64.dll';
function CC_GetVersion(): PAnsiChar; stdcall; external 'CylinderCore64.dll';
procedure CC_FreeString(Buffer: PAnsiChar); stdcall; external 'CylinderCore64.dll';
function CC_Finalize(): Integer; stdcall; external 'CylinderCore64.dll';

procedure ShowUsage;
begin
  WriteLn('Usage: TestDLL64.exe "file_path"');
  WriteLn('');
  WriteLn('Example:');
  WriteLn('  TestDLL64.exe "C:\CylinderCenterlineApp\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt"');
  WriteLn('');
  WriteLn('Note: First run takes ~47s (Python initialization), subsequent runs ~4s');
end;

procedure ProcessFile(const FilePath: string);
var
  StartTime, EndTime: TDateTime;
  Result: PAnsiChar;
begin
  WriteLn('Processing: ', ExtractFileName(FilePath));
  WriteLn('Please wait... (Python initialization + processing)');
  WriteLn('');
  
  StartTime := Now;
  try
    Result := CC_ProcessFile(PAnsiChar(AnsiString(FilePath)));
    EndTime := Now;
    
    if Result <> nil then
    begin
      WriteLn('SUCCESS - Processing completed in ', FormatDateTime('ss.zzz', EndTime - StartTime), 's');
      WriteLn('');
      WriteLn('Results:');
      WriteLn('----------------------------------------');
      WriteLn(string(AnsiString(Result)));
      WriteLn('----------------------------------------');
      CC_FreeString(Result);
    end
    else
    begin
      WriteLn('ERROR - Processing failed');
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR - Exception: ', E.Message);
    end;
  end;
end;

var
  FilePath: string;
  Version: PAnsiChar;

begin
  try
    WriteLn('TestDLL64 - CylinderCore64 DLL Test');
    WriteLn('====================================');
    
    // Check if DLL exists
    if not FileExists('CylinderCore64.dll') then
    begin
      WriteLn('ERROR: CylinderCore64.dll not found');
      WriteLn('Please ensure the DLL is in the same directory as this program.');
      ExitCode := 1;
      Exit;
    end;
    
    // Get file path from command line or prompt
    if ParamCount >= 1 then
    begin
      FilePath := ParamStr(1);
    end
    else
    begin
      // Use default test file if no parameter provided
      FilePath := '..\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt';
      if not FileExists(FilePath) then
        FilePath := 'C:\CylinderCenterlineApp\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt';
      
      if not FileExists(FilePath) then
      begin
        ShowUsage;
        ExitCode := 1;
        Exit;
      end;
      
      WriteLn('Using default test file: ', FilePath);
    end;
    
    // Validate file exists
    if not FileExists(FilePath) then
    begin
      WriteLn('ERROR: File not found: ', FilePath);
      ExitCode := 1;
      Exit;
    end;
    
    // Show version
    Version := CC_GetVersion();
    if Version <> nil then
    begin
      WriteLn('DLL Version: ', string(AnsiString(Version)));
      CC_FreeString(Version);
    end;
    WriteLn('');
    
    // Initialize DLL
    if CC_Initialize() <> 0 then
    begin
      WriteLn('ERROR: Failed to initialize DLL');
      ExitCode := 1;
      Exit;
    end;
    
    // Process the file (same as MainForm BtnRunClick)
    ProcessFile(FilePath);
    
    // Cleanup
    CC_Finalize();
    
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.