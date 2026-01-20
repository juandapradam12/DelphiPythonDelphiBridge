library CylinderCore64;

{
  CylinderCore64 DLL - 64-bit implementation
  
  Clean implementation that copies the exact working logic from MainForm.pas
  Uses existing 64-bit Python installation and Python4Delphi components
  
  Same interface as planned 32-bit version but with proven 64-bit stack
}

uses
  SysUtils,
  Classes,
  PyEngineService64 in 'PyEngineService64.pas',
  CylinderProcessor64 in 'CylinderProcessor64.pas';

{$R *.res}

// Helper function to allocate string results for DLL
function AllocateStringResult(const Str: string): PAnsiChar;
var
  AnsiStr: AnsiString;
  Size: Integer;
begin
  AnsiStr := AnsiString(Str);
  Size := Length(AnsiStr) + 1;
  GetMem(Result, Size);
  StrLCopy(Result, PAnsiChar(AnsiStr), Size - 1);
end;

// DLL Export functions - same interface as 32-bit version
function CC_Initialize(): Integer; stdcall;
begin
  try
    // Note: Python engine will be initialized lazily on first CC_ProcessFile call
    // This makes CC_Initialize fast, with the delay happening during actual processing
    Result := 0; // Success
  except
    on E: Exception do
      Result := -1; // Error
  end;
end;

function CC_ProcessFile(FilePath: PAnsiChar): PAnsiChar; stdcall;
var
  PathStr: string;
  ResultJSON: string;
begin
  try
    PathStr := string(AnsiString(FilePath));
    
    // Ensure Python is ready (lazy initialization happens here)
    PyEngine64.EnsureReady;
    
    // Call exact MainForm processing logic
    ResultJSON := ProcessCylinderFile64(PathStr);
    
    // Allocate and return result string
    Result := AllocateStringResult(ResultJSON);
  except
    on E: Exception do
    begin
      // Return error in JSON format
      ResultJSON := Format('{"success": false, "error": "%s"}', [E.Message]);
      Result := AllocateStringResult(ResultJSON);
    end;
  end;
end;

function CC_GetVersion(): PAnsiChar; stdcall;
const
  VERSION_INFO = 'CylinderCore64 DLL v1.0 - Clean 64-bit implementation';
begin
  Result := AllocateStringResult(VERSION_INFO);
end;

procedure CC_FreeString(Buffer: PAnsiChar); stdcall;
begin
  if Buffer <> nil then
    FreeMem(Buffer);
end;

function CC_Finalize(): Integer; stdcall;
begin
  try
    // Cleanup if needed - Python engine managed by service
    Result := 0; // Success
  except
    Result := -1; // Error
  end;
end;

// DLL exports section
exports
  CC_Initialize,
  CC_ProcessFile,
  CC_GetVersion,
  CC_FreeString,
  CC_Finalize;

begin
  // DLL initialization code
end.