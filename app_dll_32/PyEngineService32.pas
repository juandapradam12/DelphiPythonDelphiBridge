unit PyEngineService32;

{
  PyEngineService32 - 32-bit DLL version
  
  32-bit equivalent of PyEngineService64.pas
  Uses 32-bit embedded Python instead of 64-bit
}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, Winapi.Windows,
  PythonEngine, VarPyth;

type
  TPyEngineService32 = class
  private
    FPython: TPythonEngine;
    FInitialized: Boolean;
    function ProjectRootFromDLL: string;
    procedure AddToSysPath(const Path: string);
    procedure InitializePython;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnsureReady;
    property Python: TPythonEngine read FPython;
  end;

var
  PyEngine32: TPyEngineService32;

implementation

{ TPyEngineService32 }

constructor TPyEngineService32.Create;
begin
  inherited;
  FPython := nil;
  FInitialized := False;
  // Don't initialize Python here - do it lazily in EnsureReady
end;

destructor TPyEngineService32.Destroy;
begin
  if FPython <> nil then
    FPython.Free;
  inherited;
end;

procedure TPyEngineService32.InitializePython;
var
  RootDir, PyEmbed, PyApp, DllPath: string;
  ErrorMsg: string;
begin
  if FInitialized then
    Exit;
    
  try
    RootDir := ProjectRootFromDLL;
    
    // Use 32-bit Python path instead of 64-bit
    PyEmbed := TPath.Combine(RootDir, 'external_libraries\python-3.11.9-embed-win32');
    PyApp   := TPath.Combine(RootDir, 'app_py');
    DllPath := TPath.Combine(PyEmbed, 'python311.dll');

    // Enhanced error checking with detailed paths
    if not DirectoryExists(RootDir) then
      raise Exception.CreateFmt('Project root directory not found: %s', [RootDir]);
      
    if not DirectoryExists(PyEmbed) then
      raise Exception.CreateFmt('Python embed directory not found: %s (Root: %s)', [PyEmbed, RootDir]);
      
    if not DirectoryExists(PyApp) then
      raise Exception.CreateFmt('Python app directory not found: %s', [PyApp]);
      
    if not FileExists(DllPath) then
      raise Exception.CreateFmt('Python DLL not found: %s', [DllPath]);

    // Create Python engine with explicit configuration
    FPython := TPythonEngine.Create(nil);
    
    // Critical: Set these BEFORE LoadDll
    FPython.UseLastKnownVersion := False;
    FPython.DllName := DllPath;
    FPython.PythonHome := PyEmbed;
    
    // Ensure Windows can find dependent DLLs
    SetDllDirectory(PChar(PyEmbed));
    
    // Add the Python directory to PATH temporarily for DLL dependencies
    SetEnvironmentVariable('PATH', 
      PChar(PyEmbed + ';' + GetEnvironmentVariable('PATH')));

    // Load Python DLL with detailed error handling
    try
      FPython.LoadDll;
    except
      on E: Exception do
      begin
        ErrorMsg := Format('Failed to load Python DLL: %s'#13#10'Error: %s (%s)'#13#10'DLL exists: %s'#13#10'Directory exists: %s', 
          [DllPath, E.Message, E.ClassName, 
           BoolToStr(FileExists(DllPath), True), 
           BoolToStr(DirectoryExists(PyEmbed), True)]);
        FPython.Free;
        FPython := nil;
        raise Exception.Create(ErrorMsg);
      end;
    end;

    // Verify Python is actually loaded
    if not FPython.IsHandleValid then
    begin
      ErrorMsg := Format('Python DLL loaded but handle is invalid: %s'#13#10'This usually indicates architecture mismatch (32-bit vs 64-bit)', [DllPath]);
      FPython.Free;
      FPython := nil;
      raise Exception.Create(ErrorMsg);
    end;

    // Add app_py to sys.path with error handling
    try
      AddToSysPath(PyApp);
    except
      on E: Exception do
      begin
        ErrorMsg := Format('Failed to add Python path: %s'#13#10'Error: %s', [PyApp, E.Message]);
        raise Exception.Create(ErrorMsg);
      end;
    end;
    
    FInitialized := True;
    
  except
    on E: Exception do
    begin
      if FPython <> nil then
      begin
        FPython.Free;
        FPython := nil;
      end;
      raise Exception.CreateFmt('Python initialization failed:'#13#10'%s'#13#10'Root dir: %s'#13#10'Project location: %s', 
        [E.Message, RootDir, ParamStr(0)]);
    end;
  end;
end;

procedure TPyEngineService32.AddToSysPath(const Path: string);
begin
  // Exact same implementation as 64-bit version
  // Use os.path.normpath so spaces/accents don't break
  GetPythonEngine.ExecString(
    'import sys, os' + sLineBreak +
    'p = os.path.normpath(r"' + Path + '")' + sLineBreak +
    'sys.path.insert(0, p) if p not in sys.path else None'
  );
end;

function TPyEngineService32.ProjectRootFromDLL: string;
var
  DllDir: string;
  DllPath: array[0..MAX_PATH] of Char;
begin
  // Get DLL location instead of EXE location
  GetModuleFileName(HInstance, DllPath, MAX_PATH);
  DllDir := TPath.GetDirectoryName(string(DllPath));
  
  // DLL is in app_dll_32 subdirectory, so go up one level to project root
  Result := TPath.GetDirectoryName(DllDir);
end;

procedure TPyEngineService32.EnsureReady;
begin
  // Initialize Python lazily on first use
  if not FInitialized then
    InitializePython;
    
  // Same as 64-bit version - ensure DLL is loaded
  if (FPython <> nil) and not FPython.IsHandleValid then
    FPython.LoadDll;
end;

initialization
  PyEngine32 := TPyEngineService32.Create;

finalization
  PyEngine32.Free;

end.