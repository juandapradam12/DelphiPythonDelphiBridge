unit PyEngineService64;

{
  PyEngineService64 - 64-bit DLL version
  
  Adapted from proven working PyEngineService.pas from desktop app
  Only difference: DLL path resolution instead of EXE path resolution
}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, Winapi.Windows,
  PythonEngine, VarPyth;

type
  TPyEngineService64 = class
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
  PyEngine64: TPyEngineService64;

implementation

{ TPyEngineService64 }

constructor TPyEngineService64.Create;
begin
  inherited;
  FPython := nil;
  FInitialized := False;
  // Don't initialize Python here - do it lazily in EnsureReady
end;

destructor TPyEngineService64.Destroy;
begin
  if FPython <> nil then
    FPython.Free;
  inherited;
end;

procedure TPyEngineService64.InitializePython;
var
  RootDir, PyEmbed, PyApp, DllPath: string;
begin
  if FInitialized then
    Exit;
    
  RootDir := ProjectRootFromDLL;
  // Use exact same 64-bit Python path as desktop app
  PyEmbed := TPath.Combine(RootDir, 'external_libraries\python-3.11.9-embed-amd64');
  PyApp   := TPath.Combine(RootDir, 'app_py');
  DllPath := TPath.Combine(PyEmbed, 'python311.dll');

  if not FileExists(DllPath) then
    raise Exception.CreateFmt('Python DLL not found at: %s', [DllPath]);

  // Use exact same initialization as desktop app
  FPython := TPythonEngine.Create(nil);
  FPython.UseLastKnownVersion := False;
  FPython.DllName := DllPath;        // full path
  FPython.PythonHome := PyEmbed;

  SetDllDirectory(PChar(PyEmbed));   // optional, but helps Windows locate DLL dependencies

  // Load Python DLL - same as desktop app
  FPython.LoadDll;

  // Add app_py to sys.path - same as desktop app
  AddToSysPath(PyApp);
  
  FInitialized := True;
end;

procedure TPyEngineService64.AddToSysPath(const Path: string);
begin
  // Exact same implementation as desktop app
  // Use os.path.normpath so spaces/accents don't break
  GetPythonEngine.ExecString(
    'import sys, os' + sLineBreak +
    'p = os.path.normpath(r"' + Path + '")' + sLineBreak +
    'sys.path.insert(0, p) if p not in sys.path else None'
  );
end;

function TPyEngineService64.ProjectRootFromDLL: string;
var
  DllDir: string;
  DllPath: array[0..MAX_PATH] of Char;
begin
  // Get DLL location instead of EXE location
  GetModuleFileName(HInstance, DllPath, MAX_PATH);
  DllDir := TPath.GetDirectoryName(string(DllPath));
  
  // DLL is in cylinder_dll subdirectory, so go up one level to project root
  Result := TPath.GetDirectoryName(DllDir);
end;

procedure TPyEngineService64.EnsureReady;
begin
  // Initialize Python lazily on first use
  if not FInitialized then
    InitializePython;
    
  // Same as desktop app - ensure DLL is loaded
  if (FPython <> nil) and not FPython.IsHandleValid then
    FPython.LoadDll;
end;

initialization
  PyEngine64 := TPyEngineService64.Create;

finalization
  PyEngine64.Free;

end.