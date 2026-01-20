unit PyEngineService;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, Winapi.Windows,
  PythonEngine, VarPyth;

type
  TPyEngineService = class
  private
    FPython: TPythonEngine;
    function ProjectRootFromExe: string;
    procedure AddToSysPath(const Path: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnsureReady;
    property Python: TPythonEngine read FPython;
  end;

var
  PyEngine: TPyEngineService;

implementation

{ TPyEngineService }

constructor TPyEngineService.Create;
var
  RootDir, PyEmbed, PyApp, DllPath: string;
begin
  RootDir := ProjectRootFromExe;
  PyEmbed := TPath.Combine(RootDir, 'external_libraries\python-3.11.9-embed-amd64');
  PyApp   := TPath.Combine(RootDir, 'app_py');
  DllPath := TPath.Combine(PyEmbed, 'python311.dll');

  if not FileExists(DllPath) then
    raise Exception.CreateFmt('Python DLL not found at: %s', [DllPath]);

  FPython := TPythonEngine.Create(nil);
  FPython.UseLastKnownVersion := False;
  FPython.DllName := DllPath;        // full path
  FPython.PythonHome := PyEmbed;

  SetDllDirectory(PChar(PyEmbed));   // optional, but helps Windows locate DLL dependencies

  // Load Python DLL
  FPython.LoadDll;

  // Add app_py to sys.path
  AddToSysPath(PyApp);
end;

destructor TPyEngineService.Destroy;
begin
  FPython.Free;
  inherited;
end;

procedure TPyEngineService.AddToSysPath(const Path: string);
begin
  //  Use os.path.normpath so spaces/accents don’t break
  GetPythonEngine.ExecString(
    'import sys, os' + sLineBreak +
    'p = os.path.normpath(r"' + Path + '")' + sLineBreak +
    'sys.path.insert(0, p) if p not in sys.path else None'
  );
end;

function TPyEngineService.ProjectRootFromExe: string;
var
  AppDir: string;
begin
  // exe is usually at: app_delphi\Win64\Debug\DelphiApp.exe
  AppDir := TPath.GetDirectoryName(ParamStr(0));
  Result := TDirectory.GetParent(TDirectory.GetParent(TDirectory.GetParent(AppDir)));
end;

procedure TPyEngineService.EnsureReady;
begin
  if (FPython <> nil) and not FPython.IsHandleValid then
    FPython.LoadDll;
end;

initialization
  PyEngine := TPyEngineService.Create;

finalization
  PyEngine.Free;

end.

