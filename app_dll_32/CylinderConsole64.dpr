program CylinderConsole64;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  PythonEngine,
  VarPyth,
  PyEngineService;

function LoadFileAndConvertToBytes(const FileName: string): TBytes;
var
  FileLines: TStringList;
  Line: string;
  Parts: TArray<string>;
  X, Y, Z: Double;
  DataArray: TArray<Double>;
  DataCount: Integer;
  i: Integer;
  USFormatSettings: TFormatSettings;
begin
  USFormatSettings := TFormatSettings.Create('en-US');
  
  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(FileName);
    SetLength(DataArray, FileLines.Count * 3);
    DataCount := 0;
    
    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      
      if (Line = '') or (Line.StartsWith('#')) or (Line.StartsWith('//')) or
         (Line.StartsWith('x')) or (Line.StartsWith('X')) or
         (Line.StartsWith('Point')) or (Line.Contains('x,y,z')) or
         (Line.Contains('X,Y,Z')) then
        Continue;
      
      Line := StringReplace(Line, #9, ' ', [rfReplaceAll]);
      Parts := Line.Split([',', ' '], TStringSplitOptions.ExcludeEmpty);
      
      if Length(Parts) >= 3 then
      begin
        try
          if (Parts[0].Trim <> '') and (Parts[1].Trim <> '') and (Parts[2].Trim <> '') and
             not Parts[0].Contains('x') and not Parts[0].Contains('X') then
          begin
            X := StrToFloat(Parts[0], USFormatSettings);
            Y := StrToFloat(Parts[1], USFormatSettings);
            Z := StrToFloat(Parts[2], USFormatSettings);
            
            DataArray[DataCount] := X;
            DataArray[DataCount + 1] := Y;
            DataArray[DataCount + 2] := Z;
            Inc(DataCount, 3);
          end;
        except
          Continue;
        end;
      end;
    end;
    
    SetLength(DataArray, DataCount);
    SetLength(Result, DataCount * SizeOf(Double));
    if DataCount > 0 then
      Move(DataArray[0], Result[0], Length(Result));
    
  finally
    FileLines.Free;
  end;
end;

function ProcessFile(const FilePath: string): string;
var
  PyMemoryMain, PyRes: Variant;
  RawBytes: TBytes;
  CylinderName: string;
  PyBytesObj: PPyObject;
  PyEngine: TPyEngineService;
begin
  try
    if not FileExists(FilePath) then
    begin
      Result := Format('{"success": false, "error": "Input file not found: %s"}', [FilePath]);
      Exit;
    end;
    
    // Create and initialize Python engine
    PyEngine := TPyEngineService.Create(nil);
    try
      PyEngine.EnsureReady;
      
      // Load and convert file
      RawBytes := LoadFileAndConvertToBytes(FilePath);
      CylinderName := ChangeFileExt(ExtractFileName(FilePath), '');
      
      // Process with Python
      PyMemoryMain := Import('main');
      PyBytesObj := GetPythonEngine.PyBytes_FromStringAndSize(@RawBytes[0], Length(RawBytes));
      try
        PyRes := PyMemoryMain.main(VarPythonCreate(PyBytesObj), False, CylinderName);
        Result := string(PyRes);
      finally
        GetPythonEngine.Py_DecRef(PyBytesObj);
      end;
      
    finally
      PyEngine.Free;
    end;
    
  except
    on E: Exception do
      Result := Format('{"success": false, "error": "Console processing exception: %s"}', [E.Message]);
  end;
end;

var
  FilePath: string;

begin
  try
    if ParamCount < 1 then
    begin
      WriteLn('{"success": false, "error": "Usage: CylinderConsole64.exe <input_file_path>"}');
      ExitCode := 1;
      Exit;
    end;
    
    FilePath := Trim(ParamStr(1));
    WriteLn(ProcessFile(FilePath));
    ExitCode := 0;
    
  except
    on E: Exception do
    begin
      WriteLn(Format('{"success": false, "error": "Application exception: %s"}', [E.Message]));
      ExitCode := 1;
    end;
  end;
end.