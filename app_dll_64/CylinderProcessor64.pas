unit CylinderProcessor64;

{
  CylinderProcessor64 - Extract exact MainForm logic for 64-bit DLL
  
  Copies the working BtnRunClick and LoadFileAndConvertToBytes functions
  from MainForm.pas without any GUI dependencies
}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, Winapi.Windows,
  System.Variants, System.JSON,
  PythonEngine, VarPyth, PyEngineService64;

function ProcessCylinderFile64(const FilePath: string): string;
function LoadFileAndConvertToBytes64(const FileName: string): TBytes;

implementation

function LoadFileAndConvertToBytes64(const FileName: string): TBytes;
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
  // Exact copy of MainForm.LoadFileAndConvertToBytes
  // Parse text file and convert to IEEE 754 double bytes
  // This is exactly like your POC: parse text → numbers → bytes
  
  // Use US format settings to handle period as decimal separator
  USFormatSettings := TFormatSettings.Create('en-US');
  
  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(FileName);
    SetLength(DataArray, FileLines.Count * 3); // Pre-allocate for x,y,z per line
    DataCount := 0;
    
    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      
      // Skip empty lines, comments, and header lines
      if (Line = '') or (Line.StartsWith('#')) or (Line.StartsWith('//')) or
         (Line.StartsWith('x')) or (Line.StartsWith('X')) or
         (Line.StartsWith('Point')) or (Line.Contains('x,y,z')) or
         (Line.Contains('X,Y,Z')) then
        Continue;
      
      // Replace tabs with spaces and split
      Line := StringReplace(Line, #9, ' ', [rfReplaceAll]);
      Parts := Line.Split([',', ' '], TStringSplitOptions.ExcludeEmpty);
      
      if Length(Parts) >= 3 then
      begin
        try
          // Check if the parts look like numbers before trying to convert
          if (Parts[0].Trim <> '') and (Parts[1].Trim <> '') and (Parts[2].Trim <> '') and
             not Parts[0].Contains('x') and not Parts[0].Contains('X') and
             not Parts[1].Contains('y') and not Parts[1].Contains('Y') and
             not Parts[2].Contains('z') and not Parts[2].Contains('Z') then
          begin
            X := StrToFloat(Parts[0], USFormatSettings);
            Y := StrToFloat(Parts[1], USFormatSettings);
            Z := StrToFloat(Parts[2], USFormatSettings);
            
            // Add X, Y, Z to data array
            DataArray[DataCount] := X;
            DataArray[DataCount + 1] := Y;
            DataArray[DataCount + 2] := Z;
            Inc(DataCount, 3);
          end;
        except
          on E: Exception do
          begin
            // Skip invalid lines - could be header or malformed data
            // Silently continue to next line
            Continue;
          end;
        end;
      end;
    end;
    
    // Resize array to actual data count
    SetLength(DataArray, DataCount);
    
    // Convert TArray<Double> to TBytes (IEEE 754 double precision)
    // This is the key step: numbers → raw bytes for numpy.frombuffer
    SetLength(Result, DataCount * SizeOf(Double));
    if DataCount > 0 then
      Move(DataArray[0], Result[0], Length(Result));
    
  finally
    FileLines.Free;
  end;
end;

function ProcessCylinderFile64(const FilePath: string): string;
var
  PyMemoryMain: Variant;
  PyRes: Variant;
  RawJSON, PrettyJSON: string;
  JSONValue: TJSONValue;
  RawBytes: TBytes;
  CylinderName: string;
  StartTime: Cardinal;
  PyBytesObj: PPyObject;
begin
  // Exact copy of MainForm.BtnRunClick logic without GUI dependencies
  
  // Check if file exists
  if not FileExists(FilePath) then
    raise Exception.CreateFmt('File not found: %s', [FilePath]);

  StartTime := GetTickCount;

  try
    // Parse file and convert to IEEE 754 double bytes (Delphi does parsing)
    RawBytes := LoadFileAndConvertToBytes64(FilePath);

    // Initialize Python environment - use 64-bit service
    PyEngine64.EnsureReady;

    // Extract cylinder name from filename
    CylinderName := ChangeFileExt(ExtractFileName(FilePath), '');

    // Call memory analysis with pre-parsed numeric bytes
    PyMemoryMain := Import('main');  // Use main module (bytes-only)
    
    // Create Python bytes object using the correct Python4Delphi pattern
    // Based on Demo29 - proper way to create Python bytes from TBytes
    PyBytesObj := GetPythonEngine.PyBytes_FromStringAndSize(PAnsiChar(@RawBytes[0]), Length(RawBytes));
    try
      PyRes := PyMemoryMain.main(
        VarPythonCreate(PyBytesObj),  // Wrap the Python bytes object
        False,                        // Enable plots = False for speed
        CylinderName                  // Cylinder name
      );
    finally
      GetPythonEngine.Py_DECREF(PyBytesObj);  // Clean up the temporary Python object
    end;

    // Convert the Python result to a Delphi string
    RawJSON := VarToStr(PyRes);

    // Try to pretty-print JSON if it's valid
    PrettyJSON := RawJSON;
    try
      JSONValue := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(RawJSON), 0);
      if Assigned(JSONValue) then
      begin
        PrettyJSON := JSONValue.Format(2); // indent by 2 spaces
        JSONValue.Free;
      end;
    except
      on E: Exception do
        PrettyJSON := RawJSON; // fallback to raw output if parsing fails
    end;

    // Return the JSON result instead of displaying in memo
    Result := PrettyJSON;
    
  except
    on E: Exception do
    begin
      // Return error in JSON format
      Result := Format('{"success": false, "error": "%s", "details": "%s"}', 
        [E.ClassName, E.Message]);
    end;
  end;
end;

end.