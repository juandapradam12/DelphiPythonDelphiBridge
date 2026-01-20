unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  PythonEngine, VarPyth, PyEngineService, System.JSON, System.IOUtils;

type
  TForm1 = class(TForm)
    BtnRun: TButton;
    Memo1: TMemo;
    procedure BtnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    function LoadFileAndConvertToBytes(const FileName: string): TBytes;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Cylinder Analysis App - Memory Data Passing');
  Memo1.Lines.Add('Enter file path and press Run for ultra-fast analysis');
  Memo1.Lines.Add('💡 Example: C:\CylinderCenterlineApp\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt');
end;

procedure TForm1.BtnRunClick(Sender: TObject);
var
  PyMemoryMain, PyRes: Variant;
  PathStr: string;
  RawJSON, PrettyJSON: string;
  JSONValue: TJSONValue;
  RawBytes: TBytes;
  CylinderName: string;
  StartTime: Cardinal;
  PyBytesObj: PPyObject;  // For creating Python bytes object
begin
  // Get path from memo or fallback to default file
  if (Memo1.Lines.Count > 0) and (Trim(Memo1.Lines[0]) <> '') then
    PathStr := Trim(Memo1.Lines[0])
  else
    PathStr := 'C:\CylinderCenterlineApp\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt';

  // Convert to absolute path if it's relative
  if not TPath.IsPathRooted(PathStr) then
  begin
    PathStr := TPath.Combine(ExtractFilePath(ParamStr(0)), PathStr);
    PathStr := TPath.GetFullPath(PathStr);
  end;

  // Reset memo for new output
  Memo1.Lines.Clear;
  Memo1.Lines.Add('--- Memory Data Passing Analysis ---');
  Memo1.Lines.Add('Input path: ' + PathStr);
  
  // Check if file exists
  if not FileExists(PathStr) then
  begin
    Memo1.Lines.Add('❌ ERROR: File not found!');
    Memo1.Lines.Add('Please enter a valid file path in the memo above');
    Exit;
  end;

  StartTime := GetTickCount;

  try
    // Parse file and convert to IEEE 754 double bytes (Delphi does parsing)
    RawBytes := LoadFileAndConvertToBytes(PathStr);
    Memo1.Lines.Add(Format('✅ Converted %d points to %d bytes of IEEE 754 doubles', 
      [Length(RawBytes) div (3 * SizeOf(Double)), Length(RawBytes)]));

    // Initialize Python environment
    PyEngine.EnsureReady;

    // Extract cylinder name from filename
    CylinderName := ChangeFileExt(ExtractFileName(PathStr), '');

    // Call memory analysis with pre-parsed numeric bytes
    Memo1.Lines.Add('🚀 Calling Python with memory data...');
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

    // Show timing and results
    Memo1.Lines.Add('---');
    Memo1.Lines.Add(Format('Memory analysis completed in %d ms', [GetTickCount - StartTime]));
    Memo1.Lines.Add('--- Results ---');
    Memo1.Lines.Add(PrettyJSON);
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('---');
      Memo1.Lines.Add('Delphi error: ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end;

function TForm1.LoadFileAndConvertToBytes(const FileName: string): TBytes;
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
      
      // Skip empty lines and comments
      if (Line = '') or (Line.StartsWith('#')) or (Line.StartsWith('//')) then
        Continue;
      
      // Replace tabs with spaces and split
      Line := StringReplace(Line, #9, ' ', [rfReplaceAll]);
      Parts := Line.Split([',', ' '], TStringSplitOptions.ExcludeEmpty);
      
      if Length(Parts) >= 3 then
      begin
        try
          X := StrToFloat(Parts[0], USFormatSettings);
          Y := StrToFloat(Parts[1], USFormatSettings);
          Z := StrToFloat(Parts[2], USFormatSettings);
          
          // Add X, Y, Z to data array
          DataArray[DataCount] := X;
          DataArray[DataCount + 1] := Y;
          DataArray[DataCount + 2] := Z;
          Inc(DataCount, 3);
        except
          on E: Exception do
          begin
            // Skip invalid lines - could be header or malformed data
            // For debugging, uncomment: raise Exception.Create('Parse error line ' + IntToStr(i+1) + ': ' + E.Message);
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

end.

