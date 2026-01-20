unit TestBridge32Form;

{
  Test application for CylinderBridge32.dll - Clean 32bit→64bit bridge
  
  Tests the clean architecture:
  1. Load point cloud data into memory  
  2. Call bridge with pointer + size
  3. Bridge forwards to 64-bit engine (future)
  4. Receive JSON string back
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormTestBridge32 = class(TForm)
    Panel1: TPanel;
    BtnSelectFile: TButton;
    EdtFilePath: TEdit;
    Label1: TLabel;
    BtnProcessMemory: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    LblStatus: TLabel;
    BtnGetVersion: TButton;
    procedure BtnSelectFileClick(Sender: TObject);
    procedure BtnProcessMemoryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnGetVersionClick(Sender: TObject);
  private
    procedure LogMessage(const Msg: string);
    function LoadPointCloudToMemory(const FileName: string; out DataSize: Integer): Pointer;
  public
  end;

// Bridge32.dll functions - Clean API (no Python dependencies in 32-bit)
function CB_ProcessPointCloud(PointData: Pointer; DataSize: Integer; out ResultJSON: PAnsiChar): Integer; stdcall; external 'SimpleBridge32.dll';
procedure CB_FreeMemory(P: PAnsiChar); stdcall; external 'SimpleBridge32.dll';

var
  FormTestBridge32: TFormTestBridge32;

implementation

uses
  Vcl.FileCtrl, System.IOUtils;

{$R *.dfm}

procedure TFormTestBridge32.FormCreate(Sender: TObject);
begin
  Caption := 'Test CylinderBridge32.dll - Clean 32bit→64bit Architecture';
  EdtFilePath.Text := 'C:\CylinderCenterlineApp\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt';
  LogMessage('Form initialized. Ready to test clean 32bit→64bit bridge.');
end;

procedure TFormTestBridge32.BtnGetVersionClick(Sender: TObject);
var
  Version: PAnsiChar;
begin
  try
    LogMessage('Using SimpleBridge32.dll - Clean file communication');
    // Version info removed for simplicity
  except
    on E: Exception do
      LogMessage('ERROR during initialization: ' + E.Message);
  end;
end;

procedure TFormTestBridge32.BtnSelectFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Point Cloud Files (*.txt)|*.txt|All Files (*.*)|*.*';
    OpenDialog.InitialDir := 'C:\CylinderCenterlineApp\data\input';
    if OpenDialog.Execute then
      EdtFilePath.Text := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

function TFormTestBridge32.LoadPointCloudToMemory(const FileName: string; out DataSize: Integer): Pointer;
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
  // Load point cloud into memory as IEEE 754 doubles
  // Same logic as LoadFileAndConvertToBytes but returns pointer
  
  Result := nil;
  DataSize := 0;
  
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
      
      // Replace tabs with spaces and split on commas only (not spaces)
      Line := StringReplace(Line, #9, ',', [rfReplaceAll]);
      Parts := Line.Split([','], TStringSplitOptions.ExcludeEmpty);
      
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
          // Skip invalid lines
          Continue;
        end;
      end;
    end;
    
    // Resize array to actual data count
    SetLength(DataArray, DataCount);
    
    // Allocate memory and copy data
    if DataCount > 0 then
    begin
      DataSize := DataCount * SizeOf(Double);
      GetMem(Result, DataSize);
      Move(DataArray[0], Result^, DataSize);
    end;
    
  finally
    FileLines.Free;
  end;
end;

procedure TFormTestBridge32.BtnProcessMemoryClick(Sender: TObject);
var
  ResultJSON: PAnsiChar;
  BridgeResult: Integer;
  StartTime, BridgeStartTime: Cardinal;
  ProcessTime, BridgeTime: Double;
  FilePath: AnsiString;
begin
  if not FileExists(EdtFilePath.Text) then
  begin
    LogMessage('ERROR: File not found: ' + EdtFilePath.Text);
    Exit;
  end;

  try
    StartTime := GetTickCount;
    
    // Step 1: Pass file path directly to bridge
    LogMessage('=== Testing Clean 32bit→64bit Bridge ===');
    LogMessage('Passing file path directly to bridge...');
    LogMessage('File: ' + EdtFilePath.Text);
    
    // Convert file path to AnsiString for the bridge
    FilePath := AnsiString(EdtFilePath.Text);
    
    // Step 2: Call bridge with file path (not binary data)
    LogMessage('Calling CylinderBridge32...');
    LblStatus.Caption := 'Processing via 32bit→64bit bridge...';
    Application.ProcessMessages;
    
    BridgeStartTime := GetTickCount;
    BridgeResult := CB_ProcessPointCloud(PAnsiChar(FilePath), Length(FilePath), ResultJSON);
    BridgeTime := (GetTickCount - BridgeStartTime) / 1000.0;
    
    ProcessTime := (GetTickCount - StartTime) / 1000.0;
    
    // Step 3: Handle result
    if BridgeResult = 0 then
    begin
      LogMessage('✓ Bridge call successful');
      LogMessage(Format('⏱ Bridge processing time: %.3fs', [BridgeTime]));
      LogMessage('JSON Result:');
      LogMessage('─────────────────────────────');
      LogMessage(string(AnsiString(ResultJSON)));
      LogMessage('─────────────────────────────');
      
      LblStatus.Caption := Format('✓ Bridge completed in %.3fs (Bridge: %.3fs)', [ProcessTime, BridgeTime]);
      CB_FreeMemory(ResultJSON);
    end
    else
    begin
      LogMessage(Format('✗ Bridge call failed with code: %d (took %.3fs)', [BridgeResult, BridgeTime]));
      if ResultJSON <> nil then
      begin
        LogMessage('Error details: ' + string(AnsiString(ResultJSON)));
        CB_FreeMemory(ResultJSON);
      end;
      LblStatus.Caption := Format('Bridge call failed (%.3fs)', [BridgeTime]);
    end;
    
  except
    on E: Exception do
    begin
      LogMessage('EXCEPTION: ' + E.ClassName + ': ' + E.Message);
      LblStatus.Caption := 'Exception occurred';
    end;
  end;
end;

procedure TFormTestBridge32.LogMessage(const Msg: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  // Scroll to bottom
  SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  Application.ProcessMessages;
end;

end.