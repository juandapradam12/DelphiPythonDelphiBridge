unit TestDLLForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormTestDLL = class(TForm)
    Panel1: TPanel;
    BtnSelectFile: TButton;
    EdtFilePath: TEdit;
    Label1: TLabel;
    BtnProcessFile: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    BtnInitialize: TButton;
    LblStatus: TLabel;
    procedure BtnSelectFileClick(Sender: TObject);
    procedure BtnProcessFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnInitializeClick(Sender: TObject);
  private
    FPythonInitialized: Boolean;
    procedure LogMessage(const Msg: string);
  public
  end;

// DLL function declarations - same as MainForm but via DLL
function CC_Initialize(): Integer; stdcall; external 'CylinderCore64.dll';
function CC_ProcessFile(FilePath: PAnsiChar): PAnsiChar; stdcall; external 'CylinderCore64.dll';
function CC_GetVersion(): PAnsiChar; stdcall; external 'CylinderCore64.dll';
procedure CC_FreeString(Buffer: PAnsiChar); stdcall; external 'CylinderCore64.dll';
function CC_Finalize(): Integer; stdcall; external 'CylinderCore64.dll';

var
  FormTestDLL: TFormTestDLL;

implementation

uses
  Vcl.FileCtrl;

{$R *.dfm}

procedure TFormTestDLL.FormCreate(Sender: TObject);
var
  Version: PAnsiChar;
begin
  FPythonInitialized := False;
  
  // Set default file path
  EdtFilePath.Text := 'C:\CylinderCenterlineApp\data\input\Point Cloud - 76.2 mm OD - 25 points total.txt';
  
  // Check DLL and show version
  if not FileExists('CylinderCore64.dll') then
  begin
    LogMessage('ERROR: CylinderCore64.dll not found in application directory');
    BtnInitialize.Enabled := False;
    BtnProcessFile.Enabled := False;
  end
  else
  begin
    Version := CC_GetVersion();
    if Version <> nil then
    begin
      LogMessage('DLL Found: ' + string(AnsiString(Version)));
      CC_FreeString(Version);
    end;
  end;
  
  BtnProcessFile.Enabled := False; // Enable after initialization
  LblStatus.Caption := 'Ready - Click Initialize first';
end;

procedure TFormTestDLL.BtnSelectFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    OpenDialog.InitialDir := 'C:\CylinderCenterlineApp\data\input';
    
    if OpenDialog.Execute then
    begin
      EdtFilePath.Text := OpenDialog.FileName;
      LogMessage('Selected file: ' + ExtractFileName(OpenDialog.FileName));
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormTestDLL.BtnInitializeClick(Sender: TObject);
begin
  if FPythonInitialized then
  begin
    LogMessage('Python already initialized');
    Exit;
  end;
  
  BtnInitialize.Enabled := False;
  LblStatus.Caption := 'Initializing...';
  LogMessage('Initializing DLL...');
  Application.ProcessMessages;
  
  try
    if CC_Initialize() = 0 then
    begin
      LogMessage('DLL initialized successfully');
      LblStatus.Caption := 'Ready to process files';
      BtnProcessFile.Enabled := True;
      FPythonInitialized := True;
    end
    else
    begin
      LogMessage('ERROR: Failed to initialize DLL');
      LblStatus.Caption := 'Initialization failed';
      BtnInitialize.Enabled := True;
    end;
  except
    on E: Exception do
    begin
      LogMessage('ERROR: Exception during initialization: ' + E.Message);
      LblStatus.Caption := 'Initialization failed';
      BtnInitialize.Enabled := True;
    end;
  end;
end;

procedure TFormTestDLL.BtnProcessFileClick(Sender: TObject);
var
  FilePath: string;
  StartTime, EndTime: TDateTime;
  Result: PAnsiChar;
begin
  FilePath := Trim(EdtFilePath.Text);
  
  if not FileExists(FilePath) then
  begin
    ShowMessage('File not found: ' + FilePath);
    Exit;
  end;
  
  if not FPythonInitialized then
  begin
    ShowMessage('Please initialize the DLL first by clicking "Initialize"');
    Exit;
  end;
  
  BtnProcessFile.Enabled := False;
  LblStatus.Caption := 'Processing... (this may take time on first run)';
  LogMessage('');
  LogMessage('=== Processing File: ' + ExtractFileName(FilePath) + ' ===');
  LogMessage('Please wait... (Python initialization + processing on first run)');
  Application.ProcessMessages;
  
  StartTime := Now;
  try
    Result := CC_ProcessFile(PAnsiChar(AnsiString(FilePath)));
    EndTime := Now;
    
    if Result <> nil then
    begin
      LogMessage('SUCCESS - Processing completed in ' + 
        FormatDateTime('ss.zzz', EndTime - StartTime) + 's');
      LogMessage('');
      LogMessage('Results:');
      LogMessage('----------------------------------------');
      Memo1.Lines.Add(string(AnsiString(Result)));
      LogMessage('----------------------------------------');
      CC_FreeString(Result);
      LblStatus.Caption := 'Processing completed successfully';
    end
    else
    begin
      LogMessage('ERROR: Processing failed - NULL result returned');
      LogMessage('This could indicate a file format issue or DLL processing error');
      LblStatus.Caption := 'Processing failed';
    end;
  except
    on E: Exception do
    begin
      LogMessage('ERROR: Exception during processing: ' + E.Message);
      LogMessage('File: ' + FilePath);
      LogMessage('This might be a file format issue - check for headers like "x y z" in the file');
      LblStatus.Caption := 'Processing failed - ' + E.ClassName;
      ShowMessage('Processing Error: ' + E.Message + #13#10 + 
                  'Please check if the file contains headers or non-numeric data that should be removed.');
    end;
  end;
  
  BtnProcessFile.Enabled := True;
end;

procedure TFormTestDLL.LogMessage(const Msg: string);
begin
  Memo1.Lines.Add(TimeToStr(Now) + ' - ' + Msg);
  // Auto-scroll to bottom
  SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  Application.ProcessMessages;
end;

end.
