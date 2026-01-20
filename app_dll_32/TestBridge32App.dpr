program TestBridge32App;

{
  Test application for CylinderBridge32.dll
  
  Tests the clean 32bit→64bit bridge architecture
}

uses
  Vcl.Forms,
  TestBridge32Form in 'TestBridge32Form.pas' {FormTestBridge32};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestBridge32, FormTestBridge32);
  Application.Run;
end.