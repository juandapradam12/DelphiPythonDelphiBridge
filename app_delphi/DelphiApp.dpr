program DelphiApp;

uses
  Vcl.Forms,
  MainForm in 'forms\MainForm.pas' {Form1},
  PyEngineService in 'services\PyEngineService.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
