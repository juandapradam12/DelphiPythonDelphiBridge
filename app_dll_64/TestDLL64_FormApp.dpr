program TestDLL64_FormApp;

uses
  Vcl.Forms,
  TestDLLForm in 'TestDLLForm.pas' {FormTestDLL};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestDLL, FormTestDLL);
  Application.Run;
end.