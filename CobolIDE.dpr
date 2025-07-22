program CobolIDE;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FCobolIDE},
  CobolHighlighter in 'CobolHighlighter.pas',
  SearchForm in 'SearchForm.pas' {fSearch};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFCobolIDE, FCobolIDE);
  Application.CreateForm(TfSearch, fSearch);
  Application.Run;
end.
