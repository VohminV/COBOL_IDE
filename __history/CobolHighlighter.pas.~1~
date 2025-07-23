unit CobolHighlighter;

interface

uses
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Graphics, System.Types,
  Vcl.ComCtrls, Winapi.Messages, Winapi.Windows;

type
  TCobolHighlighter = class
  private
    FRichEdit: TRichEdit;

    //  лючевые слова
    FInstructions, FDeclarations, FIntrinsics, FFigurativeConstants: TStringList;
    FPreprocStart, FPreprocEnd: TStringList;

    // ÷вета
    ColorInstruction, ColorDeclaration, ColorIntrinsic, ColorFigConst,
    ColorNumber, ColorString, ColorChar, ColorOperator, ColorComment,
    ColorPreprocessor: TColor;

    //  омментарии и прочее
    FSingleLineStart: Char;
    FInlineComment: string;
    FContinuationChar: Char;
    FContinuationColumn: Integer;

    FHighlightOnlyChanged: Boolean;

    procedure LoadKeywordsAndColors;
    procedure LoadCommentsAndContinuation;
    procedure StyleRange(StartPos, Len: Integer; Color: TColor; Bold, Italic: Boolean);
    function IsOperatorChar(C: Char): Boolean;
    function CleanTokenStr(const S: string): string;
    procedure ProcessLine(LineIndex: Integer);
    procedure HighlightAllLines;

  public
    constructor Create(RichEdit: TRichEdit);
    destructor Destroy; override;

    procedure Highlight;
  end;

implementation

{ TCobolHighlighter }

constructor TCobolHighlighter.Create(RichEdit: TRichEdit);
begin
  inherited Create;
  FRichEdit := RichEdit;

  FInstructions := TStringList.Create;
  FDeclarations := TStringList.Create;
  FIntrinsics := TStringList.Create;
  FFigurativeConstants := TStringList.Create;

  FPreprocStart := TStringList.Create;
  FPreprocEnd := TStringList.Create;

  LoadKeywordsAndColors;
  LoadCommentsAndContinuation;

  FRichEdit.Font.Name := 'Consolas';
  FRichEdit.Font.Size := 10;
  FRichEdit.WordWrap := False;

  FHighlightOnlyChanged := False;
end;

destructor TCobolHighlighter.Destroy;
begin
  FInstructions.Free;
  FDeclarations.Free;
  FIntrinsics.Free;
  FFigurativeConstants.Free;
  FPreprocStart.Free;
  FPreprocEnd.Free;
  inherited;
end;

procedure TCobolHighlighter.LoadKeywordsAndColors;
begin
  FInstructions.CommaText := 'OPEN,CLOSE,WRITE,MOVE,PERFORM,STOP,IF,ELSE,END-IF,GO TO,READ,ACCEPT,DISPLAY,COMPUTE,INITIALIZE,RETURN,STOP RUN';
  FDeclarations.CommaText := 'PROGRAM-ID,DATA DIVISION,WORKING-STORAGE SECTION,FILE SECTION,ENVIRONMENT DIVISION,PROCEDURE DIVISION,INPUT-OUTPUT SECTION,FILE-CONTROL';
  FIntrinsics.CommaText := 'DISPLAY,ACCEPT,COMPUTE,MULTIPLY,DIVIDE,ADD,SUBTRACT,INITIALIZE,STOP,EXIT,CONTINUE';
  FFigurativeConstants.CommaText := 'ZERO,ZEROS,SPACES,HIGH-VALUES,LOW-VALUES,QUOTE,ALL';

  FPreprocStart.CommaText := 'EXEC,COPY';
  FPreprocEnd.CommaText := 'END-EXEC';

  // ÷вета
  ColorInstruction := $63C793;       // зелЄный
  ColorDeclaration := $BD82A0;       // розовый/фиолетовый
  ColorIntrinsic := $0060EC;         // синий
  ColorFigConst := $0949FF;          // €рко-синий
  ColorNumber := $22CDFF;            // бирюзовый
  ColorString := $0060EC;            // синий
  ColorChar := $0949FF;              // €рко-синий
  ColorOperator := $B7E2E8;          // бледно-жЄлтый
  ColorComment := $7B7466;           // серо-коричневый
  ColorPreprocessor := $BD82A0;      // тот же, что и декларации
end;

procedure TCobolHighlighter.LoadCommentsAndContinuation;
begin
  FSingleLineStart := '*';
  FInlineComment := '*>';

  FContinuationChar := '-';
  FContinuationColumn := 7;
end;

procedure TCobolHighlighter.StyleRange(StartPos, Len: Integer; Color: TColor; Bold, Italic: Boolean);
var
  StyleSet: TFontStyles;
begin
  FRichEdit.SelStart := StartPos;
  FRichEdit.SelLength := Len;
  FRichEdit.SelAttributes.Color := Color;
  StyleSet := [];
  if Bold then Include(StyleSet, fsBold);
  if Italic then Include(StyleSet, fsItalic);
  FRichEdit.SelAttributes.Style := StyleSet;
end;

function TCobolHighlighter.IsOperatorChar(C: Char): Boolean;
const
  Ops: TSysCharSet = ['+', '-', '*', '/', '=', '<', '>', '.', ',', '(', ')', '[', ']', ':', ';'];
begin
  Result := CharInSet(C, Ops);
end;

function TCobolHighlighter.CleanTokenStr(const S: string): string;
var
  k: Integer;
begin
  Result := '';
  for k := 1 to Length(S) do
    if CharInSet(S[k], ['A'..'Z','a'..'z','0'..'9','-']) then
      Result := Result + S[k];
end;

procedure TCobolHighlighter.ProcessLine(LineIndex: Integer);
var
  LineText: string;
  LineStart, PosInLine, TokenStart, j: Integer;
  Token, UpperToken, CleanToken: string;
  DummyFloat: Double;
begin
  LineText := FRichEdit.Lines[LineIndex];
  LineStart := FRichEdit.Perform(EM_LINEINDEX, LineIndex, 0);

  if (Length(LineText) > 0) and (LineText[1] = FSingleLineStart) then
  begin
    StyleRange(LineStart, Length(LineText), ColorComment, False, False);
    Exit;
  end;

  j := Pos(FInlineComment, LineText);
  if j > 0 then
  begin
    StyleRange(LineStart + j - 1, Length(LineText) - j + 1, ColorComment, False, False);
    SetLength(LineText, j - 1);
  end;

  PosInLine := 1;
  while PosInLine <= Length(LineText) do
  begin
    if CharInSet(LineText[PosInLine], [' ', #9]) then
    begin
      Inc(PosInLine);
      Continue;
    end;

    if (LineText[PosInLine] = '''') or (LineText[PosInLine] = '"') then
    begin
      TokenStart := PosInLine;
      j := PosInLine + 1;
      while (j <= Length(LineText)) and (LineText[j] <> LineText[PosInLine]) do
        Inc(j);
      if j <= Length(LineText) then Inc(j);
      StyleRange(LineStart + TokenStart - 1, j - TokenStart + 1, ColorString, False, False);
      PosInLine := j;
      Continue;
    end;

    if IsOperatorChar(LineText[PosInLine]) then
    begin
      StyleRange(LineStart + PosInLine - 1, 1, ColorOperator, False, False);
      Inc(PosInLine);
      Continue;
    end;

    Token := '';
    j := PosInLine;
    while (j <= Length(LineText)) and not CharInSet(LineText[j], [' ', #9]) and not IsOperatorChar(LineText[j]) do
    begin
      Token := Token + LineText[j];
      Inc(j);
    end;

    UpperToken := UpperCase(Token);
    CleanToken := UpperCase(CleanTokenStr(Token));

    if TryStrToFloat(Token, DummyFloat) then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorNumber, False, False)

    else if (FPreprocStart.IndexOf(UpperToken) >= 0) or (FPreprocEnd.IndexOf(UpperToken) >= 0) then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorPreprocessor, True, False)

    else if FInstructions.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorInstruction, True, False)

    else if FDeclarations.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorDeclaration, True, False)

    else if FIntrinsics.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorIntrinsic, True, False)

    else if FFigurativeConstants.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorFigConst, True, False);

    PosInLine := j;
  end;
end;

procedure TCobolHighlighter.HighlightAllLines;
var
  i: Integer;
begin
  FRichEdit.SelStart := 0;
  FRichEdit.SelLength := Length(FRichEdit.Text);
  FRichEdit.SelAttributes.Color := clWindowText;
  FRichEdit.SelAttributes.Style := [];

  for i := 0 to FRichEdit.Lines.Count - 1 do
    ProcessLine(i);
end;

procedure TCobolHighlighter.Highlight;
begin
  FRichEdit.Lines.BeginUpdate;
  try
    HighlightAllLines;
  finally
    FRichEdit.Lines.EndUpdate;
  end;
end;

end.

