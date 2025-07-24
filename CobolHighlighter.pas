unit CobolHighlighter;

interface

uses
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Graphics, System.Types,
  Vcl.ComCtrls, Winapi.Messages, Winapi.Windows;

type
  TCobolHighlighter = class
  private
    FRichEdit: TRichEdit;

    // Ключевые слова
    FInstructions, FDeclarations, FIntrinsics, FFigurativeConstants,
    FSpecialSections, FLevelKeywords, FContextKeywords: TStringList;
    FPreprocStart, FPreprocEnd: TStringList;

    // Цвета
    ColorInstruction, ColorDeclaration, ColorIntrinsic, ColorFigConst,
    ColorNumber, ColorString, ColorChar, ColorOperator, ColorComment,
    ColorPreprocessor, ColorLevel, ColorPIC, ColorSpecialSection,
    ColorContext, ColorFiller, ColorCondition: TColor;

    // Комментарии и прочее
    FSingleLineStart: Char;
    FInlineComment: string;
    FContinuationChar: Char;
    FContinuationColumn: Integer;

    FHighlightOnlyChanged: Boolean;
    FCurrentZoneAStart: Integer;
    FCurrentZoneBStart: Integer;
    FCurrentZoneBEnd: Integer;

    procedure LoadKeywordsAndColors;
    procedure LoadCommentsAndContinuation;
    procedure StyleRange(StartPos, Len: Integer; Color: TColor;
      Bold, Italic: Boolean);
    function IsOperatorChar(C: Char): Boolean;
    function CleanTokenStr(const S: string): string;
    function GetColumnPosition(LineIndex, CharPos: Integer): Integer;
    function IsInZoneA(LineIndex, CharPos: Integer): Boolean;
    function IsInZoneB(LineIndex, CharPos: Integer): Boolean;
    function ExtractPICFormat(const Line: string; StartPos: Integer): string;
    procedure ProcessLine(LineIndex: Integer);
    procedure HighlightAllLines;
    procedure ProcessLevelDeclarations(LineIndex: Integer; const LineText: string; LineStart: Integer);
    procedure ProcessPICDeclarations(LineIndex: Integer; const LineText: string; LineStart: Integer);
    procedure ProcessContextKeywords(LineIndex: Integer; const LineText: string; LineStart: Integer);

  public
    constructor Create(RichEdit: TRichEdit);
    destructor Destroy; override;

    procedure Highlight; overload;
    procedure HighlightLine(LineIndex: Integer); overload;

    property HighlightOnlyChanged: Boolean read FHighlightOnlyChanged
      write FHighlightOnlyChanged;
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
  FSpecialSections := TStringList.Create;
  FLevelKeywords := TStringList.Create;
  FContextKeywords := TStringList.Create;
  FPreprocStart := TStringList.Create;
  FPreprocEnd := TStringList.Create;

  LoadKeywordsAndColors;
  LoadCommentsAndContinuation;

  FRichEdit.Font.Name := 'Consolas';
  FRichEdit.Font.Size := 10;
  FRichEdit.WordWrap := False;

  FHighlightOnlyChanged := False;

  // Установка стандартных зон COBOL
  FCurrentZoneAStart := 1;
  FCurrentZoneBStart := 8;
  FCurrentZoneBEnd := 72;
end;

destructor TCobolHighlighter.Destroy;
begin
  FInstructions.Free;
  FDeclarations.Free;
  FIntrinsics.Free;
  FFigurativeConstants.Free;
  FSpecialSections.Free;
  FLevelKeywords.Free;
  FContextKeywords.Free;
  FPreprocStart.Free;
  FPreprocEnd.Free;
  inherited;
end;

procedure TCobolHighlighter.LoadKeywordsAndColors;
begin
  // Основные инструкции
  FInstructions.CommaText :=
    'OPEN,CLOSE,WRITE,MOVE,PERFORM,STOP,IF,ELSE,END-IF,GO TO,READ,ACCEPT,DISPLAY,COMPUTE,INITIALIZE,RETURN,STOP RUN,EVALUATE,WHEN,ALSO,END-EVALUATE,STRING,UNSTRING,INSPECT,TALLYING,REPLACING,UNSTRING,SEARCH,SET';

  // Декларации разделов
  FDeclarations.CommaText :=
    'PROGRAM-ID,DATA DIVISION,WORKING-STORAGE SECTION,FILE SECTION,ENVIRONMENT DIVISION,PROCEDURE DIVISION,INPUT-OUTPUT SECTION,FILE-CONTROL,LINKAGE SECTION,SPECIAL-NAMES,CONFIGURATION SECTION,LOCAL-STORAGE SECTION';

  // Встроенные функции
  FIntrinsics.CommaText :=
    'DISPLAY,ACCEPT,COMPUTE,MULTIPLY,DIVIDE,ADD,SUBTRACT,INITIALIZE,STOP,EXIT,CONTINUE,LENGTH,WHEN-COMPILED';

  // Фигуративные константы
  FFigurativeConstants.CommaText :=
    'ZERO,ZEROS,SPACES,HIGH-VALUES,LOW-VALUES,QUOTE,QUOTES,ALL,NULL,NULLS,SPACE';

  // Специальные секции
  FSpecialSections.CommaText :=
    'LINKAGE SECTION,SPECIAL-NAMES,LOCAL-STORAGE SECTION,REPORT SECTION,FILE SECTION';

  // Уровни переменных
  FLevelKeywords.CommaText :=
    '01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,66,77,88';

  // Контекстные ключевые слова
  FContextKeywords.CommaText :=
    'FILLER,RENAMES,THRU,THROUGH,USAGE,COMP,COMP-1,COMP-2,COMP-3,SYNC,SYNCHRONIZED,OCCURS,INDEXED,BY,TO,VALUE,VALUES,PIC,PICTURE';

  // Препроцессорные директивы
  FPreprocStart.CommaText := 'EXEC,COPY,EXEC SQL,EXEC CICS';
  FPreprocEnd.CommaText := 'END-EXEC';

  // Цвета (BGR формат)
  ColorInstruction := $2E7D4F;     // тёмно-зелёный
  ColorDeclaration := $8A4678;     // насыщенный пурпурный
  ColorIntrinsic := $0047B3;       // глубокий синий
  ColorFigConst := $002F9C;        // ярко-синий, но темнее и насыщеннее
  ColorNumber := $009FCF;          // бирюзовый, чуть темнее, но чистый
  ColorString := $0047B3;          // такой же синий как Intrinsic
  ColorChar := $002F9C;            // ярко-синий, совпадает с FigConst
  ColorOperator := $507D7F;        // приглушённый серо-голубой
  ColorComment := $5A4F3C;         // более тёмный коричневато-серый
  ColorPreprocessor := $8A4678;    // такой же как Declaration
  ColorLevel := $FF6600;           // оранжевый для уровней
  ColorPIC := $008080;             // темно-бирюзовый для PIC
  ColorSpecialSection := $800080;  // фиолетовый для специальных секций
  ColorContext := $008000;         // темно-зеленый для контекстных слов
  ColorFiller := $808080;          // серый для FILLER
  ColorCondition := $FF00FF;       // магента для условий (88 уровней)
end;

procedure TCobolHighlighter.LoadCommentsAndContinuation;
begin
  FSingleLineStart := '*';
  FInlineComment := '*>';

  FContinuationChar := '-';
  FContinuationColumn := 7;
end;

procedure TCobolHighlighter.StyleRange(StartPos, Len: Integer; Color: TColor;
  Bold, Italic: Boolean);
var
  StyleSet: TFontStyles;
begin
  if (StartPos < 0) or (Len <= 0) then
    Exit;

  FRichEdit.SelStart := StartPos;
  FRichEdit.SelLength := Len;
  FRichEdit.SelAttributes.Color := Color;
  StyleSet := [];
  if Bold then
    Include(StyleSet, fsBold);
  if Italic then
    Include(StyleSet, fsItalic);
  FRichEdit.SelAttributes.Style := StyleSet;
end;

function TCobolHighlighter.IsOperatorChar(C: Char): Boolean;
const
  Ops: TSysCharSet = ['+', '-', '*', '/', '=', '<', '>', '.', ',', '(', ')',
    '[', ']', ':', ';'];
begin
  Result := CharInSet(C, Ops);
end;

function TCobolHighlighter.CleanTokenStr(const S: string): string;
var
  k: Integer;
begin
  Result := '';
  for k := 1 to Length(S) do
    if CharInSet(S[k], ['A'..'Z', 'a'..'z', '0'..'9', '-']) then
      Result := Result + S[k];
end;

function TCobolHighlighter.GetColumnPosition(LineIndex, CharPos: Integer): Integer;
var
  LineText: string;
  i: Integer;
  CurrentColumn: Integer;
begin
  if (LineIndex < 0) or (LineIndex >= FRichEdit.Lines.Count) then
  begin
    Result := -1;
    Exit;
  end;

  LineText := FRichEdit.Lines[LineIndex];
  if (CharPos < 1) or (CharPos > Length(LineText)) then
  begin
    Result := -1;
    Exit;
  end;

  // Вычисляем позицию символа в колонке (учитывая табуляцию как 8 пробелов)
  CurrentColumn := 1;
  for i := 1 to CharPos - 1 do
  begin
    if (i <= Length(LineText)) and (LineText[i] = #9) then
      CurrentColumn := ((CurrentColumn - 1) div 8 + 1) * 8 + 1
    else
      Inc(CurrentColumn);
  end;

  Result := CurrentColumn;
end;

function TCobolHighlighter.IsInZoneA(LineIndex, CharPos: Integer): Boolean;
var
  Column: Integer;
begin
  Column := GetColumnPosition(LineIndex, CharPos);
  Result := (Column >= FCurrentZoneAStart) and (Column <= 7);
end;

function TCobolHighlighter.IsInZoneB(LineIndex, CharPos: Integer): Boolean;
var
  Column: Integer;
begin
  Column := GetColumnPosition(LineIndex, CharPos);
  Result := (Column >= FCurrentZoneBStart) and (Column <= FCurrentZoneBEnd);
end;

function TCobolHighlighter.ExtractPICFormat(const Line: string; StartPos: Integer): string;
var
  i, EndPos: Integer;
  InParentheses: Boolean;
begin
  Result := '';
  if StartPos > Length(Line) then
    Exit;

  // Ищем начало PIC формата
  i := StartPos;
  while (i <= Length(Line)) and (Line[i] = ' ') do
    Inc(i);

  if i > Length(Line) then
    Exit;

  // Извлекаем PIC формат
  EndPos := i;
  InParentheses := False;

  while (EndPos <= Length(Line)) and not (CharInSet(Line[EndPos], [' ', #9, '.', ',']) and not InParentheses) do
  begin
    if Line[EndPos] = '(' then
      InParentheses := True
    else if Line[EndPos] = ')' then
      InParentheses := False;
    Inc(EndPos);
  end;

  if EndPos > i then
    Result := Copy(Line, i, EndPos - i);
end;

procedure TCobolHighlighter.ProcessLevelDeclarations(LineIndex: Integer; const LineText: string; LineStart: Integer);
var
  i, j: Integer;
  Token: string;
  LevelNum: Integer;
  Column: Integer;
begin
  // Ищем уровни переменных в начале строки
  i := 1;
  while (i <= Length(LineText)) and (LineText[i] = ' ') do
    Inc(i);

  if i > Length(LineText) then
    Exit;

  // Проверяем, начинается ли строка с уровня (01-49, 66, 77, 88)
  j := i;
  while (j <= Length(LineText)) and (LineText[j] in ['0'..'9']) do
    Inc(j);

  if j > i then
  begin
    Token := Copy(LineText, i, j - i);
    if TryStrToInt(Token, LevelNum) and (FLevelKeywords.IndexOf(Token) >= 0) then
    begin
      Column := GetColumnPosition(LineIndex, i);
      // Уровни 01 и 77 должны начинаться в зоне A
      if ((LevelNum = 1) or (LevelNum = 77)) and (Column >= FCurrentZoneAStart) and (Column <= 7) then
      begin
        StyleRange(LineStart + i - 1, Length(Token), ColorLevel, True, False);
        // Если уровень 88, то это условие
        if LevelNum = 88 then
        begin
          StyleRange(LineStart + i - 1, Length(Token), ColorCondition, True, False);
        end;
      end
      else if (LevelNum in [1..49, 66, 88]) then
      begin
        StyleRange(LineStart + i - 1, Length(Token), ColorLevel, True, False);
        // Если уровень 88, то это условие
        if LevelNum = 88 then
        begin
          StyleRange(LineStart + i - 1, Length(Token), ColorCondition, True, False);
        end;
      end;
    end;
  end;
end;

procedure TCobolHighlighter.ProcessPICDeclarations(LineIndex: Integer; const LineText: string; LineStart: Integer);
var
  PICPos, i, j, Offset: Integer;
  PICFormat: string;
  PICKeyword: string;
begin
  // Ищем PIC или PICTURE
  PICPos := Pos('PIC ', UpperCase(LineText));
  if PICPos = 0 then
    PICPos := Pos('PICTURE ', UpperCase(LineText));

  if PICPos > 0 then
  begin
    // Подсвечиваем ключевое слово PIC/PICTURE
    if Copy(LineText, PICPos, 3) = 'PIC' then
    begin
      StyleRange(LineStart + PICPos - 1, 3, ColorContext, True, False);
      Offset := 4;
      PICKeyword := 'PIC';
    end
    else
    begin
      StyleRange(LineStart + PICPos - 1, 7, ColorContext, True, False);
      Offset := 8;
      PICKeyword := 'PICTURE';
    end;

    // Извлекаем и подсвечиваем формат PIC
    PICFormat := ExtractPICFormat(LineText, PICPos + Offset);
    if PICFormat <> '' then
    begin
      i := PICPos;
      while (i <= Length(LineText)) and not (UpperCase(Copy(LineText, i, Length(PICFormat))) = UpperCase(PICFormat)) do
        Inc(i);

      if i <= Length(LineText) then
        StyleRange(LineStart + i - 1, Length(PICFormat), ColorPIC, False, False);
    end;
  end;
end;

procedure TCobolHighlighter.ProcessContextKeywords(LineIndex: Integer; const LineText: string; LineStart: Integer);
var
  i, j: Integer;
  Token, UpperToken: string;
begin
  i := 1;
  while i <= Length(LineText) do
  begin
    if CharInSet(LineText[i], [' ', #9]) then
    begin
      Inc(i);
      Continue;
    end;

    // Извлекаем токен
    Token := '';
    j := i;
    while (j <= Length(LineText)) and not CharInSet(LineText[j], [' ', #9]) do
    begin
      Token := Token + LineText[j];
      Inc(j);
    end;

    UpperToken := UpperCase(Token);

    // Подсвечиваем контекстные ключевые слова
    if FContextKeywords.IndexOf(UpperToken) >= 0 then
    begin
      if UpperToken = 'FILLER' then
        StyleRange(LineStart + i - 1, Length(Token), ColorFiller, True, False)
      else
        StyleRange(LineStart + i - 1, Length(Token), ColorContext, True, False);
    end
    // Подсвечиваем специальные секции
    else if FSpecialSections.IndexOf(UpperToken) >= 0 then
    begin
      if IsInZoneB(LineIndex, i) then
        StyleRange(LineStart + i - 1, Length(Token), ColorSpecialSection, True, False);
    end;

    i := j;
  end;
end;

procedure TCobolHighlighter.ProcessLine(LineIndex: Integer);
var
  LineText: string;
  LineStart, PosInLine, TokenStart, j: Integer;
  Token, UpperToken, CleanToken: string;
  DummyFloat: Extended;
  InString: Boolean;
  QuoteChar: Char;
begin
  LineText := FRichEdit.Lines[LineIndex];
  LineStart := FRichEdit.Perform(EM_LINEINDEX, LineIndex, 0);

  // Очистить предыдущий стиль строки
  FRichEdit.SelStart := LineStart;
  FRichEdit.SelLength := Length(LineText);
  FRichEdit.SelAttributes.Color := clWindowText;
  FRichEdit.SelAttributes.Style := [];

  // Обработка комментариев
  if (Length(LineText) > 0) and (LineText[1] = FSingleLineStart) then
  begin
    StyleRange(LineStart, Length(LineText), ColorComment, False, False);
    Exit;
  end;

  // Обработка встроенных комментариев
  j := Pos(FInlineComment, LineText);
  if j > 0 then
  begin
    StyleRange(LineStart + j - 1, Length(LineText) - j + 1, ColorComment, False, False);
    SetLength(LineText, j - 1);
  end;

  // Обработка уровней переменных
  ProcessLevelDeclarations(LineIndex, LineText, LineStart);

  // Обработка PIC форматов
  ProcessPICDeclarations(LineIndex, LineText, LineStart);

  // Обработка контекстных ключевых слов
  ProcessContextKeywords(LineIndex, LineText, LineStart);

  // Основная обработка токенов
  PosInLine := 1;
  InString := False;
  QuoteChar := #0;

  while PosInLine <= Length(LineText) do
  begin
    if not InString and CharInSet(LineText[PosInLine], [' ', #9]) then
    begin
      Inc(PosInLine);
      Continue;
    end;

    // Обработка строк
    if not InString and ((LineText[PosInLine] = '''') or (LineText[PosInLine] = '"')) then
    begin
      InString := True;
      QuoteChar := LineText[PosInLine];
      TokenStart := PosInLine;
      j := PosInLine + 1;
      while (j <= Length(LineText)) and (LineText[j] <> QuoteChar) do
        Inc(j);
      if j <= Length(LineText) then
        Inc(j);
      StyleRange(LineStart + TokenStart - 1, j - TokenStart, ColorString, False, False);
      PosInLine := j;
      InString := False;
      QuoteChar := #0;
      Continue;
    end;

    if InString then
    begin
      Inc(PosInLine);
      Continue;
    end;

    // Обработка операторов
    if IsOperatorChar(LineText[PosInLine]) then
    begin
      StyleRange(LineStart + PosInLine - 1, 1, ColorOperator, False, False);
      Inc(PosInLine);
      Continue;
    end;

    // Извлечение токена
    Token := '';
    j := PosInLine;
    while (j <= Length(LineText)) and not CharInSet(LineText[j], [' ', #9]) and
      not IsOperatorChar(LineText[j]) do
    begin
      Token := Token + LineText[j];
      Inc(j);
    end;

    if Token = '' then
    begin
      Inc(PosInLine);
      Continue;
    end;

    UpperToken := UpperCase(Token);
    CleanToken := UpperCase(CleanTokenStr(Token));

    // Числа
    if TryStrToFloat(Token, DummyFloat) then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorNumber, False, False)
    // Препроцессорные директивы
    else if (FPreprocStart.IndexOf(UpperToken) >= 0) or
      (FPreprocEnd.IndexOf(UpperToken) >= 0) then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorPreprocessor, True, False)
    // Инструкции
    else if FInstructions.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorInstruction, True, False)
    // Декларации
    else if FDeclarations.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorDeclaration, True, False)
    // Встроенные функции
    else if FIntrinsics.IndexOf(CleanToken) >= 0 then
      StyleRange(LineStart + PosInLine - 1, Length(Token), ColorIntrinsic, True, False)
    // Фигуративные константы
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
    if FHighlightOnlyChanged then
      Exit
    else
      HighlightAllLines;
  finally
    FRichEdit.Lines.EndUpdate;
  end;
end;

procedure TCobolHighlighter.HighlightLine(LineIndex: Integer);
var
  SelStart, SelLength: Integer;
begin
  if (LineIndex < 0) or (LineIndex >= FRichEdit.Lines.Count) then
    Exit;

  SelStart := FRichEdit.SelStart;
  SelLength := FRichEdit.SelLength;

  FRichEdit.Lines.BeginUpdate;
  try
    ProcessLine(LineIndex);
  finally
    FRichEdit.Lines.EndUpdate;
  end;

  FRichEdit.SelStart := SelStart;
  FRichEdit.SelLength := SelLength;
end;

end.
