program Keypad4x4;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  Console,
  LCD5110_Graph,
  LCD_Graphics,
  DefaultFonts;

const
  Keys: array [0..15] of Char =
  (
     '1', '2', '3', 'A',
     '4', '5', '6', 'B',
     '7', '8', '9', 'C',
     '*', '0', '#', 'D'
  );

  RowPins: array [0..3] of Byte = (GPIO_PIN_18, GPIO_PIN_23, GPIO_PIN_24, GPIO_PIN_25); //connect to row pinouts
  ColPins: array [0..3] of Byte = (GPIO_PIN_12, GPIO_PIN_16, GPIO_PIN_20, GPIO_PIN_21); //connect to column pinouts

var
  Handle: TWindowHandle;
  myGLCD: TLCD5110;

procedure Setup();
var
  r, c: Integer;
begin
  {Let's create a console window again but this time on the left side of the screen}
  Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  {To prove that worked let's output some text on the console window}
  ConsoleWindowWriteLn(Handle, 'Welcome to 4x4 Keypad test');

  try
    myGLCD := TLCD5110.Create(stBCM2836, $3F);
    myGLCD.SetFont(SmallFont);
    myGLCD.ClrScr();
    myGLCD.Print('4x4 Keypad', CENTER, 0);
    myGLCD.Update();
  except
    on E: Exception do
    begin
      myGLCD := nil;
      ConsoleWindowWriteLn(Handle, 'Setup() error 1 : ' + E.Message);
      Exit;
    end;
  end;

  for r := Low(RowPins) to High(RowPins) do
  begin
    GPIOFunctionSelect(RowPins[r], GPIO_FUNCTION_OUT);
    GPIOOutputSet(RowPins[r], GPIO_LEVEL_LOW);
  end;

  for c := Low(ColPins) to High(ColPins) do
  begin
    GPIOPullSelect(ColPins[c], GPIO_PULL_DOWN);
    GPIOFunctionSelect(ColPins[c], GPIO_FUNCTION_IN);
  end;
end;

procedure Loop();
var
  r, c: Integer;
  s: String;
begin
  try
    myGLCD.ClrScr();
    myGLCD.Print('4x4 Keypad', CENTER, 0);

    for r := Low(RowPins) to High(RowPins) do
    begin
      GPIOOutputSet(RowPins[r], GPIO_LEVEL_HIGH);

      s := '';

      for c := Low(ColPins) to High(ColPins) do
      begin
        if GPIOInputGet(ColPins[c]) = 0 then
          s := s + '0'
        else
          s := s + '1';
      end;

      GPIOOutputSet(RowPins[r], GPIO_LEVEL_LOW);

      myGLCD.Print(s, LEFT, r * 10 + 10);
    end;

    myGLCD.Update();
    Sleep(5);
  except
    on E: Exception do
    begin
      ConsoleWindowWriteLn(Handle, 'Loop() error: ' + E.Message);
      myGLCD.Free;
      myGLCD := nil;
    end;
  end;
end;

function GetKey(): Char;
var
  r, c, x, y, vezes: Integer;
  HasKey: Boolean;
begin
  HasKey := False;

  x := -1;
  y := -1;
  vezes := 0;

  while vezes < 10 do
  begin
    HasKey := False;

    for r := Low(RowPins) to High(RowPins) do
    begin
      GPIOOutputSet(RowPins[r], GPIO_LEVEL_HIGH);

      for c := Low(ColPins) to High(ColPins) do
        if GPIOInputGet(ColPins[c]) <> 0 then
        begin
          HasKey := True;

          if (y = r) and (x = c) then
            Inc(vezes)
          else
          begin
            vezes := 0;
            y := r;
            x := c;
          end;
        end;

      GPIOOutputSet(RowPins[r], GPIO_LEVEL_LOW);
    end;

    if not HasKey then
      if x = -1 then
        Inc(vezes)
      else
      begin
        vezes := 0;
        x := -1;
        y := -1;
      end;
  end;

  if HasKey then
    Result := Keys[y * 4 + x]
  else
    Result := #0;
end;

procedure Loop2();
var
  c: Char;
begin
  try
    c := GetKey();

    if c <> #0 then
      myGLCD.Print('-> ' + c, LEFT, 10)
    else
      myGLCD.Print('->  ', LEFT, 10);

    myGLCD.Update();
  except
    on E: Exception do
    begin
      ConsoleWindowWriteLn(Handle, 'Loop2() error: ' + E.Message);
      myGLCD.Free;
      myGLCD := nil;
    end;
  end;
end;

begin
  Setup();

  while Assigned(myGLCD) do
    Loop2();

  ConsoleWindowWriteLn(Handle, #13#10'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

