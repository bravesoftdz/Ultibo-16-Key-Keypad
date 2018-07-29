program KeypadLibTst;

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
  DefaultFonts,
  KeyPadlib;

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
  KeyPad: TKeyPad;

procedure Setup();
begin
  {Let's create a console window again but this time on the left side of the screen}
  Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  {To prove that worked let's output some text on the console window}
  ConsoleWindowWriteLn(Handle, 'Welcome to 4x4 Keypad test');

  try
    // initialize display
    myGLCD := TLCD5110.Create(stBCM2836, $3F);
    myGLCD.SetFont(SmallFont);
    myGLCD.ClrScr();
    myGLCD.Print('4x4 Keypad', CENTER, 0);
    myGLCD.Print('->', LEFT, 15);
    myGLCD.Update();

    // Create Keypad access component
    KeyPad := TKeyPad.Create(4, 4, @Keys, @RowPins, @ColPins);
  except
    on E: Exception do
    begin
      myGLCD := nil;
      ConsoleWindowWriteLn(Handle, 'Setup() error 1 : ' + E.Message);
      Exit;
    end;
  end;
end;

procedure Loop();
var
  c: Char;
begin
  try
    c := KeyPad.GetKey();

    if c <> #0 then
      myGLCD.Print('-> ' + c, LEFT, 15);
    //else
    //  myGLCD.Print('->  ', LEFT, 15);

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
    Loop();

  ConsoleWindowWriteLn(Handle, #13#10'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

