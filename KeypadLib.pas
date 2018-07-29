unit KeyPadLib;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils;

{
     Col   Col   Col   Col
      1     2     3     4
  +-------------------------+
  | +---+ +---+ +---+ +---+ |
  | | 1 | | 2 | | 3 | | A | |  Row 1
  | +---+ +---+ +---+ +---+ |
  | +---+ +---+ +---+ +---+ |
  | | 4 | | 5 | | 6 | | B | |  Row 2
  | +---+ +---+ +---+ +---+ |
  | +---+ +---+ +---+ +---+ |
  | | 7 | | 8 | | 9 | | C | |  Row 3
  | +---+ +---+ +---+ +---+ |
  | +---+ +---+ +---+ +---+ |
  | | * | | 0 | | # | | D | |  Row 4
  | +---+ +---+ +---+ +---+ |
  +-+--+--+--+--+--+--+--+--+
    |  |  |  |  |  |  |  |
    |  |  |  |  |  |  |  |
    R  R  R  R  C  C  C  C
    o  o  o  o  o  o  o  o
    w  w  w  w  l  l  l  l
    1  2  3  4  1  2  3  4
}

type

  { TKeyPad }

  TKeyPad = class
  private
    FRows, FCols: Byte;
    FKeys: PChar;
    FRowPins: PByte;
    FColPins: PByte;
  public
    constructor Create(Rows, Cols: Byte; Keys: PChar; RowPins, ColPins: PByte);
    function GetKey(): Char;
  end;

implementation

uses
   Platform, GlobalConst;

{ TKeyPad }

constructor TKeyPad.Create(Rows, Cols: Byte; Keys: PChar; RowPins, ColPins: PByte);
var
  i: Integer;
begin
  inherited Create;

  FRows := Rows;
  FCols := Cols;
  FKeys := Keys;
  FRowPins := RowPins;
  FColPins := ColPins;

  for i := 0 to FRows do
  begin
    // define row GPIO pin function
    GPIOFunctionSelect(FRowPins[i], GPIO_FUNCTION_OUT);

    // define initial GPIO pin state to LOW (0)
    GPIOOutputSet(FRowPins[i], GPIO_LEVEL_LOW);
  end;

  for i := 0 to FCols do
  begin
    // define the pull state of the GPIO pin
    GPIOPullSelect(FColPins[i], GPIO_PULL_DOWN);

    // define collumn GPIO pin function
    GPIOFunctionSelect(FColPins[i], GPIO_FUNCTION_IN);
  end;
end;

function TKeyPad.GetKey: Char;
var
  r, c, Times: Byte;
  x, y: Integer;
  HasKey: Boolean;
begin
  HasKey := False;

  x := -1;
  y := -1;
  Times := 0;

  // read ten times (minimum) the TKeyPad
  while Times < 10 do
  begin
    HasKey := False;

    // scan rows
    for r := 0 to FRows - 1 do
    begin
      // set row level to high
      GPIOOutputSet(FRowPins[r], GPIO_LEVEL_HIGH);

      // scan collumns
      for c := 0 to FCols - 1 do
        // if collumn pin is at high level (1) then a key is pressed
        if GPIOInputGet(FColPins[c]) <> 0 then
        begin
          HasKey := True;

          // if the current key coordinates are the same as the previous one,
          // then inc Times
          if (y = r) and (x = c) then
            Inc(Times)
          else
          begin
            // if not, reinitialize the counting
            Times := 0;
            y := r;
            x := c;
          end;

          // when a key is pressed, it isn't necessary to continue current scan
          if Haskey then
            break;
        end;

      GPIOOutputSet(FRowPins[r], GPIO_LEVEL_LOW);

      // when a key is pressed, it isn't necessary to continue current scan
      if Haskey then
        break;
    end;

    if not HasKey then
      if x = -1 then
        Inc(Times)
      else
      begin
        Times := 0;
        x := -1;
        y := -1;
      end;
  end;

  if HasKey then
    Result := FKeys[y * FRows + x]
  else
    Result := #0;
end;

end.

