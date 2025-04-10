program Pacman;

uses
  crt;

const
  WIDTH = 20;
  HEIGHT = 10;
  MAX_LIVES = 3;

type
  TTile = (Wall, Empty, Dot, Power);
  TMap = array[1..HEIGHT, 1..WIDTH] of TTile;
  TEntity = record x, y: integer; end;

var
  Map: TMap;
  Pacman, Ghost: TEntity;
  Lives, Score: integer;
  PowerUp: boolean;
  PowerTimer: integer;

procedure InitMap;
var i, j: integer;
begin
  for i := 1 to HEIGHT do
    for j := 1 to WIDTH do
    begin
      if (i = 1) or (j = 1) or (i = HEIGHT) or (j = WIDTH) then
        Map[i][j] := Wall
      else if ((i+j) mod 11 = 0) then
        Map[i][j] := Power
      else
        Map[i][j] := Dot;
    end;
  Map[2][2] := Empty; // Start
end;

procedure DrawMap;
var i, j: integer;
begin
  clrscr;
  for i := 1 to HEIGHT do
  begin
    for j := 1 to WIDTH do
    begin
      if (i = Pacman.y) and (j = Pacman.x) then
        write('P')
      else if (i = Ghost.y) and (j = Ghost.x) then
        if PowerUp then write('g') else write('G')
      else
        case Map[i][j] of
          Wall: write('#');
          Dot: write('.');
          Power: write('O');
          Empty: write(' ');
        end;
    end;
    writeln;
  end;
  writeln('Score: ', Score, '   Lives: ', Lives);
  if PowerUp then writeln('POWER MODE ACTIVE!');
end;

procedure MoveGhost;
var dx, dy: integer;
begin
  dx := 0; dy := 0;
  case random(4) of
    0: dx := -1;
    1: dx := 1;
    2: dy := -1;
    3: dy := 1;
  end;
  if Map[Ghost.y + dy][Ghost.x + dx] <> Wall then
  begin
    Ghost.x := Ghost.x + dx;
    Ghost.y := Ghost.y + dy;
  end;
end;

procedure CheckCollision;
begin
  if (Pacman.x = Ghost.x) and (Pacman.y = Ghost.y) then
  begin
    if PowerUp then
    begin
      Score := Score + 100;
      Ghost.x := WIDTH - 2;
      Ghost.y := HEIGHT - 2;
    end
    else
    begin
      dec(Lives);
      Pacman.x := 2; Pacman.y := 2;
    end;
  end;
end;

procedure Update;
var ch: char;
    dx, dy: integer;
begin
  dx := 0; dy := 0;
  if keypressed then
  begin
    ch := readkey;
    if ch = #0 then ch := readkey; // Special key
    case ch of
      #72: dy := -1; // Up
      #80: dy := 1;  // Down
      #75: dx := -1; // Left
      #77: dx := 1;  // Right
    end;
  end;
  if Map[Pacman.y + dy][Pacman.x + dx] <> Wall then
  begin
    Pacman.x := Pacman.x + dx;
    Pacman.y := Pacman.y + dy;
    case Map[Pacman.y][Pacman.x] of
      Dot:
      begin
        Score := Score + 10;
        Map[Pacman.y][Pacman.x] := Empty;
      end;
      Power:
      begin
        PowerUp := true;
        PowerTimer := 50;
        Map[Pacman.y][Pacman.x] := Empty;
      end;
    end;
  end;
end;

function DotsLeft: boolean;
var i, j: integer;
begin
  for i := 1 to HEIGHT do
    for j := 1 to WIDTH do
      if Map[i][j] = Dot then exit(true);
  exit(false);
end;

begin
  randomize;
  InitMap;
  Lives := MAX_LIVES;
  Score := 0;
  PowerUp := false;
  Pacman.x := 2; Pacman.y := 2;
  Ghost.x := WIDTH - 2; Ghost.y := HEIGHT - 2;

  repeat
    DrawMap;
    Update;
    MoveGhost;
    CheckCollision;

    if PowerUp then
    begin
      dec(PowerTimer);
      if PowerTimer = 0 then PowerUp := false;
    end;

    delay(150);
  until (Lives = 0) or (not DotsLeft);

  clrscr;
  if Lives = 0 then
    writeln('GAME OVER!')
  else
    writeln('YOU WIN!');
  writeln('Final Score: ', Score);
end.
