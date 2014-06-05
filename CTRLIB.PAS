{ Catacomb Source Code
  Copyright (C) 1993-2014 Flat Rock Software

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

Unit CtrLib;

interface
Uses
  dos;

type
  dirtype= (north,east,south,west
  ,northeast,southeast,southwest,northwest,nodir);


Var
  oldint9: pointer;

{The new int9 handler set and clears the keydown table}

  KeyDown: array [0..127] of boolean;

{Joystick value boundaries}

  joy_xlow,joy_xhigh,joy_ylow,joy_yhigh: word;

{Mouse minimum movements}

  mou_sensitivity: word;

{Keyboard scan codes}

  key_North,key_NorthEast,key_East,key_SouthEast,key_South
  ,key_SouthWest,key_West,key_NorthWest,key_B1,key_B2: byte;


Procedure Rd_Joy (num:integer; var x,y:integer);
Procedure Rd_Joystick1 (var dir:dirtype; var button1,button2:boolean);
Procedure Rd_Joystick2 (var dir:dirtype; var button1,button2:boolean);
Procedure Rd_Mouse (var dir:dirtype; var button1,button2:boolean);
Procedure Rd_Keyboard (var dir:dirtype; var button1,button2:boolean);
Procedure ConnectKBD;
Procedure DisconnectKBD;

{==========================================================================}

implementation

{===================================}
{                                   }
{ Rd_Joy                            }
{ Read the values of the joystick   }
{ pots.  Will vary with clock speed }
{                                   }
{===================================}

Procedure Rd_Joy (num:integer; var x,y:integer);
{ num should be 1 or 2 }
var
  i,p1,p2:byte;
  b1,b2: word;
Begin
  if num=1 then
    Begin
      p1:=1;
      p2:=2;
    end
  else
    Begin
      p1:=4;
      p2:=8;
    end;
  x:=0;
  y:=0;
  inline ($FA);	{clear ints}
  port[$201]:=port[$201];
  Repeat
    i:=port[$201] and 3;
    b1:=i and p1;
    b2:=(i and p2) shr 1;
    x:=x+b1;
    y:=y+b2;
  Until (b1+b2=0) or (x>500) or (y>500);
  inline ($FB);	{set ints}
end;



{===================================================}
{                                                   }
{ Rd_Joystick1/2                                    }
{ Reads the joystick buttons and converts positions }
{ into a direction based on Joy_### boundaries      }
{                                                   }
{===================================================}

Procedure Rd_Joystick1 (var dir:dirtype; var button1,button2:boolean);
Var
  i,joyx,joyy,xmove,ymove:integer;
Begin
{get button status}
  i:=port[$201];
  if (i and $10)>0 then
    button1:=false
  else
    button1:=true;
  if (i and $20)>0 then
    button2:=false
  else
    button2:=true;

  Rd_Joy (1,joyx,joyy);

{set xmove / ymove if the joyx/y is greater than boundary values}

  if joyx< joy_xlow then
    xmove:=-1
  else
    if joyx> joy_xhigh then
      xmove:=1
    else
      xmove:=0;

  if joyy< joy_ylow then
    ymove:=-1
  else
    if joyy> joy_yhigh then
      ymove:=1
    else
      ymove:=0;

{decide direction from xmove / ymove}

  case ymove*3+xmove of
    -4: dir:=NorthWest;
    -3: dir:=North;
    -2: dir:=NorthEast;
    -1: dir:=West;
     0: dir:=nodir;
     1: dir:=East;
     2: dir:=SouthWest;
     3: dir:=South;
     4: dir:=SouthEast;
  end;
End;


Procedure Rd_Joystick2 (var dir:dirtype; var button1,button2:boolean);
Var
  i,joyx,joyy,xmove,ymove:integer;
Begin
{get button status}
  i:=port[$201];
  if (i and $40)>0 then
    button1:=false
  else
    button1:=true;
  if (i and $80)>0 then
    button2:=false
  else
    button2:=true;

  Rd_Joy (2,joyx,joyy);

{set xmove / ymove if the joyx/y is greater than boundary values}

  if joyx< joy_xlow then
    xmove:=-1
  else
    if joyx> joy_xhigh then
      xmove:=1
    else
      xmove:=0;

  if joyy< joy_ylow then
    ymove:=-1
  else
    if joyy> joy_yhigh then
      ymove:=1
    else
      ymove:=0;

{decide direction from xmove / ymove}

  case ymove*3+xmove of
    -4: dir:=NorthWest;
    -3: dir:=North;
    -2: dir:=NorthEast;
    -1: dir:=West;
     0: dir:=nodir;
     1: dir:=East;
     2: dir:=SouthWest;
     3: dir:=South;
     4: dir:=SouthEast;
  end;
End;



{===================================================}
{                                                   }
{ Rd_Mouse                                          }
{ Returns the movement direction of the mouse since }
{ the last call, and the button status.             }
{                                                   }
{===================================================}

Procedure Rd_Mouse (var dir:dirtype; var button1,button2:boolean);
var
  regs: registers;
  deltax,deltay,xmove,ymove:integer;
Begin

{read mouse status}

  regs.ax:=3;
  intr($33,regs);  {mouse status}
  button1:=(regs.bx and 1=1);
  button2:=(regs.bx and 2=2);
  regs.ax:=11;
  intr($33,regs);  {mouse motion counters}
  deltax:= integer(regs.cx);
  deltay:= integer(regs.dx);

{set xmove / ymove if the delta is greater than boundary values}

  if deltax> mou_sensitivity then
    xmove:=1
  else
    if deltax< -mou_sensitivity then
      xmove:=-1
    else
      xmove:=0;

  if deltay> mou_sensitivity then
    ymove:=1
  else
    if deltay< -mou_sensitivity then
      ymove:=-1
    else
      ymove:=0;

{decide direction from xmove / ymove}

  case ymove*3+xmove of
    -4: dir:=NorthWest;
    -3: dir:=North;
    -2: dir:=NorthEast;
    -1: dir:=West;
     0: dir:=nodir;
     1: dir:=East;
     2: dir:=SouthWest;
     3: dir:=South;
     4: dir:=SouthEast;
  end;

{reset mouse to center of screen}

  regs.cx:=320;
  regs.dx:=100;
  regs.ax:=4;
  intr($33,regs);  {setmouse}

End;




{=====================================================}
{                                                     }
{ CtrlInt9                                            }
{ An interrupt routine that intercepts ALL scan codes }
{                                                     }
{=====================================================}

Procedure CtrlInt9;
interrupt;
type
  proc = procedure;
var
  key: byte;
Begin
  port[$20]:=$20;		{tell the interrupt manager we got it}
  key:=Port[$60];	{get the scan code that caused the interrupt}
  If key>127 then
    KeyDown [key-128]:=false	{break code, key was released}
  else
    begin
      keydown [key]:=true;	{make code, key was just pressed}
      MemW[$40:$1C]:=MemW[$40:$1A];	{clear the BIOS keyboard buffer}
    end;
  Proc (OldInt9);		{give it to the BIOS}
End;


{==============================================}
{                                              }
{ ConnectKBD                                   }
{ If the KBD handler isn't active, this clears }
{ the KeyDown table and sets INT 9 to point to }
{ CtrlInt9                                     }
{                                              }
{==============================================}

Procedure ConnectKBD;
Var
  i:integer;
Begin
  for i:=0 to 127 do
    KeyDown [i]:=false;
  If OldInt9=nil then
    Begin
      GetIntVec (9,OldInt9);
      SetIntVec (9,@CtrlInt9);
    End;
end;


{============================================}
{                                            }
{ DisconnectKBD                              }
{ If the KBD handler is active, it gets shut }
{ down and the BIOS is given control.        }
{                                            }
{============================================}

Procedure DisconnectKBD;
Begin
  If OldInt9<>nil then
    Begin
      SetIntVec (9,OldInt9);
      oldint9:=nil;
    end;

End;


{=================================================}
{                                                 }
{ Rd_Keyboard                                     }
{ Checks the keyflags set by CtrlInt9 and decides }
{ on direction and button states                  }
{                                                 }
{=================================================}

Procedure Rd_Keyboard (var dir:dirtype; var button1,button2:boolean);
Var
  xmove,ymove: integer;
Begin
  Button1:=KeyDown [Key_B1];
  Button2:=KeyDown [Key_B2];

  xmove:=0;
  ymove:=0;

  If KeyDown [Key_North] then
    ymove:=-1;
  If KeyDown [Key_East] then
    xmove:=1;
  If KeyDown [Key_South] then
    ymove:=1;
  If KeyDown [Key_West] then
    xmove:=-1;
  If KeyDown [Key_NorthWest] then
    Begin
      xmove:=-1;
      ymove:=-1;
    end;
  If KeyDown [Key_NorthEast] then
    Begin
      xmove:=1;
      ymove:=-1;
    end;
  If KeyDown [Key_SouthWest] then
    Begin
      xmove:=-1;
      ymove:=1;
    end;
  If KeyDown [Key_SouthEast] then
    Begin
      xmove:=1;
      ymove:=1;
    end;

{decide direction from xmove / ymove}

  case ymove*3+xmove of
    -4: dir:=NorthWest;
    -3: dir:=North;
    -2: dir:=NorthEast;
    -1: dir:=West;
     0: dir:=nodir;
     1: dir:=East;
     2: dir:=SouthWest;
     3: dir:=South;
     4: dir:=SouthEast;
  end;

End;


{===========================================================================}

Begin
  joy_xlow:=5;
  joy_ylow:=5;
  joy_xhigh:=20;
  joy_yhigh:=20;

  mou_sensitivity:=5;

  key_north:=$48;
  key_northeast:=$49;
  key_east:=$4D;
  key_southeast:=$51;
  key_south:=$50;
  key_southwest:=$4f;
  key_west:=$4B;
  key_northwest:=$47;
  key_b1:=$1D;
  key_b2:=$38;

  oldint9:=nil;
end.
