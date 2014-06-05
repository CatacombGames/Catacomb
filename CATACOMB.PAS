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

program Catacombs;

{$DEFINE SOUNDS}
{$DEFINE SAMPLER}

Uses
  SPKlib,CTRlib,CGAscr,crt,dos,printer,CGAdata,EGAdata;

Const
  maxpics = 2047;
  numtiles = 24*24-1;   {number of tiles displayed on screen}
  numlevels = 10;
  maxobj = 200;           {maximum possible active objects}
  solidwall = 129;
  blankfloor = 128;
  leftoff = 11;
  topoff = 11;
  tile2s = 256;          {tile number where the 2*2 pictures start}
  tile3s = tile2s+64*4;
  tile4s = tile3s+19*9;
  tile5s = tile4s+19*16;
  lasttile = tile5s+19*25;

Type

  soundtype = (nosnd,blockedsnd,itemsnd,treasuresnd,bigshotsnd,shotsnd,
    tagwallsnd,tagmonsnd,tagplayersnd,killmonsnd,killplayersnd,opendoorsnd,
    potionsnd,spellsnd,noitemsnd,gameoversnd,highscoresnd,leveldonesnd,
    foundsnd);

  thinktype = (playercmd,gargcmd,dragoncmd,ramstraight,ramdiag,straight,idle,fade);
  tagtype = (benign,monster,pshot,mshot,nukeshot);
  classtype = (nothing,player,goblin,skeleton,ogre,gargoyle,dragon,wallhit,
    shot,bigshot,rock,dead1,dead2,dead3,dead4,dead5,dead6,teleporter,
    torch,lastclass);

  ActiveObj = Record
    active : boolean;     {if false, the object has not seen the player yet}
    class : classtype;
    x,y,                  {location of upper left corner in world}
    stage,                {animation frame being drawn}
    delay:byte;           {number of frames to pause without doing anything}
    dir : dirtype;        {direction facing}
    hp : byte;            {hit points}
    oldx,oldy: byte;      {position where it was last drawn}
    oldtile : integer;	  {origin tile when last drawn}
    filler : array [1..4] of byte;	{pad to 16 bytes}
  end;


  objdesc = record	{holds a copy of ActiveObj, and its class info}
    active : boolean;
    class : classtype;
    x,y,stage,delay:byte;
    dir : dirtype;
    hp : shortint;
    oldx,oldy: byte;
    oldtile : integer;
    filler1 : array [1..4] of byte;	{pad to 16 bytes}

    think : thinktype;
    contact : tagtype;
    solid : boolean;
    firstchar : word;
    size : byte;
    stages : byte;
    dirmask : byte;
    speed : word;
    hitpoints : byte;
    damage : byte;
    points : word;
    filler2 : array [1..2] of byte;	{pad to 32 bytes}
  end;


{=================}
{                 }
{ typed constants }
{                 }
{=================}
Const
  altmeters : array [0..13] of string[13] =
(#0#0#0#0#0#0#0#0#0#0#0#0#0,#190#0#0#0#0#0#0#0#0#0#0#0#0,
 #190#192#0#0#0#0#0#0#0#0#0#0#0,#190#191#192#0#0#0#0#0#0#0#0#0#0,
 #190#191#191#192#0#0#0#0#0#0#0#0#0,#190#191#191#191#192#0#0#0#0#0#0#0#0,
 #190#191#191#191#191#192#0#0#0#0#0#0#0,#190#191#191#191#191#191#192#0#0#0#0#0#0,
 #190#191#191#191#191#191#191#192#0#0#0#0#0,#190#191#191#191#191#191#191#191#192#0#0#0#0,
 #190#191#191#191#191#191#191#191#191#192#0#0#0,#190#191#191#191#191#191#191#191#191#191#192#0#0,
 #190#191#191#191#191#191#191#191#191#191#191#192#0,#190#191#191#191#191#191#191#191#191#191#191#191#193);
  meters : array [0..13] of string[13] =
(#0#0#0#0#0#0#0#0#0#0#0#0#0,#194#0#0#0#0#0#0#0#0#0#0#0#0,
 #194#196#0#0#0#0#0#0#0#0#0#0#0,#194#195#196#0#0#0#0#0#0#0#0#0#0,
 #194#195#195#196#0#0#0#0#0#0#0#0#0,#194#195#195#195#196#0#0#0#0#0#0#0#0,
 #194#195#195#195#195#196#0#0#0#0#0#0#0,#194#195#195#195#195#195#196#0#0#0#0#0#0,
 #194#195#195#195#195#195#195#196#0#0#0#0#0,#194#195#195#195#195#195#195#195#196#0#0#0#0,
 #194#195#195#195#195#195#195#195#195#196#2#0#0,#194#195#195#195#195#195#195#195#195#195#196#0#0,
 #194#195#195#195#195#195#195#195#195#195#195#196#0,#194#195#195#195#195#195#195#195#195#195#195#195#197);

const
  opposite: array[north..nodir] of dirtype=
    (south,west,north,east,southwest,northwest,northeast,southeast,nodir);


{==================}
{                  }
{ global variables }
{                  }
{==================}
var
  inpmode : (kbd,joy,mouse);
  graphmode : (CGAgr,EGAgr,VGAgr);	{video adapter to use}
  playmode : (game,demogame,demosave,editor);	{game / demo / editor}
  gamexit : (quited,killed,reseted,victorious); {determines what to do after playloop}

  exitsave: pointer;			{old exit routine}
  mouseok: boolean;

  pics : pointer;                       {grab an entire segment for pics}
  xormask : word;                        {each character drawn is EOR'd}
  sx, sy, leftedge : integer;            {0-39, 0-24 print cursor/return}

  oldtiles : array [0..numtiles] of integer; {tile displayed last refresh}
  Background : array [0..86,0..85] of integer; {base map}
  View : array [0..86,0..85] of integer; {base map with objects drawn in}
  originx, originy : integer;            {current world location of UL corn}
  priority : array [0..maxpics] of byte;	{tile draw overlap priorities}

  items : array [1..5] of integer;
  shotpower : integer;                   {0-13 characters in power meter}
  side : integer;                        {which side shots come from}
  level : integer;
  score: longint;
  boltsleft: integer;			{number of shots left in a bolt}

  highscores : Array [1..5] of record
    score : longint;
    level : integer;
    initials : Array [1..3] of char;
  End;

  o : array [0..maxobj] of activeobj;	{everything that moves is here}
  obj , altobj : objdesc;		{total info about objecton and alt}
  altnum : integer;			{o[#] of altobj}
  numobj,objecton : integer;		{number of objects in O now}
  ObjDef : array [nothing..lastclass] of Record
    think : thinktype;			{some of these sizes are for the}
    contact : tagtype;                  {convenience of the assembly routines}
    solid : boolean;
    firstchar : word;
    size : byte;
    stages : byte;
    dirmask : byte;
    speed : word;
    hitpoints : byte;
    damage : byte;
    points : word;
    filler : array [1..2] of byte;
  end;


  i,j,k,x,y,z : integer;
  playdone, leveldone: boolean;

  tempb: boolean; tempp: pointer;

  ch: char; altkey:boolean;             {last key fetched by GET}

  chkx,chky,chkspot: integer;           {spot being checked by WALK}

  regs: registers;                      {for INTR calls}

  dir: dirtype;
  button1, button2: boolean;		{returned by playerIO}

  democmds: array[0..3000] of byte;	{bits 1-3=dir, 4=b1, 5=b2}
  frameon: word;
  grmem: pointer;
  clvar: classtype;

  packbuffer: array[0..4095] of byte;

{**************************************************************************}


{L VGAPALET.OBJ}
Procedure VGAPALET;	{not realy a procedure, just data...}
begin
end;


{$L CATASM.OBJ}

{=========================================}
{                                         }
{ DRAWOBJ                                 }
{ Draws the object to TILES in the proper }
{ direction and state.                    }
{                                         }
{=========================================}

Procedure DrawObj;
External;

Procedure EraseObj;
External;

Procedure DoAll;
External;

Procedure InitRnd (randomize:boolean);
External;

Function Random (maxval:word):WORD;
External;

Procedure WaitVBL;
External;

Procedure EGAmove;
External;

Procedure CGArefresh;
External;

Procedure EGArefresh;
External;

Procedure VGArefresh;
External;

Procedure CGAcharout (x,y,ch:integer);
external;

Procedure EGAcharout (x,y,ch:integer);
external;

Procedure VGAcharout (x,y,ch:integer);
external;

Function VideoID: integer;
external;

Procedure RLEexpand (source:pointer;dest:pointer;length:longint);
external;

Procedure RLEcompress (source:pointer;dest:pointer;length:longint);
external;

{==================================}
{                                  }
{ xxxCHAROUT                       }
{ Draw a single character at SX,SY }
{ in the various modes.            }
{                                  }
{==================================}

Procedure CharOut (x,y,ch:integer);
{call proper charout based on GRMODE}
Begin
  case graphmode of
    CGAgr: CGAcharout (x,y,ch);
    EGAgr: EGAcharout (x,y,ch);
    VGAgr: VGAcharout (x,y,ch);
  end;
End;

{======================================}
{                                      }
{ PLAYSOUND                            }
{ Starts a sound playing.  Sounds play }
{ until they are through, then quit.   }
{                                      }
{======================================}

Procedure PlaySound (soundnum: soundtype);
Begin
{$ifdef sounds}
  if playmode<>demogame then        {demo is allways silent}
    PlaySound1 (integer(soundnum));
{$endif}
End;



{========================================}
{                                        }
{ GETGRMODE                              }
{ SHows the title page and gets graphic  }
{ mode from user.                        }
{                                        }
{========================================}

Procedure GetGrMode;
var
  screen: byte absolute $b800:0000;
  gotmode: boolean;
  source: pointer;
  vidcard: integer;
Begin
{
; Subsystem ID values:
; 	 0  = (none)
; 	 1  = MDA
; 	 2  = CGA
; 	 3  = EGA
; 	 4  = MCGA
; 	 5  = VGA
; 	80h = HGC
; 	81h = HGC+
; 	82h = Hercules InColor
;
}

  regs.ax:=$0004;
  intr($10,regs);   {set graphic mode to 320*200 * 4 color}
  source := @titlescr;
  move (source^,screen,$4000);

  vidcard:=VideoID;

  gotmode := false;

  ch:=upcase(readkey);
  case ch of
    'C': Begin
	   graphmode:=CGAgr;
	   gotmode:=true;
	 end;
    'E': if (vidcard=3) or (vidcard=5) then
	 begin
	   graphmode:=EGAgr;
	   gotmode:=true;
	 end;
 {$IFNDEF SAMPLER}
    'V': if (vidcard=4) or (vidcard=5) then
	 begin
	   graphmode:=VGAgr;
	   gotmode:=true;
	 end;
 {$ENDIF}
  end;
  if not gotmode then
  begin
    if (vidcard=3) or (vidcard=5) then
      graphmode:=EGAgr
    else
      graphmode:=CGAgr;
  end
end;


{=================================}
{                                 }
{ PRINT                           }
{ Prints the string to the screen }
{ at SX,SY. ']' does a return to  }
{ LEFTEDGE.                       }
{ Automatically converts to lower }
{ case set for window drawing     }
{                                 }
{=================================}

Procedure Print (s:string);
Var
  i,cn:integer;
Begin
  For i:=1 to length (s) do
    If s[i]=']' then
      Begin
        sy:=sy+1;
        sx:=leftedge;    {return to left margin, and down a line}
      End
    Else
      Begin
	cn:=ord(s[i]);
	if (cn>=ord('a')) and (cn<=ord('z')) then
	  cn:=cn-32;
	charout (sx,sy,cn);
        sx:=sx+1;
      End;
End;


{====================}
{                    }
{ SHORTNUM / LONGNUM }
{ PRINT's the number }
{                    }
{====================}

Procedure ShortNum (i:integer);
Var
  s: string [10];
  e: integer;
Begin
  str (i:1,s);
  print (s);
End;

Procedure LongNum (i:longint);
Var
  s: string [10];
  e: integer;
Begin
  str (i:1,s);
  print (s);
End;

{==============================}
{                              }
{ xxxREFRESH                   }
{ Refresh the changed areas of }
{ the tiles map in the various }
{ graphics modes.              }
{                              }
{==============================}

Procedure Refresh;
const
  demowin : array[0..4] of string[16] =
  (#14#15#15#15#15#15#15#15#15#15#15#15#15#15#15#16,
   #17' --- DEMO --- '#18,
   #17'SPACE TO START'#18,
   #17'F1 TO GET HELP'#18,
   #19#20#20#20#20#20#20#20#20#20#20#20#20#20#20#21);
var
  x,y,basex,basey: integer;
  underwin : array[0..4,0..15] of word;
Begin
  basex:=originx+4;
  basey:=originy+17;
  if playmode=demogame then
    for y:=0 to 4 do
      for x:=0 to 15 do
	begin
	  underwin[y,x]:=view[y+basey,x+basex];
	  view[y+basey,x+basex]:=word(demowin[y][x+1]) and $00ff;
	end;

  WaitVBL;
  case graphmode of
    CGAgr: CGArefresh;
    EGAgr: EGArefresh;
    VGAgr: VGArefresh;
  end;
  if playmode=demogame then
    for y:=0 to 4 do
      for x:=0 to 15 do
	view[y+basey,x+basex]:=underwin[y,x];
  waitVBL;
End;


Procedure SimpleRefresh;
Begin
  WaitVBL;
  case graphmode of
    CGAgr: CGArefresh;
    EGAgr: EGArefresh;
    VGAgr: VGArefresh;
  end;
end;


{======================================}
{                                      }
{ RESTORE                              }
{ Redraws every tile on the tiled area }
{ by setting oldtiles to -1.  Used to  }
{ erase any temporary windows.         }
{                                      }
{======================================}

Procedure ClearOld;
Begin
  fillchar (oldtiles,sizeof(oldtiles),chr($FF)); {clear all oldtiles}
end;

Procedure Restore;
Var
 i,j:integer;
Begin
  clearold;
  SimpleRefresh;
End;


{===============================}
{                               }
{ DRAWWINDOW                    }
{ Draws a window that will fill }
{ the given rectangle.  The text}
{ area of the window DOES NOT   }
{ go to the edge.  A 3-D effect }
{ is produced.                  }
{                               }
{===============================}

Procedure DrawWindow (left,top,right,bottom:integer);
Var
  x,y:integer;
Begin
  charout (left,top,14);
  for x:=left+1 to right-1 do
    charout (x,top,15);
  charout (right,top,16);

  for y:=top+1 to bottom-1 do
    begin
      charout (left,y,17);
      for x:=left+1 to right-1 do
	charout (x,y,32);
      charout (right,y,18);
    end;

  charout (left,bottom,19);
  for x:=left+1 to right-1 do
    charout (x,bottom,20);
  charout (right,bottom,21);

  sx:=left+1;
  leftedge:=sx;
  sy:=top+1;
End;

{======================}
{                      }
{ CENTERWINDOW         }
{ Centers a drawwindow }
{ that can hold a TEXT }
{ area of width/height }
{======================}

Procedure CenterWindow (width,height:integer);
Var
  x1,y1 : integer;
Begin
  if width>2 then
    centerwindow (width-2,height);
{  restore; }
  WaitVBL;
  x1:=11-width div 2;
  y1:=11-height div 2;
  DrawWindow (x1,y1,x1+width+1,y1+height+1);
End;


{==============}
{              }
{ ClearKeyDown }
{              }
{==============}

Procedure ClearkeyDown;
var
  ch: char;
Begin
  fillchar (keydown,sizeof(keydown),0);
  while keypressed do
    ch:=readkey;
end;


{================================}
{                                }
{ GET                            }
{ Basic keyboard input routine   }
{ returns upercase only.  Cursor }
{ appears at SX,SY               }
{ F keys are NOT handled         }
{================================}

Procedure Get (var ch:char);
Var
  cycle,direc,a:integer;
Begin
  clearkeydown;
  cycle:=10;
  direc:=1;
  While not Keypressed do
    Begin
      charout (sx,sy,cycle);
      for a:=1 to 5 do
	waitvbl;
      cycle:=cycle+direc;
      if cycle=14 then
        Begin
          direc:=-1;
          cycle:=13;
        end;
      if cycle=9 then
        Begin
          direc:=1;
          cycle:=10;
        end
    End;
  charout (sx,sy,ord(' '));
  altkey:=false;
  ch:=upcase(readkey);
  if ch=chr(0) then
    begin
      altkey:=true;
      ch:=readkey;
    end;
  clearkeydown;
end;


{============================================}
{                                            }
{ INPUT                                      }
{ Reads a string of LENGTH from the keyboard }
{ Cursor is displayed at SX,SY               }
{                                            }
{============================================}

Procedure Input (var s:string; length:integer);
var
  i:integer;
Begin
  i:=1;
  Repeat
   get (ch);
   if altkey or (ord(ch)=8) then
     Begin
       if (i>1) and ( (ord(ch)=75) or (not altkey)) then{backspace}
	 begin
	   dec(i);
	   sx:=sx-1;
	  end;
     end
   else
     begin
       if (ch>=' ') and (ch<chr(127)) and (i<=length) then
	 Begin
	   charout (sx,sy,ord(ch));
	   s[i]:=ch;
	   inc (i);
	   inc(sx);
	 end;
     end;

   if ch=chr(27) then
     s[1]:=ch;
  until (ch=chr(13)) or (ch=chr(27));
  s[0]:=chr(i-1);
end;


{===========================}
{                           }
{ CHECKKEYS                 }
{ If a key has been pressed }
{ it will be assigned to CH/}
{ altkey, and if it is an F }
{ key, it will be processed.}
{                           }
{===========================}

procedure CheckKeys;

  {      }
  { Help }
  {      }
  Procedure Help;
  Var
    x,y:integer;

    Function Wantmore:boolean;
    Begin
      sx:=2;
      sy:=20;
      Print ('(SPACE for more/ESC)');
      sx:=12;
      sy:=21;
      get (ch);
      if ch=chr(27) then
        wantmore:=false
      else
        wantmore:=true;
    end;

    {         }
    { DrawPic }
    {         }
    Procedure DrawPic(x,y:integer; c:classtype; dir:dirtype; stage:integer);
    var
      xx,yy,size,tilenum:integer;
    Begin
      size:=ObjDef[c].size;
      tilenum:=ObjDef[c].firstchar+size*size
	*((integer(dir) and ObjDef[c].dirmask)*ObjDef[c].stages+stage);

      For yy:=y to y+size-1 do
        for xx:=x to x+size-1 do
          Begin
            charout (xx,yy,tilenum);
            inc(tilenum);
          end;
    End;

  Begin
    CenterWindow (20,20);
    Print ('  C A T A C O M B   ]');
    Print ('   - - - - - - -    ]');
    Print (' By John Carmack &  ]');
    Print ('     PC Arcade      ]');
    Print (']');
    Print ('F1 = Help           ]');
    Print ('F2 = Sound on / off ]');
    Print ('F3 = Controls       ]');
    Print ('F4 = Game reset     ]');
    Print ('F9 = Pause          ]');
    Print ('F10= Quit           ]');
    Print (']');
    Print ('Watch the demo for  ]');
    Print ('a play example.     ]');
    Print (']');
    Print ('Hit fire at the demo]');
    Print ('to begin playing.   ]');
    if not Wantmore then
      exit;

    CenterWindow (20,20);
    Print (']Keyboard controls:  ]]');
    Print ('Move    : Arrows    ]');
    Print ('Button1 : Ctrl      ]');
    Print ('Button2 : Alt       ]');
    Print (']To switch to mouse ]');
    Print ('or joystick control,]');
    Print ('hit F3.             ]');

    if not Wantmore then
      exit;

    CenterWindow (20,20);
    Print ('Button 1 / CTRL key:]');
    Print ('Builds shot power.  ]');
    Print ('If the shot power   ]');
    Print ('meter is full when  ]');
    Print ('the button is       ]');
    Print ('released, a super   ]');
    Print ('shot will be        ]');
    Print ('launched.           ]');
    Print (']');
    For y:=11 to 18 do
      For x:=3 to 20 do
        Charout (x,y,128);

    DrawPic (4,14,player,east,2);
    DrawPic (19,15,shot,east,1);
    DrawPic (17,14,shot,east,0);
    DrawPic (15,15,shot,east,1);
    DrawPic (8,14,bigshot,east,0);

    if not Wantmore then
      exit;

    CenterWindow (20,20);
    Print ('Button 2 / ALT key:]');
    Print ('Allows you to move  ]');
    Print ('without changing the]');
    Print ('direction you are   ]');
    Print ('facing.  Good for   ]');
    Print ('searching walls and ]');
    Print ('fighting retreats.  ]');
    For y:=11 to 18 do
      For x:=3 to 20 do
	if y=15 then
	  charout (x,y,129)
	else if y=16 then
	  charout (x,y,131)
	else
	  charout (x,y,128);
    DrawPic (6,13,player,south,2);
    sx:=6;
    sy:=15;
    print (#29#29#30#30#31#31);

    if not Wantmore then
      exit;

    CenterWindow (20,20);
    Print ('"P" or "SPACE" will ]');
    Print ('take a healing      ]');
    Print ('potion if you have  ]');
    Print ('one.  This restores ]');
    Print ('the body meter to   ]');
    Print ('full strength.  Keep]');
    Print ('a sharp eye on the  ]');
    Print ('meter, because when ]');
    Print ('it runs out, you are]');
    Print ('dead!               ]]');
    Print ('"B" will cast a bolt]');
    Print ('spell if you have   ]');
    Print ('any.  You can mow   ]');
    Print ('down a lot of       ]');
    Print ('monsters with a bit ]');
    Print ('of skill.           ]');

    if not Wantmore then
      exit;

    CenterWindow (20,20);
    Print ('"N" or "ENTER" will ]');
    Print ('cast a nuke spell.  ]');
    Print ('This usually wipes  ]');
    Print ('out all the monsters]');
    Print ('near you.  Consider ]');
    Print ('it a panic button   ]');
    Print ('when you are being  ]');
    Print ('mobbed by monsters! ]]');
    Print ('               '#128#128#128']');
    Print ('Potions:       '#128#162#128']');
    Print ('               '#128#128#128']');
    Print ('Scrolls:       '#128#163#128']');
    Print (' (bolts/nukes) '#128#128#128']');
    Print ('Treasure:      '#128#167#128']');
    Print (' (points)      '#128#128#128']');
    Print ('               '#128#128#128']');

    if not Wantmore then
      exit;

  End;

  {             }
  { SoundChange }
  {             }
  Procedure SoundChange;
  label
    select;
  Begin
    CenterWindow (15,1);
    Print ('Sound: ]');
select:
    sx:=11;
    sy:=12;
    if soundon then
      xormask:=$FFFF
    else
      xormask:=0;
    Print (' ON ');
    xormask:=xormask xor $FFFF;
    Print (' OFF ');
    xormask:=0;
    sx:=10;
    Get (ch);
    if altkey and ( (ord(ch)=75) or (ord(ch)=77) ) then
      Begin
        soundon:=not soundon;
        goto select;
      end
  end;

  {             }
  { InputChange }
  {             }
  Procedure InputChange;
  label
    switch;
  var
    oldmode: integer;
  procedure calibrate;
  var
    xl,yl,xh,yh,ox,dx,dy: integer;
  begin
    restore;
    centerwindow (20,9);
    Print ('Joystick calibration]');
    Print ('--------------------]');
    Print ('Push the joystick to]');
    Print ('the UPPER LEFT and]');
    ox:=sx+10;
    Print ('hit fire:(');
    Repeat
      sx:=ox;
      Rd_Joy (1,xl,yl);
      shortnum (xl);
      print (',');
      shortnum (yl);
      print (')  ');
      Rd_Joystick1 (dir,button1,button2);
    until keypressed or button1 or button2;
    while button1 or button2 do
      Rd_Joystick1 (dir,button1,button2);

    Print (']]Push the joystick to]');
    Print ('the LOWER RIGHT and]');
    Print ('hit fire:(');
    Repeat
      sx:=ox;
      Rd_Joy (1,xh,yh);
      shortnum (xh);
      print (',');
      shortnum (yh);
      print (')  ');
      Rd_Joystick1 (dir,button1,button2);
    until keypressed or button1 or button2;
    while button1 or button2 do
      Rd_Joystick1 (dir,button1,button2);

    dx:=(xh-xl) div 4;
    dy:=(yh-yl) div 4;
    joy_xlow:=xl+dx;
    joy_xhigh:=xh-dx;
    joy_ylow:=yl+dy;
    joy_yhigh:=yh-dy;

  end;

  Begin
    oldmode:=ord(inpmode);
    CenterWindow (15,5);
    Print ('Player Control:]]');
switch:
    sx:=leftedge;
    sy:=12;
    if inpmode=kbd then
      xormask:=$FFFF
    else
      xormask:=0;
    Print ('   KEYBOARD    ]');
    if inpmode=joy then
      xormask:=$FFFF
    else
      xormask:=0;
    Print ('   JOYSTICK    ]');
    if inpmode=mouse then
      xormask:=$FFFF
    else
      xormask:=0;
    Print ('     MOUSE     ]');
    xormask:=0;

    sx:=12;
    sy:=11;
    Get (ch);
    if altkey and ( (ord(ch)=80) or (ord(ch)=77) ) then
      Begin
        if inpmode=mouse then
          inpmode:=kbd
        else
          inpmode:=succ(inpmode);
        goto switch;
      end;

    if altkey and ( (ord(ch)=72) or (ord(ch)=75) ) then
      Begin
        if inpmode=kbd then
          inpmode:=mouse
        else
          inpmode:=pred(inpmode);
        goto switch;
      end;

    if inpmode=mouse then
      Begin
	if not mouseok then
	  begin
	    playsound (blockedsnd);
	    goto switch;
	  end;
        regs.ax:=0;
        intr($33,regs);   {initialize the mouse}
      end;

    if (inpmode=joy) { and (oldmode<>ord(joy)) } then
      calibrate;
  end;


  {       }
  { Reset }
  {       }
  Procedure Reset;
  Begin
    CenterWindow (18,1);
    Print ('Reset game (Y/N)?');
    Get (ch);
    if ch='Y' then
      Begin
	gamexit:=killed;
        playdone:=true;
      end;
  end;

  {       }
  { Pause }
  {       }
  Procedure Pause;
  Begin
    CenterWindow (7,1);
    Print ('PAUSED');
    Get (ch);
  end;

  {          }
  { QuitGame }
  {          }
  Procedure QuitGame;
  Begin
    CenterWindow (12,1);
    Print ('Quit (Y/N)?');
    Get (ch);
    if ch='Y' then
      halt;			{our exit procedure takes care of it all}
  end;

Begin
  If keydown[$3b] then
    begin
      Help;
      restore;
    end;
  If keydown[$3c] then
    Begin
      SoundChange;
      restore;
    end;
  If keydown[$3d] then
    begin
      InputChange;
      restore;
    end;
  If keydown[$3e] then
    begin
      Reset;
      restore;
    end;
  If keydown[$43] then
    begin
      pause;
      restore;
    end;
  If keydown[$44] or keydown [1] then
    begin
      quitgame;
      restore;
    end;
end;

{=====================================}
{                                     }
{ PlayerIO                            }
{ Checks for special keys, then calls }
{ apropriate control routines:        }
{ KBDINPUT, JOYINPUT, MOUSEINPUT      }
{ then does any needed updating, etc  }
{                                     }
{=====================================}

Procedure PlayerIO (var direc: dirtype; var button1,button2: boolean);

Begin

{check for commands to switch modes or quit, etc}

  CheckKeys;
  case inpmode of
    kbd: Rd_Keyboard (direc,button1,button2);
    mouse: Rd_Mouse (direc,button1,button2);
    joy: Rd_Joystick1 (direc,button1,button2);
  end;
End;





{==============================}
{                              }
{ IOERROR                      }
{ Handle errors, allowing user }
{ to abort the program if they }
{ want to, or try over.        }
{                              }
{==============================}

Procedure IOerror (filename:string);
Begin
  centerwindow (19,3);
  Print ('DOS ioresult ');
  Shortnum (ioresult);
  Print (']on:');
  Print (filename);
  Print ('](R)etry or (Q)uit:');
  Get (ch);
  if ch='Q' then
    halt;			{our exit procedure cleans things up}
End;



{$i-}

{=========================================================================}


{=====================================================}
{                                                     }
{ PARALIGN                                            }
{ Sets the heap so next variable will be PARA aligned }
{                                                     }
{=====================================================}

Procedure Paralign;
Var
  state: record
    case boolean of
      true: (p: pointer);
      false: (offset,segment:word);
    End;
Begin
  mark (state.p);
  If state.offset>0 then
    Begin
      state.offset:=0;
      inc(state.segment);
      release (state.p);
    end;
end;


{========================================================}
{                                                        }
{ BLOAD                                                  }
{ Allocates paraligned memory for a file and loads it in }
{                                                        }
{========================================================}

function Bload (filename: string): pointer;
var
  iofile: file;
  len: longint;
  allocleft,recs: word;
  into,second: pointer;
begin
  Assign (iofile,filename);
  Reset (iofile,1);
  If ioresult<>0 then
    Begin
      writeln ('File not found: ',filename);
      halt;
    End;

  len:=filesize(iofile);
  paralign;

  if len>$fff0 then      {do this crap because getmem can only give $FFF0}
    begin
      getmem (into,$fff0);
      BlockRead (iofile,into^,$FFF0,recs);
      allocleft:=len-$fff0;
      while allocleft > $fff0 do
	begin
	  getmem (second,$fff0);
	  BlockRead (iofile,second^,$FFF0,recs);
	  allocleft:=allocleft-$fff0;
	end;
      getmem (second,allocleft);
      BlockRead (iofile,second^,$FFF0,recs);
    end
  else
    begin
      getmem (into,len);
      BlockRead (iofile,into^,len,recs);
    end;

  Close (iofile);
  bload:=into;
end;


{===================================}
{                                   }
{ INITGRAPHICS                      }
{ Loads the graphics and does any   }
{ needed maping or switching around }
{ Decides which files to load based }
{ on GRMODE                         }
{                                   }
{===================================}

Procedure InitGraphics;
const
  scindex = $3c4;
  scmapmask = 2;
  gcindex = $3ce;
  gcmode = 5;

Var
  iofile : file;
  x,y,memat,buff,recs,plane,planebit,t : word;
Begin
  mark (grmem);		{so memory can be released later}

  case graphmode of
    CGAgr: Begin
	     pics := ptr(seg(cgapics)+1,0);
{	     pics:=bload('CGAPICS.CAT');}
	     regs.ax:=$0004;
	     intr($10,regs);   {set graphic mode to 320*200 * 4 color}
	   end;

    EGAgr: Begin
	     pics := ptr(seg(egapics)+1,0);
{	     pics:=bload('EGAPICS.CAT');}
	     regs.ax:=$000D;
	     intr($10,regs);   {set graphic mode to 320*200 * 16 color}
	     EGAmove;		{move the tiles into latched video memory}
	   end;

    VGAgr: Begin
	     pics:=bload ('VGAPICS.CAT');
	     regs.ax:=$0013;
	     intr($10,regs);   {set graphic mode to 320*200 * 256 color}
	     regs.es:=seg(VGAPALET);
	     regs.dx:=ofs(VGAPALET);
	     regs.bx:=0;
	     regs.cx:=$100;
	     regs.ax:=$1012;
	     intr($10,regs);	{set up deluxepaint's VGA pallet}
	   end;
  end;


End;


{==============================}
{                              }
{ LOADLEVEL / SAVELEVEL        }
{ Loads map LEVEL into memory, }
{ and sets everything up.      }
{                              }
{==============================}

Procedure LoadLevel;
label
  tryopen,fileread;

const
  tokens: array[230..255] of classtype =
    (player,teleporter,goblin,skeleton,ogre,gargoyle,dragon,nothing,
     nothing,nothing,nothing,nothing,nothing,nothing,nothing,nothing,
     nothing,nothing,nothing,nothing,nothing,nothing,nothing,nothing,
     nothing,nothing);

Var
  filename : string;
  st: string;
  x,y,xx,yy,recs, btile : Integer;
  iofile: file;
  sm : array [0..4095] of byte;

Begin
  str(level:1,st);
  filename:=concat ('LEVEL',st,'.CAT');

tryopen:

  Assign (iofile,filename);
  Reset (iofile,1);
  If ioresult<>0 then
{file not found...}

      Begin
        Ioerror (filename);
        goto tryopen;       {try again...}
      End;

  BlockRead (iofile,packbuffer,4096,recs);
  close (iofile);

  RLEexpand (@packbuffer[4],@sm,4096);

  numobj:=0;
  o[0].x:=13;          {just defaults if no player token is found}
  o[0].y:=13;
  o[0].stage:=0;
  o[0].delay:=0;
  o[0].dir:=east;
  o[0].oldx:=0;
  o[0].oldy:=0;
  o[0].oldtile:=-1;


  for yy:=0 to 63 do
    for xx:=0 to 63 do
      Begin
        btile:=sm[yy*64+xx];
        if btile<230 then
          background[yy+topoff,xx+leftoff]:=btile
        else
          Begin

{hit a monster token}
            background[yy+topoff,xx+leftoff]:=blankfloor;
            if tokens[btile]=player then

{the player token determines where you start in level}

              Begin
                o[0].x:=xx+topoff;
                o[0].y:=yy+leftoff;
              end
            else

{monster tokens add to the object list}

              Begin
                inc(numobj);
                with o[numobj] do
                  Begin
                    active:=false;
                    class:=tokens[btile];
                    x:=xx+leftoff;
                    y:=yy+topoff;
                    stage:=0;
                    delay:=0;
		    dir:=dirtype(random(4));  {random 0-3}
		    hp:=ObjDef[class].hitpoints;
		    oldx:=x;
		    oldy:=y;
		    oldtile:=-1;
                  End;
              end;

            end;

          end;

fileread:


  originx := o[0].x-11;
  originy := o[0].y-11;

  shotpower:=0;
  for y:=topoff-1 to 64+topoff do
    for x:=leftoff-1 to 64+leftoff do
      view[y,x]:=background[y,x];

  sx:=33;                  {print the new level number on the right window}
  sy:=1;
  shortnum (level);
  Print (' ');          {in case it went from double to single digit}
  restore;
End;



{=================================}
{                                 }
{ LOADHIGHSCORES / SAVEHIGHSCORES }
{ Loads / saves the file or creats}
{ a new one, as needed.           }
{                                 }
{=================================}

Procedure LoadHighScores;
Var
  iofile : file;
  recs,i: Integer;
Begin
  Assign (iofile,'TOPSCORS.CAT');
  Reset (iofile,1);
  BlockRead (iofile,highscores,sizeof(highscores),recs);
  Close (iofile);
  If (ioresult<>0) or (recs<>sizeof (highscores)) then

{create a default high score table}

    For i:=1 to 5 do
      With Highscores[i] do
        Begin
          level:=1;
          score:=100;
          initials[1]:='J';
          initials[2]:='D';
          initials[3]:='C';
        End
End;


Procedure SaveHighScores;
Var
  iofile : file;
  recs : Integer;
Begin
  Assign (iofile,'TOPSCORS.CAT');
  Rewrite (iofile,1);
  BlockWrite (iofile,highscores,sizeof(highscores),recs);
  Close (iofile);
End;


{=====================}
{                     }
{ LOADDEMO / SAVEDEMO }
{                     }
{=====================}


Procedure LoadDemo;
Var
  iofile : file;
  recs : Integer;
Begin
  Assign (iofile,'DEMO.CAT');
  Reset (iofile,1);
  BlockRead (iofile,democmds,sizeof(democmds),recs);
  Close (iofile);
End;



Procedure SaveDemo;
Var
  iofile : file;
  recs : Integer;
Begin
  Assign (iofile,'DEMO.CAT');
  Rewrite (iofile,1);
  Blockwrite (iofile,democmds,sizeof(democmds),recs);
  Close (iofile);
End;


{====================}
{                    }
{ Load the sounds in }
{                    }
{====================}

Procedure LoadSounds;
Begin
  SoundData:=Bload ('SOUNDS.CAT');
End;


{$i+} {i/o checking back on}

{==========================================================================}

{$i cat_play}  {the routines which handle game play}

{==========================================================================}

{========================================}
{                                        }
{ Finished                               }
{ SHows the end page...                  }
{                                        }
{========================================}

Procedure Finished;
var
  screen: byte absolute $b800:0000;
  source: pointer;
Begin
  if graphmode<>CGAgr then
  begin
    regs.ax:=$0004;
    intr($10,regs);   {set graphic mode to 320*200 * 4 color}
  end;
  source := @endscr;
  move (source^,screen,$4000);

  playsound (treasuresnd);
  waitendsound;
  playsound (treasuresnd);
  waitendsound;
  playsound (treasuresnd);
  waitendsound;
  playsound (treasuresnd);
  waitendsound;

  while keypressed do
    ch:=readkey;

  ch:=upcase(readkey);

  release (grmem);
  initgraphics;
  DrawWindow (24,0,38,23);  {draw the right side window}
  Print ('  Level]]Score:]]Top  :]]K:]P:]B:]N:]]]');
  Print (' Shot Power]]]    Body]]]');
  PrintHighScore;
  PrintScore;
  PrintBody;
  PrintShotPower;


end;


{================================}
{                                }
{ PLAYSETUP                      }
{ Set up all data for a new game }
{ Does NOT start it playing      }
{                                }
{================================}

Procedure PlaySetup;
Var
  i:integer;
  lv: string;
Begin
  score:=0;
  shotpower:=0;
  level:=1;
  If keydown [$2E] and keydown [$14] then  {hold down 'C' and 'T' to CheaT!}
    Begin
      CenterWindow (16,2);
      Print ('Warp to which]level (1-99)?');
      input (lv,2);
      val (lv,level,i);
      if level<1 then
	level:=1;
      if level>20 then
	level:=20;
      restore;
    end;

  For i:=1 to 5 do
    items[i]:=0;

  with o[0] do
    Begin
      active := true;
      class := player;
      hp := 13;
      dir:=west;
      stage:=0;
      delay:=0;
    End;

  DrawWindow (24,0,38,23);  {draw the right side window}
  Print ('  Level]]Score:]]Top  :]]K:]P:]B:]N:]]]');
  Print (' Shot Power]]]    Body]]]');
  PrintHighScore;
  PrintBody;
  PrintShotPower;

{give them a few items to start with}

  givenuke;
  givenuke;
  givebolt;
  givebolt;
  givebolt;
  givepotion;
  givepotion;
  givepotion;

End;


{=============================}
{                             }
{ SHOWSCORES                  }
{ Draws the high score window }
{ Does NOT wait for input, etc}
{                             }
{=============================}

Procedure ShowScores;
var
  s: string;
  i,j:integer;
Begin
  centerwindow (14,14);
  Print (' High scores:]] SCORE LV BY] ----- -- ---]');
  for i:=1 to 5 do
    begin
      str(highscores[i].score:6,s);
      print (s);
      inc (sx);
      if (highscores[i].level=11) then
	begin
	  charout (sx+1,sy,167);
	  sx:=sx+3;
	end
      else
	begin
	  str(highscores[i].level:2,s);
	  print (s);
	  inc (sx);
	end;
      for j:=1 to 3 do
	print (highscores[i].initials[j]);
      print (']]');
    end;
End;



{================================}
{                                }
{ GAMEOVER                       }
{ Do a game over bit, then check }
{ for a high score, then return  }
{ to demo.                       }
{                                }
{================================}

Procedure GameOver;
var
  place,i,j:integer;
  st: string;
Begin
  WaitendSound;
  SimpleRefresh;
  while keypressed do
    ch:=readkey;
  for i:=1 to 60 do
    waitVBL;

{                                 }
{ fill in the player's high score }
{                                 }
  If score>=highscores[5].score then
    Begin
      place:=5;
      while (place>1) and (highscores[place-1].score<score) do
	dec(place);
      if place<5 then
	for i:=4 downto place do
	  highscores[i+1]:=highscores[i];	{scroll high scores down}
      with highscores[place] do
	begin
	  level:=catacombs.level;
	  score:=catacombs.score;
	  for i:=1 to 3 do
	    initials[i]:=' ';
	  ShowScores; 		{show the scores with space for your inits}
	  while keypressed do
	    ch:=readkey;
          playsound (highscoresnd);
	  sy:=7+place*2;
	  sx:=15;
	  input (st,3);
	  for i:=1 to length(st) do
	    initials[i]:=st[i];
	end;
      savehighscores;
    end;

{               }
{ no high score }
{               }
  ShowScores;
  while keypressed do
    ch:=readkey;
  i:=0;
  repeat
    waitvbl;
    inc (i);
    PlayerIO (dir,button1,button2);
  until (i>500) or button1 or button2 or keypressed;

  if button1 or button2 then
    playmode:=game
  else
    playmode:=demogame;
End;


{**************************************************************************}

{$IFNDEF SAMPLER}

{====================}
{                    }
{ EDITORLOOP         }
{ The editor mode... }
{                    }
{====================}

Procedure EditorLoop;

Label
  cmdover;

const
  samplepics : array[1..12] of string[13] =
    (#128#128#128#128#128#128#128#128#128#128#128#128#128,
     #128#128#129#129#135#128#128#128#171#171#177#128#128,
     #128#129#129#129#129#135#128#171#171#171#171#177#128,
     #128#129#129#129#129#130#128#171#178#179#180#172#128,
     #128#134#129#129#133#132#128#176#171#171#175#174#128,
     #128#128#134#131#132#128#128#128#176#173#174#128#128,
     #128#128#128#128#128#128#128#128#128#128#128#128#128,
     #128#162#128#163#128#164#128#167#128#165#128#166#128,
     #128#128#128#128#128#128#128#128#128#128#128#128#128,
     #128#230#231#232#233#234#235#236#237#238#239#240#128,
     #128#128#128#128#128#128#128#128#128#128#128#128#128,
     #128#128#128#128#128#128#128#128#128#128#128#128#128);

var
  drawtile:integer;
  ltx,lty,ltt,x,y,i:integer;
  dor: dirtype;
  b1,b2: boolean;
{$i-}

{                              }
{                              }
{ LOADLEVEL                    }
{ Loads map LEVEL into memory, }
{ nothing more                 }
{                              }
{                              }

Procedure LoadLevel;
label
  tryopen,fileread;
Var
  filename : string;
  st: string[3];
  x,y,xx,yy,recs, btile : Integer;
  iofile: file;
  tile: byte;
  sm : array [0..4095] of byte;

Begin
  str(level:1,st);
  filename:=concat ('LEVEL',st,'.CAT');

tryopen:

  Assign (iofile,filename);
  Reset (iofile,1);
  If ioresult<>0 then
{create a blank level for the editor}
      Begin
        for x:=0 to 63 do
          for y:=0 to 63 do
            background[y+topoff,x+leftoff]:=blankfloor;
        for x:=0 to 63 do
          Begin
            background[topoff,x]:=131;     {perspective north wall}
            background[topoff+63,x]:=129;  {solid south wall}
            background[x,leftoff]:=130;    {perspective west wall}
            background[x,leftoff+63]:=129; {solid east wall}
          end;
        background [topoff,leftoff]:=133;  {perspective NW corner}
        goto fileread;
      End

    else

  BlockRead (iofile,sm,4096,recs);
  Close (iofile);

  numobj:=0;

  for yy:=0 to 63 do
    for xx:=0 to 63 do
      begin
        tile:=sm[yy*64+xx];

{if tile is an exploding block, change it to a special icon for editor}

        if (tile>=136) and (tile<=145) then
          tile:=tile+35;
        background[yy+topoff,xx+leftoff]:=tile;
      end;

fileread:

  for y:=topoff to 63+topoff do
    for x:=leftoff to 63+leftoff do
      view[y,x]:=background[y,x];
  sx:=33;                  {print the new level number on the right window}
  sy:=1;
  shortnum (level);
  Print (' ');          {in case it went from double to single digit}
  restore;
End;



{            }
{ Save Level }
{            }
Procedure Saveit;
Var
  iofile : file;
  filename : string;
  x,y,recs : Integer;
  tile: byte;
  st: string[3];
  sm : array [0..4095] of byte;
Begin
  CenterWindow (9,1);
  Print ('Saving...');
  For y:=0 to 63 do
    for x:=0 to 63 do
      begin
        tile:=background[y+topoff,x+leftoff] and $00FF;

{if the tile was an exploding block, change back to undetectable}

        if (tile>=171) and (tile<=180) then
          tile:=tile-35;
        sm[y*64+x]:=tile;
      end;
  str(level:1,st);
  filename:=concat ('LEVEL',st,'.CAT');
  Assign (iofile,filename);
  Rewrite (iofile,1);
  BlockWrite (iofile,sm,4096,recs);
  Close (iofile);
  restore;
End;



{              }
{ Select Level }
{              }
function SelectLevel:boolean;
Var
  err:integer;
  lv:string;
Begin
  selectlevel:=false;              {editor won't reload a level if false}
  CenterWindow (16,2);
  Print ('Edit which level](1-99):');
  input (lv,2);
  if lv[1]=chr(27) then               {allow ESC to quit editor mode}
    Begin
      leveldone:=true;
      playdone:=true;
    end;
  val (lv,level,err);
  If level>=1 then
    selectlevel:=true;
  restore;
End;


{              }
{ Toggle Block }
{              }
Procedure ToggleBlock;
Var
  x,y,block:integer;
Begin
  x:=originx+topoff;
  y:=originy+leftoff;
  block:=background [y,x];

  If block=blankfloor then
    block:=solidwall
  else
    block:=blankfloor;

  background [y,x]:=block;
  view [y,x]:=block;
end;

{           }
{ Print map }
{           }
Procedure PrintMap;
var
  x,y,block:integer;
  ch: char;
Begin
  writeln (lst);
  Writeln (lst,'CATACOMB level ',level);
  for y:=0 to 65 do
    Begin
      for x:=0 to 65 do
	begin
	  block:=background[topoff-1+y,leftoff-1+x];
	  case block of
	    0..127: ch:=chr(block);	{ASCII}
	    128: ch:=' ';		{floor}
	    129..135: ch:='#';		{walls}
	    171..177: ch:='*';		{exploding}
	    178..180: ch:='!';		{hidden stuff}
	    162: ch:='p';		{potion}
	    163: ch:='s';		{scroll}
	    164: ch:='k';		{key}
	    165: ch:='|';		{door}
	    166: ch:='-';		{door}
	    167: ch:='$';		{treasure}
	    230..238: ch:=chr(ord('0')+block-229); {tokens}
	    else ch:='?';
	  end;
	  write (lst,ch);
      end;
    writeln (lst);
  end;
  writeln (lst,chr(12));
end;

{==================================}

Begin

  regs.ax:=0;
  intr($33,regs);    {show the mouse cursor}

  DrawWindow (24,0,38,23);  {draw the right side window}
  Print ('  Level]] Map editor]]F4=exit]F7=Load]F8=Save]^P=Print');

  sx:=25;
  leftedge:=sx;
  sy:=10;
  for i:=1 to 12 do
    Print (samplepics[i]+']');

  drawtile:=solidwall;
  ltx:=28;
  lty:=13;
  ltt:=solidwall;
  xormask:=$FFFF;
  charout (ltx,lty,ltt);         {highlight the new block}
  xormask:=0;

  level:=1;
  playdone:=false;

  Repeat
    leveldone:=false;
    originx:=0;
    originy:=0;

    If selectlevel then {let them choose which level to edit}
      loadlevel
    else
      goto cmdover;     {so if they pressed ESC, they can leave}

    repeat
      SimpleRefresh;

      regs.ax:=1;
      intr($33,regs);    {show the mouse cursor}
      waitvbl;           {make sure it gets seen}
      waitvbl;

      Repeat
        regs.ax:=3;
        intr($33,regs);  {mouse status}
      Until keypressed or (regs.bx and 3>0);

      sx:=regs.cx div 16;   {tile on screen mouse is over}
      sy:=regs.dx div 8;

      regs.ax:=2;
      intr($33,regs);    {hide the mouse cursor}

      Checkkeys;       {handles F keys and returns a keypress}

      ch:=chr(0);
      altkey:=false;
      if keypressed then
	Begin
	  ch:=upcase(readkey);
	  if ch=chr(0) then
	    Begin
	      altkey:=true;
	      ch:=readkey;
	    end
	end;

      if (sx<24) and (sy<24) then
{buttons pressed in tile map}
        Begin
          x:=originx+sx;
          y:=originy+sy;
          if (x>=leftoff) and (x<leftoff+64) and
             (y>=topoff) and (y<topoff+64) then
            Begin
              if (regs.bx and 1>0) then

{left button places/deletes a DRAWTILE}

                Begin
                  background[y,x]:=drawtile;
                  view[y,x]:=drawtile;
                end;

              if (regs.bx and 2>0) then  {right button places a BLANKFLOOR}
                Begin
                  background[y,x]:=blankfloor;
                  view[y,x]:=blankfloor;
                end;

	      if (not altkey) and ((ch>='A') and (ch<='Z')
	      or ((ch>='0') and (ch<='9') ) ) then
		Begin
		  if (ch>='0') and (ch<='9') then
		    background[y,x]:=ord(ch)+161   {map numbers are later}
		  else
		    background[y,x]:=ord(ch)+32; {map letters are lowercase}
		  view[y,x]:=background[y,x];
                  regs.ax:=4;
                  regs.cx:=regs.cx+16;
                  intr ($33,regs);        {move the mouse over}
                end;

              if (not altkey) and (ch=' ') then  {space makes a solidwall}
                Begin
                  background[y,x]:=solidwall;
                  view[y,x]:=solidwall;
                  regs.ax:=4;
                  regs.cx:=regs.cx+16;
                  intr ($33,regs);        {move the mouse over}
                end;

            end;
        end;


      x:=sx-24;
      y:=sy-9;
      if  (regs.bx and 1>0) and (x>0) and (x<14) and (y>0) and (y<13)and
       (samplepics[y][x]<>#128)then
{button pressed in samplepics}
        Begin
          charout (ltx,lty,ltt);         {unhighlight the old DRAWTILE}
          drawtile:=ord(samplepics[y][x]);
          ltx:=sx;
          lty:=sy;
          ltt:=drawtile;
          xormask:=$FFFF;
          charout (ltx,lty,ltt);         {highlight the new block}
          xormask:=0;
        end;


      Rd_Keyboard (dir,b1,b2);
      case dir of
	north: if originy>0 then
                originy:=originy-1
              else
		playsound (blockedsnd);
	west: if originx>0
           then
                originx:=originx-1
              else
		playsound(blockedsnd);
	east: if originx<51+leftoff then
                originx:=originx+1
              else
		playsound(blockedsnd);
	south: if originy<51+topoff then
                originy:=originy+1
              else
		playsound(blockedsnd);
      end;


      If keydown[$19] and keydown[$1d] then {control-P}
	PrintMap;

      If keydown[$42] then
	Begin
	  keydown[$42]:=false;
	  SaveIt;
	end;

      If keydown[$41] then
	Begin
	  keydown[$41]:=false;
	  leveldone:=true;        {so SELECTLEVEL will be called}
        end;

cmdover:

    Until leveldone or playdone;
  Until playdone;

End;

{$ENDIF}

{$i objects.pas}

{*************************************************************************}

{================}
{                }
{ exit procedure }
{                }
{================}

{$F+}
procedure Cleanup;
begin;
  exitproc:=exitsave;	{turbo's exit procedure}
  regs.ax:=3;
  intr($10,regs);	{set graphic mode to 80*25 *16 color}
  ShutdownSound;	{remove spkr int 10 handler}
  DisconnectKBD;	{remove KBD int 9 handler}
end;
{$F-}

{=========================}
{                         }
{ M A I N   P R O G R A M }
{                         }
{=========================}

Begin
  initobjects;

  fillchar (priority,sizeof(priority),chr(99));

  priority[blankfloor]:=0;
  for i:=ObjDef[teleporter].firstchar to ObjDef[Teleporter].firstchar+20 do
    priority[i]:=0;
  for clvar:=Dead2 to Dead5 do
    for i:=ObjDef[clvar].firstchar to ObjDef[clvar].firstchar+
    ObjDef[clvar].size*ObjDef[clvar].size do
      priority[i]:=0;		{deadthing}
  for i:=152 to 161 do
    priority[i]:=2;		{shots}
  for i:=ObjDef[bigshot].firstchar to ObjDef[bigshot].firstchar + 31 do
    priority[i]:=2;		{bigshot}
  for i:=0 to tile2s-1 do
    if priority [i]=99 then
      priority[i]:=3;		{most 1*1 tiles are walls, etc}
  for i:=tile2s to maxpics do
    if priority[i]=99 then
      priority[i]:=4;		{most bigger tiles are monsters}
  for i:=ObjDef[player].firstchar to ObjDef[player].firstchar + 63 do
    priority[i]:=5;		{player}


  side:=0;

  for x:=0 to 85 do
    Begin
      for y:=0 to topoff-1 do
        Begin
          view[x,y]:=solidwall;
          view[x,85-y]:=solidwall;
	  background[x,y]:=solidwall;
	  background[x,85-y]:=solidwall;
        end;
      view[86,x]:=solidwall;
    end;
  for y:=11 to 74 do
    for x:=0 to leftoff-1 do
      Begin
        view[x,y]:=solidwall;
        view[85-x,y]:=solidwall;
	background[x,y]:=solidwall;
	background[85-x,y]:=solidwall;
      end;


  mouseok:= true;		{check for a mouse driver}
  GetIntVec ($33,tempp);
  if tempp=NIL then
    mouseok:=false;
  if mem[seg(tempp^):ofs(tempp^)] = $CF then	{does it point to an IRET}
    mouseok:=false;

  LoadDemo;
  LoadSounds;
  LoadHighScores;

  GetGrMode;		{get choice of graphic / sound modes}
  InitGraphics;		{Load the graphics in}
  InitRND (true);	{warm up the random generator}

  exitsave:=exitproc;	{save off turbo's exit handler}
  exitproc:= @cleanup;	{and install ours}

  ConnectKBD;		{set up int 9 handler}
  StartupSound;         {set up int 10 handler}
  soundon:=true;        {sound on until told otherwise}

  xormask:=0;           {draw everything normal until told otherwise}

  inpmode:=kbd;   {keyboard control until told otherwise}


  playmode:=demogame;


{
    Begin
      playmode:=demosave;
      playsound (bigshotsnd);
      waitendsound;
    end;
}

  Repeat
    case playmode of

      game: Begin
	      PlaySetup;
              Playloop;
              If gamexit=killed then
                Begin

                  GameOver;
                end;
              if gamexit=victorious then
                Begin
		  finished;
                  GameOver;
                end;
              playmode:= demogame;
            End;

      demosave:	Begin             	{mode for recording a demo}
		  playsetup;
		  playloop;
		  SaveDemo;
		  playmode:=demogame;
		end;

      demogame: Begin
		  PlaySetup;
		  PlayLoop;
		  if (playmode=demogame) then
		    begin
		      score:=0;	{so demo doersn't get a high score}
		      GameOver;	{if entire demo has cycled, show highs}
		    end;
                End;

{$IFNDEF SAMPLER}

      editor : Begin
                 EditorLoop;
                 playmode:=demogame;
               End;
{$ENDIF}

    end;

  Until false;



End.

