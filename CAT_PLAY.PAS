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

{=====================================}
{                                     }
{ NEWOBJECT                           }
{ Returns the number of a free object }
{                                     }
{=====================================}

Function NewObject:integer;
label
  gotit;
Var
  i:integer;
Begin
  For i:=1 to numobj do
    If o[i].class=nothing then
	goto gotit;
  if numobj<maxobj then
    inc(numobj);
  i:=numobj;

gotit:

  o[i].oldtile:=-1;
  o[i].oldx:=0;
  o[i].oldy:=0;

  newobject:=i;
end;


{=================================}
{                                 }
{ PRINTSCORE / PRINTHIGHSCORE     }
{ Prints the scores to the screen }
{                                 }
{=================================}

Procedure PrintScore;
Var
  s:string[10];
Begin
  str(score:1,s);
  sx:=31;
  sy:=3;
  print (s);
End;

Procedure PrintHighScore;
Var
  s:string[10];
Begin
  str(highscores[1].score:1,s);
  sx:=31;
  sy:=5;
  print (s);
End;


{======================================}
{                                      }
{ PRINTSHOTPOWER                       }
{ PRINTBODY                            }
{ Draws the meter to the current value }
{                                      }
{======================================}

Procedure PrintShotPower;
Begin
  sx:=25;
  sy:=14;
  if shotpower = 13 then
    print (altmeters[13])
  else
    Print (meters[shotpower]);
End;


Procedure PrintBody;
Begin
  sx:=25;
  sy:=17;
  If o[0].hp>6 then
    Print (meters[o[0].hp])
  else
    print (altmeters[o[0].hp]);
End;


{=============================}
{                             }
{ LEVELCLEARED                }
{ Goes to the next level, or  }
{ ends game if all levels done}
{ Checks for warp teleporters }
{                             }
{=============================}

Procedure LevelCleared;
var
  warp: string[2];
  value, code: integer;

begin
  leveldone:=true;

  warp[0]:=chr(2);

  warp[1]:=chr(background[altobj.y+2,altobj.x]-161);
  if (warp[1]<'0') or (warp[1]>'9') then
    warp[1]:='0';
  warp[2]:=chr(background[altobj.y+2,altobj.x+1]-161);
  if (warp[2]<'0') or (warp[2]>'9') then
    warp[2]:=' ';
  val (warp,value,code);

  if value>0 then
    level:=value
  else
    inc(level);
  if level>numlevels then
{all levels have been completed}
    Begin
      playdone:=true;
      gamexit:=victorious;
    end;
end;


{==================================}
{                                  }
{ GIVE / TAKE POTION / KEY / BOLT  }
{ / NUKE                           }
{ Increments the item quantity and }
{ draws an extra icon if it fits   }
{                                  }
{==================================}

Procedure GiveKey;
Var
  i: integer;
Begin
  i:=items[1]+1;
  items[1]:=i;
  if i<11 then
    Charout (26+i,7,24);  {key icon}
End;


Procedure GivePotion;
Var
  i: integer;
Begin
  i:=items[2]+1;
  items[2]:=i;
  if i<11 then
    Charout (26+i,8,22);  {potion icon}
End;


Procedure GiveBolt;
Var
  i: integer;
Begin
  i:=items[3]+1;
  items[3]:=i;
  if i<11 then
    Charout (26+i,9,23);  {scroll icon}
End;



Procedure GiveNuke;
Var
  i: integer;
Begin
  i:=items[5]+1;
  items[5]:=i;
  if i<11 then
    Charout (26+i,10,23);  {scroll icon}
End;


{         }
{ TakeKey }
{         }
Function TakeKey:boolean;
Var
  i: integer;
Begin
  if items[1]>0 then
    Begin
      i:=items[1]-1;
      items[1]:=i;
      if i<10 then
        Charout (27+i,7,32);
      takekey:=true;
      playsound (opendoorsnd);
    end
  else
    Begin
      takekey:=false;
      playsound (noitemsnd);
    end;
End;


{            }
{ TakePotion }
{            }
Procedure TakePotion;
Var
  i: integer;
Begin
  if items[2]>0 then
    Begin
      i:=items[2]-1;
      items[2]:=i;
      if i<11 then
        Charout (27+i,8,32);
      playsound(potionsnd);
      o[0].hp:=13;
      obj.hp:=13;
      printbody;           {update the body meter}
    end
  else
    playsound(noitemsnd);
End;


{          }
{ CastBolt }
{          }
Procedure CastBolt;
Var
  i: integer;
Begin
  if items[3]>0 then
    begin
      i:=items[3]-1;
      items[3]:=i;
      if i<11 then
        Charout (27+i,9,32);
      boltsleft:=8;

      playsound(spellsnd);
    end
  else
    playsound(noitemsnd);
End;



{          }
{ CastNuke }
{          }
Procedure CastNuke;
Var
  i,x,y,n: integer;
  base: activeobj;
Begin
  if items[5]>0 then
    begin
      i:=items[5]-1;
      items[5]:=i;
      if i<11 then
	Charout (27+i,10,32);

      with base do
        begin
          delay:=0;
          stage:=0;
          active:=true;
          x:=obj.x;
	  y:=obj.y;      {start bigshot at same coordinate at player}
	  oldx:=x;
	  oldy:=y;
	  oldtile:=-1;
          class:=bigshot;
        end;

      for x:=-1 to 1 do
        begin            {make a whole buch of bigshots}
	  n:=newobject;
          o[n]:=base;
          o[n].x:=o[n].x+x*2;
          o[n].dir:=north;
          n:=newobject;
          o[n]:=base;
          o[n].x:=o[n].x+x*2;
          o[n].dir:=south;
	  n:=newobject;
          o[n]:=base;
          o[n].y:=o[n].y+x*2;
          o[n].dir:=east;
          n:=newobject;
          o[n]:=base;
          o[n].y:=o[n].y+x*2;
          o[n].dir:=west;
	end;

      playsound(spellsnd);
      obj.stage:=2;
      obj.delay:=4;
    end
  else
    playsound(noitemsnd);
End;



{=======================================}
{                                       }
{ PLAYSHOOT / PLAYBIGSHOOT              }
{ Launches a missile of the proper type }
{ from the current object.  Chooses from}
{ smallshot, bigshot, and monshot.      }
{                                       }
{=======================================}

Procedure PlayShoot;
Begin
  obj.stage:=2;
  obj.delay:=4;
  playsound (shotsnd);
  with o[newobject] do  {get a free spot}
    begin
      class:=shot;
      side:=side xor 1;
      delay:=0;
      stage:=0;
      active:=true;
      dir:=obj.dir;  {missile is allways facing same way as thrower}
      case dir of
        north: Begin
                 x:=obj.x+side;
                 y:=obj.y;
               end;
        east:  Begin
                 x:=obj.x+1;
                 y:=obj.y+side;
               end;
        south: Begin
                 x:=obj.x+side;
                 y:=obj.y+1;
               end;
        west:  Begin
                 x:=obj.x;
                 y:=obj.y+side;
               end;
      end;
    end;
End;



Procedure PlayBigShoot;
Begin
  obj.stage:=2;
  if boltsleft=0 then
    obj.delay:=4;
  playsound (bigshotsnd);
  with o[newobject] do  {get a free spot}
    begin
      delay:=0;
      stage:=0;
      active:=true;
      dir:=obj.dir;  {missile is allways facing same way as thrower}
      x:=obj.x;
      y:=obj.y;      {start bigshot at same coordinate at player}
      class:=bigshot;
    end;
End;


{============================}
{                            }
{ GIVESCROLL                 }
{ Randomly gives a bolt/nuke }
{                            }
{============================}

Procedure GiveScroll;
Var
  r:integer;
Begin
  case random(256) of
    0..175: GiveBolt;
    176..255: GiveNuke;
  end;
end;


{=========================================}
{                                         }
{ OPENDOOR                                }
{ Open the door with a piece at CHKX,CHKY }
{                                         }
{=========================================}

Procedure Opendoor;
var
 x,y:integer;
Begin
{clears door icons both ways from the point contacted}
 playsound (opendoorsnd);
 x:=chkx;
 y:=chky;
 If chkspot=165 then
   Begin                      {vertical doors}
     Repeat
       view[y,x]:=blankfloor;
       background[y,x]:=blankfloor;
       y:=y-1;
     until view[y,x]<>165;
     y:=chky;
     Repeat
       view[y,x]:=blankfloor;
       background[y,x]:=blankfloor;
       y:=y+1;
     until view[y,x]<>165;
   end
 else
   Begin                     {horizontal doors}
     Repeat
       view[y,x]:=blankfloor;
       background[y,x]:=blankfloor;
       x:=x-1;
     until view[y,x]<>166;
     x:=chkx;
     Repeat
       view[y,x]:=blankfloor;
       background[y,x]:=blankfloor;
       x:=x+1;
     until view[y,x]<>166;
   end

end;



{**************************************************************************}


{======================================}
{                                      }
{ TAGOBJECT                            }
{ Have the OBJ do its damage to ALTOBJ }
{                                      }
{======================================}

Procedure TagObject;

Begin
  altobj.hp:=altobj.hp-obj.damage;
  if altobj.hp<1 then

{it died}

    Begin
{      erasealtobj;		}	{because dead has lower priority}

      if altobj.class = player then
	Begin
	  o[0].hp:=0;
	  printbody;
	  playsound (killplayersnd);
	  playdone:=true;               {GAMEOVER by way of death}
	  gamexit:=killed;
	end

      else

	Begin
	  score:=score+AltObj.points; {give player points for a kill}
	  printscore;
	  playsound (killmonsnd);
	end;
      {change class to a deadthing of same size}
      o[altnum].class:=classtype(integer(dead1)-1+altobj.size);
      o[altnum].delay:=2;
      o[altnum].stage:=0; {start of fade}
    End

  else

{wasn't killed}

    Begin
      o[altnum].hp:=altobj.hp;	{save the new hp status}
      o[altnum].stage:=3;	{set it to ouch stage}
      if altnum=0 then
	Begin
	  o[0].delay:=2;
	  printbody;     {update body bar on screen}
	  playsound (tagplayersnd);
	end
      else
	Begin
	  o[altnum].delay:=4;      {four frames for monsters}
	  playsound (tagmonsnd);
	end;
    End;
End;


{==============================}
{                              }
{ INTOMONSTER                  }
{ OBJ Contacted another object }
{                              }
{==============================}

Function IntoMonster:boolean;
var
  i:integer;
  gotit:boolean;
Begin
  intomonster:=false;  {unless told otherwise.}

{figure out which object got hit}

  altnum:=0;
  gotit:=false;
  repeat
  { make a copy of the objects info into a global varriable }

    move (o[altnum],altobj.active,sizeof(o[altnum]) );
    If (altobj.class>nothing) and (altnum<>objecton) then
      begin
	move (ObjDef[altobj.class],altobj.think,sizeof(ObjDef[altobj.class]) );
	if (chkx>=altobj.x) and (chkx-altobj.x<altobj.size)
	and (chky>=altobj.y) and (chky-altobj.y<altobj.size) then
	  if altobj.solid then
	    gotit:=true
	  else
	    if (objecton=0) and (altobj.class=teleporter) then
	    {player got to the teleporter}
	      Levelcleared;
      end;
    if not gotit then
      inc(altnum);
  until (gotit) or (altnum>numobj);


  if not gotit then
    begin
      intomonster:=true;	{hit something not solid}
      exit;
    end;

{resolve contact based on attacker and target}

  Case obj.contact of

    Benign: exit;	{benign objects just don't move through others}

    monster,mshot: if altnum=0 then
	       Begin
		 tagobject;
		 obj.stage:=2;   {set it to attack stage}
		 obj.delay:=20;   {delay for several frames}
	       end
	     else if altobj.class=shot then {they can walk into shots}
	       intomonster:=true;

    pshot: if altnum>0 then
	     tagobject;

    nukeshot: Begin
		tagobject;
		intomonster:=true;   {nuke shots keep going}
	      end;
  end;
End;


{=======================================}
{                                       }
{ WALKTHROUGH                           }
{ OBJ is trying to walk through CHKSPOT }
{ at CHKX,CHKY, is it ok?               }
{                                       }
{=======================================}

Function Walkthrough:boolean;
label
 ok,notok;

Begin


case chkspot of

{ big object }

  tile2s..lasttile:
     Begin
       walkthrough:=intomonster;
       exit;
     end;

{ walls }

 129..135: Begin
  if ( (obj.contact=pshot) or (obj.contact=nukeshot) or (obj.contact=mshot) ) then
  {make an explosion over the wall}
     with o[newobject] do
       Begin
	 active:=true;
	 x:=chkx;
	 y:=chky;
	 stage:=0;
	 delay:=2;
	 class:=wallhit;
	 playsound (tagwallsnd);
       end;
    goto notok;
  end;

{ exploding walls }

  136..145: if ((obj.contact=pshot) or (obj.contact=nukeshot)) then
   Begin
     playsound (tagwallsnd);
     case chkspot of
       136..142: background[chky,chkx]:=blankfloor;
       143..145: background[chky,chkx]:=chkspot+19;
       {hidden potion,scroll,key}
     end;
     {make an explosion over the wall}
     with o[newobject] do
       Begin
	 active:=true;
	 x:=chkx;
	 y:=chky;
	 stage:=0;
	 delay:=2;
	 class:=dead1;
       end;
     if obj.contact=pshot then
       goto notok
     else
       goto ok;             {nuke shots keep going after blowing up one}
   end

  else
    Begin
      walkthrough:=false;	{nothing else goes through exploding walls}
      exit;
    end;

{ potion bottle }

   162: Begin
	  If obj.class=player then
	    Begin
	      givepotion;
	      view[chky,chkx]:=blankfloor;  {erase icon}
	      background[chky,chkx]:=blankfloor;
	      playsound(itemsnd);
	    End;
	  goto ok;       {everything but player just walks over it}
	end;

{scroll}

   163: Begin
	  If obj.class=player then
	    Begin
	      givescroll;
	      view[chky,chkx]:=blankfloor;  {erase icon}
	      background[chky,chkx]:=blankfloor;
	      playsound(itemsnd);
	    End;
	  goto ok;       {everything but player just walks over it}
	end;

{ key }

   164: Begin
	  If obj.class=player then
	    Begin
	      givekey;
	      view[chky,chkx]:=blankfloor;  {erase icon}
	      background[chky,chkx]:=blankfloor;
	      playsound(itemsnd);
	    End;
	  goto ok;       {everything but player just walks over it}
	end;

{ doors }

   165..166: Begin
	  If obj.class=player then
	    Begin
	      if takekey then
		Begin
		  opendoor;
		  goto ok;
		end;
	    End;
	  goto notok;       {blocks everything else}
	end;

{ treasure chest }

   167: Begin
	  If obj.class=player then
	    Begin
	      score:=score+500;
	      printscore;
	      background[chky,chkx]:=blankfloor;
	      playsound(treasuresnd);
	    End;
	  goto ok;       {everything but player just walks over it}
	end;

{ blowing up walls }

  29..31: goto ok;

end;


notok:
 walkthrough:=false;
 exit;

ok:
 walkthrough:=true;
End;



{==========================================}
{                                          }
{ WALK                                     }
{ Tries to move the object forward.  If it }
{ touches another object, contact will be  }
{ resolved based on CONTACT.  Returns a    }
{ true / false whether the move is OK      }
{                                          }
{==========================================}

Function Walk: boolean;
label
  goodmove,badmove;

Var
  i,size,newx,newy,deltay,deltax:integer;
  try: boolean;

Begin
  case obj.dir of
    north: Begin
             newx:=obj.x;
             newy:=obj.y-1;
             chkx:=newx;
             chky:=newy;
             deltax:=1;
             deltay:=0;
           end;
    east : Begin
             newx:=obj.x+1;
             newy:=obj.y;
	     chkx:=obj.x+obj.size;
             chky:=newy;
             deltax:=0;
             deltay:=1;
           end;
    south: Begin
             newx:=obj.x;
             newy:=obj.y+1;
             chkx:=newx;
	     chky:=obj.y+obj.size;
             deltax:=1;
             deltay:=0;
           end;
    west : Begin
             newx:=obj.x-1;
             newy:=obj.y;
             chkx:=newx;
             chky:=newy;
             deltax:=0;
             deltay:=1;
           end;
    else goto badmove;   {should never happen}
  end;

  for i:=1 to obj.size do
    Begin
       chkspot:=view[chky,chkx];
      if chkspot<>blankfloor then
	begin
	  try:=walkthrough;
	  if leveldone then			{player hit the teleporter}
	  begin
	    walk:=true;
	    exit;
	  end;
          if obj.stage=2 then		{if they attacked something, its good}
	    begin
	      walk:=true;
	      exit;
	    end;
	  If not try then		{ran into something that's not ok}
	    Goto badmove;
	end;
      chkx:=chkx+deltax;
      chky:=chky+deltay;
    End;

goodmove:

  obj.x:=newx;
  obj.y:=newy;
  obj.stage:=obj.stage xor 1;       {toggle walking frame}
  walk:=true;
  exit;

badmove:

  walk:=false;
End;


{**************************************************************************}

{================}
{                }
{ PlayerCMDTHINK }
{                }
{================}

Procedure playerCMDTHINK;
Var
  dir,olddir: dirtype;
  button1, button2: boolean;
  demobyte: byte;
Begin
  PlayerIO (dir,button1,button2);	{see what the input device is doing}
  obj.Stage:=obj.stage and 1;   {cancle attack or damaged stage}

{                    }
{ if creating a demo }
{ record the command }
{                    }
  if playmode= demosave then
    Begin
      demobyte:=(integer(dir) shl 2) or (integer(button1) shl 1)
      or (integer(button2));
      democmds[frameon]:=demobyte;
    end;


{              }
{ Cheat key... }
{              }
  if button1 and button2 and keydown[$10] then 	{'Q' + b1 + b2}
    begin
      givepotion;
      givescroll;
      givekey;
    end;

  If playmode = demogame then
    Begin
{           }
{ DemoInput }
{           }
{$IFNDEF SAMPLER}
      if keydown[$12] then		{'E' key}
	Begin                {go into editor mode}
	  playdone:=true;
	  playmode:=editor;
	  exit;
	end
      else
{$ENDIF}
	if keydown[$39] or button1 or button2 then {starts a game}
	  Begin
	    playdone:=true;
	    playmode:=game;
	    exit;
	  end;

      demobyte:=democmds[frameon];
      dir:=dirtype (demobyte shr 2);
      button1:=(demobyte and 2)>0;
      button2:=(demobyte and 1)>0;
    end;

{                      }
{ carry out the action }
{                      }

  if dir<nodir then
    Begin
      if dir>west then
	dir:=dirtype(ord(dir)-4);	{no diagonals...}

      if button2 then      {if button 2 is down, the move will not}
        olddir:=obj.dir;   {change the direction of the figure (STRAFE)}
      obj.dir:=dir;        {set new direction}
      if odd(frameon) then	{player moves once every two frames}
	begin
	  if walk then
	    Begin
	      originx := obj.x-11;
	      originy := obj.y-11;
	    end
	  else
	    Playsound (blockedsnd);
	end;
      If button2 then
        obj.dir:=olddir;   {restore original direction}
    End;

  if boltsleft>0 then

{ a bolt spell is still going off }

    Begin
      if frameon mod 3=0 then
	Begin
	  playbigshoot;		{let off a bigshot}
	  dec (boltsleft);
	end;
    end

  else

{ button 1 builds shot power / shoots }

    Begin

      if button1 then
	Begin
	  if shotpower=0 then
	    shotpower:=1	{give power one immediately}
	  else
	    if (shotpower<13) and odd(frameon) then	{give extra's only 2 frames}
	      inc(shotpower);
	  printshotpower;
	end
      else
	if shotpower>0 then   {player just released the fire button}
	  Begin
	    if shotpower=13 then
	      playbigshoot
	    else
	      Playshoot;
	    shotpower:=0;
	    printshotpower;
	  end;

    end;

{                                       }
{ keys to cast spells and drink potions }
{                                       }
  If playmode=game then
    Begin
      if keydown [$19] or keydown [$39] then		{'P' or ' ' keys}
	begin
	  if obj.hp<13 then	{don't take a potion if not needed}
	    takepotion;
	  keydown [$19]:=false;
	  keydown [$39]:=false;
	end
      else
	if keydown [$30] then				{'B' key}
	  Begin
	    castbolt;
	    keydown [$30]:=false;
	  end
	else
	  if keydown [$31] or keydown [$1c] then	{'N' or RET keys}
	    Begin
	      castnuke;
	      keydown [$31]:=false;
	      keydown [$1c]:=false;
	    end;
    end;



End;


{===============================================}
{                                               }
{ ChaseTHINK                                    }
{ have the current monster go after the player, }
{ either diagonally or straight on              }
{                                               }
{===============================================}

procedure chaseTHINK (diagonal:boolean);

var
  deltax,deltay,i: integer;
  d: array[1..2] of dirtype;
  tdir, olddir, turnaround: dirtype;
begin
  obj.Stage:=obj.stage and 1;   {cancle attack or damaged stage}
  olddir:=obj.dir;
  TurnAround:=opposite[olddir];

  deltax:=o[0].x-obj.x;
  deltay:=o[0].y-obj.y;
  for i:=1 to 2 do
    d[i]:=nodir;

  if deltax>0 then
    d[1]:= east;
  if deltax<0 then
    d[1]:= west;
  if deltay>0 then
    d[2]:=south;
  if deltay<0 then
    d[2]:=north;

  if abs(deltay)>abs(deltax) then
    begin
      tdir:=d[1];
      d[1]:=d[2];
      d[2]:=tdir;
    end;

  for i:=1 to 2 do
    if d[i]=TurnAround then
      d[i]:=nodir;


if diagonal then
begin                           {ramdiagonals try the best dir first}
  if d[1]<>nodir then
    begin
      obj.dir:=d[1];
      if walk or (obj.stage=3) then
	exit;			{either moved forward or attacked}
    end;

  if d[2]<>nodir then
    begin
      obj.dir:=d[2];
      if walk or (obj.stage=3) then
	exit;
    end;
end
else
begin             		{ramstraights try the second best dir first}

  if d[2]<>nodir then
    begin
      obj.dir:=d[2];
      if walk or (obj.stage=3) then
	exit;
    end;

  if d[1]<>nodir then
    begin
      obj.dir:=d[1];
      if walk or (obj.stage=3) then
	exit;
    end;

end;

{ there is no direct path to the player, so pick another direction }

  obj.dir:=olddir;
  if walk or (obj.stage=3) then
    exit;

  if random(256)>128 then	{randomly determine direction of search}
    begin
      for tdir:=north to west do
	if tdir<>TurnAround then
	  begin
	    obj.dir:=tdir;
	    if walk or (obj.stage=3) then
	      exit;
	  end
    end

  else

    begin
      for tdir:=west downto north do
	if tdir<>TurnAround then
	  begin
	    obj.dir:=tdir;
	    if walk or (obj.stage=3) then
	      exit;
	  end;
    end;

  obj.dir:=turnaround;
  altkey:=walk;		{last chance, don't worry about returned value}
end;


{===========}
{           }
{ GargTHINK }
{           }
{===========}

procedure gargTHINK;
var
  deltax,deltay: integer;
  n: integer;
begin

  if random (256)>220 then	{only shoot once in a while}
    begin
      obj.stage:=2;
      obj.delay:=6;
{oundon:=false;}
      playsound (shotsnd);
      n:=newobject;
      with o[n] do  {get a free spot}
	begin
	  class:=rock;
	  side:=side xor 1;
	  delay:=0;
	  stage:=0;
	  active:=true;
	  dir:=obj.dir;  {missile is allways facing same way as thrower}
	  case dir of
	    north: Begin
		     x:=obj.x+1+side;
		     y:=obj.y;
		   end;
	    east:  Begin
		     x:=obj.x+3;
		     y:=obj.y+1+side;
		   end;
	    south: Begin
		     x:=obj.x+1+side;
		     y:=obj.y+3;
		   end;
	    west:  Begin
		     x:=obj.x;
		     y:=obj.y+1+side;
		   end;
	  end;
	end;
	exit;
    end;

  ChaseTHINK (false);		{otherwise chase straight}
end;


{=============}
{             }
{ DragonTHINK }
{             }
{=============}

procedure dragonTHINK;
var
  deltax,deltay: integer;
  n: integer;
  facing: dirtype;
begin
  deltax:=o[0].x-obj.x;
  deltay:=o[0].y-obj.y;
  if abs(deltax) > abs(deltay) then
    begin
      if deltax>0 then
	facing:=west
      else
	facing:=east;
    end
  else
    begin
      if deltay>0 then
	facing:=north
      else
	facing:=south;
    end;

  if random (256)>220 then	{only shoot once in a while}
    begin
      obj.stage:=2;
      obj.delay:=6;
{oundon:=false;}
      playsound (shotsnd);
      n:=newobject;
      with o[n] do  {get a free spot}
	begin
	  class:=bigshot;
	  side:=side xor 1;
	  delay:=0;
	  stage:=0;
	  active:=true;
	  dir:=obj.dir;  {missile is allways facing same way as thrower}
	  case dir of
	    north: Begin
		     x:=obj.x+1+side;
		     y:=obj.y;
		   end;
	    east:  Begin
		     x:=obj.x+3;
		     y:=obj.y+1+side;
		   end;
	    south: Begin
		     x:=obj.x+1+side;
		     y:=obj.y+3;
		   end;
	    west:  Begin
		     x:=obj.x;
		     y:=obj.y+1+side;
		   end;
	  end;
	end;
	exit;
    end;

  ChaseTHINK (false);		{otherwise chase straight}
end;



{==============}
{              }
{ ShooterTHINK }
{              }
{==============}

Procedure shooterTHINK;
Begin
  if (obj.x<originx-1) or (obj.y<originy-1)
  or (obj.x>originx+22) or (obj.y>originy+22)
  or not walk or (obj.stage=2) then
    Begin
      obj.class:=nothing;
    end;
End;


{===========}
{           }
{ IdleTHINK }
{           }
{===========}

Procedure idleTHINK;
Begin
  inc(obj.stage);
  obj.delay:=2;
  if obj.stage=Obj.stages then
    obj.stage:=0;
End;


{===========}
{           }
{ FadeTHINK }
{           }
{===========}

Procedure fadeTHINK;
Begin
  inc(obj.stage);
  obj.delay:=2;
  sx:=37;
  sy:=0;
  if obj.stage = obj.stages then
    Begin
      obj.class:=nothing;
    end;
End;


{========================================}
{                                        }
{ THINK                                  }
{ Decides what the object is going to do }
{ and does it. The object will definately}
{ be redrawn, if nothing else            }
{                                        }
{========================================}

Procedure Think;
Begin
  If obj.delay>0 then
    dec (obj.delay)      {if the object has a delay, it will do NOTHING}

  Else
    Begin
      If random (255) < Obj.speed then
	Case Obj.think of
          playerCMD : PlayerCMDTHINK;
	  ramstraight : ChaseTHINK (false);
	  ramdiag : ChaseTHINK (true);
	  gargCMD: gargTHINK;
	  dragonCMD: dragonTHINK;
	  straight : ShooterTHINK;
          idle : IdleTHINK;
          fade : FadeTHINK;
        end;
    end;

End;


{==========}
{          }
{ DoActive }
{          }
{==========}

Procedure DoActive;
Begin
{see if it is way off screen, so deactivate}
  if (Obj.x<originx-10) or (obj.x>originx+34)
  or (obj.y<originy-10) or (obj.y>originy+34) then
    begin
      o[objecton].active:=false;
    end
  else
    begin
      Think;
      eraseobj;
      If playdone=true then
	exit;
   {redraw it even if it hasn't moved, in case it was hit}
      if obj.class>nothing then
	drawobj;
      {write the temporary info back into the array}
      move (obj.active,o[objecton],sizeof(o[objecton]) );
    end;
end;

{============}
{            }
{ DoInactive }
{            }
{============}

Procedure DoInactive;
Begin
  {if the object just became visable, make it active}

  If (obj.x+obj.size>=originx) and (obj.x<originx+24)
  and (obj.y+obj.size>=originy) and (obj.y<originy+24) then
    begin
      obj.active:=true;
      obj.dir:=north;
      {write the temporary info back into the array}
      move (obj.active,o[objecton],sizeof(o[objecton]) );
    end;
End;


{======================================}
{                                      }
{ PLAYLOOP                             }
{ All the action is directed from here }
{                                      }
{======================================}

Procedure PlayLoop;
var
  i,j:integer;
label
  done;

Begin
  Repeat   {until playdone}

    if (playmode<>demogame) and (playmode<>demosave) then
      Begin
	Centerwindow (11,2);
	Print (' Entering]level ');
	shortnum (level);
	Print ('...');
	playsound (leveldonesnd);
	waitendsound;
      end;

    clearold;    {don't refresh the window yet}

    loadlevel; {load the level to play}
    leveldone:= false;

    if (playmode=demogame) or (playmode=demosave) then
      initrnd (false);

    playdone:=false;
    frameon:=0;
    boltsleft:=0;
    shotpower:=0;
    printshotpower;

    doall;

    if (playmode=demosave) or (playmode=demogame) then
      playdone:=true;

  until playdone;

End;

