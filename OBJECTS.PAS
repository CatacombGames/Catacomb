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

{                        }
{ initialize object info }
{                        }

Procedure initobjects;
Begin

with ObjDef [player] do
  begin
    think	:= playercmd;
    contact	:= benign;
    solid	:= true;
    firstchar	:= tile2s;
    size	:= 2;
    stages	:= 4;
    dirmask	:= 3;
    speed	:= 256;
    hitpoints	:= 12;
    damage	:= 0;
    points	:= 0;
  end;


with ObjDef [goblin] do
  begin
    think	:= ramstraight;
    contact	:= monster;
    solid	:= true;
    firstchar	:= tile2s+64;
    size	:= 2;
    stages	:= 4;
    dirmask	:= 3;
    speed	:= 75;
    hitpoints	:= 1;
    damage	:= 1;
    points	:= 50;
  end;


with ObjDef [skeleton] do
  begin
    think	:= ramdiag;
    contact	:= monster;
    solid	:= true;
    firstchar	:= tile2s+128;
    size	:= 2;
    stages	:= 4;
    dirmask	:= 3;
    speed	:= 130;
    hitpoints	:= 1;
    damage	:= 1;
    points	:= 150;
  end;


with ObjDef [ogre] do
  begin
    think	:= ramstraight;
    contact	:= monster;
    solid	:= true;
    firstchar	:= tile3s;
    size	:= 3;
    stages	:= 4;
    dirmask	:= 3;
    speed	:= 75;
    hitpoints	:= 5;
    damage	:= 2;
    points	:= 250;
  end;


with ObjDef [gargoyle] do
  begin
    think	:= gargcmd;
    contact	:= monster;
    solid	:= true;
    firstchar	:= tile4s;
    size	:= 4;
    stages	:= 4;
    dirmask	:= 3;
    speed	:= 150;
    hitpoints	:= 10;
    damage	:= 3;
    points	:= 500;
  end;


with ObjDef [dragon] do
  begin
    think	:= dragoncmd;
    contact	:= monster;
    solid	:= true;
    firstchar	:= tile5s;
    size	:= 5;
    stages	:= 4;
    dirmask	:= 3;
    speed	:= 100;
    hitpoints	:= 100;
    damage	:= 5;
    points	:= 1000;
  end;


with ObjDef [wallhit] do	{an explosion on a wall that didn't blow up}
  begin
    think	:= fade;
    contact	:= benign;
    solid	:= true;
    firstchar	:= 26;
    size	:= 1;
    stages	:= 3;
    dirmask	:= 0;
    speed	:= 80;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;


with ObjDef [dead1] do
  begin
    think	:= fade;
    contact	:= benign;
    solid	:= false;
    firstchar	:= 29;
    size	:= 1;
    stages	:= 3;
    dirmask	:= 0;
    speed	:= 80;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;


with ObjDef [dead2] do
  begin
    think	:= fade;
    contact	:= benign;
    solid	:= false;
    firstchar	:= tile2s+224;
    size	:= 2;
    stages	:= 3;
    dirmask	:= 0;
    speed	:= 80;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;


with ObjDef [dead3] do
  begin
    think	:= fade;
    contact	:= benign;
    solid	:= false;
    firstchar	:= tile3s + 9*16;
    size	:= 3;
    stages	:= 3;
    dirmask	:= 0;
    speed	:= 80;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;


with ObjDef [dead4] do
  begin
    think	:= fade;
    contact	:= benign;
    solid	:= false;
    firstchar	:= tile4s + 16*16;
    size	:= 4;
    stages	:= 3;
    dirmask	:= 0;
    speed	:= 80;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;


with ObjDef [dead5] do
  begin
    think	:= fade;
    contact	:= benign;
    solid	:= false;
    firstchar	:= tile5s + 25*16;
    size	:= 5;
    stages	:= 3;
    dirmask	:= 0;
    speed	:= 80;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;

with ObjDef [shot] do
  begin
    think	:= straight;
    contact	:= pshot;
    solid	:= false;
    firstchar	:= 154;
    size	:= 1;
    stages	:= 2;
    dirmask	:= 3;
    speed	:= 256;
    hitpoints	:= 0;
    damage	:= 1;
    points	:= 0;
  end;


with ObjDef [rock] do
  begin
    think	:= straight;
    contact	:= mshot;
    solid	:= false;
    firstchar	:= 152;
    size	:= 1;
    stages	:= 2;
    dirmask	:= 0;
    speed	:= 256;
    hitpoints	:= 0;
    damage	:= 1;
    points	:= 0;
  end;


with ObjDef [bigshot] do
  begin
    think	:= straight;
    contact	:= nukeshot;
    solid	:= false;
    firstchar	:= tile2s+192;
    size	:= 2;
    stages	:= 2;
    dirmask	:= 3;
    speed	:= 256;
    hitpoints	:= 0;
    damage	:= 1;
    points	:= 0;
  end;


with ObjDef [teleporter] do
  begin
    think	:= idle;
    contact	:= benign;
    solid	:= false;
    firstchar	:= tile2s+236;
    size	:= 2;
    stages	:= 5;
    dirmask	:= 0;
    speed	:= 200;
    hitpoints	:= 0;
    damage	:= 0;
    points	:= 0;
  end;




{

with ObjDef [player] do
  begin
    think	:= (playercmd,gargcmd,dragcmd,ramstraight,ramdiag,straight,idle,fade,;
    contact	:= (benign,monster,pshot,mshot,nukeshot);
    solid	:= boolean;
    firstchar	:= ;
    size	:= ;
    stages	:= ;
    dirmask	:= ;
    speed	:= ;
    hitpoints	:= ;
    damage	:= ;
    points	:= ;
  end;


}

end;