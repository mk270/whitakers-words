-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936–2010)
--
-- This is a free program, which means it is proper to copy it and pass
-- it on to your friends. Consider it a developmental item for which
-- there is no charge. However, just for form, it is Copyrighted
-- (c). Permission is hereby freely given for any and all use of program
-- and data. You can sell it as your own, but at least tell me.
--
-- This version is distributed without obligation, but the developer
-- would appreciate comments and suggestions.
--
-- All parts of the WORDS system, source code and data files, are made freely
-- available to anyone who wishes to use them, for whatever purpose.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Words_Engine.Trick_Tables is

   Tricks_Exception : exception;

   type Trick_Class is (TC_Flip_Flop, TC_Flip, TC_Internal, TC_Slur);

   type Trick (Op : Trick_Class := TC_Flip_Flop) is
      record
         Max : Integer := 0;
         case Op is
            when TC_Flip_Flop =>
               FF1 : Unbounded_String := Null_Unbounded_String;
               FF2 : Unbounded_String := Null_Unbounded_String;
            when TC_Flip =>
               FF3 : Unbounded_String := Null_Unbounded_String;
               FF4 : Unbounded_String := Null_Unbounded_String;
            when TC_Internal =>
               I1 : Unbounded_String := Null_Unbounded_String;
               I2 : Unbounded_String := Null_Unbounded_String;
            when TC_Slur =>
               S1 : Unbounded_String := Null_Unbounded_String;
         end case;
      end record;

   type TricksT is array (Integer range <>) of Trick;

   type Strings is array (Integer range <>) of Unbounded_String;

   function Member (Needle   : Unbounded_String;
                    Haystack : Strings) return Boolean;

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   function Common_Prefix (S : String) return Boolean;

   function Get_Tricks_Table (C : Character) return TricksT;

   function Get_Slur_Tricks_Table (C : Character) return TricksT;

   Any_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Internal, I1 => +"ae", I2 => +"e"),
     (Max => 0, Op => TC_Internal, I1 => +"bul", I2 => +"bol"),
     (Max => 0, Op => TC_Internal, I1 => +"bol", I2 => +"bul"),
     (Max => 0, Op => TC_Internal, I1 => +"cl", I2 => +"cul"),
     (Max => 0, Op => TC_Internal, I1 => +"cu", I2 => +"quu"),
     (Max => 0, Op => TC_Internal, I1 => +"f", I2 => +"ph"),
     (Max => 0, Op => TC_Internal, I1 => +"ph", I2 => +"f"),
     (Max => 0, Op => TC_Internal, I1 => +"h", I2 => +""),
     (Max => 0, Op => TC_Internal, I1 => +"oe", I2 => +"e"),
     (Max => 0, Op => TC_Internal, I1 => +"vul", I2 => +"vol"),
     (Max => 0, Op => TC_Internal, I1 => +"vol", I2 => +"vul"),
     (Max => 0, Op => TC_Internal, I1 => +"uol", I2 => +"vul")
  );

   Mediaeval_Tricks : constant TricksT := (
     --  Harrington/Elliott    1.1.1
     (Max => 0, Op => TC_Internal, I1 => +"col", I2 => +"caul"),
     --  Harrington/Elliott    1.3
     (Max => 0, Op => TC_Internal, I1 => +"e", I2 => +"ae"),
     (Max => 0, Op => TC_Internal, I1 => +"o", I2 => +"u"),
     (Max => 0, Op => TC_Internal, I1 => +"i", I2 => +"y"),
     --  Harrington/Elliott    1.3.1
     (Max => 0, Op => TC_Internal, I1 => +"ism", I2 => +"sm"),
     (Max => 0, Op => TC_Internal, I1 => +"isp", I2 => +"sp"),
     (Max => 0, Op => TC_Internal, I1 => +"ist", I2 => +"st"),
     (Max => 0, Op => TC_Internal, I1 => +"iz", I2 => +"z"),
     (Max => 0, Op => TC_Internal, I1 => +"esm", I2 => +"sm"),
     (Max => 0, Op => TC_Internal, I1 => +"esp", I2 => +"sp"),
     (Max => 0, Op => TC_Internal, I1 => +"est", I2 => +"st"),
     (Max => 0, Op => TC_Internal, I1 => +"ez", I2 => +"z"),
     --  Harrington/Elliott    1.4
     (Max => 0, Op => TC_Internal, I1 => +"di", I2 => +"z"),
     (Max => 0, Op => TC_Internal, I1 => +"f", I2 => +"ph"),
     (Max => 0, Op => TC_Internal, I1 => +"is", I2 => +"ix"),
     (Max => 0, Op => TC_Internal, I1 => +"b", I2 => +"p"),
     (Max => 0, Op => TC_Internal, I1 => +"d", I2 => +"t"),
     (Max => 0, Op => TC_Internal, I1 => +"v", I2 => +"b"),
     (Max => 0, Op => TC_Internal, I1 => +"v", I2 => +"f"),
     (Max => 0, Op => TC_Internal, I1 => +"v", I2 => +"f"),
     (Max => 0, Op => TC_Internal, I1 => +"s", I2 => +"x"),
     --  Harrington/Elliott    1.4.1
     (Max => 0, Op => TC_Internal, I1 => +"ci", I2 => +"ti"),
     --  Harrington/Elliott    1.4.2
     (Max => 0, Op => TC_Internal, I1 => +"nt", I2 => +"nct"),
     (Max => 0, Op => TC_Internal, I1 => +"s", I2 => +"ns"),
     --  Others
     (Max => 0, Op => TC_Internal, I1 => +"ch", I2 => +"c"),
     (Max => 0, Op => TC_Internal, I1 => +"c", I2 => +"ch"),
     (Max => 0, Op => TC_Internal, I1 => +"th", I2 => +"t"),
     (Max => 0, Op => TC_Internal, I1 => +"t", I2 => +"th")
  );

end Words_Engine.Trick_Tables;
