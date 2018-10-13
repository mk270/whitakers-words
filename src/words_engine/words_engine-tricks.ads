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

with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Words_Engine.Explanation_Package; use Words_Engine.Explanation_Package;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Words_Engine.Tricks is

   procedure Syncope
     (W       : String;
      Pa      : in out Parse_Array;
      Pa_Last : in out Integer;
      Xp      : in out Explanations);

   procedure Try_Tricks
     (W           : String;
      Pa          : in out Parse_Array;
      Pa_Last     : in out Integer;
      Line_Number : Integer;
      Word_Number : Integer;
      Xp          : in out Explanations);

   procedure Try_Slury
     (W           : String;
      Pa          : in out Parse_Array;
      Pa_Last     : in out Integer;
      Line_Number : Integer;
      Word_Number : Integer;
      Xp          : in out Explanations);

   procedure Roman_Numerals
     (Input_Word : String;
      Pa         : in out Parse_Array;
      Pa_Last    : in out Integer;
      Xp         : in out Explanations);

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

   type Tricks is array (Integer range <>) of Trick;

   package Tables is
      function Common_Prefix (S : String) return Boolean;
   end Tables;

end Words_Engine.Tricks;