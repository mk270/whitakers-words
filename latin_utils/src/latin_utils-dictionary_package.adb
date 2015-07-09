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

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
package body Latin_Utils.Dictionary_Package is

   ---------------------------------------------------------------------------

   MNPC_IO_Default_Width : constant Natural := 6;
   Numeral_Value_Type_IO_Default_Width : constant Natural := 5;

   ---------------------------------------------------------------------------

   function Number_Of_Stems (Part : Part_Of_Speech_Type) return Stem_Key_Type is
   begin
      case Part is
         when n       => return 2;
         when pron    => return 2;
         when pack    => return 2;
         when adj     => return 4;
         when num     => return 4;
         when adv     => return 3;
         when v       => return 4;
         when vpar    => return 0;
         when supine  => return 0;
         when prep    => return 1;
         when conj    => return 1;
         when interj  => return 1;
         when x       => return 0;
         when tackon .. suffix => return 0;
      end case;
   end Number_Of_Stems;

   ---------------------------------------------------------------------------

   package body Parse_Record_IO is separate;
   package body Noun_Entry_IO is separate;
   package body Pronoun_Entry_IO is separate;
   package body Propack_Entry_IO is separate;
   package body Adjective_Entry_IO is separate;
   package body Numeral_Entry_IO is separate;
   package body Adverb_Entry_IO is separate;
   package body Verb_Entry_IO is separate;
   package body Preposition_Entry_IO is separate;
   package body Conjunction_Entry_IO is separate;
   package body Interjection_Entry_IO is separate;
   package body Part_Entry_IO is separate;
   package body Kind_Entry_IO is separate;
   package body Translation_Record_IO is separate;
   package body Dictionary_Entry_IO is separate;

   ---------------------------------------------------------------------------

   function "<" (Left, Right : Part_Entry) return Boolean is
   begin
      if Left.pofs = Right.pofs  then
         case Left.pofs is
            when n =>
               if Left.n.Decl < Right.n.Decl  or else
                 (Left.n.Decl = Right.n.Decl  and then
                 Left.n.Gender < Right.n.Gender)  or else
                 ((Left.n.Decl = Right.n.Decl  and
                 Left.n.Gender = Right.n.Gender)  and then
                 Left.n.Kind < Right.n.Kind)
               then
                  return True;
               end if;
            when pron =>
               if Left.pron.Decl < Right.pron.Decl  or else
                 (Left.pron.Decl = Right.pron.Decl  and then
                 Left.pron.Kind < Right.pron.Kind)
               then
                  return True;
               end if;
            when pack =>
               if Left.pack.Decl < Right.pack.Decl  or else
                 (Left.pack.Decl = Right.pack.Decl  and then
                 Left.pack.Kind < Right.pack.Kind)
               then
                  return True;
               end if;
            when adj =>
               if Left.adj.Decl < Right.adj.Decl   or else
                 (Left.adj.Decl = Right.adj.Decl  and then
                 Left.adj.Co < Right.adj.Co)
               then
                  return True;
               end if;
            when num =>
               if Left.num.Decl < Right.num.Decl  or else
                 (Left.num.Decl = Right.num.Decl  and then
                 Left.num.Sort < Right.num.Sort)  or else
                 ((Left.num.Decl = Right.num.Decl)  and then
                 (Left.num.Sort = Right.num.Sort)   and then
                 Left.num.Value < Right.num.Value)
               then
                  return True;
               end if;
            when adv =>
                  return Left.adv.Co < Right.adv.Co;
            when v =>
               if (Left.v.Con < Right.v.Con)  or else
                 (Left.v.Con = Right.v.Con  and then
                 Left.v.Kind < Right.v.Kind)
               then
                  return True;
               end if;
            when prep =>
               return Left.prep.Obj < Right.prep.Obj;
            when vpar .. supine =>
               null;
            when x =>
               null;
            when conj .. suffix =>
               null;
         end case;
      else
         return Left.pofs < Right.pofs;
      end if;
      return False;
   exception
      when Constraint_Error  =>
         return Left.pofs < Right.pofs;
   end "<";

   ---------------------------------------------------------------------------

   overriding function "<=" (Left, Right : Area_Type) return Boolean is
   begin
      if Right = Left or else Right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   ---------------------------------------------------------------------------
   -- Used to initialize Latin_Utils Dictionary_Package by setting various variables.
   -- FIXME: Make AT LEAST some of these initializations to happen in spec,
   --   thus eliminating risks of someone modifying them and in result breaking
   --   every nested package.
   procedure Initialize
   is
   begin
      Dictionary_Kind_IO.Default_Width := Dictionary_Kind'Width;

      Area_Type_IO.Default_Width := Area_Type'Width;

      Geo_Type_IO.Default_Width := Geo_Type'Width;

      Frequency_Type_IO.Default_Width := Frequency_Type'Width;

      Source_Type_IO.Default_Width := Source_Type'Width;

      Parse_Record_IO.Default_Width :=
        Stem_Type_IO.Default_Width + 1 +
        Inflection_Record_IO.Default_Width + 1 +
        Dictionary_Kind_IO.Default_Width + 1 +
        MNPC_IO_Default_Width;
      Noun_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Gender_Type_IO.Default_Width + 1 +
        Noun_Kind_Type_IO.Default_Width;
      Pronoun_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Pronoun_Kind_Type_IO.Default_Width;
      Propack_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Pronoun_Kind_Type_IO.Default_Width;
      Adjective_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Comparison_Type_IO.Default_Width;
      Adverb_Entry_IO.Default_Width :=
        Comparison_Type_IO.Default_Width;
      Verb_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Verb_Kind_Type_IO.Default_Width;
      Preposition_Entry_IO.Default_Width := 0;
      Conjunction_Entry_IO.Default_Width := 0;

      Interjection_Entry_IO.Default_Width := 0;
      Numeral_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Numeral_Sort_Type_IO.Default_Width + 1 +
        Numeral_Value_Type_IO_Default_Width;

      Part_Entry_IO.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
        Numeral_Entry_IO.Default_Width;     --  Largest

      Translation_Record_IO.Default_Width :=
        Age_Type_IO.Default_Width + 1 +
        Area_Type_IO.Default_Width + 1 +
        Geo_Type_IO.Default_Width + 1 +
        Frequency_Type_IO.Default_Width + 1 +
        Source_Type_IO.Default_Width;

      Dictionary_Entry_IO.Default_Width := 4 * (Max_Stem_Size + 1) +
        Part_Entry_IO.Default_Width + 1 +
        Translation_Record_IO.Default_Width + 1 +
        Max_Meaning_Size;
   end Initialize;

   ---------------------------------------------------------------------------

begin
   Initialize;
end Latin_Utils.Dictionary_Package;
