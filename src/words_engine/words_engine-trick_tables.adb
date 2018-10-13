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

package body Words_Engine.Trick_Tables is
   function Member (Needle   : Unbounded_String;
                    Haystack : Strings)
                   return Boolean
   is
   begin
      for S in Haystack'Range loop
         if Needle = Haystack (S) then
            return True;
         end if;
      end loop;
      return False;
   end Member;

   function Common_Prefix (S : String) return Boolean is
      --  Common prefixes that have corresponding words (prepositions
      --  usually) which could confuse TWO_WORDS.  We wish to reject
      --  these.
      Common_Prefixes : constant Strings := (
        +"dis",
        +"ex",
        +"in",
        +"per",
        +"prae",
        +"pro",
        +"re",
        +"si",
        +"sub",
        +"super",
        +"trans"
        );
   begin
      return Member (+S, Common_Prefixes);
   end Common_Prefix;

   A_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"adgn", FF2 => +"agn"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"adsc",  FF2 => +"asc"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"adsp",  FF2 => +"asp"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"arqui", FF2 => +"arci"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"arqu",  FF2 => +"arcu"),
     (Max => 0, Op => TC_Flip, FF3 => +"ae",    FF4 => +"e"),
     (Max => 0, Op => TC_Flip, FF3 => +"al", FF4 => +"hal"),
     (Max => 0, Op => TC_Flip, FF3 => +"am", FF4 => +"ham"),
     (Max => 0, Op => TC_Flip, FF3 => +"ar", FF4 => +"har"),
     (Max => 0, Op => TC_Flip, FF3 => +"aur", FF4 => +"or")
   );

   D_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"dampn", FF4 => +"damn"),
     --  OLD p.54,
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"dij", FF2 => +"disj"),
     --  OLD p.55,
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"dir", FF2 => +"disr"),
     --  OLD p.54,
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"dir", FF2 => +"der"),
     --  OLD p.507/54,
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"del", FF2 => +"dil")
   );

   E_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"ecf", FF2 => +"eff"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"ecs", FF2 => +"exs"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"es", FF2 => +"ess"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"ex", FF2 => +"exs"),
     (Max => 0, Op => TC_Flip, FF3 => +"eid", FF4 => +"id"),
     (Max => 0, Op => TC_Flip, FF3 => +"el", FF4 => +"hel"),
     (Max => 0, Op => TC_Flip, FF3 => +"e", FF4 => +"ae")
   );

   F_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"faen", FF2 => +"fen"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"faen", FF2 => +"foen"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"fed", FF2 => +"foed"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"fet", FF2 => +"foet"),
     (Max => 0, Op => TC_Flip, FF3 => +"f", FF4 => +"ph")
   ); -- Try lead then all

   G_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Flip, FF3 => +"gna", FF4 => +"na")
   );

   H_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"har", FF4 => +"ar"),
     (Max => 0, Op => TC_Flip, FF3 => +"hal", FF4 => +"al"),
     (Max => 0, Op => TC_Flip, FF3 => +"ham", FF4 => +"am"),
     (Max => 0, Op => TC_Flip, FF3 => +"hel", FF4 => +"el"),
     (Max => 0, Op => TC_Flip, FF3 => +"hol", FF4 => +"ol"),
     (Max => 0, Op => TC_Flip, FF3 => +"hum", FF4 => +"um")
   );

   Tricks_Exception : exception;

   function Get_Tricks_Table (C : Character) return TricksT is
   begin
      case C is
         when 'a' =>
            return A_Tricks;
         when 'd' =>
            return D_Tricks;
         when 'e' =>
            return E_Tricks;
         when 'f' =>
            return F_Tricks;
         when 'g' =>
            return G_Tricks;
         when 'h' =>
            return H_Tricks;
         when others =>
            raise Tricks_Exception;
      end case;
   end Get_Tricks_Table;

end Words_Engine.Trick_Tables;
