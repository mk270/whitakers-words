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

   K_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"k", FF4 => +"c"),
     (Max => 0, Op => TC_Flip, FF3 => +"c", FF4 => +"k")
   );

   L_Tricks : constant TricksT := (1 =>
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"lub", FF2 => +"lib")
   );

   M_Tricks : constant TricksT := (1 =>
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"mani", FF2 => +"manu")
   );

   N_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"na", FF4 => +"gna"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"nihil", FF2 => +"nil")
   );

   O_Tricks : constant TricksT := (
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"obt", FF2 => +"opt"),
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"obs", FF2 => +"ops"),
     (Max => 0, Op => TC_Flip, FF3 => +"ol", FF4 => +"hol"),
     (Max => 1, Op => TC_Flip, FF3 => +"opp", FF4 => +"op"),
     (Max => 0, Op => TC_Flip, FF3 => +"or", FF4 => +"aur")
   );

   P_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"ph", FF4 => +"f"),
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"pre", FF2 => +"prae")
   );

   --  From Oxford Latin Dictionary p.1835 "sub-"
   S_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"subsc", FF2 => +"susc"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"subsp", FF2 => +"susp"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"subc", FF2 => +"susc"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"succ", FF2 => +"susc"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"subt", FF2 => +"supt"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"subt", FF2 => +"sust")
   );

   T_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"transv", FF2 => +"trav")
   );

   U_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"ul", FF4 => +"hul"),
     (Max => 0, Op => TC_Flip, FF3 => +"uol", FF4 => +"vul")
            --  u is not v for this purpose
   );

   Y_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Flip, FF3 => +"y", FF4 => +"i")
   );

   Z_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Flip, FF3 => +"z", FF4 => +"di")
   );

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
         when 'k' =>
            return K_Tricks;
         when 'l' =>
            return L_Tricks;
         when 'm' =>
            return M_Tricks;
         when 'n' =>
            return N_Tricks;
         when 'o' =>
            return O_Tricks;
         when 'p' =>
            return P_Tricks;
         when 's' =>
            return S_Tricks;
         when 't' =>
            return T_Tricks;
         when 'u' =>
            return U_Tricks;
         when 'y' =>
            return Y_Tricks;
         when 'z' =>
            return Z_Tricks;
         when others =>
            raise Tricks_Exception;
      end case;
   end Get_Tricks_Table;

   A_Slur_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"abs", FF2 => +"aps"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"acq", FF2 => +"adq"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"ante", FF2 => +"anti"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"auri", FF2 => +"aure"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"auri", FF2 => +"auru"),
     (Max => 0, Op => TC_Slur,      S1  => +"ad")
   );

   C_Slur_Tricks : constant TricksT := (
     (Max => 0, Op => TC_Flip, FF3 => +"circum", FF4 => +"circun"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"con", FF2 => +"com"),
     (Max => 0, Op => TC_Flip, FF3 => +"co", FF4 => +"com"),
     (Max => 0, Op => TC_Flip, FF3 => +"co", FF4 => +"con"),
     (Max => 0, Op => TC_Flip_Flop, FF1 => +"conl", FF2 => +"coll")
   );

   I_Slur_Tricks : constant TricksT := (
     (Max => 1, Op => TC_Slur,      S1  => +"in"),
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"inb", FF2 => +"imb"),
     (Max => 1, Op => TC_Flip_Flop, FF1 => +"inp", FF2 => +"imp")
     -- for some forms of eo the stem "i" grates with
     -- an "is .. ." ending
   );

   N_Slur_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Flip, FF3 => +"nun", FF4 => +"non")
   );

   O_Slur_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Slur, S1 => +"ob")
   );

   Q_Slur_Tricks : constant TricksT := (1 =>
     (Max => 0, Op => TC_Flip_Flop,
      FF1 => +"quadri", FF2 => +"quadru")
   );

   S_Slur_Tricks : constant TricksT := (
     --  Latham,
     (Max => 0, Op => TC_Flip, FF3 => +"se", FF4 => +"ce"),
     --  From Oxford Latin Dictionary p.1835 "sub-"
     (Max => 0, Op => TC_Slur, S1  => +"sub")
   );

   function Get_Slur_Tricks_Table (C : Character) return TricksT is
   begin
      case C is
         when 'a' =>
            return A_Slur_Tricks;
         when 'c' =>
            return C_Slur_Tricks;
         when 'i' =>
            return I_Slur_Tricks;
         when 'n' =>
            return N_Slur_Tricks;
         when 'o' =>
            return O_Slur_Tricks;
         when 'q' =>
            return Q_Slur_Tricks;
         when 's' =>
            return S_Slur_Tricks;
         when others =>
            raise Tricks_Exception;
      end case;
   end Get_Slur_Tricks_Table;

end Words_Engine.Trick_Tables;
