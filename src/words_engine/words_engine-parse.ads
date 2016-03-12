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

with Latin_Utils.Config; use Latin_Utils.Config;
with Ada.Containers.Vectors; use Ada.Containers;
with Words_Engine.List_Package; use Words_Engine.List_Package;

package Words_Engine.Parse is
   package Result_Container is new Vectors (Natural, Word_Analysis);

   -- Parse (and *print*) a line of Latin or English
   procedure Parse_Line (Configuration : Configuration_Type;
                         Input_Line    : String);

   -- This function provides access to the raw Ada types used by the
   -- WORDS engine; these are not currently understood or documented,
   -- which is a job for the future. The function is being exposed now
   -- to encourage the development or documentation of an API to WORDS'
   -- internals
   function Analyse_Line (Configuration : Configuration_Type;
                          Input_Line    : String)
     return Result_Container.Vector;
end Words_Engine.Parse;
