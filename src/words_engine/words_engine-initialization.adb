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

with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Words_Engine.Word_Package; use Words_Engine.Word_Package;

package body Words_Engine.Initialization is
   procedure Initialize_Engine is
   begin
      Initialize_Word_Parameters;
      Initialize_Developer_Parameters;
      Initialize_Word_Package;
   end Initialize_Engine;
end Words_Engine.Initialization;
