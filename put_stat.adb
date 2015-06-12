with TEXT_IO;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
procedure PUT_STAT(S : STRING) is
begin
  if TEXT_IO.IS_OPEN(STATS) then
    TEXT_IO.PUT_LINE(STATS, S);
  end if;
end PUT_STAT;




