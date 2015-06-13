with text_io;
with developer_parameters; use developer_parameters;
procedure put_stat(s : string) is
begin
   if text_io.is_open(stats) then
	  text_io.put_line(stats, s);
   end if;
end put_stat;




