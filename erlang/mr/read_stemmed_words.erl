-module(read_stemmed_words).
-compile(export_all).

from_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    BLines = binary:split(Bin, <<$\n>>, [global,trim]),
    lists:map(fun(BLin) -> [Word, StemmedWord] = binary:split(BLin, <<$\t>>),
			   {binary_to_list(Word), binary_to_list(StemmedWord)}
	      end, BLines).
