%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% Use the mr as a wc.
%%%
%%% Example usage (interpreter).
%%%
%%% 1> c(mr).
%%% 2> c(mr_wc).
%%% 3> c(read_mxm).
%%% 4> {Words, Tracks} = read_mxm:from_file("mxm_dataset_test.txt").
%%% 5> {ok, MR} = mr:start(8).
%%% 6> RevIdx = mr_wc:reverse_index(MR, {Words, Tracks}).
%%%
%%% Now you can do something like:
%%%
%%% 7> dict:fetch("love", RevIdx).
%%%
%%% which returns a list of all songs that have the word "love".
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mr_wc).
-compile(export_all).
-define(otherwise, true).

%% Count the sum of all the words in the given list of tracks.
count(MR, Tracks) ->
    {ok, Sum} = mr:job(MR,
		       fun(Track) ->
			       {_,_,WordBags} = read_mxm:parse_track(Track),
			       lists:foldl(fun({_,Cnt}, Sum) -> Sum+Cnt end, 0, WordBags)
		       end,
		       fun(WordsInTrack, Total) ->
			       Total+WordsInTrack
		       end,
		       0,
		       Tracks),
    io:format("Counted ~B tracks. Size of result is ~B~n", [length(Tracks), Sum]),
    Sum.

%% Compute the average number of different words in a song _and_ the
%% average total number of words in a song.
compute_averages(MR, {Words, Tracks}) ->
    {ok, {AvgDiff, AvgWords, _}} = mr:job(MR,
					  fun(Track) ->
						  %% Output: Number of different words + Number of words in song
						  {_,_,WordBags} = read_mxm:parse_track(Track),
						  lists:foldl(fun({_,Cnt},{L,Sum}) -> {L+1,Sum+Cnt} end, {0,0},WordBags)
					  end,
					  fun({NDiffWords, NWords}, {AvgDiffWords,AvgWords,NTracks}) ->
						  TotalDiff = AvgDiffWords*NTracks + NDiffWords,
						  TotalWords = AvgWords*NTracks + NWords,
						  NNTracks = NTracks + 1,
						  {TotalDiff/NNTracks, TotalWords/NNTracks, NNTracks}
					  end,
					  {0,0,0},
					  Tracks),
    {AvgDiff, AvgWords}.

%% For a given word, find the MSD track ID's for all songs with that word.
%%
%% If the word is not on stemmed form, we try to find the stemmed form
%% by parsing and reading the file "stemmed_words.txt" (assuming it
%% exists). For parsing this file we utilise the same given MR.
%%
%% Words: List of words
%% Tracks: List of tracks - 
%% Mapper:  Output Song Id + plus list of word indices
%% Reducer: Figure if our given song should be added to the list of
%%          songs containing WordIdx
grep(MR, Word, {Words, Tracks}) ->
    %%WordIdx = index_of(Word, Words),
    WordIdx = case lists:member(Word, Words) of
    		  true ->
    		      index_of(Word, Words);
    		  false ->
		      io:format("Stemming '~s'~n", [Word]),
    		      {ok, Bin} = file:read_file("stemmed_words.txt"),
    		      BLines = binary:split(Bin, <<$\n>>, [global,trim]),
    		      {ok, WordMapping} = mr:job(MR,
						 fun(BLin) ->
							 [OrigWord,StemmedWord] = binary:split(BLin, <<$\t>>),
							 {binary_to_list(OrigWord), binary_to_list(StemmedWord)}
						 end,
						 fun(Tuple, List) ->
							 [Tuple|List]
						 end,
						 [],
						 BLines),
    		      LookupRes = proplists:lookup(Word, WordMapping),
    		      case LookupRes of
    		          {_, Stemmed} ->
		              io:format("'~s' got stemmed to '~s'~n", [Word, Stemmed]),
    		              index_of(Stemmed, Words);
    		          none ->
    		              -1
    		      end
    	      end,
    {ok, Result} = mr:job(MR,
			  fun(Track) ->
				  {_, MXMID, WordBags} = read_mxm:parse_track(Track),
				  {Idxs, _} = lists:unzip(WordBags),
				  {MXMID, Idxs}
			  end,
			  fun({MXMID, Idxs}, ListOfTracks) ->
				  case lists:member(WordIdx, Idxs) of
				      true ->
					  [MXMID|ListOfTracks];
				      false ->
					  ListOfTracks
				  end
			  end,
			  [],
			  Tracks),
    Result. %% In mxm_dataset_test: 8188 songs contain the word "love" (of 27143 songs)


%% Compute a reverse index, that is, a mapping from words to songs where they occur
%%
%% Mapper:  Same as grep, output {SongId, [Index]}
%% Reducer: Fold the id's over the dictionary, first getting the word
%%          (by index), then updating the dictionary using Word as key
%%          appending the song id if it already exists, otherwise
%%          start a new list.
reverse_index(MR, {Words, Tracks}) ->
    {ok, Result} = mr:job(MR,
			  fun(Track) ->
				  {_, MXMID, WordBags} = read_mxm:parse_track(Track),
				  {Idxs,_} = lists:unzip(WordBags),
				  WordList = lists:map(fun(Idx) -> lists:nth(Idx, Words) end, Idxs), 
				  {MXMID, WordList}
			  end,
			  fun({MXMID, WordList}, Dict) ->
				  lists:foldl(fun(Word, D) ->
						      dict:update(Word, fun(L) -> [MXMID|L] end, [MXMID], D)
					      end,
					      Dict,
					      WordList)
			  end,
			  dict:new(),
			  Tracks),
    Result.

%% > RevIdx = mr_wc:reverse_index(MR, {Words, Tracks}).
%% > length(dict:fetch("love", RevIdx)). #=> 8188

%%% Utility functions
index_of(Item, List) ->
    index_of(Item, List, 1).

index_of(_, [], _) ->
    -1;
index_of(Item, [Item|_], Idx) ->
    Idx;
index_of(Item, [_|Tail], Idx) ->
    index_of(Item, Tail, Idx+1).
		       
