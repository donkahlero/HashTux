-module(miner_filter).

-export([filter_insta/2]).




%%
%% @doc Filters the final results according to the options specified.
%%
%% each H is a separate list containing results (key-value)
filter_insta(Res, []) 	 -> ok;
filter_insta([H1|T1], Options) -> ok.
