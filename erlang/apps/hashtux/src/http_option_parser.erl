%% @author jerker
%% @doc @todo Add description to http_option_parser.


-module(http_option_parser).

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_options/1]).

parse_options(Req) -> 
	% Get the services parameter, encoded by the client as a list of binaries
	% we then want to convert this into a list of atoms for easier handling
	% through our erlang application
	{ServicesJSON, _} = cowboy_req:qs_val(<<"services">>, Req, []),
	ServicesBinList = jsx:decode(ServicesJSON),
	ServicesAtomList = list_bins_to_list_atoms(ServicesBinList),
	
	
	io:format("Services: ~p~n", [ServicesBinList]),
	io:format("Services atoms: ~p~n", [ServicesAtomList]),
	[].

	



%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Convert a list of bins to a list of atoms, by recursively taking each
% element and first turning it into a list, then an atom.
% First the interface method
list_bins_to_list_atoms(List) -> list_bins_to_list_atoms([] , List).
	
% Then the actual implementation
list_bins_to_list_atoms([], List) -> List;
list_bins_to_list_atoms([CurrentBin | Rest], List) -> 
	BinAsList = binary_to_list(CurrentBin),
	BinAsAtom = list_to_atom(BinAsList),
	io:format("List: ~p~n ", [List]),
	io:format("Current atom: ~p~n ", [BinAsAtom]),
	io:format("Current string: ~p~n ", [BinAsList]),
	list_bins_to_list_atoms(lists:append(BinAsAtom, List), Rest).
