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
	{ServicesJSON, _} = cowboy_req:qs_val(<<"services">>, Req, <<"[]">>),
	ServicesBinList = jsx:decode(ServicesJSON),
	ServicesAtomList = list_bins_to_list_atoms(ServicesBinList),
	
	% The same with content type
	{ContentTypeJSON, _} = cowboy_req:qs_val(<<"content_type">>, Req, <<"[]">>),
	ContentTypeBinList = jsx:decode(ContentTypeJSON),
	ContentTypeAtomList = list_bins_to_list_atoms(ContentTypeBinList),
	
	% Language - NOTE THIS SHOULD BE PROVIDED AS TWO CHARACTERS BY CLIENT
	LanguageBin = cowboy_req:qs_val(<<"language">>, Req, list_to_binary("en")),
	LanguageString = binary_to_list(LanguageBin),
	LanguageAtom = list_to_atom(LanguageString),
	
	% Limit should be provided as an int
	LimitBin = cowboy_req:qs_val(<<"limit">>, Req, <<"100">>),
	LimitString = binary_to_list(LimitBin),
	LimitInt = list_to_integer(LimitString),
	
	% Return the list of options, as key-value pairs
	[{service, ServicesAtomList},
	 {content_type, ContentTypeAtomList},
	 {language, LanguageAtom},
	 {limit, LimitInt}].

	



%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Convert a list of bins to a list of atoms, by recursively taking each
% element and first turning it into a list, then an atom.
% First the interface method
list_bins_to_list_atoms(List) -> list_bins_to_list_atoms(List, []).
	
% Then the actual implementation
list_bins_to_list_atoms([], List) -> List;
list_bins_to_list_atoms([CurrentBin | Rest], List) -> 
	BinAsList = binary_to_list(CurrentBin),
	BinAsAtom = list_to_atom(BinAsList),
	list_bins_to_list_atoms(Rest, lists:append(List, [BinAsAtom])).
