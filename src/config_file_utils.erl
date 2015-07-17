-module(config_file_utils).

-export([require_values/3, optional_values/3, extract_config/3]).

optional_config_value(App, {Key, ProcFunc, Default}, ExFunc) ->
	case ExFunc(App, Key) of 
          {ok, Val} -> {ok, ProcFunc, Val}; 
	  undefined -> {ok, ProcFunc, Default}
        end.

required_config_value(App, {Key, ProcFunc}, ExFunc) -> 
	case ExFunc(App, Key) of
          {ok, Val} -> {ok, ProcFunc, Val}; 
	  undefined -> {error, [{missing_config_value, App, Key}]}
	end.

extract_req_value_fun(App, ExFunc) ->
    fun(KeySpec) ->
      required_config_value(App, KeySpec, ExFunc) 
    end.

extract_opt_value_fun(App, ExFunc) ->
    fun(KeySpec) ->
      optional_config_value(App, KeySpec, ExFunc) 
    end.

collect_required_values({error, A},{error, EL}) -> {error, A ++ EL};
collect_required_values({error, A},_) -> {error, A};
collect_required_values(_, {error, EL}) -> {error, EL};
collect_required_values({ok, PFunc, Val}, State) -> 
	case catch(PFunc(State, Val)) of
	  {error, ER} -> {error, [{config_func_failed, ER}]};
	  {'EXIT', ER} -> {error, [{config_func_failed, ER}]};
          A -> A		
        end.

optional_values(App, KeySpecs, StartState, ExFunc) ->
	ConfigVals = lists:map(extract_opt_value_fun(App, ExFunc), KeySpecs),
	lists:foldl(fun collect_required_values/2, StartState, ConfigVals).

optional_values(App, KeySpecs, StartState) ->
  optional_values(App, KeySpecs, StartState, fun application:get_env/2).

require_values(App, KeySpecs, StartState) ->
  require_values(App, KeySpecs, StartState, fun application:get_env/2).

require_values(App, KeySpecs, StartState, ExFunc) ->
	ConfigVals = lists:map(extract_req_value_fun(App, ExFunc), KeySpecs),
	lists:foldl(fun collect_required_values/2, StartState, ConfigVals).

extract_config(App, Specs, StartState) ->
	RSpecs = config_key_spec:requireds(Specs),
	OSpecs = config_key_spec:optionals(Specs),
	extract_configuration_with_specs(App, RSpecs, OSpecs, StartState).

extract_configuration_with_specs(App, ReqKeySpecs, OptKeySpecs, StartState) ->
        optional_values(App, OptKeySpecs,
  	  require_values(App, ReqKeySpecs, StartState)
	).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(my_state, {val_1, val_2}).

good_config_values_test() ->
	LFunc = lookup_func_for(my_app, dict:from_list([{val3, "ok"}])),
	KeySpecs = [{val3, fun(S,V) -> S#my_state{val_1 = V} end}],
	ConfigResults = require_values(my_app, KeySpecs, #my_state{}, LFunc),
	?assertMatch(#my_state{val_1 = "ok"}, ConfigResults).

existing_value_bad_func_test() ->
	LFunc = lookup_func_for(my_app, dict:from_list([{val3, "ok"}])),
	KeySpecs = [{val3, fun(_,_) -> throw({'EXIT', ok}) end}],
	ConfigResults = require_values(my_app, KeySpecs, #my_state{}, LFunc),
        {error, ErrorKeys} = ConfigResults,
	?assertMatch([{config_func_failed, _}],ErrorKeys),
	?assertEqual(erlang:length(ErrorKeys), 1).


missing_value_test() ->
	LFunc = lookup_func_for(my_app, dict:from_list([{val3, "ok"}])),
	KeySpecs = [{not_there_1, nofun}, {val3, fun(_,_) -> ok end}, {not_there_2, nofun}],
	ConfigResults = require_values(my_app, KeySpecs, #my_state{}, LFunc),
        {error, ErrorKeys} = ConfigResults,
	?assert(lists:member({missing_config_value, my_app, not_there_1},ErrorKeys)),
	?assert(lists:member({missing_config_value, my_app, not_there_2},ErrorKeys)),
	?assertEqual(erlang:length(ErrorKeys), 2).

lookup_func_for(AppName, Dict) ->
	fun(App, Key) -> 
			case (App == AppName) of
				true -> case dict:find(Key, Dict) of
						error -> undefined;
						{ok, V} -> {ok, V}
					end;
				_ -> {error, {missing_config_value, App, Key}}
		end
end.

-endif.
