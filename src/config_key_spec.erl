-module(config_key_spec).

-export([new/0, add_required/3, add_optional/4, optionals/1, requireds/1]).

new() -> [].

add_required(Key, ExtractFun, KSpec) -> KSpec ++ [{required, {Key, ExtractFun}}].

add_optional(Key, ExtractFun, Default, KSpec) -> KSpec ++ [{optional, {Key, ExtractFun, Default}}].

optionals(KSpec) -> extract_where(fun is_optional/1, KSpec).
requireds(KSpec) -> extract_where(fun is_required/1, KSpec).


extract_where(FilterF, KSpec) ->
	lists:map(fun(X) ->
          element(2, X)
	end,
	lists:filter(FilterF, KSpec)).

is_optional({optional, _}) -> true;
is_optional(_) -> false.

is_required({required, _}) -> true;
is_required(_) -> false.
