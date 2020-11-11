-module(erl_data_list@foreign).
-export([rangeImpl/3, length/1, reverse/1, concat/1]).

rangeImpl(N, M, S) -> lists:seq(N, M, S).

length(XS) -> erlang:length(XS).

reverse(XS) -> lists:reverse(XS).
concat(XS) -> lists:append(XS).
