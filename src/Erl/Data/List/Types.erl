-module(erl_data_list_types@foreign).
-export([nil/0, cons/2, unconsImpl/3, null/1, filter/2, mapImpl/2, appendImpl/2, foldlImpl/3, foldrImpl/3]).

nil() -> [].

cons(X, XS) -> [X|XS].

unconsImpl(Just, _, [H|T]) -> Just(#{head => H, tail => T});
unconsImpl(_, Nothing, []) -> Nothing.

null([]) -> true;
null(_) -> false.

appendImpl(XS, YS) -> lists:append(XS, YS).
mapImpl(F, XS) -> lists:map(F, XS).

% erlang folds have same argument order unlike PureScript
foldlImpl(F, I, XS) -> lists:foldl(fun (X, A) -> (F(A))(X) end, I, XS).
foldrImpl(F, I, XS) -> lists:foldr(fun (X, A) -> (F(X))(A) end, I, XS).

filter(P, XS) -> lists:filter(P, XS).