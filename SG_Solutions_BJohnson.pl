% Brenden Johnson 3520.01 Study Guide Prolog Solutions.
% 1 compute x^y
pow(_, 0, 1).
pow(X, Y, Z) :- Y > 0, Y1 is Y - 1, pow(X, Y1, Z1), Z is Z1 * X.

% 2 Create a function that computes x^2 without using multiplication
square(0, 0).
square(N, Result) :- N > 0, N1 is N - 1, square(N1, Result1), Result is Result1 + N + N - 1.

% 3 Create a function that takes a list of pairs of integers, and orders the elements of each pair in non-increasing order.
order_pairs([], []).
order_pairs([(X,Y)|T], [(X1,Y1)|T1]) :- X >= Y -> (X1 = X, Y1 = Y); (X1 = Y, Y1 = X), order_pairs(T, T1).

% 4 Create a function that takes a list of integers and returns a pair consisting of the sum of the numbers in even positions and the sum of the numbers in odd positions
sum_positions([], 0, 0).
sum_positions([X], X, 0).
sum_positions([X,Y|T], (Even, Odd)) :- sum_positions(T, (Even1, Odd1)), Even is Even1 + X, Odd is Odd1 + Y.
sum_positions(List, (Even, Odd)) :- sum_positions(List, Even, Odd).



% 5 Create a function that takes a list of pairs of integers and returns a list with the sum of the elements of every pair
sum_pairs([], []).
sum_pairs([(X,Y)|T], [Z|T1]) :- Z is X + Y, sum_pairs(T, T1).


% 6 Create a function that takes a list of pairs of integers and returns a pair with the sum of the elements in the first position and the sum of the elements in the second position.
sum_positions2([], 0, 0).
sum_positions2([(X,Y)|T], (First, Second)) :- sum_positions2(T, (First1, Second1)), First is First1 + X, Second is Second1 + Y.
sum_positions2(List, (First, Second)) :- sum_positions2(List, First, Second).


% 7 Polynomials may be represented as lists of integer, where each integer is the coefficient of the corresponding monomial
evalPoly([],_,0).
evalPoly([X|[]],_,X).
evalPoly([Coeff|T],Val,Result) :- length([Coeff|T],M),
                                  N is M-1,
                                  evalMono(Coeff,N,Val,R1),
                                  evalPoly(T,Val,R2),
                                  Result is R1+R2.

evalMono(Coeff,Expo,Val,Result) :- R1 is Val^Expo, Result is Coeff*R1.

% 8 Create a function that eliminates all duplicate elements from a list
remove_duplicates([],[]).
remove_duplicates([H|T], List) :- member(H, T), remove_duplicates(T, List).
remove_duplicates([H|T], [H|List]) :- not(member(H, T)), remove_duplicates(T, List).



% 9 Create a function that packs consecutive duplicate elements of a list into sublists
pack([], []).
pack([X|Xs], R) :- pack_helper(Xs, X, [], R).

pack_helper([], X, Acc, [[X|Acc]]).
pack_helper([X|Xs], X, Acc, R) :- pack_helper(Xs, X, [X|Acc], R).
pack_helper([X|Xs], Y, Acc, [[Y|Acc]|R]) :- X \= Y, pack_helper(Xs, X, [], R).


% 10  Create a function that packs all duplicate elements of a list into sublists
fullPack([],[]).
fullPack([E|T],Result) :- headPackAndRem(E,[E|T],[],[],res(Pack,NL)), fullPack(NL,R1), Result = [Pack|R1].

headPackAndRem(_,[],Acc1,Acc2,Result) :- reverse(Acc2,R1), Result = res(Acc1,R1).
headPackAndRem(E,[H|T],Acc1,Acc2,Result) :- E = H -> headPackAndRem(E,T,[E|Acc1],Acc2,Result); headPackAndRem(E,T,Acc1,[H|Acc2],Result).


% 11 Create a function that computes the length encoding of a list, which is a list of pairs with every elements and times it appears consecutively at a given position.
encode(List, Encoded) :- packer(List, Packed), encode_helper(Packed, Encoded).

packer([], []).
packer([X|Xs], [[X|Xp]|Ys]) :- span(=(X), Xs, Xp, Ys1), packer(Ys1, Ys).

span(_, [], [], []).
span(P, [X|Xs], [X|Ys], Zs) :- call(P, X), span(P, Xs, Ys, Zs).
span(P, [X|Xs], [], [X|Xs]) :- \+ call(P, X).

encode_helper([], []).
encode_helper([[X|Xs]|Ys], [(N, X)|Zs]) :- length([X|Xs], N), encode_helper(Ys, Zs).


% 12 Create a function that decodes length encoding.
decode([],[]).
decode([code(Count,Elem)|T],Result) :- decodeSingCode(Count,Elem,[],R1), decode(T,R2), append(R1,R2,Result).

decodeSingCode(0,_,Acc,Acc).
decodeSingCode(Count,Elem,Acc,Result) :- NewCount is (Count-1), decodeSingCode(NewCount,Elem,[Elem|Acc],Result).


% 13 Create a function that takes a list and two integers a and b and returns the sublist that starts on a and ends on b
sublist(_, 0, -1, []).
sublist([H|T], 0, B, [H|Sub]) :- B >= 0, NewB is B - 1, sublist(T, 0, NewB, Sub).
sublist([_|T], A, B, Sub) :- A > 0, NewA is A - 1, NewB is B - 1, sublist(T, NewA, NewB, Sub).





% 14 Create a version of the function in problem (13) that when a > b it
% doesn't return an empty list, but the sublist that starts at a and
% ends in b wrapping around the end of the list.
sublist_wrap(List, A, B, Sub) :- length(List, Len),(A =< B ->  A1 is A - 1, B1 is B - 1, sublist(List, A1, B1, Sub); split_at(A, List, L1, L2),
        append(L2, L1, List1), B1 is B + Len - A, sublist(List1, 0, B1, Sub)).


split_at(N, List, L1, L2) :- length(L1, N), append(L1, L2, List).


% 15 Create a a function that takes a pair of integers a and b, and returns a list with every integer between a and b.
range(A, A, [A]).
range(A, B, [A|T]) :- A < B, NewA is A + 1, range(NewA, B, T).



% 16 Create a function that takes a number k and a list, and returns a list of lists with all the combinations of k distinct elements from the list
combination(_,0,[[]]).
combination(L,K,R) :- length(L,R1), K = R1, R = [L].
combination(L,K,R) :- J is K-1, combination(L,J,R1),
                      combination_h(L,R1,[],R).

combination_h([],_,Acc,Acc).
combination_h([H|T],Pcomb,Acc,R) :- proj_u(H,Pcomb,[],R1),
                                    sp_u(R1,Acc,R2),
                                    combination_h(T,Pcomb,R2,R).

proj_u(_,[],Acc,Acc).
proj_u(X,[H|T],Acc,R) :- member(X,H) ->  proj_u(X,T,Acc,R);
                         proj_u(X,T,[[X|H]|Acc],R).

sp_u([],S2,S2).
sp_u(S1,[],S1).
sp_u([H|T],S2,R) :- sp_m(H,S2) ->  sp_u(T,S2,R);
                    sp_u(T,[H|S2],R).

sp_m(_,[]) :- !, fail.
sp_m(X,[H|T]) :- subset(X,H), subset(H,X);
                 sp_m(X,T).


% 17 Create a function that takes a list and returns a pair with two lists: one with elements that are less than or equal to the first element, and the other with the elements that are greater than the first element.
split_list([], _, [], []).
split_list([H|T], Pivot, [H|LessOrEqual], Greater) :- H =< Pivot, split_list(T, Pivot, LessOrEqual, Greater).
split_list([H|T], Pivot, LessOrEqual, [H|Greater]) :- H > Pivot, split_list(T, Pivot, LessOrEqual, Greater).

split_list([H|T], SplitLists) :- split_list(T, H, LessOrEqual, Greater), SplitLists = [[H|LessOrEqual], Greater].


% 18 Create a function that takes two sorted lists and merges them into a single sorted list.
merge_lists(X,[],X).
merge_lists([],Y,Y).
merge_lists([X|TX],[Y|TY],[X|TZ]) :- X =< Y, merge_lists(TX,[Y|TY],TZ).
merge_lists([X|TX],[Y|TY],[Y|TZ]) :- X > Y, merge_lists([X|TX],TY,TZ).


% 19 Create a function that takes a list of integers and returns a pair with the least and greatest elements in the list.
min_max_list([X], X, X).
min_max_list([X|Xs], Min, Max) :- min_max_list(Xs, Min1, Max1), Min is min(X, Min1), Max is max(X, Max1).

min_max_list(List, (Min, Max)) :- min_max_list(List, Min, Max).



% 20 Create a function that takes a list and an element and removes all copies of this element from the list
remove_all(_, [], []).
remove_all(X, [X|T], List) :- remove_all(X, T, List).
remove_all(X, [H|T], [H|List]) :- X \= H, remove_all(X, T, List).

























