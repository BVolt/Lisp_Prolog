/*Brenden Johnson 3520.01 Assignment 3*/
/*1A Is member*/
is_member(X, [X|_]).
is_member(X, [_|T]) :- is_member(X, T).

/*1B Is Subset.*/
is_subset([], _).
is_subset([H|T], L2) :- is_member(H, L2), is_subset(T, L2).

/*1C Is Union*/
is_union([], L, L).
is_union([H|T], L2, LR) :- is_member(H, L2), is_union(T, L2, LR).
is_union([H| T], L2, [H|LR]) :- \+ is_member(H, L2), is_union(T, L2, LR).

/*1D is intersect*/
is_intersect([], _, []).
is_intersect([H|T], L2, [H|LR]) :- is_member(H, L2), is_intersect(T, L2, LR).
is_intersect([H|T], L2, LR) :- \+ is_member(H, L2), is_intersect(T, L2, LR).


/*1E is power*/
is_power([], [[]]).
is_power([H|T], LR) :- is_power(T, LR1), add_to(H, LR1, LR2), append(LR1, LR2, LR).
add_to(_, [], []).
add_to(X, [H|T], [[X|H]|T2]) :- add_to(X, T, T2).


/*2 Quick sort*/
quicksort([], []).
quicksort([H|T], SL) :- q_partition(T, H, Left, Right), quicksort(Left, SLLeft), quicksort(Right, SLRight), append(SLLeft, [H|SLRight], SL).

q_partition([], _, [], []).
q_partition([H|T], Pivot, [H|Left], Right) :- H =< Pivot, q_partition(T, Pivot, Left, Right).
q_partition([H|T], Pivot, Left, [H|Right]) :- H > Pivot, q_partition(T, Pivot, Left, Right).

/*3 Merge Sort*/
mergesort([], []).
mergesort([X], [X]).
mergesort(L, SL) :- split(L, L1, L2), mergesort(L1, SL1), mergesort(L2, SL2), merg(SL1, SL2, SL).


split([], [], []).
split([X], [X], []).
split([H1, H2|T], [H1|T1], [H2|T2]) :- split(T, T1, T2).


merg([], L, L).
merg(L, [], L).
merg([H1|T1], [H2|T2], [H1|Merged]) :- H1 =< H2, merg(T1, [H2|T2], Merged).
merg([H1|T1], [H2|T2], [H2|Merged]) :- H1 > H2, merg([H1|T1], T2, Merged).


/*4 Are Amicable*/
are_amicable(A, B) :- amicable_sum(A, BSum), amicable_sum(B, ASum), A == ASum, B == BSum, A \== B.

amicable_sum(N, Sum) :- Half is N // 2, sum_divisors(N, Half, Sum, 0).

sum_divisors(_, D, Sum, Acc) :- D =:= 0, Sum = Acc.
sum_divisors(N, D, Sum, Acc) :- D > 0, (N mod D =:= 0 ->(NewSum is Acc + D, NewD is D - 1, sum_divisors(N, NewD, Sum, NewSum)); (NewD is D - 1, sum_divisors(N, NewD, Sum, Acc))).



