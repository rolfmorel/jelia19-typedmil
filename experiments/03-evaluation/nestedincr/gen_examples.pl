gen_nestedincr(A,B):-maplist(gen_nestedincr1,A,B).
gen_nestedincr1(A,B):-maplist(gen_nestedincr2,A,B).
gen_nestedincr2(A,B):-succ(A,C),succ(C,B).

gen_instance(Input,Output):-
  random(2,5,NumRows),
  findall(SubList,(
    between(1,NumRows,_),
    random(3,5,NumColumns),
    findall(N,(between(1,NumColumns,_),random(0,8,N)),SubList)),Input),
  gen_nestedincr(Input,Output).

tmp(1,1):-!.
tmp(X,Y):-random(1,X,Y).

gen_neg_instance(Input,Output):-
  gen_instance(Input,_),!,
  length(Input,NumRows),
  MaxChange is NumRows-1,
  tmp(MaxChange,NumToChange),
  randset(NumToChange,NumRows,IndexesToChange),
  findall(Outlist,(
    nth1(Index,Input,Sublist),
    (\+member(Index,IndexesToChange) -> gen_nestedincr1(Sublist,Outlist); Outlist=Sublist)
    ),Output).

to_asp([],l()):-!.
to_asp([H|T],l(H2,T2)):-!,to_asp(H,H2),to_asp(T,T2).
to_asp(X,X).

from_asp(l(),[]):-!.
from_asp(l(H2,T2),[H|T]):-!,from_asp(H2,H),from_asp(T2,T).
from_asp(X,X).

gen_instance_asp(X1,Y1):-
  gen_instance(X,Y),to_asp(X,X1),to_asp(Y,Y1).
gen_neg_instance_asp(X1,Y1):-
  gen_neg_instance(X,Y),to_asp(X,X1),to_asp(Y,Y1).

print_pos_example:-gen_instance(X,Y),format("p(~w,~w).\n", [X,Y]).
print_pos_examples(N):-findall(M,(between(1,N,M),print_pos_example),_).
print_neg_example:-gen_neg_instance(X,Y),format("q(~w,~w).\n", [X,Y]).
print_neg_examples(N):-findall(M,(between(1,N,M),print_neg_example),_).

print_pos_example_asp:-gen_instance_asp(X,Y),format("p(~w,~w).\n", [X,Y]).
print_pos_examples_asp(N):-findall(M,(between(1,N,M),print_pos_example_asp),_).
print_neg_example_asp:-gen_neg_instance_asp(X,Y),format("q(~w,~w).\n", [X,Y]).
print_neg_examples_asp(N):-findall(M,(between(1,N,M),print_neg_example_asp),_).

print_pos_ex_asp(N):-
  findall(M,(between(1,N,M),gen_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),format("pos_ex(f,~w,~w).\n",[X1,Y1])),_).
print_pos_ex_typed_asp(N):-
  findall(M,(between(1,N,M),gen_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),format("pos_ex(f,(list(list(char)),list(list(char))),~w,~w).\n",[X1,Y1])),_).
print_pos_ex_asp_both(N):-
  findall(M,(between(1,N,M),gen_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),
  format("pos_ex(f,~w,~w).\n",[X1,Y1]),format("pos_ex_t(f,(list(list(char)),list(list(char))),~w,~w).\n",[X1,Y1])),_).
