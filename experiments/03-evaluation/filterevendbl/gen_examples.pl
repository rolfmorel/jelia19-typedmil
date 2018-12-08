gen_droplast([],[]).
gen_droplast([A|T],[B|Out]):-
  gen_droplast_aux(A,B),
  gen_droplast(T,Out).

gen_droplast_aux([_],[]).
gen_droplast_aux([H|T],[H|Out]):-
  gen_droplast_aux(T,Out).

gen_finddup(A,B):-head(A,B),gen_finddup_1(A,B).
gen_finddup(A,B):-tail(A,C),gen_finddup(C,B).
gen_finddup_1(A,B):-tail(A,C),element(C,B).

gen_filterevendbl(A,B):-gen_filterevendbl1(A,C),gen_filterevendbl2(C,B).
gen_filterevendbl1(A,B):-filter(A,B,even).
gen_filterevendbl2(A,B):-maplist(double,A,B).

double(A,B):-B is 2*A.
even(A):-0 is A mod 2.
odd(A):-1 is A mod 2.

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),!,
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).

%gen_instance(Input,Output):-
%  random(2,5,NumRows),
%  findall(SubList,(
%    between(1,NumRows,_),
%    random(3,5,NumColumns),
%    randseq(NumColumns,10,SubList)),Input),
%  gen_droplast(Input,Output).
gen_instance(Input,Output):-
  random(4,10,NumElts),
  random(1,NumElts,NumEven),
  NumOdd is NumElts - NumEven,
  findall(N,(between(1,NumOdd,_),random(0,5,M),N is 2*M+1),Input1),
  findall(N,(between(1,NumEven,_),random(0,3,M),N is 2*M),Input2),
  append(Input1,Input2,Input3),
  random_permutation(Input3,Input),
  gen_filterevendbl(Input,Output).

tmp(1,1):-!.
tmp(X,Y):-random(1,X,Y).

gen_neg_instance(Input,Output):-
  gen_instance(Input,Out1),!,
  random(0,10,Q),
  random_permutation([Q|Out1],Output).

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
print_neg_example_asp:-gen_neg_instance_asp(X,Y),format("pos_ex(~w,~w).\n", [X,Y]).
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
