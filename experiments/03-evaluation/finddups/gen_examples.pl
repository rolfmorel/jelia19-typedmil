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

element(A,B):-member(B,A).

%gen_instance(Input,Output):-
%  random(2,5,NumRows),
%  findall(SubList,(
%    between(1,NumRows,_),
%    random(3,5,NumColumns),
%    randseq(NumColumns,10,SubList)),Input),
%  gen_droplast(Input,Output).
gen_instance(Input,Output):-
  random(4,10,NumElts),
  findall(C,(between(1,NumElts,_),random(65,91,N),random(0,2,Q),M is N + Q*32,char_code(X,M),format(atom(C),"'~w'",[X])),Input1),
  random(0,NumElts,IdxDup),
  nth0(IdxDup,Input1,Output),
  append([Output],Input1,Inp),
  random_permutation(Inp,Input).

tmp(1,1):-!.
tmp(X,Y):-random(1,X,Y).

gen_neg_instance(Input,Output):-
  gen_instance(Input1,_),!,
  findall(C,(between(65,91,N),random(0,2,Q),M is N + Q*32,char_code(X,M),format(atom(C),"'~w'",[X]),\+member(C,Input1)),Unique),
  random_member(Output,Unique),
  append([Output],Input1,Input2),
  random_permutation(Input2,Input).

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
