filter([],[],_F). filter([A|T1],[A|T2],F):-
  call(F,A),!,
  filter(T1,T2,F). filter([_|T1],T2,F):-
  filter(T1,T2,F).

uppercase(A):-upcase_atom(A,A),char_code(A,_). tolower(A,B):-downcase_atom(A,B),char_code(A,_). %tolower2(A,C):-downcase_atom(A,X),char_code(A,_),format(atom(C),"'~w'",[X]).

gen_filtercapslower(A,B):-gen_filtercapslower1(A,C),gen_filtercapslower2(C,B). gen_filtercapslower1(A,B):-filter(A,B,uppercase). gen_filtercapslower2(A,B):-maplist(tolower,A,B).

element(A,B):-member(B,A).

%gen_instance(Input,Output):- % random(2,5,NumRows), % findall(SubList,( % between(1,NumRows,_), % random(3,5,NumColumns), % randseq(NumColumns,10,SubList)),Input), % 
gen_droplast(Input,Output). gen_instance(Input,Output):-
  random(3,5,NumElts),
  findall(X,(between(1,NumElts,_),random(65,91,N),random(0,2,Q),M is N + Q*32,char_code(X,M),format(atom(C),"'~w'",[X])),Input),
  gen_filtercapslower(Input,Output).

tmp(1,1):-!. tmp(X,Y):-random(1,X,Y).

gen_neg_instance(Input,Output):-
  gen_instance(Input,Out1),!,
  (random(65,91,N),random(0,2,Q),M is N + Q*32,char_code(X,M),format(atom(C),"~w",[X])),
  random_permutation([X|Out1],Output).

to_asp([],l()):-!. to_asp([H|T],l(H2,T2)):-!,to_asp(H,H2),to_asp(T,T2). to_asp(X,X).

from_asp(l(),[]):-!. from_asp(l(H2,T2),[H|T]):-!,from_asp(H2,H),from_asp(T2,T). from_asp(X,X).

gen_instance_asp(X1,Y1):-
  gen_instance(X,Y),to_asp(X,X1),to_asp(Y,Y1). gen_neg_instance_asp(X1,Y1):-
  gen_neg_instance(X,Y),to_asp(X,X1),to_asp(Y,Y1).

print_pos_example:-gen_instance(X,Y),format("p(~k,~k).\n", [X,Y]). print_pos_examples(N):-findall(M,(between(1,N,M),print_pos_example),_). 
print_neg_example:-gen_neg_instance(X,Y),format("q(~k,~k).\n", [X,Y]). print_neg_examples(N):-findall(M,(between(1,N,M),print_neg_example),_).

print_pos_example_asp:-gen_instance_asp(X,Y),format("p(~w,~w).\n", [X,Y]). print_pos_examples_asp(N):-findall(M,(between(1,N,M),print_pos_example_asp),_). 
print_neg_example_asp:-gen_neg_instance_asp(X,Y),format("pos_ex(~w,~w).\n", [X,Y]). print_neg_examples_asp(N):-findall(M,(between(1,N,M),print_neg_example_asp),_).

fix_list(l(),l()).
fix_list(l(A,TL),l(B,TL2)):-format(atom(B),"'~w'",[A]),fix_list(TL,TL2).

print_pos_ex_asp(N):-
  findall(M,(between(1,N,M),gen_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),format("pos_ex(f,~w,~w).\n",[X1,Y1])),_).
print_pos_ex_typed_asp(N):-
  findall(M,(between(1,N,M),gen_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),format("pos_ex(f,(list(char)),list(char)),~w,~w).\n",[X1,Y1])),_).
print_pos_ex_asp_both(N):-
  findall(M,(between(1,N,M),gen_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),
  fix_list(X1,X2),fix_list(Y1,Y2),
  format("pos_ex(f,~w,~w).\n",[X2,Y2]),format("pos_ex_t(f,(list(char),list(char)),~w,~w).\n",[X2,Y2])),_).
print_neg_ex_asp_both(N):-
  findall(M,(between(1,N,M),gen_neg_instance(X,Y),
  to_asp(X,X1),to_asp(Y,Y1),
  fix_list(X1,X2),fix_list(Y1,Y2),
  format("neg_ex(f,~w,~w).\n",[X2,Y2]),format("neg_ex_t(f,(list(char),list(char)),~w,~w).\n",[X2,Y2])),_).
