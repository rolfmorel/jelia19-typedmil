:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
%metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_last3(A,B):-last(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_max_list6(A,B):-max_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_msort8(A,B):-msort(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_reverse10(A,B):-reverse(A,B).
my_flatten11(A,B):-flatten(A,B).
my_set12(A):-list_to_set(A,A).
my_even13(A):-0 is A mod 2.
my_succ14(A,B):-succ(A,B),B =< 10.
my_double15(N,M):-M is 2*N,M =< 10.
my_min_list16(A,B):-min_list(A,B).
my_odd17(A):-1 is A mod 2.
my_lowercase18(A):-downcase_atom(A,A).
my_tolower19(A,B):-downcase_atom(A,B).
my_len20(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_sumlist5,[list(int),int]).
prim(my_max_list6,[list(int),int]).
prim(my_pred7,[int,int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_toupper9,[char,char]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_set12,[list(_)]).
prim(my_even13,[int]).
prim(my_succ14,[int,int]).
prim(my_double15,[int,int]).
prim(my_min_list16,[list(int),int]).
prim(my_odd17,[int]).
prim(my_lowercase18,[char]).
prim(my_tolower19,[char,char]).
prim(my_len20,[list(_),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),char],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['B','u','B','j','j','d','o','O'],'j').
p(['O','H','W','P','f','w','T','H','f','q'],'H').
p(['E','h','z','h','o'],'h').
p(['t','k','R','t','k','p','b'],'t').
p(['v','K','K','s','y','G','k','I','Z','D'],'K').
q(['C','C','r','u','c','k','w','o','z'],'c').
q(['P','n','p','n','F','I','S','I','r'],'P').
q(['t','d','s','L','h','w','I','n','I'],'d').
q(['L','[','A','F','d','d','Y'],'[').
q(['A','i','T','H','L','H'],'T').
