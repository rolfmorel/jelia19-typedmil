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
my_min_list4(A,B):-min_list(A,B).
my_set5(A):-list_to_set(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_pred10(A,B):-succ(B,A),A > 0.
my_odd11(A):-1 is A mod 2.
my_tolower12(A,B):-downcase_atom(A,B).
my_flatten13(A,B):-flatten(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_list_to_set15(A,B):-list_to_set(A,B).
my_toupper16(A,B):-upcase_atom(A,B).
my_msort17(A,B):-msort(A,B).
my_reverse18(A,B):-reverse(A,B).
my_even19(A):-0 is A mod 2.
my_lowercase20(A):-downcase_atom(A,A).
my_sumlist21(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_set5,[list(_)]).
prim(my_uppercase6,[char]).
prim(my_max_list7,[list(int),int]).
prim(my_len8,[list(_),int]).
prim(my_succ9,[int,int]).
prim(my_pred10,[int,int]).
prim(my_odd11,[int]).
prim(my_tolower12,[char,char]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_double14,[int,int]).
prim(my_list_to_set15,[list(T),list(T)]).
prim(my_toupper16,[char,char]).
prim(my_msort17,[list(int),list(int)]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_even19,[int]).
prim(my_lowercase20,[char]).
prim(my_sumlist21,[list(int),int]).
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
p(['l','f','W','g','l','i','P'],'l').
p(['P','q','f','E','q','N'],'q').
p(['M','g','e','s','g','P','e'],'e').
p(['f','z','k','z','b','w','a'],'z').
p(['y','c','L','d','B','H','c','m'],'c').
q(['c','N','l','d','g','X','P','D','I','S','l'],'I').
q(['I','v','w','H','B','G','B'],'v').
q(['d','G','i','q','W','E','N','b','g','E','V'],'i').
q(['X','i','D','I','m','s','s','b'],'I').
q(['X','Y','x','{','v','x','X','I','B'],'{').
