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
my_min_list3(A,B):-min_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_set5(A):-list_to_set(A,A).
my_succ6(A,B):-succ(A,B),B =< 10.
my_last7(A,B):-last(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_list_to_set9(A,B):-list_to_set(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_toupper12(A,B):-upcase_atom(A,B).
my_odd13(A):-1 is A mod 2.
my_flatten14(A,B):-flatten(A,B).
my_reverse15(A,B):-reverse(A,B).
my_even16(A):-0 is A mod 2.
my_uppercase17(A):-upcase_atom(A,A).
my_tolower18(A,B):-downcase_atom(A,B).
my_len19(A,B):-length(A,B).
my_lowercase20(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_set5,[list(_)]).
prim(my_succ6,[int,int]).
prim(my_last7,[list(T),T]).
prim(my_double8,[int,int]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_max_list10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_toupper12,[char,char]).
prim(my_odd13,[int]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_even16,[int]).
prim(my_uppercase17,[char]).
prim(my_tolower18,[char,char]).
prim(my_len19,[list(_),int]).
prim(my_lowercase20,[char]).
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
p(['e','A','D','e','v','C','d'],'e').
p(['J','G','G','o','G'],'G').
p(['p','O','a','s','a','a','o'],'a').
p(['f','e','P','e','E','D','S'],'e').
p(['r','V','j','B','t','q','d','M','M'],'M').
q(['Q','P','p','c','P','n'],'c').
q(['L','h','U','f','M','U'],'L').
q(['K','H','Q','Q','z','U'],'z').
q(['Z','p','p','y','M','H','p','w'],'Z').
q(['D','a','a','m','g','K','k','o','D','I'],'I').
