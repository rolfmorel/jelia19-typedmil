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
my_flatten3(A,B):-flatten(A,B).
my_set4(A):-list_to_set(A,A).
my_msort5(A,B):-msort(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_uppercase7(A):-upcase_atom(A,A).
my_lowercase8(A):-downcase_atom(A,A).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_even13(A):-0 is A mod 2.
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_list_to_set17(A,B):-list_to_set(A,B).
my_succ18(A,B):-succ(A,B),B =< 10.
my_odd19(A):-1 is A mod 2.
my_tolower20(A,B):-downcase_atom(A,B).
my_reverse21(A,B):-reverse(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_set4,[list(_)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_double6,[int,int]).
prim(my_uppercase7,[char]).
prim(my_lowercase8,[char]).
prim(my_max_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_toupper11,[char,char]).
prim(my_pred12,[int,int]).
prim(my_even13,[int]).
prim(my_len14,[list(_),int]).
prim(my_last15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_list_to_set17,[list(T),list(T)]).
prim(my_succ18,[int,int]).
prim(my_odd19,[int]).
prim(my_tolower20,[char,char]).
prim(my_reverse21,[list(T),list(T)]).
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
p(['v','D','V','h','v','v','D','J','k','n'],'v').
p(['L','Z','I','W','I'],'I').
p(['S','r','t','G','q','c','r'],'r').
p(['r','i','O','D','D'],'D').
p(['w','d','w','x','P','o'],'w').
q(['A','r','O','q','g','P','G','O','w','W'],'q').
q(['E','H','C','z','s','E'],'H').
q(['p','M','f','q','F','q','U','S','O'],'S').
q(['a','G','q','h','D','q','M','X'],'G').
q(['A','X','R','u','X','V','U','y','u'],'V').
