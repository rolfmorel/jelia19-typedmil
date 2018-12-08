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
my_len3(A,B):-length(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_reverse5(A,B):-reverse(A,B).
my_even6(A):-0 is A mod 2.
my_lowercase7(A):-downcase_atom(A,A).
my_toupper8(A,B):-upcase_atom(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_odd10(A):-1 is A mod 2.
my_sumlist11(A,B):-sumlist(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_succ13(A,B):-succ(A,B),B =< 10.
my_msort14(A,B):-msort(A,B).
my_last15(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_even6,[int]).
prim(my_lowercase7,[char]).
prim(my_toupper8,[char,char]).
prim(my_pred9,[int,int]).
prim(my_odd10,[int]).
prim(my_sumlist11,[list(int),int]).
prim(my_uppercase12,[char]).
prim(my_succ13,[int,int]).
prim(my_msort14,[list(int),list(int)]).
prim(my_last15,[list(T),T]).
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
p(['n','K','b','G','f','U','n','m','I'],'n').
p(['g','Q','V','M','L','G','H','g'],'g').
p(['Y','I','c','u','G','s','w','a','w','L'],'w').
p(['A','k','P','H','P'],'P').
p(['j','y','P','E','E','i','R','A','f','R'],'E').
q(['U','u','D','z','y','b','i','D','c','c','P'],'u').
q(['v','Q','v','J','S','J','s','h','U','L'],'Q').
q(['u','i','M','t','M','x','L','J','l','s','i'],'L').
q(['s','C','z','e','M','s'],'M').
q(['h','E','F','E','h','a','T'],'T').
