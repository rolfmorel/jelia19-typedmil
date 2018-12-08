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
my_list_to_set3(A,B):-list_to_set(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_max_list5(A,B):-max_list(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_odd7(A):-1 is A mod 2.
my_min_list8(A,B):-min_list(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_toupper10(A,B):-upcase_atom(A,B).
my_even11(A):-0 is A mod 2.
my_len12(A,B):-length(A,B).
my_set13(A):-list_to_set(A,A).
my_lowercase14(A):-downcase_atom(A,A).
my_uppercase15(A):-upcase_atom(A,A).
my_msort16(A,B):-msort(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_reverse18(A,B):-reverse(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_succ4,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_tolower6,[char,char]).
prim(my_odd7,[int]).
prim(my_min_list8,[list(int),int]).
prim(my_pred9,[int,int]).
prim(my_toupper10,[char,char]).
prim(my_even11,[int]).
prim(my_len12,[list(_),int]).
prim(my_set13,[list(_)]).
prim(my_lowercase14,[char]).
prim(my_uppercase15,[char]).
prim(my_msort16,[list(int),list(int)]).
prim(my_sumlist17,[list(int),int]).
prim(my_reverse18,[list(T),list(T)]).
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
p(['Z','C','Q','Q','p','l','K'],'Q').
p(['b','s','P','c','o','c','h','c','i'],'c').
p(['E','q','e','w','w','E','V','A'],'w').
p(['F','k','K','v','E','f','E','E','p','z'],'E').
p(['B','c','q','X','c','T'],'c').
q(['w','w','h','c','{','o','R','I'],'{').
q(['B','h','L','t','C','t','L','Z'],'Z').
q(['R','u','K','u','G','y','v','F','s','A'],'v').
q(['b','D','p','G','T','E','W','D'],'W').
q(['A','F','U','A','Y','e','j','U','m','Y','H'],'H').
