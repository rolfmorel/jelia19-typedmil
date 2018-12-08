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
my_msort4(A,B):-msort(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_last6(A,B):-last(A,B).
my_len7(A,B):-length(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_set10(A):-list_to_set(A,A).
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_max_list13(A,B):-max_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_lowercase15(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_msort4,[list(int),list(int)]).
prim(my_toupper5,[char,char]).
prim(my_last6,[list(T),T]).
prim(my_len7,[list(_),int]).
prim(my_tolower8,[char,char]).
prim(my_succ9,[int,int]).
prim(my_set10,[list(_)]).
prim(my_min_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_lowercase15,[char]).
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
p(['e','r','k','P','e'],'e').
p(['C','P','r','O','r','c','i','K'],'r').
p(['z','l','z','C','l'],'z').
p(['l','l','V','T','O','c','F','u'],'l').
p(['P','K','F','K','n','Y'],'K').
q(['v','f','j','S','d','S','e','q','K','H'],'f').
q(['i','n','n','z','K','r','s','J','L','x','a'],'r').
q(['c','p','v','S','o','S'],'o').
q(['q','h','[','d','f','o','q'],'[').
q(['q','n','L','i','X','n','w','O'],'i').
