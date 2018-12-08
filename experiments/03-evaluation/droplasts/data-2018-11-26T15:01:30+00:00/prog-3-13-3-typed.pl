:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_uppercase3(A):-upcase_atom(A,A).
my_last4(A,B):-last(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_list_to_set9(A,B):-list_to_set(A,B).
my_even10(A):-0 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_set12(A):-list_to_set(A,A).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_toupper14(A,B):-upcase_atom(A,B).
my_element15(A,B):-member(B,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_last4,[list(T),T]).
prim(my_tolower5,[char,char]).
prim(my_len6,[list(_),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_lowercase8,[char]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_even10,[int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_set12,[list(_)]).
prim(my_toupper14,[char,char]).
prim(my_element15,[list(T),T]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['H','S','h'],['m','s','R']],[['H','S'],['m','s']]).
p([['c','z','m'],['v','q','U','Y']],[['c','z'],['v','q','U']]).
p([['q','n','v'],['u','v','w','u'],['s','I','J','W']],[['q','n'],['u','v','w'],['s','I','J']]).
p([['J','T','p'],['n','v','i']],[['J','T'],['n','v']]).
p([['j','B','K'],['q','w','x','s'],['g','X','Y','W']],[['j','B'],['q','w','x'],['g','X','Y']]).
q([['X','D','S','M'],['U','U','t']],[['X','D','S','M'],['U','U']]).
q([['H','Q','u','q'],['a','T','G','H'],['j','P','B'],['u','x','U','d']],[['H','Q','u'],['a','T','G','H'],['j','P'],['u','x','U','d']]).
q([['J','T','I','V'],['i','C','R'],['z','x','h'],['o','N','o']],[['J','T','I','V'],['i','C'],['z','x'],['o','N','o']]).
q([['F','s','B'],['W','D','P']],[['F','s'],['W','D','P']]).
q([['S','Q','e'],['a','I','z','w'],['W','E','v','G']],[['S','Q','e'],['a','I','z'],['W','E','v','G']]).
