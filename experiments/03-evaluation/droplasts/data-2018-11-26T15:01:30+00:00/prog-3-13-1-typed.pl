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

my_lowercase3(A):-downcase_atom(A,A).

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

my_odd5(A):-1 is A mod 2.
my_element6(A,B):-member(B,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_min_list9(A,B):-min_list(A,B).
my_flatten10(A,B):-flatten(A,B).
my_even11(A):-0 is A mod 2.
my_pred12(A,B):-succ(B,A),A > 0.
my_last13(A,B):-last(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_max_list15(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_lowercase3,[char]).
prim(my_odd5,[int]).
prim(my_element6,[list(T),T]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_succ8,[int,int]).
prim(my_min_list9,[list(int),int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_even11,[int]).
prim(my_pred12,[int,int]).
prim(my_last13,[list(T),T]).
prim(my_tolower14,[char,char]).
prim(my_max_list15,[list(int),int]).
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
p([['S','h','G'],['D','Y','z','x'],['h','B','v','f'],['B','j','T']],[['S','h'],['D','Y','z'],['h','B','v'],['B','j']]).
p([['j','p','R'],['U','m','K','e']],[['j','p'],['U','m','K']]).
p([['K','P','r','T'],['q','U','p','Y']],[['K','P','r'],['q','U','p']]).
p([['D','c','J'],['G','x','P']],[['D','c'],['G','x']]).
p([['T','H','n'],['R','r','S'],['U','p','c','U']],[['T','H'],['R','r'],['U','p','c']]).
q([['e','Y','X'],['T','D','I'],['S','F','B']],[['e','Y'],['T','D','I'],['S','F','B']]).
q([['B','b','e','C'],['P','r','i','d'],['K','N','r']],[['B','b','e','C'],['P','r','i'],['K','N','r']]).
q([['N','G','K'],['p','g','d'],['L','H','v'],['n','e','Y']],[['N','G','K'],['p','g','d'],['L','H'],['n','e','Y']]).
q([['q','J','v','k'],['O','E','n']],[['q','J','v','k'],['O','E']]).
q([['x','r','K'],['f','V','O','k']],[['x','r','K'],['f','V','O']]).
