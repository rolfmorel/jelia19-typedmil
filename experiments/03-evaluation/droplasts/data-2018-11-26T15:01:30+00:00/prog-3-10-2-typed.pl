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

my_element3(A,B):-member(B,A).
my_list_to_set4(A,B):-list_to_set(A,B).
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_max_list7(A,B):-max_list(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_min_list9(A,B):-min_list(A,B).

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

my_flatten11(A,B):-flatten(A,B).
my_tolower12(A,B):-downcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_len5,[list(_),int]).
prim(my_head6,[list(T),T]).
prim(my_max_list7,[list(int),int]).
prim(my_uppercase8,[char]).
prim(my_min_list9,[list(int),int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_tolower12,[char,char]).
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
p([['B','S','E'],['l','o','k','p'],['R','v','T','q'],['x','z','d']],[['B','S'],['l','o','k'],['R','v','T'],['x','z']]).
p([['f','A','F'],['Z','u','v','t'],['V','x','w','J'],['L','t','c']],[['f','A'],['Z','u','v'],['V','x','w'],['L','t']]).
p([['N','j','p','Z'],['l','O','y']],[['N','j','p'],['l','O']]).
p([['s','y','F'],['L','e','m']],[['s','y'],['L','e']]).
p([['j','g','q','m'],['n','A','D'],['u','E','v']],[['j','g','q'],['n','A'],['u','E']]).
q([['a','Z','O','w'],['u','e','v','e'],['w','r','t','S'],['v','v','Z']],[['a','Z','O','w'],['u','e','v'],['w','r','t','S'],['v','v','Z']]).
q([['V','c','m','f'],['c','d','k','u']],[['V','c','m'],['c','d','k','u']]).
q([['t','a','i'],['M','H','E','y'],['p','S','N']],[['t','a','i'],['M','H','E','y'],['p','S']]).
q([['l','j','F','m'],['B','e','B'],['j','H','H','y'],['K','o','v']],[['l','j','F','m'],['B','e','B'],['j','H','H','y'],['K','o']]).
q([['J','s','x'],['q','J','s'],['D','z','d'],['R','O','G']],[['J','s'],['q','J','s'],['D','z'],['R','O','G']]).
