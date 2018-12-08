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

my_set3(A):-list_to_set(A,A).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_head4,[list(T),T]).
prim(my_last5,[list(T),T]).
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
p([['g','g','d'],['D','i','X','P'],['X','G','q','l'],['Z','C','u','T']],[['g','g'],['D','i','X'],['X','G','q'],['Z','C','u']]).
p([['O','i','g','x'],['t','W','M'],['k','O','Y']],[['O','i','g'],['t','W'],['k','O']]).
p([['I','G','H','x'],['i','y','i'],['U','x','m'],['b','J','W']],[['I','G','H'],['i','y'],['U','x'],['b','J']]).
p([['c','l','w','a'],['T','M','d'],['G','Z','p'],['h','s','V','B']],[['c','l','w'],['T','M'],['G','Z'],['h','s','V']]).
p([['b','t','H','Q'],['O','v','Z','O']],[['b','t','H'],['O','v','Z']]).
q([['n','y','s'],['f','F','X'],['U','W','v']],[['n','y','s'],['f','F'],['U','W','v']]).
q([['Q','A','h'],['V','j','P']],[['Q','A'],['V','j','P']]).
q([['w','o','s','w'],['O','N','q']],[['w','o','s'],['O','N','q']]).
q([['f','v','N'],['w','t','U','g']],[['f','v','N'],['w','t','U']]).
q([['K','H','P','T'],['e','Y','B','Y'],['V','Z','H','A']],[['K','H','P'],['e','Y','B','Y'],['V','Z','H','A']]).
