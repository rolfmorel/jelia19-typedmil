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

my_len3(A,B):-length(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_last4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
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
p([['G','O','g','z'],['C','p','x','Y'],['m','r','J','H'],['G','g','U']],[['G','O','g'],['C','p','x'],['m','r','J'],['G','g']]).
p([['y','o','H','X'],['H','g','M']],[['y','o','H'],['H','g']]).
p([['p','K','X','d'],['Z','J','Z','w'],['L','A','j'],['F','W','V','U']],[['p','K','X'],['Z','J','Z'],['L','A'],['F','W','V']]).
p([['P','l','w','T'],['G','h','l','X'],['W','n','Y'],['s','I','q','X']],[['P','l','w'],['G','h','l'],['W','n'],['s','I','q']]).
p([['t','V','j','T'],['V','x','k']],[['t','V','j'],['V','x']]).
q([['i','Z','W'],['N','l','M','y'],['q','X','J'],['N','x','L']],[['i','Z'],['N','l','M','y'],['q','X'],['N','x','L']]).
q([['M','o','G'],['F','v','k'],['f','D','a','h'],['w','Y','k','g']],[['M','o','G'],['F','v'],['f','D','a','h'],['w','Y','k']]).
q([['T','o','g','K'],['c','x','W'],['g','y','r','C'],['U','d','L','O']],[['T','o','g','K'],['c','x'],['g','y','r','C'],['U','d','L','O']]).
q([['h','U','I','n'],['t','k','j','M'],['g','D','G','N']],[['h','U','I'],['t','k','j','M'],['g','D','G','N']]).
q([['n','H','N'],['s','A','h','u']],[['n','H'],['s','A','h','u']]).
