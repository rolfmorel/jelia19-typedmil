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
my_min_list4(A,B):-min_list(A,B).
my_uppercase5(A):-upcase_atom(A,A).
my_pred6(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_min_list4,[list(int),int]).
prim(my_uppercase5,[char]).
prim(my_pred6,[int,int]).
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
p([['j','g','U'],['N','B','c']],[['j','g'],['N','B']]).
p([['p','i','Z'],['e','J','f'],['Q','Q','o'],['h','c','x','E']],[['p','i'],['e','J'],['Q','Q'],['h','c','x']]).
p([['W','I','i','H'],['k','W','C']],[['W','I','i'],['k','W']]).
p([['Q','p','b','u'],['h','J','B','X'],['x','v','o','V']],[['Q','p','b'],['h','J','B'],['x','v','o']]).
p([['E','J','p'],['Q','i','M','h'],['Z','F','l','M']],[['E','J'],['Q','i','M'],['Z','F','l']]).
q([['Z','R','V','a'],['T','l','x','C'],['g','c','M','O']],[['Z','R','V'],['T','l','x','C'],['g','c','M','O']]).
q([['n','L','p','Z'],['T','u','w','F'],['h','Z','K','a']],[['n','L','p','Z'],['T','u','w'],['h','Z','K','a']]).
q([['N','d','G'],['S','R','R'],['X','C','M'],['L','e','e','e']],[['N','d'],['S','R'],['X','C','M'],['L','e','e','e']]).
q([['j','P','S','U'],['K','G','p','A'],['H','R','B','j']],[['j','P','S','U'],['K','G','p'],['H','R','B','j']]).
q([['S','a','P'],['O','a','b','h'],['r','q','P']],[['S','a'],['O','a','b','h'],['r','q','P']]).
