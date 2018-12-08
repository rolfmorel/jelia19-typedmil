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

my_min_list3(A,B):-min_list(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_odd5(A):-1 is A mod 2.
my_uppercase6(A):-upcase_atom(A,A).
my_set7(A):-list_to_set(A,A).
my_len8(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_min_list3,[list(int),int]).
prim(my_lowercase4,[char]).
prim(my_odd5,[int]).
prim(my_uppercase6,[char]).
prim(my_set7,[list(_)]).
prim(my_len8,[list(_),int]).
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
p([['h','e','l'],['E','Z','E']],[['h','e'],['E','Z']]).
p([['q','E','c'],['T','Z','v','o']],[['q','E'],['T','Z','v']]).
p([['P','l','D','h'],['D','V','N','p']],[['P','l','D'],['D','V','N']]).
p([['N','m','F'],['h','g','d']],[['N','m'],['h','g']]).
p([['U','n','C'],['Y','H','j','n'],['o','V','o']],[['U','n'],['Y','H','j'],['o','V']]).
q([['r','k','x','A'],['j','O','T','j']],[['r','k','x','A'],['j','O','T']]).
q([['V','b','S','S'],['G','g','i','a'],['b','Z','F'],['N','v','N','S']],[['V','b','S','S'],['G','g','i'],['b','Z','F'],['N','v','N','S']]).
q([['D','r','z','f'],['X','q','o','H'],['f','X','M','c'],['m','T','l']],[['D','r','z','f'],['X','q','o','H'],['f','X','M','c'],['m','T']]).
q([['B','R','Q'],['v','j','V','j'],['j','D','G','U']],[['B','R','Q'],['v','j','V'],['j','D','G','U']]).
q([['f','D','B'],['D','c','m'],['r','N','K'],['m','T','H','F']],[['f','D','B'],['D','c','m'],['r','N','K'],['m','T','H']]).
