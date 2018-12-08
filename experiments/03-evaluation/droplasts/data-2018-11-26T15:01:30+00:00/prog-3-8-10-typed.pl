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

my_pred3(A,B):-succ(B,A),A > 0.
my_element4(A,B):-member(B,A).
my_succ5(A,B):-succ(A,B),B =< 10.
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A).
my_msort8(A,B):-msort(A,B).
my_len9(A,B):-length(A,B).
my_even10(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_element4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_set6,[list(_)]).
prim(my_lowercase7,[char]).
prim(my_msort8,[list(int),list(int)]).
prim(my_len9,[list(_),int]).
prim(my_even10,[int]).
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
p([['U','V','D'],['k','s','e'],['R','q','s'],['O','F','y','e']],[['U','V'],['k','s'],['R','q'],['O','F','y']]).
p([['Q','a','X'],['Q','o','z','O'],['n','w','Q'],['Q','T','t']],[['Q','a'],['Q','o','z'],['n','w'],['Q','T']]).
p([['n','h','E','Q'],['y','w','Z'],['t','n','O','X'],['a','k','I']],[['n','h','E'],['y','w'],['t','n','O'],['a','k']]).
p([['v','x','g','A'],['K','s','o','K'],['n','V','A']],[['v','x','g'],['K','s','o'],['n','V']]).
p([['n','e','g'],['o','q','N']],[['n','e'],['o','q']]).
q([['l','L','Q','U'],['c','R','C'],['D','H','n'],['c','d','r','q']],[['l','L','Q'],['c','R'],['D','H','n'],['c','d','r','q']]).
q([['p','M','q'],['g','s','e'],['P','i','Q','k'],['G','h','f']],[['p','M'],['g','s','e'],['P','i','Q','k'],['G','h']]).
q([['J','B','v','H'],['F','f','D'],['G','f','u','j'],['n','O','F','B']],[['J','B','v','H'],['F','f'],['G','f','u','j'],['n','O','F','B']]).
q([['Q','N','g','P'],['Q','J','C'],['s','A','H','i']],[['Q','N','g'],['Q','J','C'],['s','A','H','i']]).
q([['R','Q','Q'],['N','T','i','D'],['i','q','X'],['c','a','K']],[['R','Q'],['N','T','i','D'],['i','q','X'],['c','a','K']]).
