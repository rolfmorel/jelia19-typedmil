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
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_element6(A,B):-member(B,A).
my_len7(A,B):-length(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_uppercase9(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_lowercase3,[char]).
prim(my_min_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_element6,[list(T),T]).
prim(my_len7,[list(_),int]).
prim(my_pred8,[int,int]).
prim(my_uppercase9,[char]).
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
p([['g','q','g'],['H','o','K'],['F','w','O']],[['g','q'],['H','o'],['F','w']]).
p([['K','Y','P','Q'],['j','F','L']],[['K','Y','P'],['j','F']]).
p([['A','U','V','O'],['z','i','A'],['u','A','e','E']],[['A','U','V'],['z','i'],['u','A','e']]).
p([['Q','F','Y','f'],['c','d','g']],[['Q','F','Y'],['c','d']]).
p([['g','h','A'],['D','E','m']],[['g','h'],['D','E']]).
q([['b','X','R'],['V','w','u','B'],['a','e','E']],[['b','X','R'],['V','w','u','B'],['a','e']]).
q([['l','b','O','o'],['y','Z','H'],['U','T','p']],[['l','b','O','o'],['y','Z'],['U','T','p']]).
q([['W','e','B'],['X','N','e','i'],['v','Z','W'],['u','R','t','v']],[['W','e','B'],['X','N','e','i'],['v','Z'],['u','R','t','v']]).
q([['m','Z','l'],['d','q','k','r'],['n','q','E']],[['m','Z','l'],['d','q','k'],['n','q','E']]).
q([['t','x','H'],['E','u','p']],[['t','x','H'],['E','u']]).
