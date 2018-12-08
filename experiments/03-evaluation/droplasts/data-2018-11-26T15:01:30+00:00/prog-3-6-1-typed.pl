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

my_list_to_set3(A,B):-list_to_set(A,B).
my_msort4(A,B):-msort(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_sumlist6(A,B):-sumlist(A,B).
my_len7(A,B):-length(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_msort4,[list(int),list(int)]).
prim(my_lowercase5,[char]).
prim(my_sumlist6,[list(int),int]).
prim(my_len7,[list(_),int]).
prim(my_double8,[int,int]).
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
p([['R','G','k','O'],['I','S','j','J']],[['R','G','k'],['I','S','j']]).
p([['u','E','B','e'],['z','X','x'],['H','G','y','U']],[['u','E','B'],['z','X'],['H','G','y']]).
p([['j','K','p'],['p','p','J','d'],['A','Z','g']],[['j','K'],['p','p','J'],['A','Z']]).
p([['i','G','y','r'],['U','k','Y']],[['i','G','y'],['U','k']]).
p([['S','B','Q'],['c','x','v']],[['S','B'],['c','x']]).
q([['s','o','g','B'],['Q','Z','C','K']],[['s','o','g','B'],['Q','Z','C']]).
q([['K','k','m','R'],['n','W','b']],[['K','k','m'],['n','W','b']]).
q([['k','X','I'],['q','t','z'],['T','B','k','X'],['m','L','a']],[['k','X'],['q','t'],['T','B','k','X'],['m','L','a']]).
q([['s','M','c','Z'],['k','c','H','D'],['z','C','K','d'],['w','A','F']],[['s','M','c'],['k','c','H','D'],['z','C','K','d'],['w','A']]).
q([['d','K','A','k'],['f','W','j','m']],[['d','K','A'],['f','W','j','m']]).
