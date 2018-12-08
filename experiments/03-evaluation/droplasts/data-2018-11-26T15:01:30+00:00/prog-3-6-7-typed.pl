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

my_odd3(A):-1 is A mod 2.
my_head4([H|_],H).
my_pred5(A,B):-succ(B,A),A > 0.
my_last6(A,B):-last(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_element8(A,B):-member(B,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_odd3,[int]).
prim(my_head4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_last6,[list(T),T]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_element8,[list(T),T]).
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
p([['u','u','I','o'],['r','I','i','w'],['m','x','K','X']],[['u','u','I'],['r','I','i'],['m','x','K']]).
p([['e','P','Q'],['O','r','a'],['r','T','A'],['y','o','S']],[['e','P'],['O','r'],['r','T'],['y','o']]).
p([['Y','T','P'],['J','n','Q']],[['Y','T'],['J','n']]).
p([['K','u','g'],['j','t','y']],[['K','u'],['j','t']]).
p([['z','V','T','u'],['M','L','r','M'],['w','v','N','T'],['m','X','x','U']],[['z','V','T'],['M','L','r'],['w','v','N'],['m','X','x']]).
q([['H','a','s','i'],['p','I','k'],['j','y','K','y']],[['H','a','s'],['p','I','k'],['j','y','K','y']]).
q([['z','a','c'],['L','E','N','s'],['c','T','s'],['c','A','d']],[['z','a','c'],['L','E','N'],['c','T'],['c','A','d']]).
q([['C','D','B'],['J','X','K'],['e','o','Z','p']],[['C','D','B'],['J','X','K'],['e','o','Z']]).
q([['w','e','L','U'],['e','g','I','D'],['Y','I','Y','E'],['Q','H','g']],[['w','e','L','U'],['e','g','I'],['Y','I','Y'],['Q','H','g']]).
q([['Y','h','S'],['g','S','A'],['C','R','S','Y'],['N','D','p']],[['Y','h'],['g','S'],['C','R','S','Y'],['N','D','p']]).
