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
my_succ4(A,B):-succ(A,B),B =< 10.
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A),A > 0.
my_odd8(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_succ4,[int,int]).
prim(my_len5,[list(_),int]).
prim(my_head6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_odd8,[int]).
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
p([['Y','P','p','u'],['t','g','x'],['L','i','M'],['d','p','K','W']],[['Y','P','p'],['t','g'],['L','i'],['d','p','K']]).
p([['x','p','a'],['x','W','F']],[['x','p'],['x','W']]).
p([['Y','A','m'],['E','x','j','c'],['N','F','x','V'],['G','p','C','w']],[['Y','A'],['E','x','j'],['N','F','x'],['G','p','C']]).
p([['O','P','Q'],['K','Z','l','h'],['h','p','T'],['Z','Y','v']],[['O','P'],['K','Z','l'],['h','p'],['Z','Y']]).
p([['j','N','J','G'],['a','x','c','B'],['j','i','z','h']],[['j','N','J'],['a','x','c'],['j','i','z']]).
q([['x','l','U','D'],['b','A','t','k'],['r','v','I'],['r','v','s']],[['x','l','U'],['b','A','t','k'],['r','v','I'],['r','v','s']]).
q([['P','n','Y'],['E','M','c']],[['P','n'],['E','M','c']]).
q([['x','g','w'],['B','C','S'],['W','S','V']],[['x','g','w'],['B','C','S'],['W','S']]).
q([['b','s','p'],['y','I','l']],[['b','s','p'],['y','I']]).
q([['R','q','W','P'],['I','l','f','c'],['S','k','F','L'],['y','Q','r','B']],[['R','q','W'],['I','l','f','c'],['S','k','F','L'],['y','Q','r']]).
