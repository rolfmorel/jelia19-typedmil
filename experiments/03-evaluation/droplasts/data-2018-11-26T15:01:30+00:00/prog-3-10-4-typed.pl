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

my_sumlist3(A,B):-sumlist(A,B).
my_element4(A,B):-member(B,A).
my_msort5(A,B):-msort(A,B).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_odd8(A):-1 is A mod 2.
my_tolower9(A,B):-downcase_atom(A,B).
my_head10([H|_],H).
my_flatten11(A,B):-flatten(A,B).
my_min_list12(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_element4,[list(T),T]).
prim(my_msort5,[list(int),list(int)]).
prim(my_last6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_odd8,[int]).
prim(my_tolower9,[char,char]).
prim(my_head10,[list(T),T]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_min_list12,[list(int),int]).
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
p([['F','c','Z','g'],['K','r','E'],['H','K','e','d'],['P','b','e']],[['F','c','Z'],['K','r'],['H','K','e'],['P','b']]).
p([['M','o','F'],['F','F','t','t'],['h','p','h','W'],['C','Z','F','Z']],[['M','o'],['F','F','t'],['h','p','h'],['C','Z','F']]).
p([['t','S','u'],['r','U','j'],['C','w','Q','o']],[['t','S'],['r','U'],['C','w','Q']]).
p([['y','Z','q','L'],['K','z','L']],[['y','Z','q'],['K','z']]).
p([['H','v','f'],['q','O','O','N'],['X','L','W','p']],[['H','v'],['q','O','O'],['X','L','W']]).
q([['k','U','H'],['X','W','T'],['F','W','l','i']],[['k','U'],['X','W','T'],['F','W','l','i']]).
q([['R','B','L'],['S','Q','x','c'],['W','M','Y','h']],[['R','B','L'],['S','Q','x'],['W','M','Y','h']]).
q([['t','X','M','J'],['E','l','p','w'],['D','t','u','M']],[['t','X','M'],['E','l','p','w'],['D','t','u','M']]).
q([['E','V','V','z'],['b','w','H'],['y','s','R'],['B','G','L']],[['E','V','V','z'],['b','w','H'],['y','s'],['B','G']]).
q([['A','A','T','d'],['J','P','n'],['Y','h','b','U']],[['A','A','T'],['J','P','n'],['Y','h','b','U']]).
