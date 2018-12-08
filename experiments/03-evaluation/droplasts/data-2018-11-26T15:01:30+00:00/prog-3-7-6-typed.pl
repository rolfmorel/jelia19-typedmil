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

my_element3(A,B):-member(B,A).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_len6(A,B):-length(A,B).
my_even7(A):-0 is A mod 2.
my_list_to_set8(A,B):-list_to_set(A,B).
my_msort9(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_pred5,[int,int]).
prim(my_len6,[list(_),int]).
prim(my_even7,[int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_msort9,[list(int),list(int)]).
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
p([['Y','u','u'],['H','U','Y']],[['Y','u'],['H','U']]).
p([['w','A','M'],['K','b','o'],['L','K','I'],['S','K','D','y']],[['w','A'],['K','b'],['L','K'],['S','K','D']]).
p([['e','t','Y','g'],['Y','k','W','E'],['k','X','X','D'],['N','b','U','i']],[['e','t','Y'],['Y','k','W'],['k','X','X'],['N','b','U']]).
p([['o','r','I'],['K','Q','R'],['T','G','M'],['I','x','y']],[['o','r'],['K','Q'],['T','G'],['I','x']]).
p([['s','Z','L','e'],['M','i','p','X'],['b','o','K','C']],[['s','Z','L'],['M','i','p'],['b','o','K']]).
q([['T','P','C'],['T','S','d','o']],[['T','P','C'],['T','S','d']]).
q([['C','z','L','E'],['D','h','J'],['h','V','i'],['i','u','b']],[['C','z','L'],['D','h'],['h','V','i'],['i','u','b']]).
q([['s','i','a','m'],['K','R','h','O']],[['s','i','a'],['K','R','h','O']]).
q([['h','k','L'],['c','a','j','l']],[['h','k'],['c','a','j','l']]).
q([['H','n','b','q'],['m','g','C']],[['H','n','b','q'],['m','g']]).
