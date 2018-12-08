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

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_succ5(A,B):-succ(A,B),B =< 10.
my_flatten6(A,B):-flatten(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_list_to_set8(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_double7,[int,int]).
prim(my_list_to_set8,[list(T),list(T)]).
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
p([['j','Z','Q','w'],['d','Q','g'],['n','J','A','C']],[['j','Z','Q'],['d','Q'],['n','J','A']]).
p([['A','x','H'],['G','H','k','N'],['t','I','R'],['W','V','k','O']],[['A','x'],['G','H','k'],['t','I'],['W','V','k']]).
p([['g','R','q','W'],['i','u','y','h']],[['g','R','q'],['i','u','y']]).
p([['S','i','R','j'],['J','g','S','z']],[['S','i','R'],['J','g','S']]).
p([['T','U','k'],['D','Q','U'],['D','H','a'],['F','u','h','m']],[['T','U'],['D','Q'],['D','H'],['F','u','h']]).
q([['T','Q','q','k'],['g','p','f','C'],['m','R','m','h']],[['T','Q','q'],['g','p','f','C'],['m','R','m','h']]).
q([['K','S','G','Q'],['C','A','O','A'],['r','B','P','L'],['f','A','v','c']],[['K','S','G','Q'],['C','A','O','A'],['r','B','P'],['f','A','v']]).
q([['Z','v','e','A'],['T','c','M','V']],[['Z','v','e','A'],['T','c','M']]).
q([['P','b','K','t'],['r','G','N','Z'],['m','t','W']],[['P','b','K','t'],['r','G','N','Z'],['m','t']]).
q([['d','u','w'],['V','x','H','G'],['j','i','H']],[['d','u'],['V','x','H','G'],['j','i','H']]).
