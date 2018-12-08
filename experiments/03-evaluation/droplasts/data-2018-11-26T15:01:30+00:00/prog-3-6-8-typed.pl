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

my_even3(A):-0 is A mod 2.
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_head8([H|_],H).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_even3,[int]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_len6,[list(_),int]).
prim(my_pred7,[int,int]).
prim(my_head8,[list(T),T]).
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
p([['w','c','t'],['y','D','V','p']],[['w','c'],['y','D','V']]).
p([['t','J','o','l'],['w','T','N']],[['t','J','o'],['w','T']]).
p([['u','a','F','k'],['j','Z','N','b'],['D','l','q','Z'],['H','U','U']],[['u','a','F'],['j','Z','N'],['D','l','q'],['H','U']]).
p([['T','P','D'],['P','O','L']],[['T','P'],['P','O']]).
p([['v','C','V'],['a','k','R']],[['v','C'],['a','k']]).
q([['t','p','M'],['e','w','z','J'],['E','f','f','U'],['v','X','r']],[['t','p','M'],['e','w','z'],['E','f','f','U'],['v','X','r']]).
q([['l','K','s','y'],['y','n','e'],['P','Y','e'],['K','X','i']],[['l','K','s','y'],['y','n'],['P','Y'],['K','X','i']]).
q([['S','J','l','N'],['J','l','o','t'],['R','x','G','b']],[['S','J','l'],['J','l','o','t'],['R','x','G','b']]).
q([['h','z','c','M'],['o','L','Y'],['w','q','O','Z'],['G','Z','n']],[['h','z','c','M'],['o','L','Y'],['w','q','O','Z'],['G','Z']]).
q([['y','Z','u'],['b','u','d','x']],[['y','Z','u'],['b','u','d']]).
