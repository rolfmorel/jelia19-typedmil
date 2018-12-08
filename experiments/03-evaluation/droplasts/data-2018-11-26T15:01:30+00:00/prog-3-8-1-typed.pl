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

my_msort3(A,B):-msort(A,B).
my_even4(A):-0 is A mod 2.
my_flatten5(A,B):-flatten(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_msort3,[list(int),list(int)]).
prim(my_even4,[int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_lowercase6,[char]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_sumlist8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_pred10,[int,int]).
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
p([['d','n','K','K'],['I','D','C']],[['d','n','K'],['I','D']]).
p([['F','G','A','P'],['s','z','h'],['R','A','k','t'],['z','j','Y']],[['F','G','A'],['s','z'],['R','A','k'],['z','j']]).
p([['H','E','g','k'],['b','G','v','w'],['E','t','n'],['J','t','S','J']],[['H','E','g'],['b','G','v'],['E','t'],['J','t','S']]).
p([['K','b','G','S'],['o','v','h','d'],['v','t','d'],['d','t','S']],[['K','b','G'],['o','v','h'],['v','t'],['d','t']]).
p([['x','c','l','r'],['P','u','m']],[['x','c','l'],['P','u']]).
q([['G','n','R','N'],['m','S','n','Z']],[['G','n','R','N'],['m','S','n']]).
q([['F','C','b','z'],['w','n','h','r'],['o','y','e'],['a','f','y']],[['F','C','b','z'],['w','n','h','r'],['o','y','e'],['a','f']]).
q([['N','P','i','n'],['n','n','s','a'],['c','n','S']],[['N','P','i','n'],['n','n','s'],['c','n','S']]).
q([['B','w','w'],['x','S','E']],[['B','w'],['x','S','E']]).
q([['x','E','d','Y'],['c','W','M','K']],[['x','E','d','Y'],['c','W','M']]).
