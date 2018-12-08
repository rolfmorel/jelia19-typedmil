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
my_max_list4(A,B):-max_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_last7(A,B):-last(A,B).
my_set8(A):-list_to_set(A,A).
my_len9(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_max_list4,[list(int),int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_succ6,[int,int]).
prim(my_last7,[list(T),T]).
prim(my_set8,[list(_)]).
prim(my_len9,[list(_),int]).
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
p([['X','W','z'],['q','K','S','h'],['B','Y','v','n'],['D','N','w','s']],[['X','W'],['q','K','S'],['B','Y','v'],['D','N','w']]).
p([['M','t','n','e'],['W','G','a'],['O','k','c'],['p','Q','u','B']],[['M','t','n'],['W','G'],['O','k'],['p','Q','u']]).
p([['q','z','W','I'],['N','r','U']],[['q','z','W'],['N','r']]).
p([['u','H','T'],['M','c','k','y'],['l','J','i'],['M','c','A']],[['u','H'],['M','c','k'],['l','J'],['M','c']]).
p([['I','H','Z'],['i','Z','w'],['m','C','N','J']],[['I','H'],['i','Z'],['m','C','N']]).
q([['q','x','Y','g'],['w','J','r'],['g','S','l','G']],[['q','x','Y','g'],['w','J','r'],['g','S','l']]).
q([['y','k','p'],['D','M','U','X']],[['y','k','p'],['D','M','U']]).
q([['j','Z','X','W'],['D','l','X']],[['j','Z','X'],['D','l','X']]).
q([['a','x','e','j'],['N','T','T','h'],['J','Y','M','x'],['D','D','N','W']],[['a','x','e','j'],['N','T','T','h'],['J','Y','M'],['D','D','N','W']]).
q([['h','D','C','y'],['J','P','w']],[['h','D','C','y'],['J','P']]).
