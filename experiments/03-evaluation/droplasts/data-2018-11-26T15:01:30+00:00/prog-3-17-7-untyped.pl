:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
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
my_tolower4(A,B):-downcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_toupper6(A,B):-upcase_atom(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_len8(A,B):-length(A,B).
my_flatten9(A,B):-flatten(A,B).
my_last10(A,B):-last(A,B).
my_head11([H|_],H).
my_odd12(A):-1 is A mod 2.
my_uppercase13(A):-upcase_atom(A,A).
my_element14(A,B):-member(B,A).
my_sumlist15(A,B):-sumlist(A,B).
my_min_list16(A,B):-min_list(A,B).
my_set17(A):-list_to_set(A,A).
my_pred18(A,B):-succ(B,A),A > 0.
my_even19(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_msort3/2).
prim(my_tolower4/2).
prim(my_lowercase5/1).
prim(my_toupper6/2).
prim(my_double7/2).
prim(my_len8/2).
prim(my_flatten9/2).
prim(my_last10/2).
prim(my_head11/2).
prim(my_odd12/1).
prim(my_uppercase13/1).
prim(my_element14/2).
prim(my_sumlist15/2).
prim(my_min_list16/2).
prim(my_set17/1).
prim(my_pred18/2).
prim(my_even19/1).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['W','J','B'],['f','D','R','G'],['R','y','H'],['G','l','t','m']],[['W','J'],['f','D','R'],['R','y'],['G','l','t']]).
p([['Q','c','j','s'],['z','b','Y','S'],['r','y','q','X']],[['Q','c','j'],['z','b','Y'],['r','y','q']]).
p([['c','g','F','R'],['P','p','b'],['B','S','Z','n']],[['c','g','F'],['P','p'],['B','S','Z']]).
p([['o','k','K'],['C','X','C']],[['o','k'],['C','X']]).
p([['K','e','W'],['N','L','g'],['G','r','H'],['k','X','P']],[['K','e'],['N','L'],['G','r'],['k','X']]).
q([['l','y','x','I'],['X','j','X','i'],['R','j','M','d']],[['l','y','x','I'],['X','j','X'],['R','j','M','d']]).
q([['j','r','K','H'],['r','s','q'],['K','Y','O'],['W','h','H','X']],[['j','r','K'],['r','s'],['K','Y','O'],['W','h','H','X']]).
q([['b','y','j'],['v','D','S'],['w','D','B','w'],['L','q','U']],[['b','y','j'],['v','D'],['w','D','B','w'],['L','q']]).
q([['x','S','Z'],['D','A','R','i']],[['x','S'],['D','A','R','i']]).
q([['R','O','b'],['Q','t','g','j'],['d','K','N','T'],['n','B','e']],[['R','O','b'],['Q','t','g','j'],['d','K','N'],['n','B','e']]).
