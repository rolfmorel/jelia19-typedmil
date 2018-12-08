:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_reverse4(A,B):-reverse(A,B).
my_last5(A,B):-last(A,B).
my_set6(A):-list_to_set(A,A).
my_min_list7(A,B):-min_list(A,B).
my_head8([H|_],H).
my_list_to_set9(A,B):-list_to_set(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_element11(A,B):-member(B,A).
my_odd12(A):-1 is A mod 2.
my_max_list13(A,B):-max_list(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_lowercase15(A):-downcase_atom(A,A).
my_flatten16(A,B):-flatten(A,B).
my_len17(A,B):-length(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_succ19(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_reverse4/2).
prim(my_last5/2).
prim(my_set6/1).
prim(my_min_list7/2).
prim(my_head8/2).
prim(my_list_to_set9/2).
prim(my_double10/2).
prim(my_element11/2).
prim(my_odd12/1).
prim(my_max_list13/2).
prim(my_toupper14/2).
prim(my_lowercase15/1).
prim(my_flatten16/2).
prim(my_len17/2).
prim(my_sumlist18/2).
prim(my_succ19/2).
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
p(['S',b,m,'E','C','H',f,c,n],[s,e,c,h]).
p([j,e,v,'I','Z',z,f],[i,z]).
p([i,x,c,w,l],[]).
p(['J',z,v,'X','Q',z,'Q','H'],[j,x,q,q,h]).
p(['V','S','F','H','L',x,n],[v,s,f,h,l]).
q([b,b,c,m,j,'R','C'],[r,c,f]).
q(['F','Z','V','B'],[v,'X',z,b,f]).
q([c,k,'R',f,'G','Q','W'],['N',q,r,w,g]).
q([v,'B','O',t,y,'N','I',k,'F'],[f,o,n,o,i,b]).
q(['A','X',b,x,q,'A',g,'U'],[a,p,x,a,u]).
