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

my_lowercase4(A):-downcase_atom(A,A).
my_succ5(A,B):-succ(A,B),B =< 10.
my_double6(N,M):-M is 2*N,M =< 10.
my_sumlist7(A,B):-sumlist(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_max_list9(A,B):-max_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_lowercase4/1).
prim(my_succ5/2).
prim(my_double6/2).
prim(my_sumlist7/2).
prim(my_toupper8/2).
prim(my_max_list9/2).
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
p(['O','C',y,w,x,'K',h,'B'],[o,c,k,b]).
p(['X','B',h,o,c],[x,b]).
p([s,p,'T',f,'O',n,z,h,'W'],[t,o,w]).
p([b,m,'S',h,'U'],[s,u]).
p(['E','J',w,'Y','N',k,s,'Y','R'],[e,j,y,n,y,r]).
q([b,'E',k,'H','J',o,'U',l,g],[h,u,h,e,j]).
q([t,f,r,u,b,o,w],[f]).
q(['C',q,'B',y],[u,b,c]).
q([h,t,c,b,'N','A'],['K',n,a]).
q(['M','D','D','D',b,t],[m,m,d,d,d]).
