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
my_double5(N,M):-M is 2*N,M =< 10.
my_max_list6(A,B):-max_list(A,B).
my_tail7([_|TL],TL).
my_element8(A,B):-member(B,A).
my_odd9(A):-1 is A mod 2.
my_min_list10(A,B):-min_list(A,B).
my_set11(A):-list_to_set(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_lowercase4/1).
prim(my_double5/2).
prim(my_max_list6/2).
prim(my_tail7/2).
prim(my_element8/2).
prim(my_odd9/1).
prim(my_min_list10/2).
prim(my_set11/1).
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
p(['D',i,k,'J','Q'],[d,j,q]).
p([l,a,'P',v,'K',p,o],[p,k]).
p([k,n,b,g,x,'Q','X','I','V'],[q,x,i,v]).
p(['T',v,k,'T','G',v,i],[t,t,g]).
p(['U',i,i,i,h,p,'T',j,'N'],[u,t,n]).
q([k,'G','T',n,'B'],[g,b,t,w]).
q([k,'G',k,'D',n,f,u,z],[g,d,'G']).
q([t,h,'M',y,c,w,'C','K',d],[m,k,u,c]).
q(['D',i,'K','I'],[i,d,z,k]).
q(['H',d,'H',s,'I',z,s,u],[h,h,i,'P']).
