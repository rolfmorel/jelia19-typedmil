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

my_last4(A,B):-last(A,B).
my_len5(A,B):-length(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_odd7(A):-1 is A mod 2.
my_msort8(A,B):-msort(A,B).
my_even9(A):-0 is A mod 2.
my_element10(A,B):-member(B,A).
my_head11([H|_],H).
my_set12(A):-list_to_set(A,A).
my_min_list13(A,B):-min_list(A,B).
my_max_list14(A,B):-max_list(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_lowercase16(A):-downcase_atom(A,A).
my_sumlist17(A,B):-sumlist(A,B).
my_succ18(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_last4/2).
prim(my_len5/2).
prim(my_double6/2).
prim(my_odd7/1).
prim(my_msort8/2).
prim(my_even9/1).
prim(my_element10/2).
prim(my_head11/2).
prim(my_set12/1).
prim(my_min_list13/2).
prim(my_max_list14/2).
prim(my_pred15/2).
prim(my_lowercase16/1).
prim(my_sumlist17/2).
prim(my_succ18/2).
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
p([s,'H',t,h,'I','W',r,w,'L'],[h,i,w,l]).
p(['G','S','B',k,c],[g,s,b]).
p(['K','Y','U','H',i,'A','I'],[k,y,u,h,a,i]).
p(['A','P','S',r,'B',e,g,'Z'],[a,p,s,b,z]).
p([n,'Q',v,t,v,t,'A'],[q,a]).
q(['B','M','N','D',r],[m,u,b,d,n]).
q(['Q',e,'T','I','K',c],[i,t,k,'Z',q]).
q([o,c,z,d],[p]).
q(['I',c,'D','E','O',n,z,'F'],[i,o,j,f,d,e]).
q(['Z',f,m,b],[z,'Z']).
