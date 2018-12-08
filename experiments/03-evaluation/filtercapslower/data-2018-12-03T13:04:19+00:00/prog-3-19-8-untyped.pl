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

my_msort4(A,B):-msort(A,B).
my_len5(A,B):-length(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_even7(A):-0 is A mod 2.
my_element8(A,B):-member(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_flatten11(A,B):-flatten(A,B).
my_max_list12(A,B):-max_list(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_last14(A,B):-last(A,B).
my_reverse15(A,B):-reverse(A,B).
my_tail16([_|TL],TL).
my_head17([H|_],H).
my_succ18(A,B):-succ(A,B),B =< 10.
my_odd19(A):-1 is A mod 2.
my_min_list20(A,B):-min_list(A,B).
my_pred21(A,B):-succ(B,A),A > 0.
my_toupper22(A,B):-upcase_atom(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_msort4/2).
prim(my_len5/2).
prim(my_double6/2).
prim(my_even7/1).
prim(my_element8/2).
prim(my_sumlist9/2).
prim(my_lowercase10/1).
prim(my_flatten11/2).
prim(my_max_list12/2).
prim(my_list_to_set13/2).
prim(my_last14/2).
prim(my_reverse15/2).
prim(my_tail16/2).
prim(my_head17/2).
prim(my_succ18/2).
prim(my_odd19/1).
prim(my_min_list20/2).
prim(my_pred21/2).
prim(my_toupper22/2).
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
p(['E',o,'T',v,'D',q,'P',j],[e,t,d,p]).
p([l,n,m,'Y','D','L','L',o],[y,d,l,l]).
p(['N','M','G',w,'Y'],[n,m,g,y]).
p([o,'G',e,z,'Z'],[g,z]).
p([b,u,'M','O','Y','V',t,'O','Q'],[m,o,y,v,o,q]).
q(['W','F','L','L','J','J','P','Q',h],[j,q,p,'G',w,l,l,j,f]).
q(['Q',y,h,u,g,'E','O',y],[e,o,q,x]).
q(['C','I',a,s,z],[i,c,i]).
q([j,'V','U','X',b,'S',i,'C',q],[u,'K',s,v,x,c]).
q([h,'H','R','H',w,'R','I',r],[r,n,r,h,h,i]).
