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

my_len4(A,B):-length(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_flatten6(A,B):-flatten(A,B).
my_set7(A):-list_to_set(A,A).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_succ11(A,B):-succ(A,B),B =< 10.
my_msort12(A,B):-msort(A,B).
my_max_list13(A,B):-max_list(A,B).
my_min_list14(A,B):-min_list(A,B).
my_list_to_set15(A,B):-list_to_set(A,B).
my_even16(A):-0 is A mod 2.
my_element17(A,B):-member(B,A).
my_lowercase18(A):-downcase_atom(A,A).
my_last19(A,B):-last(A,B).
my_toupper20(A,B):-upcase_atom(A,B).
my_odd21(A):-1 is A mod 2.
my_pred22(A,B):-succ(B,A),A > 0.
my_double23(N,M):-M is 2*N,M =< 10.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_len4/2).
prim(my_sumlist5/2).
prim(my_flatten6/2).
prim(my_set7/1).
prim(my_reverse8/2).
prim(my_head9/2).
prim(my_tail10/2).
prim(my_succ11/2).
prim(my_msort12/2).
prim(my_max_list13/2).
prim(my_min_list14/2).
prim(my_list_to_set15/2).
prim(my_even16/1).
prim(my_element17/2).
prim(my_lowercase18/1).
prim(my_last19/2).
prim(my_toupper20/2).
prim(my_odd21/1).
prim(my_pred22/2).
prim(my_double23/2).
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
p([k,'Z','N',w,l,'C'],[z,n,c]).
p(['A',c,f,p,'R','I',k,q,n],[a,r,i]).
p(['P','A','N','J','U','W',p],[p,a,n,j,u,w]).
p([p,'N','D','C','G','P'],[n,d,c,g,p]).
p([z,p,d,'F','A'],[f,a]).
q([q,i,'L','X','Y','E',f,'P'],[e,y,'Y',x,p,l]).
q(['F',g,'A',h],[a,y,f]).
q(['B',e,b,x,n,'H','T'],[h,b,c,t]).
q(['I','H','B','X',l,z],[x,i,b,h,o]).
q(['T','M',a,a,k,u],[m,t,o]).
