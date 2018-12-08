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

my_toupper4(A,B):-upcase_atom(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_even7(A):-0 is A mod 2.
my_pred8(A,B):-succ(B,A),A > 0.
my_msort9(A,B):-msort(A,B).
my_len10(A,B):-length(A,B).
my_tail11([_|TL],TL).
my_reverse12(A,B):-reverse(A,B).
my_head13([H|_],H).
my_succ14(A,B):-succ(A,B),B =< 10.
my_element15(A,B):-member(B,A).
my_odd16(A):-1 is A mod 2.
my_max_list17(A,B):-max_list(A,B).
my_double18(N,M):-M is 2*N,M =< 10.
my_lowercase19(A):-downcase_atom(A,A).
my_flatten20(A,B):-flatten(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_toupper4/2).
prim(my_sumlist5/2).
prim(my_list_to_set6/2).
prim(my_even7/1).
prim(my_pred8/2).
prim(my_msort9/2).
prim(my_len10/2).
prim(my_tail11/2).
prim(my_reverse12/2).
prim(my_head13/2).
prim(my_succ14/2).
prim(my_element15/2).
prim(my_odd16/1).
prim(my_max_list17/2).
prim(my_double18/2).
prim(my_lowercase19/1).
prim(my_flatten20/2).
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
p(['F',c,'J','E',m,'Q','K','P',u],[f,j,e,q,k,p]).
p(['D',x,'D',n,u],[d,d]).
p([q,o,l,'B',x,i,k,'Q'],[b,q]).
p([v,'W',j,'L',u,'N','C','H'],[w,l,n,c,h]).
p([m,h,'T',n,h,'M',q,'X',q],[t,m,x]).
q([e,'J','U','W',o,'O',c,s],[j,f,o,w,u]).
q([i,g,'V',h,j,a,l,m,k],[v,'J']).
q([w,'U',e,'C',v,'L'],[u,l,'T',c]).
q(['N',y,d,'H','P',d,i,u],[p,v,n,h]).
q([y,'A',u,'W'],[w,a,n]).
