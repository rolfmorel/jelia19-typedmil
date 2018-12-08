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
my_reverse5(A,B):-reverse(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_odd7(A):-1 is A mod 2.
my_double8(N,M):-M is 2*N,M =< 10.
my_msort9(A,B):-msort(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_len12(A,B):-length(A,B).
my_flatten13(A,B):-flatten(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_succ17(A,B):-succ(A,B),B =< 10.
my_set18(A):-list_to_set(A,A).
my_min_list19(A,B):-min_list(A,B).
my_element20(A,B):-member(B,A).
my_toupper21(A,B):-upcase_atom(A,B).
my_even22(A):-0 is A mod 2.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_last4/2).
prim(my_reverse5/2).
prim(my_pred6/2).
prim(my_odd7/1).
prim(my_double8/2).
prim(my_msort9/2).
prim(my_sumlist10/2).
prim(my_tail11/2).
prim(my_len12/2).
prim(my_flatten13/2).
prim(my_lowercase14/1).
prim(my_head15/2).
prim(my_max_list16/2).
prim(my_succ17/2).
prim(my_set18/1).
prim(my_min_list19/2).
prim(my_element20/2).
prim(my_toupper21/2).
prim(my_even22/1).
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
p([p,'D','D',y,'Q'],[d,d,q]).
p([w,'M','O',k,'T',h,l,'D',i],[m,o,t,d]).
p(['O',h,'R',q,'N','W','A','F'],[o,r,n,w,a,f]).
p(['J',v,'T',c],[j,t]).
p([m,'I',t,z,x,c,'R',k],[i,r]).
q(['M',j,'P','X','I',z,'W',b],['S',p,w,i,m,x]).
q(['Q','I',x,p],[q,i,'A']).
q([j,m,e,c,a],[r]).
q([m,l,k,'L',e,'S',c,'D'],[s,'D',l,d]).
q([x,'P',a,'V',n,b,q],[v,p,'P']).
