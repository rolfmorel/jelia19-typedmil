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

my_odd4(A):-1 is A mod 2.
my_head5([H|_],H).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_last8(A,B):-last(A,B).
my_element9(A,B):-member(B,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_succ11(A,B):-succ(A,B),B =< 10.
my_reverse12(A,B):-reverse(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_msort14(A,B):-msort(A,B).
my_list_to_set15(A,B):-list_to_set(A,B).
my_set16(A):-list_to_set(A,A).
my_double17(N,M):-M is 2*N,M =< 10.
my_lowercase18(A):-downcase_atom(A,A).
my_min_list19(A,B):-min_list(A,B).
my_len20(A,B):-length(A,B).
my_flatten21(A,B):-flatten(A,B).
my_even22(A):-0 is A mod 2.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_odd4/1).
prim(my_head5/2).
prim(my_sumlist6/2).
prim(my_tail7/2).
prim(my_last8/2).
prim(my_element9/2).
prim(my_toupper10/2).
prim(my_succ11/2).
prim(my_reverse12/2).
prim(my_pred13/2).
prim(my_msort14/2).
prim(my_list_to_set15/2).
prim(my_set16/1).
prim(my_double17/2).
prim(my_lowercase18/1).
prim(my_min_list19/2).
prim(my_len20/2).
prim(my_flatten21/2).
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
p(['H','R','P',x,d,r,u,i,q],[h,r,p]).
p(['L','V',z,'I','P','G'],[l,v,i,p,g]).
p(['B',f,'A','N',c,'V'],[b,a,n,v]).
p([v,x,'V','G',u,f,a],[v,g]).
p([a,'W','P',h,'Q'],[w,p,q]).
q(['B','J','K','M',x,'A','B','K'],['L',a,k,b,k,b,j,m]).
q(['U',v,q,u,'X','M',u],[m,x,'C',u]).
q(['P','P',x,'N','W',d],[n,w,'U',p,p]).
q([u,y,'O',v,'X',l,'I',g,u],[x,o,a,i]).
q(['P','A','T','H',w,s,b,q],[a,p,h,t,u]).
