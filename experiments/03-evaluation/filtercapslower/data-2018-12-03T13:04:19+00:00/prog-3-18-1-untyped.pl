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

my_tail4([_|TL],TL).
my_lowercase5(A):-downcase_atom(A,A).
my_len6(A,B):-length(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_flatten8(A,B):-flatten(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_reverse10(A,B):-reverse(A,B).
my_max_list11(A,B):-max_list(A,B).
my_even12(A):-0 is A mod 2.
my_pred13(A,B):-succ(B,A),A > 0.
my_head14([H|_],H).
my_succ15(A,B):-succ(A,B),B =< 10.
my_set16(A):-list_to_set(A,A).
my_element17(A,B):-member(B,A).
my_msort18(A,B):-msort(A,B).
my_toupper19(A,B):-upcase_atom(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_min_list21(A,B):-min_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_tail4/2).
prim(my_lowercase5/1).
prim(my_len6/2).
prim(my_list_to_set7/2).
prim(my_flatten8/2).
prim(my_double9/2).
prim(my_reverse10/2).
prim(my_max_list11/2).
prim(my_even12/1).
prim(my_pred13/2).
prim(my_head14/2).
prim(my_succ15/2).
prim(my_set16/1).
prim(my_element17/2).
prim(my_msort18/2).
prim(my_toupper19/2).
prim(my_sumlist20/2).
prim(my_min_list21/2).
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
p(['A','H',q,x,c,'K',z,'F','S'],[a,h,k,f,s]).
p([v,'X','I',i,'V','A','U',z],[x,i,v,a,u]).
p([m,x,b,z,x,e,t,'U',s],[u]).
p(['S','C','W','V',g,s,x,p],[s,c,w,v]).
p(['M',a,x,a,'S','Y','G',b,'D'],[m,s,y,g,d]).
q([r,'M','A',r,j,'I'],[i,m,q,a]).
q(['V',m,'Q','F','X'],[f,v,u,x,q]).
q(['F','W',g,w,'E',n,'K',q,e],[k,e,w,f,h]).
q(['H','P','L','Y','L','Y'],[h,l,'L',p,y,y,l]).
q(['O',t,z,'C','Q',e,'Y','T'],[y,u,c,o,t,q]).
