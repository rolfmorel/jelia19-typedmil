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

my_succ4(A,B):-succ(A,B),B =< 10.
my_head5([H|_],H).
my_odd6(A):-1 is A mod 2.
my_len7(A,B):-length(A,B).
my_msort8(A,B):-msort(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_toupper10(A,B):-upcase_atom(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_tail12([_|TL],TL).
my_last13(A,B):-last(A,B).
my_min_list14(A,B):-min_list(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_max_list16(A,B):-max_list(A,B).
my_set17(A):-list_to_set(A,A).
my_element18(A,B):-member(B,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_succ4/2).
prim(my_head5/2).
prim(my_odd6/1).
prim(my_len7/2).
prim(my_msort8/2).
prim(my_pred9/2).
prim(my_toupper10/2).
prim(my_lowercase11/1).
prim(my_tail12/2).
prim(my_last13/2).
prim(my_min_list14/2).
prim(my_double15/2).
prim(my_max_list16/2).
prim(my_set17/1).
prim(my_element18/2).
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
p(['T',j,'E','I',q,w,w],[t,e,i]).
p([a,'N',j,'Y',r,'U'],[n,y,u]).
p([c,'M',i,q,'Y'],[m,y]).
p([h,h,'S',y,'W',x],[s,w]).
p([h,h,'S','M','A',b,'Y','I','B'],[s,m,a,y,i,b]).
q([r,k,'I',o,j,'Z',h],[z,z,i]).
q(['R','D',u,'B'],['W',b,d,r]).
q(['Z','Y','X',r,l,'P','B','Y',h],[b,z,p,x,y,'T',y]).
q([m,g,'B',i,'M','I','N'],[m,h,i,n,b]).
q([u,'J','M',v,'T'],[j,t,m,'T']).
