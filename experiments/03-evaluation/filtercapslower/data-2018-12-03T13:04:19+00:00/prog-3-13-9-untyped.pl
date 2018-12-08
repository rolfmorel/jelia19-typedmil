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

my_element4(A,B):-member(B,A).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_even8(A):-0 is A mod 2.
my_lowercase9(A):-downcase_atom(A,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_max_list11(A,B):-max_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_flatten13(A,B):-flatten(A,B).
my_msort14(A,B):-msort(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_list_to_set16(A,B):-list_to_set(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_element4/2).
prim(my_tail5/2).
prim(my_len6/2).
prim(my_pred7/2).
prim(my_even8/1).
prim(my_lowercase9/1).
prim(my_toupper10/2).
prim(my_max_list11/2).
prim(my_reverse12/2).
prim(my_flatten13/2).
prim(my_msort14/2).
prim(my_sumlist15/2).
prim(my_list_to_set16/2).
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
p([h,'V',h,b,k,f,r],[v]).
p(['S','E',t,'C',k,'O',e,'M','L'],[s,e,c,o,m,l]).
p([h,w,'B','H',v,c,'F',n],[b,h,f]).
p(['V','A','C',e,c,'B','T','Q'],[v,a,c,b,t,q]).
p(['K','P','Y','S'],[k,p,y,s]).
q([l,o,'Y','B','D','F','H','H','E'],[h,b,'O',f,d,y,h,e]).
q(['V','S',f,'X','H','Q'],[s,v,h,x,q,l]).
q([e,v,'Z',j,'D'],[z,d,'X']).
q(['V',e,'R',n,'Y','F','J',s,'K'],[y,k,r,j,v,'Y',f]).
q(['V','H','L',o],[h,v,l,'N']).
