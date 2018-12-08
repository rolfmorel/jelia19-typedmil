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

my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_reverse6(A,B):-reverse(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_max_list11(A,B):-max_list(A,B).
my_flatten12(A,B):-flatten(A,B).
my_element13(A,B):-member(B,A).
my_msort14(A,B):-msort(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_succ16(A,B):-succ(A,B),B =< 10.
my_set17(A):-list_to_set(A,A).
my_odd18(A):-1 is A mod 2.
my_last19(A,B):-last(A,B).
my_pred20(A,B):-succ(B,A),A > 0.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_head4/2).
prim(my_len5/2).
prim(my_reverse6/2).
prim(my_min_list7/2).
prim(my_sumlist8/2).
prim(my_list_to_set9/2).
prim(my_lowercase10/1).
prim(my_max_list11/2).
prim(my_flatten12/2).
prim(my_element13/2).
prim(my_msort14/2).
prim(my_toupper15/2).
prim(my_succ16/2).
prim(my_set17/1).
prim(my_odd18/1).
prim(my_last19/2).
prim(my_pred20/2).
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
p([j,f,'L','Q','A',g,b,'T'],[l,q,a,t]).
p([p,q,'J',j,'R','N','Q','N','K'],[j,r,n,q,n,k]).
p(['I',p,x,'I','D','R',q,'O'],[i,i,d,r,o]).
p([a,'R','L','B',i,g],[r,l,b]).
p([m,'C',o,'G','T','K',o,r,'A'],[c,g,t,k,a]).
q(['P',z,'S','T',p,'M'],[n,p,m,s,t]).
q(['O',v,'M',u,'W',e,'Y',t],[o,m,y,'O',w]).
q([i,'X','J',h],['H',j,x]).
q(['B',e,'H',f,s,'X','O','X','K'],[b,x,k,p,o,x,h]).
q(['Z','N','W','C','B',b,'M'],[m,c,z,w,b,t,n]).
