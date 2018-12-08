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
my_max_list5(A,B):-max_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_msort8(A,B):-msort(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_min_list10(A,B):-min_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_len4/2).
prim(my_max_list5/2).
prim(my_sumlist6/2).
prim(my_toupper7/2).
prim(my_msort8/2).
prim(my_pred9/2).
prim(my_min_list10/2).
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
p(['L','V',y,'B','N'],[l,v,b,n]).
p([e,'D','Y','R'],[d,y,r]).
p([u,y,o,'T'],[t]).
p(['V','X',a,'B',r,i,'K',n],[v,x,b,k]).
p([a,'E','W','P','K',e,n,m],[e,w,p,k]).
q(['B',c,'F',h,d],[b,f,b]).
q([j,c,b,'W','A','Q',d,'L',g],[a,'R',l,w,q]).
q(['K','E','M','N',w,s,'W',g,y],[m,e,w,'O',k,n]).
q([r,'L','Q','B',j,b,i,j],[l,'E',b,q]).
q([n,u,'Z','N','L'],[l,n,l,z]).
