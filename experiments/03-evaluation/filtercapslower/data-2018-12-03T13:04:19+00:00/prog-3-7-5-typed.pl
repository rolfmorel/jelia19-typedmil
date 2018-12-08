:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
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
my_element5(A,B):-member(B,A).
my_sumlist6(A,B):-sumlist(A,B).
my_head7([H|_],H).
my_max_list8(A,B):-max_list(A,B).
my_odd9(A):-1 is A mod 2.
my_set10(A):-list_to_set(A,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_succ4,[int,int]).
prim(my_element5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_odd9,[int]).
prim(my_set10,[list(_)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['U','W','B',k],[u,w,b]).
p(['F',s,'D','T','K','P','D',n],[f,d,t,k,p,d]).
p(['V',g,o,'D'],[v,d]).
p([c,'Y','V',b,'A',w,'S','K'],[y,v,a,s,k]).
p([a,'I','F',x,q],[i,f]).
q([j,n,u,'L'],['K',l]).
q(['G','Q','Y',g,l,'J',t],[y,g,q,j,n]).
q(['E','Y','I','M','M','W','H',u,x],[y,i,n,w,e,h,m,m]).
q([w,m,'K','N','R',p,'O',r],[r,o,n,h,k]).
q([m,a,'E',j],['B',e]).
