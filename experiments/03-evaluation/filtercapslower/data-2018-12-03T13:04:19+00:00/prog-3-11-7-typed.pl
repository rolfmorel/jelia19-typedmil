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

my_lowercase4(A):-downcase_atom(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_last6(A,B):-last(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_set9(A):-list_to_set(A,A).
my_succ10(A,B):-succ(A,B),B =< 10.
my_toupper11(A,B):-upcase_atom(A,B).
my_reverse12(A,B):-reverse(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_max_list14(A,B):-max_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_last6,[list(T),T]).
prim(my_sumlist7,[list(int),int]).
prim(my_len8,[list(_),int]).
prim(my_set9,[list(_)]).
prim(my_succ10,[int,int]).
prim(my_toupper11,[char,char]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_pred13,[int,int]).
prim(my_max_list14,[list(int),int]).
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
p([f,'E',u,s,a],[e]).
p(['Z','G',o,'T',z,'R','D',s,k],[z,g,t,r,d]).
p([d,'S','K',r,r,a],[s,k]).
p([k,'I','L',x,'Q'],[i,l,q]).
p([r,f,'K',k,'H',j,x],[k,h]).
q(['J',d,'L','E','Q'],[e,j,l,w,q]).
q(['X','X','F','C','Q',i,'A','S'],[c,m,s,q,f,a,x,x]).
q([m,'R',m,y,'I','Q','O',t],[o,o,q,r,i]).
q(['B','E','Y','X','A',l,v,g,s],[e,a,'J',y,x,b]).
q(['M','P',z,'O',h,w,j],['B',m,o,p]).
