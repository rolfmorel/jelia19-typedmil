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

my_msort4(A,B):-msort(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_set6(A):-list_to_set(A,A).
my_head7([H|_],H).
my_toupper8(A,B):-upcase_atom(A,B).
my_min_list9(A,B):-min_list(A,B).
my_len10(A,B):-length(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_even12(A):-0 is A mod 2.
my_tail13([_|TL],TL).
my_odd14(A):-1 is A mod 2.
my_flatten15(A,B):-flatten(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_element18(A,B):-member(B,A).
my_lowercase19(A):-downcase_atom(A,A).
my_double20(N,M):-M is 2*N,M =< 10.
my_last21(A,B):-last(A,B).
my_list_to_set22(A,B):-list_to_set(A,B).
my_sumlist23(A,B):-sumlist(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_succ5,[int,int]).
prim(my_set6,[list(_)]).
prim(my_head7,[list(T),T]).
prim(my_toupper8,[char,char]).
prim(my_min_list9,[list(int),int]).
prim(my_len10,[list(_),int]).
prim(my_pred11,[int,int]).
prim(my_even12,[int]).
prim(my_tail13,[list(T),list(T)]).
prim(my_odd14,[int]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_max_list17,[list(int),int]).
prim(my_element18,[list(T),T]).
prim(my_lowercase19,[char]).
prim(my_double20,[int,int]).
prim(my_last21,[list(T),T]).
prim(my_list_to_set22,[list(T),list(T)]).
prim(my_sumlist23,[list(int),int]).
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
p(['C',n,m,p,q,q,'L',n],[c,l]).
p([m,'B','H',o,'A',x],[b,h,a]).
p([e,'O','N','M',k,'D',e,t],[o,n,m,d]).
p(['P',d,'T',t,'Z','X','N','D',m],[p,t,z,x,n,d]).
p(['C','T',v,q,'D','X',u,'B'],[c,t,d,x,b]).
q(['W',v,'Y','U','R',z,'Q',p],[w,q,u,r,y,a]).
q(['J',a,'T',p,p],['G',j,t]).
q([m,e,o,a,'E','Y'],[e,y,'J']).
q(['G',v,'B','E','N'],[n,y,e,g,b]).
q([j,'M',x,'O','E',d,'R',k],[o,m,'E',e,r]).
