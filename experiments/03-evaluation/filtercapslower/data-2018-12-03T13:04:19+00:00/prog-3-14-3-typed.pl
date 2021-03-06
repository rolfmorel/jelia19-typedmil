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

my_min_list4(A,B):-min_list(A,B).
my_element5(A,B):-member(B,A).
my_odd6(A):-1 is A mod 2.
my_msort7(A,B):-msort(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_head9([H|_],H).
my_set10(A):-list_to_set(A,A).
my_toupper11(A,B):-upcase_atom(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_tail13([_|TL],TL).
my_reverse14(A,B):-reverse(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_max_list16(A,B):-max_list(A,B).
my_flatten17(A,B):-flatten(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_min_list4,[list(int),int]).
prim(my_element5,[list(T),T]).
prim(my_odd6,[int]).
prim(my_msort7,[list(int),list(int)]).
prim(my_double8,[int,int]).
prim(my_head9,[list(T),T]).
prim(my_set10,[list(_)]).
prim(my_toupper11,[char,char]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_tail13,[list(T),list(T)]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_pred15,[int,int]).
prim(my_max_list16,[list(int),int]).
prim(my_flatten17,[list(list(T)),list(T)]).
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
p([j,'G','J',j,'L',a,'C',t,h],[g,j,l,c]).
p([g,m,'M',j,m,b],[m]).
p([y,w,v,'N',l],[n]).
p([c,k,v,'X'],[x]).
p(['G','P',c,t,'Y','B'],[g,p,y,b]).
q(['H',g,'Q',y,'T'],[h,t,w,q]).
q(['L',y,l,'H',c,'J'],['O',h,l,j]).
q(['J','M','H','N'],[h,b,n,m,j]).
q(['S',h,o,'J',y,s,d,j],[e,j,s]).
q(['V','K','N',z,n,y,m,'H'],[k,v,h,n,'P']).
