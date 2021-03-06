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
my_succ5(A,B):-succ(A,B),B =< 10.
my_element6(A,B):-member(B,A).
my_set7(A):-list_to_set(A,A).
my_min_list8(A,B):-min_list(A,B).
my_even9(A):-0 is A mod 2.
my_flatten10(A,B):-flatten(A,B).
my_tail11([_|TL],TL).
my_msort12(A,B):-msort(A,B).
my_last13(A,B):-last(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_odd16(A):-1 is A mod 2.
my_head17([H|_],H).
my_list_to_set18(A,B):-list_to_set(A,B).
my_double19(N,M):-M is 2*N,M =< 10.
my_reverse20(A,B):-reverse(A,B).
my_max_list21(A,B):-max_list(A,B).
my_len22(A,B):-length(A,B).
my_pred23(A,B):-succ(B,A),A > 0.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_succ5,[int,int]).
prim(my_element6,[list(T),T]).
prim(my_set7,[list(_)]).
prim(my_min_list8,[list(int),int]).
prim(my_even9,[int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_tail11,[list(T),list(T)]).
prim(my_msort12,[list(int),list(int)]).
prim(my_last13,[list(T),T]).
prim(my_toupper14,[char,char]).
prim(my_sumlist15,[list(int),int]).
prim(my_odd16,[int]).
prim(my_head17,[list(T),T]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_double19,[int,int]).
prim(my_reverse20,[list(T),list(T)]).
prim(my_max_list21,[list(int),int]).
prim(my_len22,[list(_),int]).
prim(my_pred23,[int,int]).
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
p(['O',e,p,'Y','G',l,'P',d],[o,y,g,p]).
p([i,f,'V','P','D',b],[v,p,d]).
p(['W','V','J','P',l,r,c,d],[w,v,j,p]).
p(['R','M','S',u,z],[r,m,s]).
p([l,'J',u,'N',x,'K'],[j,n,k]).
q(['B',h,n,p,t,s,a,'Z'],[u,b,z]).
q([w,'D',c,'B','X','C',r,l,'V'],[v,c,b,x,d,s]).
q([m,n,i,'I','D',x,d,'W'],[w,i,'J',d]).
q([c,'R','S','W','I'],[r,i,s,'R',w]).
q([w,'M',z,'P',u,t],[p,m,s]).
