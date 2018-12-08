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
my_reverse5(A,B):-reverse(A,B).
my_element6(A,B):-member(B,A).
my_tail7([_|TL],TL).
my_msort8(A,B):-msort(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_flatten10(A,B):-flatten(A,B).
my_head11([H|_],H).
my_pred12(A,B):-succ(B,A),A > 0.
my_odd13(A):-1 is A mod 2.
my_sumlist14(A,B):-sumlist(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_last16(A,B):-last(A,B).
my_len17(A,B):-length(A,B).
my_even18(A):-0 is A mod 2.
my_succ19(A,B):-succ(A,B),B =< 10.
my_min_list20(A,B):-min_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_element6,[list(T),T]).
prim(my_tail7,[list(T),list(T)]).
prim(my_msort8,[list(int),list(int)]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_head11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_odd13,[int]).
prim(my_sumlist14,[list(int),int]).
prim(my_double15,[int,int]).
prim(my_last16,[list(T),T]).
prim(my_len17,[list(_),int]).
prim(my_even18,[int]).
prim(my_succ19,[int,int]).
prim(my_min_list20,[list(int),int]).
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
p(['N',e,b,v,'Z','P','J',t],[n,z,p,j]).
p(['R','F',d,b],[r,f]).
p(['O',d,j,s,'F'],[o,f]).
p([u,g,'W',o,m,'I',p,r,e],[w,i]).
p(['K',l,x,m,e,l,'M','W',d],[k,m,w]).
q([y,'J',x,'V',r,'K'],[j,'P',k,v]).
q([f,'Z','A',b,c],['I',z,a]).
q(['Z','H',d,f,'R',u],['U',z,h,r]).
q([u,j,x,e,'S'],['I',s]).
q([m,'M',v,'Z',j,'S','P','V',i],[v,s,z,t,m,p]).
