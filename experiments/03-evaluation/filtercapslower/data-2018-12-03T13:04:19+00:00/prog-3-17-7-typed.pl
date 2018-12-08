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

my_tail4([_|TL],TL).
my_len5(A,B):-length(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_head7([H|_],H).
my_last8(A,B):-last(A,B).
my_max_list9(A,B):-max_list(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_toupper11(A,B):-upcase_atom(A,B).
my_set12(A):-list_to_set(A,A).
my_flatten13(A,B):-flatten(A,B).
my_msort14(A,B):-msort(A,B).
my_min_list15(A,B):-min_list(A,B).
my_odd16(A):-1 is A mod 2.
my_pred17(A,B):-succ(B,A),A > 0.
my_reverse18(A,B):-reverse(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
my_sumlist20(A,B):-sumlist(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_tail4,[list(T),list(T)]).
prim(my_len5,[list(_),int]).
prim(my_lowercase6,[char]).
prim(my_head7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_succ10,[int,int]).
prim(my_toupper11,[char,char]).
prim(my_set12,[list(_)]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_msort14,[list(int),list(int)]).
prim(my_min_list15,[list(int),int]).
prim(my_odd16,[int]).
prim(my_pred17,[int,int]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_list_to_set19,[list(T),list(T)]).
prim(my_sumlist20,[list(int),int]).
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
p(['L','U',p,'F','N','N',y,'O',h],[l,u,f,n,n,o]).
p([l,d,'C','K','G'],[c,k,g]).
p(['R','N','M','H','Z'],[r,n,m,h,z]).
p(['R','J',m,b,w,'W','M',m,'F'],[r,j,w,m,f]).
p([l,h,o,'T','C','U','I','Y','V'],[t,c,u,i,y,v]).
q(['O',n,'G','M','O',d,'M',y,j],[m,v,g,o,o,m]).
q([q,'J','K',l,g],[d,j,k]).
q(['G',w,h,d,'C',g,y],[a,c,g]).
q([y,'P','F',v,'G'],[f,s,g,p]).
q([q,c,'H','L'],[h,l,'D']).
