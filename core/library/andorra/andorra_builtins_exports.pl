% this builtins have to be implemented in andorra_builtins.pl
builtin_export(arithmetic,is,2,0) .
builtin_export(arithmetic,<,2,0) .
builtin_export(arithmetic,=<,2,0) .
builtin_export(arithmetic,>,2,0) .
builtin_export(arithmetic,>=,2,0) .
builtin_export(arithmetic,=:=,2,0) .
builtin_export(arithmetic,=\=,2,0) .
builtin_export(arithmetic,arithexpression,1,0) .
builtin_export(atomic_basic,name,2,0) .
builtin_export(atomic_basic,atom_codes,2,0) .
builtin_export(atomic_basic,number_codes,2,0) .
builtin_export(atomic_basic,number_codes,3,0) .
builtin_export(atomic_basic,atom_length,2,0) .
builtin_export(atomic_basic,atom_concat,3,0) .
builtin_export(atomic_basic,sub_atom,4,0) .
builtin_export(basic_props,int,1,0) .
builtin_export(basic_props,nnegint,1,0) .
builtin_export(basic_props,flt,1,0) .     
builtin_export(basic_props,num,1,0) .
builtin_export(basic_props,atm,1,0) .
builtin_export(basic_props,struct,1,0) .
builtin_export(basic_props,gnd,1,0) .
builtin_export(basic_props,constant,1,0) .
builtin_export(basic_props,callable,1,0) .
builtin_export(term_typing,atom,1,0) . 
builtin_export(term_typing,integer,1,0) . 
builtin_export(term_typing,float,1,0) . 
builtin_export(term_typing,number,1,0) . 
builtin_export(term_typing,atomic,1,0) .  
builtin_export(term_basic,arg,3,0) .
builtin_export(term_basic,functor,3,0) .
builtin_export(term_basic,=..,2,0) .


% these builtins need a valid_ entry in andorra.pl
builtin_constrain(arithmetic,<,2,0) .
builtin_constrain(arithmetic,=<,2,0) .
builtin_constrain(arithmetic,>,2,0) .
builtin_constrain(arithmetic,>=,2,0) .
builtin_constrain(arithmetic,=:=,2,0) .
builtin_constrain(arithmetic,=\=,2,0) .
builtin_constrain(basic_props,int,1,0) .
builtin_constrain(basic_props,nnegint,1,0) .
builtin_constrain(basic_props,flt,1,0) .         
builtin_constrain(basic_props,num,1,0) .
builtin_constrain(basic_props,atm,1,0) .
builtin_constrain(basic_props,struct,1,0) .
builtin_constrain(basic_props,constant,1,0) .
builtin_constrain(basic_props,callable,1,0) .
builtin_constrain(term_typing,atom,1,0) . 
builtin_constrain(term_typing,integer,1,0) . 
builtin_constrain(term_typing,float,1,0) . 
builtin_constrain(term_typing,number,1,0) . 
builtin_constrain(term_typing,atomic,1,0) .  


% nothing have to be done with these builtins
determinate_builtin(basiccontrol,true,0,0) .
determinate_builtin(basiccontrol,fail,0,0) . 
determinate_builtin(term_basic,=,2,0).
determinate_builtin(basic_props,term,1,0) .
determinate_builtin(data_facts,close_predicate,1,close_predicate(fact)) .
determinate_builtin(data_facts,open_predicate,1,open_predicate(fact)) .
determinate_builtin(term_basic,'C',3,0) .
%prueba
determinate_builtin(basiccontrol,!,0,0) .

% a wakeup is needed before these builtins
sensitive_builtin(attributes,attach_attribute,2,0).
sensitive_builtin(attributes,get_attribute,2,0).
sensitive_builtin(attributes,update_attribute,2,0).
sensitive_builtin(attributes,detach_attribute,1,0).
sensitive_builtin(write,write,1,0).
sensitive_builtin(write,write,2,0).
%prueba
%sensitive_builtin(basiccontrol,!,0,0) .
sensitive_builtin(term_typing,var,1,0) .
sensitive_builtin(term_typing,nonvar,1,0) .
sensitive_builtin(term_typing,ground,1,0) .
sensitive_builtin(term_typing,type,2,0) . 
%prueba
%sensitive_builtin(term_compare,==,2,0). 
%prueba
%sensitive_builtin(term_compare,\==,2,0).
sensitive_builtin(term_compare,@<,2,0). 
sensitive_builtin(term_compare,@=<,2,0). 
sensitive_builtin(term_compare,@>,2,0). 
sensitive_builtin(term_compare,@>=,2,0). 
sensitive_builtin(term_compare,compare,3,0).
sensitive_builtin(basiccontrol,repeat,0,0). 
sensitive_builtin(data_facts,asserta_fact,1,asserta_fact(fact)) .
sensitive_builtin(data_facts,asserta_fact,2,asserta_fact(fact,?)) .
sensitive_builtin(data_facts,assertz_fact,1,assertz_fact(fact)) .
sensitive_builtin(data_facts,assertz_fact,2,assertz_fact(fact,?)) .
sensitive_builtin(data_facts,retract_fact,1,retract_fact(fact)) .
sensitive_builtin(data_facts,retractall_fact,1,retractall_fact(fact)) .
sensitive_builtin(data_facts,retract_fact_nb,1,retract_fact_nb(fact)) .
sensitive_builtin(data_facts,set_fact,1,set_fact(fact)) .
sensitive_builtin(data_facts,erase,1,0) .
sensitive_builtin(io_aux,message,2,0) .
sensitive_builtin(io_aux,message_lns,4,0) .
sensitive_builtin(io_aux,error,1,0) .
sensitive_builtin(io_aux,warning,1,0) .
sensitive_builtin(io_aux,note,1,0) .
sensitive_builtin(io_aux,message,1,0) .
sensitive_builtin(io_aux,debug,1,0) .
sensitive_builtin(io_aux,inform_user,1,0) .
sensitive_builtin(io_aux,display_string,1,0) .
sensitive_builtin(io_aux,display_list,1,0) .
sensitive_builtin(io_aux,display_term,1,0) .
sensitive_builtin(io_basic,get_code,2,0) .
sensitive_builtin(io_basic,get_code,1,0) .
sensitive_builtin(io_basic,get1_code,2,0) .
sensitive_builtin(io_basic,get1_code,1,0) .
sensitive_builtin(io_basic,peek_code,2,0) .
sensitive_builtin(io_basic,peek_code,1,0) .
sensitive_builtin(io_basic,skip_code,2,0) .
sensitive_builtin(io_basic,skip_code,1,0) .
sensitive_builtin(io_basic,put_code,2,0) .
sensitive_builtin(io_basic,put_code,1,0) .
sensitive_builtin(io_basic,nl,1,0) .
sensitive_builtin(io_basic,nl,0,0) .
sensitive_builtin(io_basic,tab,2,0) .
sensitive_builtin(io_basic,tab,1,0) .
sensitive_builtin(io_basic,getct,2,0) .
sensitive_builtin(io_basic,getct1,2,0) .
sensitive_builtin(io_basic,display,2,0) .
sensitive_builtin(io_basic,display,1,0) .
sensitive_builtin(io_basic,displayq,2,0) .
sensitive_builtin(io_basic,displayq,1,0) .
sensitive_builtin(term_basic,copy_term,2,0) .
