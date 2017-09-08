:- module(cookies, [main/0], []).

:- use_module(library(pillow/html)).
:- use_module(library(http/http_forms)).
:- use_module(library(http/cgi)).
:- include(library(pillow/ops)).

main :-
        get_cookies(CKs),
        get_form_value(CKs, cookie_filling, Filling),
        get_form_value(CKs, number_cookies, Number_or_void),
        form_default(Number_or_void, one, Number),
	%
	cgi_read_request(Request),
	http_parse_form(Request, Info),
	%
        ( member(filling=New_Filling, Info) ->
            set_cookie(cookie_filling, New_Filling),
            output_page(New_Filling, Number, Response)
        ; member(number=New_Number, Info) ->
            set_cookie(number_cookies, New_Number),
            output_page(Filling, New_Number, Response)
        ;
            output_page(Filling, Number, Response)
        ),
 	cgi_write_response(Response).

output_page(Filling, Number, Response) :-
        HTML = [
          start,
          env(p,[],["You have ",Number," ",Filling," cookie(s)."]),
          env(form,[],["To change the filling of the cookie write here:",
                       input$[type=text,name=filling],
                       input$[type=submit,value="change"]]),
          env(form,[],["To change the number of the cookies write here:",
                       input$[type=text,name=number],
                       input$[type=submit,value="change"]]),
          end],
	html2terms(Str, HTML),
 	Response = html_string(Str).
