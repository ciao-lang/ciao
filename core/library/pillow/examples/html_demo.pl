:- use_package(pillow).
:- use_module(library(file_utils), [string_to_file/2]).

main :- 
	HTML = [
            start,
	    comment("This document was generated from Prolog"),
	    title('Sample HTML document generated from Prolog'),
	    h1('Sample HTML document'),
	    --,
	    h2('Miscellaneous'),
	    "This is a ",ref('#label',it(reference))," to ",
	    bf(["another ",it(point)])," in this document.",$,
	    'Let''s be ',
	    image('http://localhost/images/smile.happy.gif',[alt=':-)']),
	    '!',
	    preformatted(['These lines',
	    [ 'are ',b(preformatted),'.'],
	      'See?']),
	    'We have left here a ',strong(free),' variable:',_X,$,
	    'This is in verbatim: ',
	    samp(verbatim('<NOTE> write "&amp;" to insert an &')),\\,
	    'But this is not:',
	    samp('<NOTE> write "&amp;" to insert an &'),$,
	    label(label,['This is the point referenced ',it('above.')]),
	    h2("Lists"),
	    h3(tt(itemize)),
	    itemize([one,two,['and ','3']]),
	    h3(tt(enumerate)),
	    enumerate([b(red),i(green),tt(blue)]),
	    h3(tt(description)),
	    description([
	      (one,b(two),'Description of one and two'),
	      (three,'Idem of three'),
	      ['This ',b(also)]]),
	    --,
	    ref('mailto:clip@dia.fi.upm.es',address('clip@dia.fi.upm.es')),
	    end
	], 
	html2terms(Str, HTML),
	string_to_file(Str, 'html_demo.html').
